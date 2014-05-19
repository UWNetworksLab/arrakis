/**
 * \file
 * \brief Deferred events (ie. timers)
 */

/*
 * Copyright (c) 2009, 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/waitset_chan.h>
#include <stdio.h>

// FIXME: why do I need quite so many dispatcher headers?
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish_kpi/dispatcher_shared.h>

#include "waitset_chan_priv.h"

// kludge: the kernel currently reports time in ms rather than us
#define SYSTIME_MULTIPLIER 1000

static void update_wakeup_disabled(dispatcher_handle_t dh)
{
    struct dispatcher_generic *dg = get_dispatcher_generic(dh);
    struct dispatcher_shared_generic *ds = get_dispatcher_shared_generic(dh);

    if (dg->deferred_events == NULL) {
        ds->wakeup = 0;
    } else {
        ds->wakeup = dg->deferred_events->time / SYSTIME_MULTIPLIER;
    }
}

/**
 * \brief Returns the system time when the current dispatcher was last dispatched
 */
systime_t get_system_time(void)
{
    dispatcher_handle_t dh = curdispatcher();
    struct dispatcher_shared_generic *ds = get_dispatcher_shared_generic(dh);
    return ds->systime * SYSTIME_MULTIPLIER;
}

void deferred_event_init(struct deferred_event *event)
{
    assert(event != NULL);
    waitset_chanstate_init(&event->waitset_state, CHANTYPE_DEFERRED);
    event->next = event->prev = NULL;
    event->time = 0;
}

/**
 * \brief Register a deferred event
 *
 * \param ws Waitset
 * \param delay Delay in microseconds
 * \param closure Event closure to execute
 * \param event Storage for event metadata
 */
errval_t deferred_event_register(struct deferred_event *event,
                                 struct waitset *ws, delayus_t delay,
                                 struct event_closure closure)
{
    errval_t err;

    dispatcher_handle_t dh = disp_disable();
    err = waitset_chan_register_disabled(ws, &event->waitset_state, closure);
    if (err_is_ok(err)) {
        struct dispatcher_generic *dg = get_dispatcher_generic(dh);

        // XXX: determine absolute time for event (ignoring time since dispatch!)
        event->time = get_system_time() + delay;

        // enqueue in sorted list of pending timers
        for (struct deferred_event *e = dg->deferred_events, *p = NULL; ;
             p = e, e = e->next) {
            if (e == NULL || e->time > event->time) {
                if (p == NULL) { // insert at head
                    assert(e == dg->deferred_events);
                    event->prev = NULL;
                    event->next = e;
                    if (e != NULL) {
                        e->prev = event;
                    }
                    dg->deferred_events = event;
                } else {
                    event->next = e;
                    event->prev = p;
                    p->next = event;
                    if (e != NULL) {
                        e->prev = event;
                    }
                }
                break;
            }
        }
    }

    update_wakeup_disabled(dh);

    disp_enable(dh);

    return err;
}

static void usleep_callback(void *val)
{
    bool *wakeup = val;
    *wakeup = true;
}

errval_t barrelfish_usleep(delayus_t delay)
{
    struct deferred_event dev;
    struct waitset ws;
    errval_t err;
    bool wakeup = false;

    deferred_event_init(&dev);
    waitset_init(&ws);
    err = deferred_event_register(&dev, &ws, delay,
                                  MKCLOSURE(usleep_callback, &wakeup));
    if(err_is_fail(err)) {
        return err;
    }

    while(!wakeup) {
        err = event_dispatch(&ws);
        if(err_is_fail(err)) {
            return err;
        }
    }

    return waitset_destroy(&ws);
}

/**
 * \brief Cancel a deferred event that has not yet fired
 */
errval_t deferred_event_cancel(struct deferred_event *event)
{
    enum ws_chanstate chanstate = event->waitset_state.state;
    dispatcher_handle_t dh = disp_disable();
    errval_t err = waitset_chan_deregister_disabled(&event->waitset_state);
    if (err_is_ok(err) && chanstate != CHAN_PENDING) {
        // remove from dispatcher queue
        struct dispatcher_generic *disp = get_dispatcher_generic(dh);
        if (event->prev == NULL) {
            disp->deferred_events = event->next;
        } else {
            event->prev->next = event->next;
        }
        if (event->next != NULL) {
            event->next->prev = event->prev;
        }
        update_wakeup_disabled(dh);
    }

    disp_enable(dh);
    return err;
}

static void periodic_event_handler(void *arg)
{
    struct periodic_event *e = arg;
    assert(e != NULL);

    // re-register (first, in case the handler wishes to cancel it)
    errval_t err = deferred_event_register(&e->de, e->waitset, e->period,
                           MKCLOSURE(periodic_event_handler, e));
    assert(err_is_ok(err));

    //printf("%s:%s: periodic handler %p\n", disp_name(), __func__, e->cl.handler);

    // run handler
    e->cl.handler(e->cl.arg);
}

/**
 * \brief Create a periodic event
 *
 * A periodic event will repeatedly run a closure at a fixed rate until cancelled.
 *
 * \param event Storage for event state
 * \param ws Waitset
 * \param period Period, in microseconds
 * \param closure Closure to run
 */
errval_t periodic_event_create(struct periodic_event *event, struct waitset *ws,
                               delayus_t period, struct event_closure closure)
{
    assert(event != NULL);
    deferred_event_init(&event->de);
    event->cl = closure;
    event->waitset = ws;
    event->period = period;
    return deferred_event_register(&event->de, ws, period,
                                   MKCLOSURE(periodic_event_handler, event));
}

/// Cancel a periodic event
errval_t periodic_event_cancel(struct periodic_event *event)
{
    return deferred_event_cancel(&event->de);
}


/// Trigger any pending deferred events, while disabled
void trigger_deferred_events_disabled(dispatcher_handle_t dh, systime_t now)
{
    struct dispatcher_generic *dg = get_dispatcher_generic(dh);
    struct deferred_event *e;
    errval_t err;

    now *= SYSTIME_MULTIPLIER;

    for (e = dg->deferred_events; e != NULL && e->time <= now; e = e->next) {
        err = waitset_chan_trigger_disabled(&e->waitset_state, dh);
        assert_disabled(err_is_ok(err));
    }
    dg->deferred_events = e;
    if (e != NULL) {
        e->prev = NULL;
    }
    update_wakeup_disabled(dh);
}
