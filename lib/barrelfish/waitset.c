/**
 * \file
 * \brief Waitset and low-level event handling mechanism
 *
 * A "wait set" is a collection of channels to wait on, much like an
 * FDSET in POSIX. There should be a default, static wait set for each
 * dispatcher. Threads which wait for events specify the wait set they
 * are waiting on.
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish/threads.h>
#include <barrelfish/dispatch.h>
#include "threads_priv.h"
#include "waitset_chan_priv.h"
#include <stdio.h>
#include <string.h>

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
#  include <barrelfish/ump_endpoint.h>
#endif

#if defined(__x86_64__) || defined(__i386__)
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>
static inline cycles_t cyclecount(void)
{
    return rdtsc();
}
#elif defined(__arm__) && defined(__gem5__)
/**
 * XXX: Gem5 doesn't support the ARM performance monitor extension
 * therefore we just poll a fixed number of times instead of using
 * cycle counts. POLL_COUNT is deliberately set to 42, guess why! ;)
 */
#define POLL_COUNT	42
#elif defined(__arm__)
#include <arch/arm/barrelfish_kpi/asm_inlines_arch.h>
static inline cycles_t cyclecount(void)
{
    return get_cycle_count();
}
#else
static inline cycles_t cyclecount(void)
{
    USER_PANIC("called on non-x86 architecture. why are we polling?");
    return 0;
}
#endif

// FIXME: bogus default value. need to measure this at boot time
#define WAITSET_POLL_CYCLES_DEFAULT 2000

/// Maximum number of cycles to spend polling channels before yielding CPU
cycles_t waitset_poll_cycles = WAITSET_POLL_CYCLES_DEFAULT;

/**
 * \brief Initialise a new waitset
 */
void waitset_init(struct waitset *ws)
{
    assert(ws != NULL);
    ws->pending = ws->polled = ws->idle = NULL;
    ws->waiting_threads = NULL;
    ws->polling = false;
}

/**
 * \brief Destroy a previously initialised waitset
 */
errval_t waitset_destroy(struct waitset *ws)
{
    assert(ws != NULL);

    // FIXME: do we want to support cancelling all the pending events/channels?
    if (ws->pending || ws->waiting_threads) {
        return LIB_ERR_WAITSET_IN_USE;
    }

    // remove idle and polled channels from waitset
    struct waitset_chanstate *chan, *next;
    for (chan = ws->idle; chan != NULL; chan = next) {
        next = chan->next;
        assert(chan->state == CHAN_IDLE);
        assert(chan->waitset == ws);
        chan->waitset = NULL;
        chan->next = chan->prev = NULL;

        if (next == ws->idle) {
            break;
        }
    }
    ws->idle = NULL;

    for (chan = ws->polled; chan != NULL; chan = next) {
        next = chan->next;
        assert(chan->state == CHAN_POLLED);
        assert(chan->waitset == ws);
        chan->waitset = NULL;
        chan->next = chan->prev = NULL;

        if (next == ws->polled) {
            break;
        }
    }
    ws->polled = NULL;

    return SYS_ERR_OK;
}

/// Returns a channel with a pending event on the given waitset, or NULL
static struct waitset_chanstate *get_pending_event_disabled(struct waitset *ws)
{
    // are there any pending events on the waitset?
    if (ws->pending == NULL) {
        return NULL;
    }

    // dequeue next pending event
    struct waitset_chanstate *chan = ws->pending;
    if (chan->next == chan) {
        assert_disabled(chan->prev == chan);
        ws->pending = NULL;
    } else {
        ws->pending = chan->next;
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
    }
#ifndef NDEBUG
    chan->prev = chan->next = NULL;
#endif

    // mark not pending
    assert_disabled(chan->state == CHAN_PENDING);
    chan->state = CHAN_UNREGISTERED;
    chan->waitset = NULL;

    return chan;
}

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
/**
 * \brief Poll an incoming UMP endpoint.
 * This is logically part of the UMP endpoint implementation, but placed here
 * for easier inlining.
 */
static inline void ump_endpoint_poll(struct waitset_chanstate *chan)
{
    /* XXX: calculate location of endpoint from waitset channel state */
    struct ump_endpoint *ep = (struct ump_endpoint *)
        ((char *)chan - offsetof(struct ump_endpoint, waitset_state));

    if (ump_endpoint_can_recv(ep)) {
        errval_t err = waitset_chan_trigger(chan);
        assert(err_is_ok(err)); // should not be able to fail
    }
}
#endif // CONFIG_INTERCONNECT_DRIVER_UMP

void arranet_polling_loop_proxy(void) __attribute__((weak));
void arranet_polling_loop_proxy(void)
{
    USER_PANIC("Network polling not available without Arranet!\n");
}

/// Helper function that knows how to poll the given channel, based on its type
static void poll_channel(struct waitset_chanstate *chan)
{
    switch (chan->chantype) {
#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    case CHANTYPE_UMP_IN:
        ump_endpoint_poll(chan);
        break;
#endif // CONFIG_INTERCONNECT_DRIVER_UMP

    case CHANTYPE_LWIP_SOCKET:
        arranet_polling_loop_proxy();
        break;

    default:
        assert(!"invalid channel type to poll!");
    }
}

// pollcycles_*: arch-specific implementation for polling.
//               Used by get_next_event().
//
//   pollcycles_reset()  -- return the number of pollcycles we want to poll for
//   pollcycles_update() -- update the pollcycles variable. This is needed for
//                          implementations where we don't have a cycle counter
//                          and we just count the number of polling operations
//                          performed
//   pollcycles_expired() -- check if pollcycles have expired
//
// We might want to move them to architecture-specific files, and/or create a
// cleaner interface. For now, I just wanted to keep them out of
// get_next_event()

#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 3
static __attribute__((noinline, unused))
#else
static inline
#endif
cycles_t pollcycles_reset(void)
{
    cycles_t pollcycles;
#if defined(__arm__) && !defined(__gem5__)
    reset_cycle_counter();
    pollcycles = waitset_poll_cycles;
#elif defined(__arm__) && defined(__gem5__)
    pollcycles = 0;
#else
    pollcycles = cyclecount() + waitset_poll_cycles;
#endif
    return pollcycles;
}

#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 3
static __attribute__((noinline, unused))
#else
static inline
#endif
cycles_t pollcycles_update(cycles_t pollcycles)
{
    cycles_t ret = pollcycles;
    #if defined(__arm__) && defined(__gem5__)
    ret++;
    #endif
    return ret;
}

#if defined(__ARM_ARCH_7A__) && defined(__GNUC__) \
	&& __GNUC__ == 4 && __GNUC_MINOR__ <= 6 && __GNUC_PATCHLEVEL__ <= 3
static __attribute__((noinline, unused))
#else
static inline
#endif
bool pollcycles_expired(cycles_t pollcycles)
{
    bool ret;
    #if defined(__arm__) && !defined(__gem5__)
    ret = (cyclecount() > pollcycles || is_cycle_counter_overflow());
    #elif defined(__arm__) && defined(__gem5__)
    ret = pollcycles >= POLL_COUNT;
    #else
    ret = cyclecount() > pollcycles;
    #endif
    return ret;
}

static errval_t get_next_event_debug(struct waitset *ws,
        struct event_closure *retclosure, bool debug)
{
    struct waitset_chanstate *chan;
    bool was_polling = false;
    cycles_t pollcycles;

    assert(ws != NULL);
    assert(retclosure != NULL);

    // unconditionally disable ourselves and check for events
    // if we decide we have to start polling, we'll jump back up here
    goto check_for_events;

    /* ------------ POLLING LOOP; RUNS WHILE ENABLED ------------ */
polling_loop:
    was_polling = true;
    assert(ws->polling); // this thread is polling
    // get the amount of cycles we want to poll for
    pollcycles = pollcycles_reset();

    // while there are no pending events, poll channels
    while (ws->polled != NULL && ws->pending == NULL) {
        struct waitset_chanstate *nextchan = NULL;
        // NB: Polling policy is to return as soon as a pending event
        // appears, not bother looking at the rest of the polling queue
        for (chan = ws->polled;
             chan != NULL && chan->waitset == ws && chan->state == CHAN_POLLED
                 && ws->pending == NULL;
             chan = nextchan) {

            nextchan = chan->next;
            poll_channel(chan);
            // update pollcycles
            pollcycles = pollcycles_update(pollcycles);
            // yield the thread if we exceed the cycle count limit
            if (ws->pending == NULL && pollcycles_expired(pollcycles)) {
                if (debug) {
                if (strcmp(disp_name(), "netd") != 0) {
                    // Print the callback trace so that we know which call is leading
                    // the schedule removal and
                    printf("%s: callstack: %p %p %p %p\n", disp_name(),
                            __builtin_return_address(0),
                            __builtin_return_address(1),
                            __builtin_return_address(2),
                            __builtin_return_address(3));
                }

                }
                thread_yield();
                pollcycles = pollcycles_reset();
            }
        }

        // ensure that we restart polling from the place we left off here,
        // if the next channel is a valid one
        if (nextchan != NULL && nextchan->waitset == ws
            && nextchan->state == CHAN_POLLED) {
            ws->polled = nextchan;
        }
    }

    /* ------------ STATE MACHINERY; RUNS WHILE DISABLED ------------ */
check_for_events: ;
    dispatcher_handle_t handle = disp_disable();

    // are there any pending events on the waitset?
    chan = get_pending_event_disabled(ws);
    if (chan != NULL) {
        // if we need to poll, and we have a blocked thread, wake it up to do so
        if (was_polling && ws->polled != NULL && ws->waiting_threads != NULL) {
            // start a blocked thread polling
            struct thread *t;
            t = thread_unblock_one_disabled(handle, &ws->waiting_threads, NULL);
            assert_disabled(t == NULL); // shouldn't see a remote thread
        } else if (was_polling) {
            // I'm stopping polling, and there is nobody else
            assert_disabled(ws->polling);
            ws->polling = false;
        }
        disp_enable(handle);

        *retclosure = chan->closure;
        return SYS_ERR_OK;
    }

    // If we got here and there are channels to poll but no-one is polling,
    // then either we never polled, or we lost a race on the channel we picked.
    // Either way, we'd better start polling again.
    if (ws->polled != NULL && (was_polling || !ws->polling)) {
        if (!was_polling) {
            ws->polling = true;
        }
        disp_enable(handle);
        goto polling_loop;
    }

    // otherwise block awaiting an event
    chan = thread_block_disabled(handle, &ws->waiting_threads);

    if (chan == NULL) {
        // not a real event, just a wakeup to get us to start polling!
        assert(ws->polling);
        goto polling_loop;
    } else {
        *retclosure = chan->closure;
        return SYS_ERR_OK;
    }
}

/**
 * \brief Wait for (block) and return next event on given waitset
 *
 * Wait until something happens, either activity on some channel, or a deferred
 * call, and then return the corresponding closure. This is the core of the
 * event-handling system.
 *
 * \param ws Waitset
 * \param retclosure Pointer to storage space for returned event closure
 */
errval_t get_next_event(struct waitset *ws, struct event_closure *retclosure)
{
    return get_next_event_debug(ws, retclosure, false);
}



/**
 * \brief Return next event on given waitset, if one is already pending
 *
 * This is essentially a non-blocking variant of get_next_event(). It should be
 * used with great care, to avoid the creation of busy-waiting loops.
 *
 * \param ws Waitset
 * \param retclosure Pointer to storage space for returned event closure
 *
 * \returns LIB_ERR_NO_EVENT if nothing is pending
 */
errval_t check_for_event(struct waitset *ws, struct event_closure *retclosure)
{
    struct waitset_chanstate *chan;
    int pollcount = 0;

    assert(ws != NULL);
    assert(retclosure != NULL);

 recheck: ;
    // are there any pending events on the waitset?
    dispatcher_handle_t handle = disp_disable();
    chan = get_pending_event_disabled(ws);
    disp_enable(handle);
    if (chan != NULL) {
        *retclosure = chan->closure;
        return SYS_ERR_OK;
    }

    // if there are no pending events, poll all channels once
    if (ws->polled != NULL && pollcount++ == 0) {
        for (chan = ws->polled;
             chan != NULL && chan->waitset == ws && chan->state == CHAN_POLLED;
             chan = chan->next) {

            poll_channel(chan);
            if (ws->pending != NULL) {
                goto recheck;
            }

            if (chan->next == ws->polled) { // reached the start of the queue
                break;
            }
        }
    }

    return LIB_ERR_NO_EVENT;
}

/**
 * \brief Wait for (block) and dispatch next event on given waitset
 *
 * Wait until something happens, either activity on some channel, or deferred
 * call, and then call the corresponding closure.
 *
 * \param ws Waitset
 */

errval_t event_dispatch(struct waitset *ws)
{
    struct event_closure closure;
    errval_t err = get_next_event(ws, &closure);
    if (err_is_fail(err)) {
        return err;
    }

    assert(closure.handler != NULL);
    closure.handler(closure.arg);
    return SYS_ERR_OK;
}

errval_t event_dispatch_debug(struct waitset *ws)
{
    struct event_closure closure;
    errval_t err = get_next_event_debug(ws, &closure, false);
    if (err_is_fail(err)) {
        return err;
    }

    assert(closure.handler != NULL);
//    printf("%s: event_dispatch: %p: \n", disp_name(), closure.handler);


    closure.handler(closure.arg);
    return SYS_ERR_OK;
}

/**
 * \brief check and dispatch next event on given waitset
 *
 * Check if there is any pending activity on some channel, or deferred
 * call, and then call the corresponding closure.
 *
 * Do not wait!  In case of no pending events, return err LIB_ERR_NO_EVENT.
 *
 * \param ws Waitset
 */
errval_t event_dispatch_non_block(struct waitset *ws)
{
    struct event_closure closure;
    errval_t err = check_for_event(ws, &closure);

    if (err_is_fail(err)) {
        return err;
    }

    assert(closure.handler != NULL);
    closure.handler(closure.arg);
    return SYS_ERR_OK;
}


/**
 * \privatesection
 * "Private" functions that are called only by the channel implementations
 */

/**
 * \brief Initialise per-channel waitset state
 *
 * \param chan Channel state
 * \param chantype Channel type
 */
void waitset_chanstate_init(struct waitset_chanstate *chan,
                            enum ws_chantype chantype)
{
    assert(chan != NULL);
    chan->waitset = NULL;
    chan->chantype = chantype;
    chan->state = CHAN_UNREGISTERED;
#ifndef NDEBUG
    chan->prev = chan->next = NULL;
#endif
}

/**
 * \brief Destroy previously-initialised per-channel waitset state
 * \param chan Channel state
 */
void waitset_chanstate_destroy(struct waitset_chanstate *chan)
{
    assert(chan != NULL);
    if (chan->waitset != NULL) {
        errval_t err = waitset_chan_deregister(chan);
        assert(err_is_ok(err)); // can't fail if registered
    }
}

/**
 * \brief Register a closure to be called when a channel is triggered
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time.
 * This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register_disabled(struct waitset *ws,
                                        struct waitset_chanstate *chan,
                                        struct event_closure closure)
{
    if (chan->waitset != NULL) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    chan->waitset = ws;

    // channel must not already be registered!
    assert_disabled(chan->next == NULL && chan->prev == NULL);
    assert_disabled(chan->state == CHAN_UNREGISTERED);

    // this is probably insane! :)
    assert_disabled(closure.handler != NULL);

    // store closure
    chan->closure = closure;

    // enqueue this channel on the waitset's queue of idle channels
    if (ws->idle == NULL) {
        chan->next = chan->prev = chan;
        ws->idle = chan;
    } else {
        chan->next = ws->idle;
        chan->prev = chan->next->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }
    chan->state = CHAN_IDLE;

    return SYS_ERR_OK;
}

/**
 * \brief Register a closure on a channel, and mark the channel as polled
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time. Additionally, mark the channel as polled.
 * This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_register_polled_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle)
{
    if (chan->waitset != NULL) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    chan->waitset = ws;

    // channel must not already be registered!
    assert_disabled(chan->next == NULL && chan->prev == NULL);
    assert_disabled(chan->state == CHAN_UNREGISTERED);

    // store closure
    chan->closure = closure;

    // enqueue this channel on the waitset's queue of polled channels
    if (ws->polled == NULL) {
        chan->next = chan->prev = chan;
        ws->polled = chan;
        if (ws->waiting_threads != NULL && !ws->polling) {
            // start a blocked thread polling
            ws->polling = true;
            struct thread *t;
            t = thread_unblock_one_disabled(handle, &ws->waiting_threads, NULL);
            assert_disabled(t == NULL); // shouldn't see a remote thread: waitsets are per-dispatcher
        }
    } else {
        chan->next = ws->polled;
        chan->prev = chan->next->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }
    chan->state = CHAN_POLLED;

    return SYS_ERR_OK;
}

/**
 * \brief Register a closure to be called when a channel is triggered
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time.
 * This function must only be called when enabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register(struct waitset *ws, struct waitset_chanstate *chan,
                               struct event_closure closure)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_register_disabled(ws, chan, closure);
    disp_enable(handle);
    return err;
}

/**
 * \brief Register a closure on a channel, and mark the channel as polled
 *
 * In the Future, call the closure on a thread associated with the waitset
 * when the channel is triggered. Only one closure may be registered per
 * channel state at any one time. Additionally, mark the channel as polled.
 * This function must only be called when enabled. It is equivalent to
 * calling waitset_chan_register() followed by waitset_chan_start_polling().
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_register_polled(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_register_polled_disabled(ws, chan, closure, handle);
    disp_enable(handle);
    return err;
}

/**
 * \brief Mark an idle channel as polled
 *
 * The given channel will periodically have its poll function called.
 * The channel must already be registered.
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_start_polling(struct waitset_chanstate *chan)
{
    errval_t err = SYS_ERR_OK;

    dispatcher_handle_t handle = disp_disable();

    struct waitset *ws = chan->waitset;
    if (ws == NULL) {
        err = LIB_ERR_CHAN_NOT_REGISTERED;
        goto out;
    }

    assert(chan->state != CHAN_UNREGISTERED);
    if (chan->state != CHAN_IDLE) {
        goto out; // no-op if polled or pending
    }

    // remove from idle queue
    if (chan->next == chan) {
        assert(chan->prev == chan);
        assert(ws->idle == chan);
        ws->idle = NULL;
    } else {
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
        if (ws->idle == chan) {
            ws->idle = chan->next;
        }
    }

    // enqueue on polled queue
    if (ws->polled == NULL) {
        ws->polled = chan;
        chan->next = chan->prev = chan;
        if (ws->waiting_threads != NULL && !ws->polling) {
            // start a blocked thread polling
            ws->polling = true;
            struct thread *t;
            t = thread_unblock_one_disabled(handle, &ws->waiting_threads, NULL);
            assert(t == NULL); // shouldn't see a remote thread: waitsets are per-dispatcher
        }
    } else {
        chan->next = ws->polled;
        chan->prev = ws->polled->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }
    chan->state = CHAN_POLLED;

out:
    disp_enable(handle);
    return err;
}

/**
 * \brief Stop polling the given channel, making it idle again
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_stop_polling(struct waitset_chanstate *chan)
{
    errval_t err = SYS_ERR_OK;

    dispatcher_handle_t handle = disp_disable();

    struct waitset *ws = chan->waitset;
    if (ws == NULL) {
        err = LIB_ERR_CHAN_NOT_REGISTERED;
        goto out;
    }

    assert(chan->state != CHAN_UNREGISTERED);
    if (chan->state != CHAN_POLLED) {
        goto out; // no-op if idle or pending
    }

    // remove from polled queue
    if (chan->next == chan) {
        assert(chan->prev == chan);
        assert(ws->polled == chan);
        ws->polled = NULL;
    } else {
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
        if (ws->polled == chan) {
            ws->polled = chan->next;
        }
    }

    // enqueue on idle queue
    if (ws->idle == NULL) {
        ws->idle = chan;
        chan->next = chan->prev = chan;
    } else {
        chan->next = ws->idle;
        chan->prev = ws->idle->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }
    chan->state = CHAN_IDLE;

out:
    disp_enable(handle);
    return err;
}

/**
 * \brief Cancel a previous callback registration
 *
 * Remove the registration for a callback on the given channel.
 * This function must only be called when disabled.
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_deregister_disabled(struct waitset_chanstate *chan)
{
    assert_disabled(chan != NULL);
    struct waitset *ws = chan->waitset;
    if (ws == NULL) {
        return LIB_ERR_CHAN_NOT_REGISTERED;
    }

    // remove this channel from the queue in which it is waiting
    chan->waitset = NULL;
    assert_disabled(chan->next != NULL && chan->prev != NULL);

    if (chan->next == chan) {
        // only thing in the list: must be the head
        assert_disabled(chan->prev == chan);
        switch (chan->state) {
        case CHAN_IDLE:
            assert_disabled(chan == ws->idle);
            ws->idle = NULL;
            break;

        case CHAN_POLLED:
            assert_disabled(chan == ws->polled);
            ws->polled = NULL;
            break;

        case CHAN_PENDING:
            assert_disabled(chan == ws->pending);
            ws->pending = NULL;
            break;

        default:
            assert_disabled(!"invalid channel state in deregister");
        }
    } else {
        assert_disabled(chan->prev != chan);
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
        switch (chan->state) {
        case CHAN_IDLE:
            if (chan == ws->idle) {
                ws->idle = chan->next;
            }
            break;

        case CHAN_POLLED:
            if (chan == ws->polled) {
                ws->polled = chan->next;
            }
            break;

        case CHAN_PENDING:
            if (chan == ws->pending) {
                ws->pending = chan->next;
            }
            break;

        default:
            assert_disabled(!"invalid channel state in deregister");
        }
    }
    chan->state = CHAN_UNREGISTERED;

#ifndef NDEBUG
    chan->prev = chan->next = NULL;
#endif

    return SYS_ERR_OK;
}

/**
 * \brief Cancel a previous callback registration
 *
 * Remove the registration for a callback on the given channel.
 * This function must only be called when enabled.
 *
 * \param chan Waitset's per-channel state
 */
errval_t waitset_chan_deregister(struct waitset_chanstate *chan)
{
    dispatcher_handle_t handle = disp_disable();
    errval_t err = waitset_chan_deregister_disabled(chan);
    disp_enable(handle);
    return err;
}

/**
 * \brief Migrate callback registrations to a new waitset.
 *
 * \param chan Old waitset's per-channel state to migrate
 * \param new_ws New waitset to migrate to
 */
void waitset_chan_migrate(struct waitset_chanstate *chan,
                          struct waitset *new_ws)
{
    struct waitset *ws = chan->waitset;

    // Only when registered
    if(ws == NULL) {
        return;
    }

    switch(chan->state) {
    case CHAN_IDLE:
        if (chan->next == chan) {
            assert(chan->prev == chan);
            assert(ws->idle == chan);
            ws->idle = NULL;
        } else {
            chan->prev->next = chan->next;
            chan->next->prev = chan->prev;
            if (ws->idle == chan) {
                ws->idle = chan->next;
            }
        }

        if (new_ws->idle == NULL) {
            new_ws->idle = chan;
            chan->next = chan->prev = chan;
        } else {
            chan->next = new_ws->idle;
            chan->prev = new_ws->idle->prev;
            chan->next->prev = chan;
            chan->prev->next = chan;
        }
        break;

    case CHAN_POLLED:
        if (chan->next == chan) {
            assert(chan->prev == chan);
            assert(ws->polled == chan);
            ws->polled = NULL;
        } else {
            chan->prev->next = chan->next;
            chan->next->prev = chan->prev;
            if (ws->polled == chan) {
                ws->polled = chan->next;
            }
        }

        if (new_ws->polled == NULL) {
            new_ws->polled = chan;
            chan->next = chan->prev = chan;
        } else {
            chan->next = new_ws->polled;
            chan->prev = new_ws->polled->prev;
            chan->next->prev = chan;
            chan->prev->next = chan;
        }
        break;

    case CHAN_PENDING:
        if (chan->next == chan) {
            assert(chan->prev == chan);
            assert(ws->pending == chan);
            ws->pending = NULL;
        } else {
            chan->prev->next = chan->next;
            chan->next->prev = chan->prev;
            if (ws->pending == chan) {
                ws->pending = chan->next;
            }
        }

        if (new_ws->pending == NULL) {
            new_ws->pending = chan;
            chan->next = chan->prev = chan;
        } else {
            chan->next = new_ws->pending;
            chan->prev = new_ws->pending->prev;
            chan->next->prev = chan;
            chan->prev->next = chan;
        }
        break;

    case CHAN_UNREGISTERED:
        // Do nothing
        break;
    }

    // Remember new waitset association
    chan->waitset = new_ws;
}

/**
 * \brief Trigger an event callback on a channel
 *
 * Marks the given channel as having a pending event, causing some future call
 * to get_next_event() to return the registered closure.
 * This function must only be called when disabled.
 *
 * \param chan Waitset's per-channel state
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger_disabled(struct waitset_chanstate *chan,
                                       dispatcher_handle_t handle)
{
    assert_disabled(chan != NULL);
    struct waitset *ws = chan->waitset;
    assert_disabled(ws != NULL);
    assert_disabled(chan->prev != NULL && chan->next != NULL);

    // no-op if already pending
    if (chan->state == CHAN_PENDING) {
        return SYS_ERR_OK;
    }

    // remove from previous queue (either idle or polled)
    if (chan->next == chan) {
        assert_disabled(chan->prev == chan);
        if (chan->state == CHAN_IDLE) {
            assert_disabled(ws->idle == chan);
            ws->idle = NULL;
        } else {
            assert_disabled(chan->state == CHAN_POLLED);
            assert_disabled(ws->polled == chan);
            ws->polled = NULL;
        }
    } else {
        chan->prev->next = chan->next;
        chan->next->prev = chan->prev;
        if (chan->state == CHAN_IDLE) {
            if (ws->idle == chan) {
                ws->idle = chan->next;
            }
        } else {
            assert_disabled(chan->state == CHAN_POLLED);
            if (ws->polled == chan) {
                ws->polled = chan->next;
            }
        }
    }

    // is there a thread blocked on this waitset? if so, awaken it with the event
    if (ws->waiting_threads != NULL) {
        chan->waitset = NULL;
#ifndef NDEBUG
        chan->prev = chan->next = NULL;
#endif
        chan->state = CHAN_UNREGISTERED;
        struct thread *t;
        t = thread_unblock_one_disabled(handle, &ws->waiting_threads, chan);
        assert_disabled(t == NULL);
        return SYS_ERR_OK;
    }

    // else mark channel pending and move to end of pending event queue
    chan->state = CHAN_PENDING;
    if (ws->pending == NULL) {
        ws->pending = chan;
        chan->next = chan->prev = chan;
    } else {
        chan->next = ws->pending;
        chan->prev = ws->pending->prev;
        assert_disabled(ws->pending->next != NULL);
        assert_disabled(ws->pending->prev != NULL);
        assert_disabled(chan->prev != NULL);
        chan->next->prev = chan;
        chan->prev->next = chan;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Trigger an event callback on a channel
 *
 * Marks the given channel as having a pending event, causing some future call
 * to get_next_event() to return the registered closure.
 * This function must only be called when enabled.
 *
 * \param chan Waitset's per-channel state
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger(struct waitset_chanstate *chan)
{
    dispatcher_handle_t disp = disp_disable();
    errval_t err = waitset_chan_trigger_disabled(chan, disp);
    disp_enable(disp);
    return err;
}

/**
 * \brief Trigger a specific event callback on an unregistered channel
 *
 * This function is equivalent to waitset_chan_register_disabled() immediately
 * followed by waitset_chan_trigger_disabled(), but avoids unneccessary queue
 * manipulation. This function must only be called when disabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 * \param disp Current dispatcher pointer
 */
errval_t waitset_chan_trigger_closure_disabled(struct waitset *ws,
                                               struct waitset_chanstate *chan,
                                               struct event_closure closure,
                                               dispatcher_handle_t handle)
{
    assert_disabled(chan != NULL);
    assert_disabled(ws != NULL);

    // check if already registered
    if (chan->waitset != NULL || chan->state != CHAN_UNREGISTERED) {
        return LIB_ERR_CHAN_ALREADY_REGISTERED;
    }

    assert_disabled(chan->prev == NULL && chan->next == NULL);

    // set closure
    chan->closure = closure;

    // is there a thread blocked on this waitset? if so, awaken it with the event
    if (ws->waiting_threads != NULL) {
        struct thread *t;
        t = thread_unblock_one_disabled(handle, &ws->waiting_threads, chan);
        assert_disabled(t == NULL);
        return SYS_ERR_OK;
    }

    // mark channel pending and place on end of pending event queue
    chan->waitset = ws;
    chan->state = CHAN_PENDING;
    if (ws->pending == NULL) {
        ws->pending = chan;
        chan->next = chan->prev = chan;
    } else {
        chan->next = ws->pending;
        chan->prev = ws->pending->prev;
        chan->next->prev = chan;
        chan->prev->next = chan;
    }

    assert(ws->pending->prev != NULL && ws->pending->next != NULL);

    return SYS_ERR_OK;
}


/**
 * \brief Trigger a specific event callback on an unregistered channel
 *
 * This function is equivalent to waitset_chan_register()
 * followed by waitset_chan_trigger(), but avoids unneccessary queue
 * manipulation. This function must only be called when enabled.
 *
 * \param ws Waitset
 * \param chan Waitset's per-channel state
 * \param closure Event handler
 */
errval_t waitset_chan_trigger_closure(struct waitset *ws,
                                      struct waitset_chanstate *chan,
                                      struct event_closure closure)
{
    dispatcher_handle_t disp = disp_disable();
    errval_t err = waitset_chan_trigger_closure_disabled(ws, chan, closure, disp);
    disp_enable(disp);
    return err;
}
