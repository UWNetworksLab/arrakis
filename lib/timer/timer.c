/**
 * \file
 * \brief Generic timer library.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <timer/timer.h>
#include <if/timer_defs.h>


/*****************************************************************
 * Debug printer
 *****************************************************************/
#if defined(TIMER_CLIENT_DEBUG) || defined(GLOBAL_DEBUG)
#define TIMER_DEBUG(x...) printf("timer_client: " x)
#else
#define TIMER_DEBUG(x...) ((void)0)
#endif

/*************************************************************//**
 * \defGroup LocalStates Local states
 *
 * @{
 * 
 ****************************************************************/

/**
 * \brief 
 *
 * The timer priority queue (sorted by expiration time)
 *
 */
static struct timer *timer_queue = NULL;

/**
 * \brief 
 *
 * Timers pending insertion into the queue (sorted by expiration time)
 *
 */
static struct timer *timers_pending_insertion;

/**
 * \brief 
 *
 * Did we already request the remaining time but not yet see a reply?
 *
 */
static bool remaining_time_requested;

/**
 * \brief 
 *
 * Mutex protecting timer library state.
 *
 */
static struct thread_mutex timer_mutex = THREAD_MUTEX_INITIALIZER;


/**
 * \brief 
 *
 * Are we connected to the timer driver?
 *
 */
static bool timer_connected = false;

static struct timer_binding *the_timer_binding;

/*
 * @}
 */


/*************************************************************//**
 * \defGroup MessageHandlers Message Handlers
 *
 * (...)
 *
 * @{
 *
 ****************************************************************/

static void timer_wakeup(void);
static void timer_insert_pending(uint64_t time);

/**
 * \brief Handle a wakeup event from the timer driver.
 */
static void wakeup_handler(struct timer_binding *b)
{
    thread_mutex_lock(&timer_mutex);
    // top timer just expired: insert anything that was pending
    timer_insert_pending(0);
    remaining_time_requested = false;
    timer_wakeup();
    thread_mutex_unlock(&timer_mutex);
}

/**
 * \brief Handle a remaining time message from the timer driver.
 *
 */
static void remaining_handler(struct timer_binding *b, uint64_t time)
{
    thread_mutex_lock(&timer_mutex);
    if (remaining_time_requested) {
        timer_insert_pending(time);
        remaining_time_requested = false;
    }
    thread_mutex_unlock(&timer_mutex);
}

/*
 * @}
 */

/**************************************************************//**
 * \defGroup FlounderVtables Flounder vtable
 *
 * @{
 *
 *****************************************************************/


static struct timer_rx_vtbl timer_rx_vtbl = {
    .wakeup = wakeup_handler,
    .remaining = remaining_handler,
};

/*
 * @}
 */


/*************************************************************//**
 * \defGroup ConnectionHandlers Connection Handlers
 *
 * @{
 *
 ****************************************************************/

/**
 * \brief Connection service logic
 *
 * This handler is called when the connection to the timer server is
 * established.
 *
 */
static void bind_cb(void *st, errval_t err, struct timer_binding *b)
{
    TIMER_DEBUG("bind_cb: connection ready\n");

    thread_mutex_lock(&timer_mutex);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = timer_rx_vtbl;

    if (timer_queue != NULL) {
        // program timer for first timeout
        TIMER_DEBUG("connection_logic: set timer for %lu\n", timer_queue->expires);
        err = b->tx_vtbl.set_timeout(b, NOP_CONT, timer_queue->expires);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to set timeout on new connection");
            abort(); // FIXME: what else can we do here?
        }
    }

    assert(timers_pending_insertion == NULL);
    timer_connected = true;
    the_timer_binding = b;

    thread_mutex_unlock(&timer_mutex);

    TIMER_DEBUG("bind_cb: done\n");
}


/*
 * @}
 */


/*************************************************************//**
 * \defGroup TimerQueueAPI Timer Queue API
 * 
 * (...)
 *
 * @{
 * 
 ****************************************************************/


/**
 * \brief Add timer 'q' to the timer queue.
 *
 * \param q     Pointer to timer to add to the queue.
 */
static void timer_queue_add(struct timer *q)
{
    assert(q->next == NULL);
    assert(q->prev == NULL);

    TIMER_DEBUG("timer_queue_add: called for %p of duration %lu, expiring %lu\n",
    		q, q->duration, q->expires);

    for(struct timer *i = timer_queue; i != NULL; i = i->next) {
    	TIMER_DEBUG("timer_queue_add: loop for timer %p duration %lu expiring %lu \n",
    			i, i->duration, i->expires);
        // Sort into priority queue
        if(q->expires < i->expires) {
            i->expires -= q->expires;

            q->prev = i->prev;
            q->next = i;
            if(i->prev != NULL) {
                i->prev->next = q;
            } else {
                assert(timer_queue == i);
                timer_queue = q;
            }
            i->prev = q;

            TIMER_DEBUG("timer_queue_add: terminated. %p inserted before %p\n",
            		q, i);

            return;
        } else {
            q->expires -= i->expires;
            TIMER_DEBUG("timer_queue_add: reducing expire for %p by %lu,  new exp %lu\n",
            		q, i->expires, q->expires);

        }

        // Add at queue end
        if(i->next == NULL) {
            i->next = q;
            q->prev = i;
            q->next = NULL;

            TIMER_DEBUG("timer_queue_add: terminated (eoq)\n");

            return;
        }
    }

    // Add as only element in queue
    assert(timer_queue == NULL);
    timer_queue = q;
    q->prev = NULL;
    q->next = NULL;

    TIMER_DEBUG("timer_queue_add: terminated (single)\n");
}

/**
 * \brief Remove timer 'q' from the timer queue.
 *
 * \param q     Pointer to timer to remove.
 */
static void timer_queue_remove(struct timer *q)
{
    TIMER_DEBUG("timer_queue_remove: called\n");

    if (q->prev != NULL) {
        q->prev->next = q->next;
    }

    if (q->next != NULL) {
        q->next->expires += q->expires;
        q->next->prev = q->prev;
    }

    if (q->prev == NULL) {
        if (q == timer_queue) {
            timer_queue = q->next;
        } else if (q == timers_pending_insertion) {
            timers_pending_insertion = q->next;
        } else {
            assert(!"invalid timer queue");
        }
    }

    q->next = NULL;
    q->prev = NULL;

    TIMER_DEBUG("timer_queue_remove: terminated\n");
}

/*
 * @}
 */


/**
 * \brief Handle wakeup event from timer driver.
 *
 * This function maintains the timer queue upon timer expiry.
 * It calls appropriate callback functions and re-arms periodic timers.
 */
static void timer_wakeup(void)
{
    errval_t err;
    struct timer *periodic = NULL;

    TIMER_DEBUG("timer_wakeup: called\n");

    /* wakeup the top timer on the queue, and any with the same expiry */
    if (timer_queue != NULL) {
        timer_queue->expires = 0;
    }

    for(struct timer *t = timer_queue, *next; t != NULL; t = next) {
        // Only process expired timers
        if (t->expires != 0) {
            break;
        }

        // Remove from queue, saving next
        next = t->next;
        timer_queue_remove(t);

        // Call callback if registered
        if(t->callback != NULL) {
        	TIMER_DEBUG("timer_wakeup: calling callback\n");
            t->callback(t, t->data);
        } else {
            TIMER_DEBUG("timer_wakeup: no callback\n");
        }

        if (t->periodic) {
            // Update period and put into local periodic list for re-insertion
            TIMER_DEBUG("timer_wakeup: periodic timer %lu ticks\n", t->duration);
            t->expires = t->duration;
            t->next = periodic;
            periodic = t;
        }
    }

    // check that we got everything
    assert(timer_queue == NULL || timer_queue->expires > 0);

    /* re-insert periodic timers */
    for (struct timer *t = periodic, *next; t != NULL; t = next) {
        next = t->next;
        t->next = NULL;
        timer_queue_add(t);
    }

    /* set the next timer in the queue */
    if (timer_queue != NULL) {
        TIMER_DEBUG("timer_wakeup: new timeout %lu\n", timer_queue->expires);
        err = the_timer_binding->tx_vtbl.set_timeout(the_timer_binding, NOP_CONT,
                                                      timer_queue->expires);
        // FIXME: what should I do with this error? we need an errback :(
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to send timer message");
            abort();
        }
    }

    TIMER_DEBUG("timer_wakeup: terminated\n");
}


/**
 * \brief Insert pending timers given the remaining time on the top timer.
 *
 * This function is given the remaining time of the currently-running timer,
 * which allows us to insert new pending timeouts into the queue with the
 * correct expiry times.
 */
static void timer_insert_pending(uint64_t remaining)
{
    /* update expiry of top timer */
    if (timer_queue != NULL && timer_queue->expires > remaining) {
        timer_queue->expires = remaining;
    }

    /* add pending timers to queue */
    for (struct timer *t = timers_pending_insertion, *next; t != NULL; t = next) {
        next = t->next;
        t->next = NULL;
        timer_queue_add(t);
    }

    timers_pending_insertion = NULL;
}


/*************************************************************//**
 * \defGroup TimerAPI Timer API
 * 
 * (...)
 *
 * @{
 * 
 ****************************************************************/


/**
 * \brief Create new timer.
 *
 * \return Pointer to new timer.
 */
struct timer *timer_new(void)
{
    TIMER_DEBUG("timer_new: called\n");

    struct timer *t = malloc(sizeof(struct timer));
    assert(t != NULL);

    t->next = NULL;
    t->prev = NULL;
    t->expires = 0;
    t->duration = 0;
    t->periodic = false;
    t->callback = NULL;
    t->data = NULL;

    TIMER_DEBUG("timer_new: [%p] terminated\n", t);

    return t;
}

/**
 * \brief Start timer.
 *
 * This sets off a timer. It will start running for the duration set
 * through timer_set_duration().
 *
 * \param timer Pointer to timer to start.
 */
void timer_start(struct timer *timer)
{
    errval_t err = SYS_ERR_OK;

    TIMER_DEBUG("timer_start: called for [%p]\n", timer);

    assert(timer->next == NULL);
    assert(timer->prev == NULL);
    assert(timer->duration > 0);

    timer->expires = timer->duration;

    TIMER_DEBUG("timer_start: expires in %ld\n", timer->expires);

    thread_mutex_lock_nested(&timer_mutex); // might be in a callback

    // if we're not connected yet, just queue it up
    // the timers will start running when we connect
    if (!timer_connected) {
        TIMER_DEBUG("timer_start: queuing while we await the connection\n");
        timer_queue_add(timer);
    } else if (timer_queue == NULL) {
        TIMER_DEBUG("timer_start: inserting and setting timer\n");

        // insert in queue directly
        timer_queue_add(timer);
        err = the_timer_binding->tx_vtbl.set_timeout(the_timer_binding, NOP_CONT,
                                                      timer->expires);
    } else {
        TIMER_DEBUG("timer_start: queuing for insertion\n");

        // add to pending insertion queue, send off request for the remaining time
        // (if we haven't already)
        if (!remaining_time_requested) {
            err = the_timer_binding->tx_vtbl.get_remaining(the_timer_binding, NOP_CONT);
            remaining_time_requested = true;
        }

        timer->next = timers_pending_insertion;
        timers_pending_insertion = timer;
    }

    thread_mutex_unlock(&timer_mutex);

    // FIXME: no way to return an error from this silly API!
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send timer message");
        abort();
    }

    TIMER_DEBUG("timer_start: terminated\n");

}

/**
 * \brief Stop timer.
 *
 * This stops a running timer.
 *
 * \param timer Pointer to timer to stop.
 */
void timer_stop(struct timer *timer)
{
    errval_t err = SYS_ERR_OK;

    thread_mutex_lock_nested(&timer_mutex); // might be in a callback

    if(!timer_connected){
        timer_queue_remove(timer);
    }

    // remove from queue if present
    if (timer_is_running(timer)) {
        if (timer_queue == timer) {
            // top timer on queue, update driver
            if (timer->next == NULL) {
                TIMER_DEBUG("removing only timer on queue: cancel HW timer\n");
                err = the_timer_binding->tx_vtbl.cancel_timeout(the_timer_binding,
                                                                NOP_CONT);
            } else if (timer->next->expires > 0) {
                TIMER_DEBUG("removing top timer on queue: extend HW timer\n");
            assert(the_timer_binding != NULL);
                err = the_timer_binding->tx_vtbl
                        .add_to_timeout(the_timer_binding, NOP_CONT,
                                        timer->next->expires);
            }

            // FIXME: no way to return an error from this silly API!
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "failed to send timer message");
                abort();
            }
        }

        timer_queue_remove(timer);
    }

    thread_mutex_unlock(&timer_mutex);
}

/**
 * \brief Return remaining time of timer.
 *
 * \param timer Pointer to running timer.
 *
 * \bug Blocking call (waits for reply from driver)
 *
 * \returns Remaining time, in us.
 */
uint64_t timer_remaining(struct timer *timer)
{
    errval_t err;

    assert(timer != NULL);

    thread_mutex_lock_nested(&timer_mutex); // might be in a callback

    // send off request for the remaining time (if we haven't already)
    if (!remaining_time_requested) {
        err = the_timer_binding->tx_vtbl.get_remaining(the_timer_binding, NOP_CONT);
        assert(err_is_ok(err)); // XXX
        remaining_time_requested = true;
    }

    thread_mutex_unlock(&timer_mutex);

    // block waiting for the reply which will update the top timer's expiry time
    // XXX: shouldn't block on default waitset! if we're in a callback we'll deadlock
    while (remaining_time_requested) {
        messages_wait_and_handle_next(); // XXX
    }

    thread_mutex_lock_nested(&timer_mutex); // might be in a callback

    // compute remaining time by adding expiries
    uint64_t remaining = 0;
    for (; timer != NULL; timer = timer->prev) {
        remaining += timer->expires;
    }

    thread_mutex_unlock(&timer_mutex);

    return remaining;
}

/**
 * \brief Destroy a timer.
 *
 * Stops and removes timer from system.
 *
 * \param timer Pointer to timer to destroy.
 */
void timer_destroy(struct timer *timer)
{
    TIMER_DEBUG("timer_destroy: called\n");

    if(timer == NULL) {
        TIMER_DEBUG("timer_destroy: terminated (null-timer)\n");
        return;
    }

    timer_stop(timer);
    free(timer);

    TIMER_DEBUG("timer_destroy: terminated (freed)\n");
}

/**
 * \brief Initialize timer subsystem.
 */
errval_t timer_init(void)
{
    thread_mutex_lock(&timer_mutex);

    if (timer_queue != NULL){
        TIMER_DEBUG("### Timer already started ###\n");
    	return SYS_ERR_OK;
    }
    timers_pending_insertion = NULL;

    // Register with timer driver
    TIMER_DEBUG("timer_init: looking up timer\n");
    iref_t iref;
    errval_t err = nameservice_blocking_lookup("timer", &iref);
    if (err_is_fail(err)) {
        TIMER_DEBUG("timer_init: could not connect to Timer.\n");
        goto out;
    }
    assert(iref != 0);

    timer_connected = false;
    TIMER_DEBUG("timer_init: connecting to timer\n");
    err = timer_bind(iref, bind_cb, NULL, get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);

 out:
    thread_mutex_unlock(&timer_mutex);

    return err;
    /*
    while(!timer_connected){
    	messages_wait_and_handle_next();
    }
    */

    return err;
}

/**
 * \brief Returns true iff the given timer is active / enqueued
 */
bool timer_is_running(struct timer *timer)
{
    return timer && (timer->next || timer->prev ||
                     timer_queue == timer || timers_pending_insertion == timer);
}
/*
 * @}
 */
