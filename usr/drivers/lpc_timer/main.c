/**
 * \file lpc_timer_main
 * \brief x86 legacy timer driver.
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
#include <if/timer_defs.h>
#include "timer.h"

#include "lpc_timer_debug.h"

static struct timer_rx_vtbl timer_rx_vtbl; // forward declaration


/*************************************************************//**
 * \defGroup DataTypes Data types
 *
 * @{
 *
 ****************************************************************/

/**
 * \brief Per-client closure
 *
 * This structure defines the per-client state stored in \c b->st.
 *
 * It records the timeout value of the client, the #timer_binding of
 * the client and builds a doubly-linked list of all currently active
 * client closures. This list is updated each time a client sets or changes
 * its timeout, or a timer expires.
 */
struct timer_client {
    uint64_t expiry;   /**< Timer expiry of this client (as difference from prev) */
    struct timer_binding *binding; /**< Client's own binding */
    struct timer_client *prev;                       /**< Service response closure of the client 
                                                          with the next timeout after me */
    struct timer_client *next;                       /**< Service response closure of the client 
                                                          with the timeout before me */
};

/*
 * @}
 */


/*************************************************************//**
 * \defGroup LocalStates Local states
 *
 * @{
 * 
 ****************************************************************/

/**
 * \brief Head of the list of all clients with timers, sorted by timeout expiry
 *
 * If \c NULL, this variable indicates that the list of pending timers is empty.
 * Otherwise, it points to the #timer_client closure of the client with
 * the next timeout.
 *
 */
static struct timer_client *timers = NULL;

/*
 * @}
 */


/*************************************************************//**
 * \defGroup QueueManagement Management of the timer queue
 *
 * These functions maintain the queue of pending timers and maintain
 * the hardware timer state.
 *
 * @{
 * 
 ****************************************************************/

static inline bool is_enqueued(struct timer_client *client)
{
    return (timers == client || client->prev != NULL || client->next != NULL);
}

static void remove_timeout(struct timer_client *client)
{
    // If necessary, update the pointer to the head of list
    if (timers == client){
        // We must indeed be the head
        assert(client->prev == NULL);

        // Update the head of list
        timers = client->next;

        // update this client with whatever time it had remaining
        client->expiry = lpc_timer_read();

        // program HW for next timer
        if (client->next != NULL) {
            lpc_timer_set(client->expiry + client->next->expiry);
        } else {
            lpc_timer_set(0);
        }
    }

    // Remove it from the \c timers list
    if (client->prev != NULL) {
        client->prev->next = client->next;
    }

    if (client->next != NULL) {
        client->next->expiry += client->expiry;
        client->next->prev = client->prev;
    }

    client->prev = client->next = NULL;
}


static void add_timeout(struct timer_client *client)
{
    // must not already be enqueued
    if(is_enqueued(client)){
        remove_timeout(client);
    }
    assert(!is_enqueued(client));
    LPC_DEBUG("add_timeout: for client[%p] with expiry %lu\n",
    		client, client->expiry);
    // update remaining time of current (running) timeout
    if (timers != NULL) {
        timers->expiry = lpc_timer_read();
    }

    // insert into queue
    for (struct timer_client *i = timers; i != NULL; i = i->next) {
        LPC_DEBUG("add_timeout: looping, current timer[%p] with expiry %lu\n",
        		i, i->expiry);

        // Sort into priority queue
        if (client->expiry < i->expiry) {
            i->expiry -= client->expiry;

            client->prev = i->prev;
            client->next = i;
            if (i->prev != NULL) { // in middle of queue
                i->prev->next = client;
                LPC_DEBUG("add_timeout: client[%p] added in middle\n", client);
            } else { // add head of queue
                assert(timers == i);
                timers = client;
                LPC_DEBUG("add_timeout: client[%p] added in head\n", client);
                // update HW timer
                lpc_timer_set(client->expiry);
            }
            i->prev = client;
            return;
        } else {
            client->expiry -= i->expiry;
            LPC_DEBUG("add_timeout: client[%p] expiry reduced by %lu, new expiry %lu\n",
            		client, i->expiry, client->expiry);
        }

        // Add at queue end
        if (i->next == NULL) {
            i->next = client;
            client->prev = i;
            client->next = NULL;
            LPC_DEBUG("add_timeout: client[%p] added in end\n", client);

            return;
        }
    } /* end for: */

    // Add as only element in queue
    assert(timers == NULL);
    timers = client;
    client->prev = NULL;
    client->next = NULL;
    LPC_DEBUG("add_timeout: client[%p] added as only element\n", client);

    // update HW timer
    lpc_timer_set(client->expiry);
}


/*
 * @}
 */


/*************************************************************//**
 * \defGroup ConnectionHandlers Connection Handlers
 * 
 * These handlers are called by the stubs generated from if/timer.if.
 *
 * @{
 *
 ****************************************************************/

/**
 * \brief Export handler
 *
 * This handler is called when the server is set-up and listening.
 *
 * First, we check that the listening phase indeed succeeded. Second,
 * we publicly register the \c timer service.
 * 
 */
static void export_cb(void *st, errval_t err, iref_t iref)
{
    LPC_DEBUG("export_cb: called\n");

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // Register the service
    assert(iref != NULL_IREF);
    err = nameservice_register("timer", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to register timer service");
        abort();
    }


    LPC_DEBUG("export_cb: terminated\n");

}

/**
 * \brief Error handler
 *
 * This handler is called when an error occurs on a binding.
 *
 * We have to update the #timers list, to maintain its
 * invariant. Moreover, we can free the per-client closure.
 *
 * \pre #timers is a valid doubly-linked list of active
 *      #timer_client, ordered by timeout (the closest at the head).
 *
 * \post #timers' is equal to #timers with #st removed (if it was present).
 *
 * \note If the disconnecting client was at the head of the list, then
 *       \code timers == client \endcode. Hence, we have to update \c
 *       timers to maintain the list invariant.
 *
 */
static void error_handler(struct timer_binding *b, errval_t err)
{
    LPC_DEBUG("disconn_hander: called\n");

    // Retrieve the per-client closure
    struct timer_client *client = b->st;
    assert(client != NULL);

    // If the client is in the queue, cancel its timer and remove it
    if (is_enqueued(client)) {
        remove_timeout(client);
    }

    // Free the per-client closure
    free(client);

    LPC_DEBUG("disconn_hander: terminated\n");

}

/**
 * \brief Connection service logic
 *
 * This handler is called when a client tries to connect to the timer
 * server.
 *
 */
static errval_t connect_cb(void *st, struct timer_binding *b)
{
    LPC_DEBUG("conn_hander: called\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = timer_rx_vtbl;
    b->error_handler = error_handler;

    // Allocate the per-client closure
    struct timer_client *client = malloc(sizeof(struct timer_client));
    assert(client != NULL);

    // If there is no enough memory
    if (client == NULL) {
        // Reject the connection
        LPC_DEBUG("conn_hander: connection rejected\n");
        return LIB_ERR_MALLOC_FAIL;
    }

    // Store this per-client closure
    b->st = client;
    client->binding = b;

    // Set flags for async delivery of wakeup notifications and replies
    b->control(b, IDC_CONTROL_CLEAR_SYNC);

    LPC_DEBUG("conn_hander: connected with client [%p]. terminated\n", client);

    // And accept the connection
    return SYS_ERR_OK;
}

static void set_timeout_handler(struct timer_binding *b, uint64_t timeout)
{
    // Retrieve the per-client closure
    struct timer_client *client = b->st;
    assert(client != NULL);
	LPC_DEBUG("set_timeout_handler: client [%p] adding timeout %lu\n",
				client, timeout);

    // If the client is in the queue, cancel its timer first
    if (is_enqueued(client)) {
    	LPC_DEBUG("set_timeout_handler: already in queue, removing\n");
        remove_timeout(client);
    }

    client->expiry = timeout;
	LPC_DEBUG("set_timeout_handler: client [%p] calling add_timeout(%lu)\n",
				client, timeout);

    add_timeout(client);
}

static void add_to_timeout_handler(struct timer_binding *b, uint64_t increment)
{
    // Retrieve the per-client closure
    struct timer_client *client = b->st;
    assert(client != NULL);

    // If the client is in the queue, extend its timer
    if (is_enqueued(client)) {
        remove_timeout(client);
        client->expiry += increment;
        add_timeout(client);
    } else { // if the timer already fired, just reset it for the increment
        client->expiry = increment;
        add_timeout(client);
    }
}

static void cancel_timeout_handler(struct timer_binding *b)
{
    // Retrieve the per-client closure
    struct timer_client *client = b->st;
    assert(client != NULL);

    // If the client is in the queue, cancel its timer
    if (is_enqueued(client)) {
        remove_timeout(client);
    }
}

static void get_remaining_handler(struct timer_binding *b)
{
    errval_t err;
    uint64_t remaining = 0;

    // Retrieve the per-client closure
    struct timer_client *client = b->st;
    assert(client != NULL);

    // If the client is in the queue, calculate remaining time of its timer
    if (is_enqueued(client)) {
        assert(timers != NULL);

        // update remaining time for element at head of queue (read HW)
        timers->expiry = lpc_timer_read();

        // walk backwards through all timers to head of queue, adding expiry
        for (struct timer_client *t = client; t != NULL; t = t->prev) {
            remaining += t->expiry;
        }
    }

    // send reply (0 if not enqueued)
    err = b->tx_vtbl.remaining(b, NOP_CONT, remaining);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sending reply failed");
    }
}


/*
 * @}
 */


/**************************************************************//**
 * \defGroup FlounderVtables Flounder vtables
 *
 * @{
 *
 *****************************************************************/

/**
 * \brief Flounder server call vtable
 *
 * Here, we register the connection and message handlers implemented above.
 *
 */
static struct timer_rx_vtbl timer_rx_vtbl = {
    .set_timeout = set_timeout_handler,
    .add_to_timeout = add_to_timeout_handler,
    .cancel_timeout = cancel_timeout_handler,
    .get_remaining = get_remaining_handler,
};

/*
 * @}
 */


/*************************************************************//**
 * \defGroup Main Main
 * 
 * @{
 *
 *****************************************************************/

static void handle_timer(void)
{
    errval_t err;

    struct timer_client *client = timers, *next;

    if (client == NULL) { // spurious wakeup?
        LPC_DEBUG("handle_timer: spurious wakeup?\n");
        return;
    }

    // the top timer just expired
    client->expiry = 0;

    // wake up head and all following clients with same expiry time
    for (; client != NULL && client->expiry == 0; client = next) {
        next = client->next;

        LPC_DEBUG("handle_timer: sending wakeup\n");
        err = client->binding->tx_vtbl.wakeup(client->binding, NOP_CONT);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error sending wakeup");
        }

        // dequeue
        client->prev = NULL;
        client->next = NULL;
        /* Fix for dangling linked list */
        if(next != NULL) {
        	next->prev = NULL;
        }

    } /* end for: */

    // update head of queue
    timers = client;

    // set next timeout if necessary
    if (timers != NULL) {
        timers->prev = NULL;
        lpc_timer_set(timers->expiry);
    }
}

void timer_init_complete(void)
{
    errval_t err;

    // Offer timer service
    err = timer_export(NULL, export_cb, connect_cb, get_default_waitset(),
                       IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "timer_export failed");
    }
}

int main(int argc, char *argv[])
{
    errval_t err;

    // Initialize driver (will call timer_init_complete when done)
    err = lpc_timer_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "lpc_timer_init\n");
    }

    // Register interrupt handler
    lpc_timer_register_handler(handle_timer);

    // Stick around waiting for interrupts/messages
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch");
        }
    }
}



/*
 * @}
 */
