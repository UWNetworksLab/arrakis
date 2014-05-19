/**
 * \file
 * \brief Bfscope (trace server) support for the monitor.
 */

/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/event_queue.h>
#include <trace/trace.h>
#include <sys/time.h>
#include "monitor.h"

struct bfscope_flush_state
{
	struct intermon_binding *ib;
	iref_t iref;
};

struct bfscope_ack_state
{
	struct intermon_binding* to_initiator_binding;
};

struct notify_bfscope_state
{
	struct monitor_binding *monitor_binding;
	struct event_queue_node qnode;
};

// --------- Global variables

// Monitor binding to the process that initiated the trace_start
static struct monitor_binding *requester_monitor_binding = NULL;

// Intermonitor binding between monitor on the core that initiated the flush request
// and monitor on the core that runs bfscope
static struct intermon_binding *requester_intermon_binding = NULL;

// ----------

//-----------
// Receiving a flush ack message from bfscope
//-----------

static void bfscope_intermon_flush_ack_continue(void *arg);
static void bfscope_intermon_flush_forward_handler(struct intermon_binding *imb, iref_t iref);
static void bfscope_intermon_flush_ack(struct intermon_binding *ib);

/*
 * We received a message from bfscope, telling us that the flushing has been
 * completed.
 *
 * So let's forward this information to the initiator.
 */
static void bfscope_monitor_flush_ack_handler(struct monitor_binding *mb)
{
	if(requester_intermon_binding == NULL) {
		// The initiator and bfscope run on the same core, do no intermon communication

		bfscope_intermon_flush_ack(NULL);
		return;
	}

	struct bfscope_ack_state *state = malloc(sizeof (struct bfscope_ack_state));
	state->to_initiator_binding = requester_intermon_binding;

	requester_intermon_binding = NULL;

	bfscope_intermon_flush_ack_continue(state);
}

// -------

static void bfscope_monitor_flush_send_continue(void* arg)
{

	struct bfscope_flush_state *state = (struct bfscope_flush_state*) arg;

	assert(state->ib != NULL);

	// Send the intermonitor message
	errval_t err;
	err = state->ib->tx_vtbl.bfscope_flush_send_forward(state->ib, MKCONT(free, state), state->iref);

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, must be repeated for this core
		err = state->ib->register_send(state->ib, state->ib->waitset, MKCONT(&bfscope_monitor_flush_send_continue, state));
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		//TODO: Error handling
		USER_PANIC_ERR(err, "Could not broadcast trace_monitor_broadcast_start_continue");
	} else {
		// Everything was ok, do nothing.
	}

}

/*
 * This function is called on the monitor that initiated the "trace_flush" command.
 *
 * Let's forward the message to the core on which bfscope is running.
 */
static void bfscope_monitor_flush_send_handler(struct monitor_binding *mb, iref_t iref)
{
	printf("bfscope_monitor_flush_send_handler\n");

	requester_monitor_binding = mb;

	struct bfscope_flush_state *state = malloc(sizeof(struct bfscope_flush_state));
	state->iref = iref;

	// Get the coreid on which bfscope is running
	coreid_t coreid;
	errval_t err = iref_get_core_id(iref, &coreid);
	if (err_is_fail(err)) {
		USER_PANIC_ERR(err, "iref_get_core_id for bfscope failed");
	}

	printf("bfscope runs on core: %d\n", coreid);

	if(coreid == my_core_id) {
		printf("bfscope runs on the same core as the initiator of the flush request\n");

		// Send message to bfscope directly
		bfscope_intermon_flush_forward_handler(NULL, iref);
		return;
	}

	err = intermon_binding_get(coreid, &state->ib);

	if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "intermon_binding_get failed");
	}

	bfscope_monitor_flush_send_continue(state);
}


/*
 * This function is called on the monitor on which the initiator of the flush
 * message is running, once the flush has been performed and the ack has been
 * received.
 */
static void bfscope_monitor_flush_finished_successfully(void *arg)
{
	// Reset the global state

	requester_monitor_binding = NULL;
}

static void bfscope_intermon_flush_ack_continue(void* arg)
{
	struct bfscope_ack_state *state = (struct bfscope_ack_state*) arg;
	struct intermon_binding *intermon_binding = state->to_initiator_binding;

	errval_t err;

	err = intermon_binding->tx_vtbl.bfscope_flush_ack_forward(intermon_binding, MKCONT(free, state));

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		err = intermon_binding->register_send(intermon_binding, intermon_binding->waitset, MKCONT(&bfscope_intermon_flush_ack_continue, state));
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		//TODO: Error handling
		USER_PANIC_ERR(err, "Could not forward ack in bfscope_intermon_flush_ack_continue");
	} else {
		// Everything was ok, do nothing.
	}
}

static void bfscope_send_flush_msg_to_bfscope(void* arg)
{
	errval_t err;

	struct notify_bfscope_state *state = (struct notify_bfscope_state*) arg;
	struct monitor_binding *monitor_binding = state->monitor_binding;

	err = monitor_binding->tx_vtbl.bfscope_flush_send(monitor_binding, MKCONT(free, state), 0);

	if (err_is_ok(err)) {
		event_mutex_unlock(&monitor_binding->mutex);
	} else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		err = monitor_binding->register_send(monitor_binding, monitor_binding->waitset, MKCONT(&bfscope_send_flush_msg_to_bfscope, state));
		assert(err_is_ok(err));
	} else {
		event_mutex_unlock(&monitor_binding->mutex);
		//TODO: Error handling
		USER_PANIC_ERR(err, "Could not send flush message to bfscope");
	}
}

/*
 * The flush message has received on the monitor where bfscope is running.
 *
 * Let's Notify bfscope about the flush request.
 */
static void bfscope_intermon_flush_forward_handler(struct intermon_binding *imb, iref_t iref)
{
	printf("bfscope_intermon_flush_forward_handler on core %d.\n", my_core_id);

	// Store the intermonitor binding so that we can later reply
	requester_intermon_binding = imb;

	// Notify bfscope

	struct notify_bfscope_state *state = malloc(sizeof(struct notify_bfscope_state));
	//memset(state, 0, sizeof(struct trace_broadcast_start_state));

	// Get the monitor binding to bfscope
	errval_t err = iref_get_binding(iref, &state->monitor_binding);
	if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "iref_get_binding for bfscope failed");
	}
	// Send the message to bfscope
	event_mutex_enqueue_lock(&state->monitor_binding->mutex, &state->qnode, MKCLOSURE(&bfscope_send_flush_msg_to_bfscope, state));

}

static void bfscope_monitor_flush_forward_ack_to_requester(void *arg)
{

	assert(requester_monitor_binding != NULL);

	errval_t err;

	err = requester_monitor_binding->tx_vtbl.bfscope_flush_ack(requester_monitor_binding, MKCONT(bfscope_monitor_flush_finished_successfully, NULL));

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		err = requester_monitor_binding->register_send(requester_monitor_binding,
				requester_monitor_binding->waitset, MKCONT(&bfscope_monitor_flush_forward_ack_to_requester, NULL));
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		//TODO: Error handling
		USER_PANIC_ERR(err, "Could not reply to flush bfscope_monitor_flush_forward_ack_to_requester");
	} else {
		// Everything was ok, do nothing.
	}
}

// This function is automatically called when the initiating monitor receives an ack from another monitor
static void bfscope_intermon_flush_ack(struct intermon_binding *ib)
{
	printf("bfscope_intermon_flush_ack\n");

	bfscope_monitor_flush_forward_ack_to_requester(NULL);
}

//------------------------------------------------------------------------------
// Message Table Initialization
//------------------------------------------------------------------------------


// set up receive vtable in the intermonitor interface
errval_t bfscope_intermon_init(struct intermon_binding *ib)
{
	ib->rx_vtbl.bfscope_flush_send_forward = &bfscope_intermon_flush_forward_handler;
	ib->rx_vtbl.bfscope_flush_ack_forward = &bfscope_intermon_flush_ack;

	return SYS_ERR_OK;
}

// set up receive vtable in the monitor interface
errval_t bfscope_monitor_init(struct monitor_binding *mb)
{
    mb->rx_vtbl.bfscope_flush_send = &bfscope_monitor_flush_send_handler;
    mb->rx_vtbl.bfscope_flush_ack = &bfscope_monitor_flush_ack_handler;

    return SYS_ERR_OK;
}
