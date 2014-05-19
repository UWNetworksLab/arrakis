/**
 * \file
 * \brief Tracing support for the monitor.
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
#include <barrelfish/dispatcher_arch.h>
#include <trace/trace.h>
#include <sys/time.h>
#include "monitor.h"

static void trace_intermon_send_time_measurement_request(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem);
static void trace_intermon_send_time_measurement_ack(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem);

static void trace_intermon_send_prepare(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem);
static void trace_intermon_notify_next_core(coreid_t origin_core);
static void trace_intermon_send_prepare_finished(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem);
static void trace_monitor_send_prepare_finish(struct monitor_binding *mb, struct monitor_msg_queue_elem *elem);
static void trace_monitor_prepare_finished_successfully(void *arg);

// Global variables for communication with initiator program
static struct monitor_binding *initiator_to_monitor_binding;
static struct monitor_msg_queue_elem *initiator_monitor_elem;

//------------------------------------------------------------------------------
// Time measurement
//------------------------------------------------------------------------------

struct trace_measure_time_state {
	struct intermon_msg_queue_elem elem;
	coreid_t origin_core;
	uint64_t t0;
	uint64_t t1;
};

/*
 * Perform a time measurement, relative to core 0.
 */
static void trace_monitor_measure_time(coreid_t origin_core)
{
	printf("trace_monitor_measure_time on core: %d\n", my_core_id);

	if(my_core_id == 0) {

	    dispatcher_handle_t handle = curdispatcher();
	    struct dispatcher_generic *disp = get_dispatcher_generic(handle);
	    struct trace_buffer *trace_buf = disp->trace_buf;

	    trace_buf->t_offset = 0;

	    // Notify next core
	    trace_intermon_notify_next_core(origin_core);

	} else {

		// Measure time relative to core 0

		// TODO implement

		struct trace_measure_time_state *state = malloc(sizeof(struct trace_measure_time_state));

		struct intermon_binding *ib;
		errval_t err = intermon_binding_get(0, &ib);
		assert(err_is_ok(err));

		state->elem.cont = trace_intermon_send_time_measurement_request;
		state->origin_core = origin_core;

		trace_intermon_send_time_measurement_request(ib, &state->elem);

	}
}

/*
 * Send a time measurement request to core 0.
 */
static void trace_intermon_send_time_measurement_request(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem)
{
	errval_t err;

	struct trace_measure_time_state *state = (struct trace_measure_time_state*) elem;

	err = ib->tx_vtbl.trace_measure(ib, MKCONT(free, state), state->origin_core, rdtsc());

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, so it must be repeated
		struct intermon_state *ist = ib->st;
		err = intermon_enqueue_send(ib, &ist->queue, get_default_waitset(), &state->elem.queue);
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "Sending trace_intermon_send_time_measurement_request failed");
		// TODO error handling
	} // Else: Everything is ok, do nothing


}

/*
 * We received a message for a time measurement.
 */
static void trace_intermon_measure_recv(struct intermon_binding *ib, coreid_t origin_core, uint64_t t0)
{
	// All measurements are relative to core 0, thus this monitor must be running
	// on core 0.
	assert(my_core_id == 0);

	uint64_t t1 = rdtsc();

	struct trace_measure_time_state *state = malloc(sizeof(struct trace_measure_time_state));

	state->elem.cont = trace_intermon_send_time_measurement_ack;
	state->origin_core = origin_core;
	state->t0 = t0;
	state->t1 = t1;

	trace_intermon_send_time_measurement_ack(ib, &state->elem);
}

/*
 * Send a time measurement back to the core who started the time measurement.
 */
static void trace_intermon_send_time_measurement_ack(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem)
{
	errval_t err;

	struct trace_measure_time_state *state = (struct trace_measure_time_state*) elem;

	err = ib->tx_vtbl.trace_measure_ack(ib, MKCONT(free, state), state->origin_core, state->t0, state->t1, rdtsc());

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, so it must be repeated
		struct intermon_state *ist = ib->st;
		err = intermon_enqueue_send(ib, &ist->queue, get_default_waitset(), &state->elem.queue);
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "Sending trace_intermon_send_time_measurement_ack failed");
		// TODO error handling
	} // Else: Everything is ok, do nothing


}

/*
 * The monitor who started a time measurement received the response from core 0.
 */
static void trace_intermon_measure_ack_recv(struct intermon_binding* ib, coreid_t origin_core, uint64_t t0, uint64_t t1, uint64_t t2)
{
	uint64_t t3 = rdtsc();

	printf("NTP result: %" PRIu64 " %" PRIu64 " %" PRIu64 " %" PRIu64 "\n", t0,t1,t2,t3);

    // Network Time Protocol formula
    int64_t offset = (((t1-t0)+(t2-t3))/2);

	dispatcher_handle_t handle = curdispatcher();
	struct dispatcher_generic *disp = get_dispatcher_generic(handle);
	struct trace_buffer *trace_buf = disp->trace_buf;

	trace_buf->t_offset = offset;

	// Notify next core
	trace_intermon_notify_next_core(origin_core);
}

//------------------------------------------------------------------------------
// Intermonitor Communication for Preparation
//------------------------------------------------------------------------------

struct trace_prepare_state {
	struct intermon_msg_queue_elem elem;
	coreid_t origin_core;
};

/*
 * Notify either the next core to send prepare, or if there is no next core,
 * notify the origin core that the preparation is finished.
 */
static void trace_intermon_notify_next_core(coreid_t origin_core)
{
	coreid_t next_core = my_core_id;

	struct trace_prepare_state *state = malloc(sizeof (struct trace_prepare_state));
	state->origin_core = origin_core;
	state->elem.cont = trace_intermon_send_prepare;

	bool has_more_cores = true;
	errval_t err = SYS_ERR_OK;

	struct intermon_binding *ib;

	// Search for the next core to send the prepare message to
	do {

		if (next_core == MAX_COREID) {
			// There are no more cores to notify
			has_more_cores = false;
			break;
		}

		next_core++;

		err = intermon_binding_get(next_core, &ib);

	} while(err_is_fail(err));


	if (has_more_cores) {
		// There is a another core

		assert(ib != NULL);
		assert(next_core <= MAX_COREID);

		printf("Sending a prepare to core: %d\n", next_core);

		trace_intermon_send_prepare(ib, &state->elem);

	} else {
		// There is no more core, notify the origin core

		if (my_core_id == origin_core) {
			// We are the origin core, just notify the user program

			assert(initiator_to_monitor_binding != NULL);

			initiator_monitor_elem = malloc(sizeof(struct monitor_msg_queue_elem));
			initiator_monitor_elem->cont = trace_monitor_send_prepare_finish;

			trace_monitor_send_prepare_finish(initiator_to_monitor_binding, initiator_monitor_elem);

		} else {
			// Notify the monitor on the core on which the initiator is running

			err = intermon_binding_get(origin_core, &ib);

			assert(err_is_ok(err));

			struct intermon_msg_queue_elem *elem = malloc(sizeof(struct intermon_msg_queue_elem));

			trace_intermon_send_prepare_finished(ib, elem);

		}

	}

}

/*
 * Send a message from a monitor to the monitor on which the initiator is running,
 * to tell the monitor that it should tell the initiator that the preparation is
 * finished.
 */
static void trace_intermon_send_prepare_finished(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem)
{
	errval_t err;

	err = ib->tx_vtbl.trace_prepare_finished(ib, MKCONT(free, elem));

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, so it must be repeated
		struct intermon_state *ist = ib->st;
		err = intermon_enqueue_send(ib, &ist->queue, get_default_waitset(), &elem->queue);
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "Sending trace_prepare_finished failed");
		// TODO error handling
	} // Else: Everything is ok, do nothing


}

/*
 * The monitor on which the initiator is running received a message from a
 * different monitor, telling it that the preparation is finished. Forward this
 * information to the user program.
 */
static void trace_intermon_prepare_finished_recv(struct intermon_binding *ib)
{
	assert(initiator_to_monitor_binding != NULL);

	initiator_monitor_elem = malloc(sizeof(struct monitor_msg_queue_elem));
	initiator_monitor_elem->cont = trace_monitor_send_prepare_finish;

	trace_monitor_send_prepare_finish(initiator_to_monitor_binding, initiator_monitor_elem);
}

/*
 * Send a prepare message to the next monitor.
 */
static void trace_intermon_send_prepare(struct intermon_binding *ib, struct intermon_msg_queue_elem *elem)
{
	errval_t err;

	struct trace_prepare_state *state = (struct trace_prepare_state*) elem;

	err = ib->tx_vtbl.trace_prepare(ib, MKCONT(free, state), state->origin_core);

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, so it must be repeated
		printf("trace_intermon_send_prepare flounder busy\n");
		struct intermon_state *ist = ib->st;
		err = intermon_enqueue_send(ib, &ist->queue,
		                    get_default_waitset(), &state->elem.queue);
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "Sending trace prepare failed");
		// TODO error handling
	} // Else: Everything is ok, do nothing

}

/*
 * This monitor has received a prepare message.
 */
static void trace_intermon_prepare_recv(struct intermon_binding *ib, coreid_t origin_core)
{
	trace_monitor_measure_time(origin_core);
}

//------------------------------------------------------------------------------
// Monitor communicating with user program
//------------------------------------------------------------------------------

/*
 * This is the function that is invoked when we receive a prepare message from
 * a user program.
 */
static void trace_monitor_prepare_recv(struct monitor_binding *mb, coreid_t origin_core)
{
	printf("trace_monitor_prepare_recv\n");

	initiator_to_monitor_binding = mb;

	if (my_core_id == 0) {
		// Perform time measurement, and notify next core afterwards

		return trace_monitor_measure_time(origin_core);
	} else {
		// Notify core 0 to start measurement

		struct trace_prepare_state *state = malloc(sizeof(struct trace_prepare_state));
		state->origin_core = origin_core;
		state->elem.cont = trace_intermon_send_prepare;

		struct intermon_binding *ib;

		errval_t err = intermon_binding_get(0, &ib);

		assert(err_is_ok(err));

		trace_intermon_send_prepare(ib, &state->elem);
	}
}

/*
 * This function sends the prepare_finished message from the monitor to the
 * user program.
 */
static void trace_monitor_send_prepare_finish(struct monitor_binding *mb, struct monitor_msg_queue_elem *elem)
{

	errval_t err;

	err = mb->tx_vtbl.trace_prepare_finished(mb,
			MKCONT(trace_monitor_prepare_finished_successfully, NULL));

	if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
		// Sending failed, so it must be repeated
		struct monitor_state *ist = mb->st;
		err = monitor_enqueue_send(mb, &ist->queue, get_default_waitset(), &elem->queue);
		assert(err_is_ok(err));
	} else if(err_is_fail(err)) {
		USER_PANIC_ERR(err, "Sending trace_prepare_finished failed");
		// TODO error handling
	} // Else: everything is ok, do nothing
}

/*
 * This function is called to reset the state, once the preparation is finished.
 */
static void trace_monitor_prepare_finished_successfully(void *arg)
{
	free(initiator_monitor_elem);
	initiator_monitor_elem = NULL;
	initiator_to_monitor_binding = NULL;
}

//------------------------------------------------------------------------------
// Message Table Initialization
//------------------------------------------------------------------------------


// set up receive vtable in the intermonitor interface
errval_t trace_intermon_init(struct intermon_binding *ib)
{
	ib->rx_vtbl.trace_prepare = &trace_intermon_prepare_recv;
	ib->rx_vtbl.trace_prepare_finished = &trace_intermon_prepare_finished_recv;
	ib->rx_vtbl.trace_measure = &trace_intermon_measure_recv;
	ib->rx_vtbl.trace_measure_ack = &trace_intermon_measure_ack_recv;

	return SYS_ERR_OK;
}

// set up receive vtable in the monitor interface
errval_t trace_monitor_init(struct monitor_binding *mb)
{
    mb->rx_vtbl.trace_prepare = &trace_monitor_prepare_recv;

    return SYS_ERR_OK;
}
