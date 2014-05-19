/**
 * \file
 * \brief IDC export implementation
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/idc_export.h>
#include <if/monitor_defs.h>

static void alloc_iref_reply_handler(struct monitor_binding *b,
                                     uintptr_t service_id, iref_t iref,
                                     errval_t success)
{
    struct idc_export *e = (void *)service_id;
    if (err_is_ok(success)) {
        e->iref = iref;
    }
    e->export_callback(e->export_cb_st, success, iref);
}

// private state for queuing on monitor send
struct idc_export_send_state {
    struct event_queue_node qnode;
    struct idc_export *e;
    struct monitor_binding *mb;
};

static void alloc_iref_request_sender(void *arg)
{
    struct idc_export_send_state *st = arg;
    struct idc_export *e = st->e;
    struct monitor_binding *mb = st->mb;

    /* Send alloc_iref request to the monitor */
    errval_t err = mb->tx_vtbl.alloc_iref_request(mb, NOP_CONT, (uintptr_t)e);
    if (err_is_ok(err)) {
        free(st);
        event_mutex_unlock(&mb->mutex);
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = mb->register_send(mb, mb->waitset,
                                MKCONT(alloc_iref_request_sender,st));
        assert(err_is_ok(err)); // shouldn't fail, as we have the mutex
    } else { // permanent error
        free(st);
        event_mutex_unlock(&mb->mutex);
        e->export_callback(e->export_cb_st, err, 0);
    }
}

/**
 * \brief Initiate the exporting of a service from the current dispatcher
 *
 * This call intiates the export process by requesting an IREF from the monitor.
 *
 * \param e IDC export state (must be filled in by caller)
 */
errval_t idc_export_service(struct idc_export *e)
{
    e->iref = 0;

    struct idc_export_send_state *st = malloc(sizeof(struct idc_export_send_state));
    assert(st != NULL);

    struct monitor_binding *mb = get_monitor_binding();
    st->e = e;
    st->mb = mb;

    // wait for the ability to use the monitor binding
    event_mutex_enqueue_lock(&mb->mutex, &st->qnode,
                             MKCLOSURE(alloc_iref_request_sender, st));
    return SYS_ERR_OK;
}

void idc_export_init(void)
{
    struct monitor_binding *mcb = get_monitor_binding();
    mcb->rx_vtbl.alloc_iref_reply = alloc_iref_reply_handler;
}
