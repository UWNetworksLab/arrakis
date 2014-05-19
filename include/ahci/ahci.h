/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_H
#define _AHCI_H

#include <barrelfish/waitset.h>
#include <barrelfish/idc.h>
#include <barrelfish/event_mutex.h>
#include <flounder/flounder.h>
#include <ahci/ahci_util.h>
#include <ahci/ahci_dma_pool.h>
#include <dev/ata_identify_dev.h>

struct ahci_binding;

typedef void ahci_bind_continuation_fn(void *st, errval_t err,
		struct ahci_binding *_binding);
typedef bool ahci_can_send_fn(struct ahci_binding *_binding);
typedef errval_t ahci_register_send_fn(struct ahci_binding *_binding,
		struct waitset *ws, struct event_closure _continuation);
typedef errval_t ahci_change_waitset_fn(struct ahci_binding *_binding,
		struct waitset *ws);
typedef errval_t ahci_control_fn(struct ahci_binding *_binding,
		idc_control_t control);
typedef void ahci_error_handler_fn(struct ahci_binding *_binding, errval_t err);

/*
 * Message type signatures (receive)
 */
typedef void ahci_command_completed_method_fn(struct ahci_binding *_binding,
		void *tag);

/*
 * Receive VTable
 */
struct ahci_rx_vtbl {
    ahci_command_completed_method_fn *command_completed;
};

/*
 * The binding structure
 */
struct ahci_binding {
    /* user state */
    void *st;

    /* waitset for receive handlers and send continuations */
    struct waitset *waitset;

    /* Mutex for the use of user code. */
    /* Must be held before any operation where there is a possibility of */
    /* concurrent access to the same binding (eg. multiple threads, or */
    /* asynchronous event handlers that use the same binding object). */
    struct event_mutex mutex;

    /* returns true iff a message could currently be accepted by the binding */
    ahci_can_send_fn *can_send;

    /* register an event for when a message is likely to be able to be sent */
    ahci_register_send_fn *register_send;

    /* change the waitset used by a binding */
    ahci_change_waitset_fn *change_waitset;

    /* perform control operations */
    ahci_control_fn *control;

    /* error handler for async errors */
    ahci_error_handler_fn *error_handler;

    /* Message receive functions (filled in by user) */
    struct ahci_rx_vtbl rx_vtbl;

    /* Private state belonging to the binding implementation */
    uint8_t port_id;
    struct ahci_port_info port_info;
    struct waitset_chanstate register_chanstate;
    struct waitset_chanstate tx_cont_chanstate;

    uint8_t *identify_data;
    size_t identify_length;
    ata_identify_t identify;
};

errval_t ahci_issue_command(struct ahci_binding *_binding,
		struct event_closure _continuation, void *tag, uint8_t *fis,
		size_t fis_length, bool is_write, struct ahci_dma_region *buf,
		size_t buflen);

errval_t ahci_close(struct ahci_binding *_binding,
		struct event_closure _continuation);

errval_t ahci_init(uint8_t port, ahci_bind_continuation_fn *_continuation,
		void *st, struct waitset *waitset);

#endif // _AHCI_H
