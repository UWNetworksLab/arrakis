/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <contmng/contmng.h>
#include <ipv4/lwip/inet.h>

#include <if/e10k_defs.h>

#include "port_management_support.h"
#include "device_manager_debug.h"



/******************************************************************************
 * Local state
 ******************************************************************************/

/** Connection to e10k management service */
struct e10k_binding *binding = NULL;

struct cont_queue *c_queue = NULL;


/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

// Callback from e10k
static void idc_filter_registered(struct e10k_binding *b,
                                  uint64_t buf_id_rx,
                                  uint64_t buf_id_tx,
                                  errval_t err,
                                  uint64_t filter)
{
    NDM_DEBUG("e10k_idc_filter_registered(f=%"PRIu64" rx=%"PRIu64" tx=%"PRIu64
            ")\n", filter, buf_id_rx, buf_id_tx);
    handle_filter_response(filter, err, filter, buf_id_rx, buf_id_tx, 1);
}

// Callback from e10k
static void idc_filter_unregistered(struct e10k_binding *b,
                                    uint64_t filter,
                                    errval_t err)
{
    NDM_DEBUG("e10k_idc_filter_unregistered(%"PRIu64")\n", filter);
}

static errval_t send_register_port_filter(struct q_entry e)
{
    if (binding->can_send(binding)) {
        return binding->tx_vtbl.register_port_filter(
                binding, MKCONT(cont_queue_callback, c_queue),
                e.plist[0], e.plist[1], e.plist[2], e.plist[3], e.plist[4]);
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/** Register filter with e10k card driver */
static void idc_register_port_filter(uint64_t buf_id_rx,
                                     uint64_t buf_id_tx,
                                     uint8_t queue,
                                     e10k_port_type_t type,
                                     uint16_t port)
{
    struct q_entry entry;
    NDM_DEBUG("e10k_idc_register_port_filter(q=%d p=%d rx=%"PRIu64" tx=%"
            PRIu64")\n", queue, port, buf_id_rx, buf_id_tx);

    memset(&entry, 0, sizeof(struct q_entry));

    entry.handler = send_register_port_filter;
    entry.binding_ptr = binding;
    entry.plist[0] = buf_id_rx;
    entry.plist[1] = buf_id_tx;
    entry.plist[2] = queue;
    entry.plist[3] = type;
    entry.plist[4] = port;

    enqueue_cont_q(c_queue, &entry);
}

static errval_t send_unregister_filter(struct q_entry e)
{
    if (binding->can_send(binding)) {
        return binding->tx_vtbl.unregister_filter(
                binding, MKCONT(cont_queue_callback, c_queue),
                e.plist[0]);
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

/** Unregister filter with e10k card driver */
static void idc_unregister_filter(uint64_t filter)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));

    entry.handler = send_unregister_filter;
    entry.binding_ptr = binding;
    entry.plist[0] = filter;

    enqueue_cont_q(c_queue, &entry);
}

static struct e10k_rx_vtbl rx_vtbl = {
    .filter_registered = idc_filter_registered,
    .filter_unregistered = idc_filter_unregistered,
};

// Callback for bind
static void bind_cb(void *st, errval_t err, struct e10k_binding *b)
{
    assert(err_is_ok(err));

    NDM_DEBUG("Sucessfully connected to management interface\n");

    b->rx_vtbl = rx_vtbl;

    binding = b;
    c_queue = create_cont_q("e10k_filters");
}

/** Open connection to management interface */
static void connect_to_mngif(char *dev_name)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_e10kmng";
    char name[strlen(dev_name) + strlen(suffix) + 1];

    // Build label for management service
    sprintf(name, "%s%s", dev_name, suffix);

    // Connect to service
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    r = e10k_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

/******************************************************************************
 * Operations for filter interface
 ******************************************************************************/

static void init_filters(char *dev_name, qid_t qid)
{
    NDM_DEBUG("e10_flt: init %s %d\n", dev_name, (int) qid);

    // Check if we are already initialized from another queue
    if (binding != NULL) {
        return;
    }

    connect_to_mngif(dev_name);

    // waiting for connection to succeed.
    NDM_DEBUG("e10k_init_filters: wait connection\n");
    while (binding == NULL) {
        messages_wait_and_handle_next();
    }
}

static void reg_arp_filters(uint64_t id, uint64_t len_rx,
                            uint64_t len_tx)
{
    USER_PANIC("reg_arp_filters() not supported in e10k filters");
}

static errval_t reg_filters(uint16_t port,
                            port_type_t type,
                            bufid_t buffer_id_rx,
                            bufid_t buffer_id_tx,
                            appid_t appid,
                            qid_t qid)
{
    e10k_port_type_t t;
    assert(binding != NULL);

    NDM_DEBUG("e10k_reg_filters()\n");

    if (type == net_ports_PORT_TCP) {
        t = e10k_PORT_TCP;
    } else {
        t = e10k_PORT_UDP;
    }

    idc_register_port_filter(buffer_id_rx, buffer_id_tx, qid, t, port);

    return SYS_ERR_OK;
}

static void unreg_filters(uint64_t filter_id, qid_t qid)
{
    assert(binding != NULL);

    NDM_DEBUG("e10k_unreg_filters()\n");
    idc_unregister_filter(filter_id);
}


/******************************************************************************
 * Get signature of this service
 ******************************************************************************/

static struct filters_tx_vtbl e10k_filts_mng = {
    .type = "e10k_filters",
    .init_filters = init_filters,
    .reg_arp_filters = reg_arp_filters,
    .reg_filters = reg_filters,
    .unreg_filters = unreg_filters,
};

struct filters_tx_vtbl *get_e10k_filt_mng_sign(void)
{
    return &e10k_filts_mng;
}


