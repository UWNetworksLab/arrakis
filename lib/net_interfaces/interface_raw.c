/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <net_interfaces/net_interfaces.h>

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>
#include <contmng/contmng.h>

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048
#define BUFFER_COUNT ((128*1024*1024) / BUFFER_SIZE)


static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role);

static errval_t idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len,
                               uint64_t more_chunks, uint64_t flags,
                               bool blocking);

static void idc_get_mac_address(struct net_queue_manager_binding *binding,
                                uint64_t qid);

static uint64_t queue = 0;
static uint64_t card_mac = -1ULL;

static struct net_queue_manager_binding *binding_rx = NULL;
static uint64_t bufid_rx = -1ULL;

static struct net_queue_manager_binding *binding_tx = NULL;
static uint64_t bufid_tx = -1ULL;

static struct capref buffer_frame;
void *buffer_base = NULL;
size_t buffer_size = 2048;
size_t buffer_count = BUFFER_COUNT;



/******************************************************************************/
/* Buffer management */

errval_t buffer_tx_add(size_t idx, size_t offset, size_t len,
                       size_t more_chunks, uint64_t flags)
{

    errval_t err = SYS_ERR_OK;
    err = idc_raw_add_buffer(binding_tx, idx * BUFFER_SIZE + offset, len,
            (uint64_t)more_chunks, flags, 0);
    return err;
}

errval_t buffer_rx_add(size_t idx)
{

    errval_t err = SYS_ERR_OK;
    err = idc_raw_add_buffer(binding_rx, idx * BUFFER_SIZE, BUFFER_SIZE, 0, 0, 1);
    return err;
}


static void alloc_mem(struct capref *frame, void** virt, size_t size)
{
    errval_t r;
    vregion_flags_t flags;

    r = frame_alloc(frame, size, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Allocating memory region frame failed!");
    }

    flags = VREGION_FLAGS_READ_WRITE;
    r = vspace_map_one_frame_attr(virt, size, *frame, flags, NULL, NULL);
    if (!err_is_ok(r)) {
        USER_PANIC("Mapping memory region frame failed!");
    }
    memset(*virt, 0, size);
}

static void buffers_init(size_t count)
{
    struct waitset *ws = get_default_waitset();

    alloc_mem(&buffer_frame, &buffer_base, BUFFER_SIZE * count);

    idc_register_buffer(binding_rx, buffer_frame, NULL_CAP, queue, count,
                        RX_BUFFER_ID);
    while (bufid_rx == -1ULL) { event_dispatch(ws); }

    idc_register_buffer(binding_tx, buffer_frame, NULL_CAP, queue, count,
                        TX_BUFFER_ID);
    while (bufid_tx == -1ULL) { event_dispatch(ws); }
}


/******************************************************************************/
/* Flounder interface */

static errval_t send_raw_add_buffer(struct q_entry entry)
{
    struct net_queue_manager_binding *b = entry.binding_ptr;

    if (b->can_send(b)) {

        errval_t err = b->tx_vtbl.raw_add_buffer(
                b, MKCONT(cont_queue_callback, b->st),
                   entry.plist[0], entry.plist[1], entry.plist[2],
                   entry.plist[3]);
        return err;
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static errval_t idc_raw_add_buffer(struct net_queue_manager_binding *binding,
                               uint64_t offset, uint64_t len,
                               uint64_t more_chunks, uint64_t flags, bool blocking)
{

    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_raw_add_buffer;
    entry.binding_ptr = binding;
    entry.plist[0] = offset;
    entry.plist[1] = len;
    entry.plist[2] = more_chunks;
    entry.plist[3] = flags;


    struct waitset *ws = get_default_waitset();
    int passed_events = 0;
    while (can_enqueue_cont_q(binding->st) == false) {
        if (passed_events > 5) {
            return CONT_ERR_NO_MORE_SLOTS;
        }
        event_dispatch(ws);
        ++passed_events;
    }


    if (blocking) {
        while (queue_used_slots(binding->st) > 1) {
            /*
            printf("%s:The event count is %d\n", disp_name(),
                        queue_used_slots(binding->st));
            cont_queue_show_queue(binding->st);
            */
            event_dispatch(ws);
        }
    }

    enqueue_cont_q(binding->st, &entry);
    return SYS_ERR_OK;
}


static errval_t send_register_buffer(struct q_entry entry)
{
    struct net_queue_manager_binding *b = entry.binding_ptr;

    if (b->can_send(b)) {
//        printf("#### send_register_buffer, calling continuation\n");
        errval_t err = b->tx_vtbl.register_buffer(
                b, MKCONT(cont_queue_callback, b->st),
                entry.cap, entry.cap2,
                entry.plist[0], entry.plist[1], entry.plist[2]);
        return err;
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_register_buffer;
    entry.binding_ptr = binding;
    entry.cap = buf;
    entry.cap2 = sp;
    entry.plist[0] = qid;
    entry.plist[1] = slots;
    entry.plist[2] = role;

    enqueue_cont_q(binding->st, &entry);
}

static errval_t send_get_mac_address(struct q_entry entry)
{
    struct net_queue_manager_binding *b = entry.binding_ptr;

    if (b->can_send(b)) {
 //       printf("#### send_get_mac_address, calling continuation\n");
        errval_t err = b->tx_vtbl.get_mac_address(
                b, MKCONT(cont_queue_callback, b->st),
                entry.plist[0]);
        return err;
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}


static void idc_get_mac_address(struct net_queue_manager_binding *binding,
                                uint64_t qid)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_get_mac_address;
    entry.binding_ptr = binding;
    entry.plist[0] = qid;

    enqueue_cont_q(binding->st, &entry);
}

static void new_buffer_id(struct net_queue_manager_binding *st, errval_t err,
                          uint64_t queueid, uint64_t buffer_id)
{
    assert(err_is_ok(err));

    if (st == binding_rx) {
        bufid_rx = buffer_id;
    } else {
        bufid_tx = buffer_id;
    }
}

// Returns the bufferid for specified type (RX, TX)
uint64_t get_rx_bufferid(void)
{
    return bufid_rx;
}

uint64_t get_tx_bufferid(void)
{
    return bufid_tx;
}

static void raw_xmit_done(struct net_queue_manager_binding *st,
                          uint64_t offset, uint64_t len, uint64_t more,
                          uint64_t flags)
{
    size_t idx = offset / BUFFER_SIZE;

    if (st == binding_rx) {
        benchmark_rx_done(idx, len, more, flags);
    } else {
        benchmark_tx_done(idx);
    }
}

static void get_mac_address_response(struct net_queue_manager_binding *st,
                                     uint64_t qid, uint64_t mac)
{
    card_mac = mac;
}

static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .raw_xmit_done = raw_xmit_done,
    .get_mac_address_response = get_mac_address_response,
};


static void bind_cb_rx(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->st = create_cont_q("interface_raw_rx");
//    struct cont_queue *q = (struct cont_queue *)b->st;
//    q->debug = 1;
    b->rx_vtbl = rx_vtbl;
    binding_rx = b;
}

static void bind_cb_tx(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->st = create_cont_q("interface_raw_tx");
//   struct cont_queue *q = (struct cont_queue *)b->st;
//   q->debug = 1;
    b->rx_vtbl = rx_vtbl;
    binding_tx = b;
}


static void connect_to_driver(const char *cname, uint64_t qid, bool isRX)
{
    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64"", cname, qid);
    err = nameservice_blocking_lookup(qm_name, &iref);
    assert(err_is_ok(err));

    if (isRX) {
        err = net_queue_manager_bind(iref, bind_cb_rx, NULL, get_default_waitset(),
                IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));
    } else {
        err = net_queue_manager_bind(iref, bind_cb_tx, NULL, get_default_waitset(),
                IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));
    }
}

void net_if_init(const char* cardname, uint64_t qid)
{
    static bool initialized = false;
    struct waitset *ws = get_default_waitset();

    // Only initialize once
    if (initialized) {
        return;
    }

    queue = qid;

    // Connect RX path
    connect_to_driver(cardname, queue, true);
    while (binding_rx == NULL) { event_dispatch(ws); }

    // Connect TX path
    connect_to_driver(cardname, queue, false);
    while (binding_tx == NULL) { event_dispatch(ws); }

    buffers_init(BUFFER_COUNT);

    // Get MAC address
    idc_get_mac_address(binding_rx, queue);
    while (card_mac == -1ULL) { event_dispatch(ws); }


    initialized = true;
}

void net_if_terminate(void)
{
    vspace_unmap(buffer_base);
    cap_delete(buffer_frame);
}

void benchmark_get_mac_address(uint8_t *mac)
{
    memcpy(mac, &card_mac, 6);
}


