/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "elb.h"

#include <barrelfish/net_constants.h>
#include <if/net_queue_manager_defs.h>
#include <barrelfish/bulk_transfer_arch.h>
#include <procon/procon.h>
#include "elb_debug.h"

#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have
#define BUFFER_SIZE 2048

// Defined in the benchmark file

static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role);

static uint64_t queue = 0;

static struct net_queue_manager_binding *binding_rx = NULL;
static struct shared_pool_private *spp_rx = NULL;
static uint64_t bufid_rx = -1ULL;

static struct net_queue_manager_binding *binding_tx = NULL;
static struct shared_pool_private *spp_tx = NULL;
static uint64_t bufid_tx = -1ULL;

static struct capref buffer_frame;
void *buffer_base = NULL;
size_t buffer_size = 2048;

/******************************************************************************/
/* Buffer management */

errval_t buffer_tx_add(size_t idx, size_t len)
{
    struct slot_data s = {
        .buffer_id = bufid_tx,
        .no_pbufs = 1,
        .offset = idx * BUFFER_SIZE,
        .len = len,
        .client_data = idx + 1,
    };
    //printf("buffer_tx_add()\n");
    bool ret = sp_produce_slot(spp_tx, &s);
    if (ret) {
        return (SYS_ERR_OK);
    }
    return CONT_ERR_NO_MORE_SLOTS;
}

void buffer_rx_add(size_t idx)
{
    bool result;
    //printf("buffer_rx_add()\n");
    copy_data_into_slot(spp_rx, bufid_rx,
                        (spp_rx->c_read_id - 1) % spp_rx->c_size,
                        idx * BUFFER_SIZE,
                        BUFFER_SIZE, 1, idx + 1, 0);
    result = sp_set_read_index(spp_rx,
                               (spp_rx->c_read_id + 1) % spp_rx->c_size);
    assert(result);
}

static void check_rx_ring(void)
{
    struct slot_data s;

    while (1) {
        if (!sp_ghost_read_slot(spp_rx, &s)) {
            break;
        }

        //printf("benchmark_rx_done()\n");
        benchmark_rx_done(s.client_data - 1, s.len);
    }
}

static void check_tx_ring(void)
{
    struct slot_data s;
    uint64_t start, stop, i;
    bool res;

    if (sp_queue_empty(spp_tx)) {
        stop = spp_tx->c_write_id;
    } else {
        stop = spp_tx->c_read_id;
    }
    start = spp_tx->pre_write_id;

    if (start == stop) {
        return;
    }

    i = start;
    while (sp_c_between(start, i, stop, spp_tx->c_size)) {
        if (!sp_is_slot_clear(spp_tx, i)) {
            res = sp_clear_slot(spp_tx, &s, i);
            assert(res);
            assert(s.client_data == 0);

            //printf("benchmark_tx_done()\n");
            benchmark_tx_done(s.client_data - 1);
        }
        i = (i + 1) % spp_tx->c_size;
    }
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
    bool res;

    alloc_mem(&buffer_frame, &buffer_base, BUFFER_SIZE * count);

    // In the RX buffer we have to make sure the queue manager knows that there
    // are no buffers in it atm.
    spp_rx = sp_create_shared_pool(count, RX_BUFFER_ID);
    res = sp_set_write_index(spp_rx, count - 1);
    spp_rx->ghost_read_id = count - 1;
    assert(res);

    spp_tx = sp_create_shared_pool(count, TX_BUFFER_ID);

    idc_register_buffer(binding_rx, buffer_frame, spp_rx->cap, queue, count,
                        RX_BUFFER_ID);
    while (bufid_rx == -1ULL) { event_dispatch(ws); }

    idc_register_buffer(binding_tx, buffer_frame, spp_tx->cap, queue, count,
                        TX_BUFFER_ID);
    while (bufid_tx == -1ULL) { event_dispatch(ws); }
}


/******************************************************************************/
/* Flounder interface */

static void idc_register_buffer(struct net_queue_manager_binding *binding,
                                struct capref buf, struct capref sp,
                                uint64_t qid, uint64_t slots, uint8_t role)
{
    errval_t err;
    err = net_queue_manager_register_buffer__tx(binding, NOP_CONT, buf, sp,
                                                queue, slots, role);
}

static void new_buffer_id(struct net_queue_manager_binding *st, errval_t err,
                          uint64_t queueid, uint64_t buffer_id)
{
    printf("new_buffer_id(%"PRIu64")\n", buffer_id);

    assert(err_is_ok(err));

    if (st == binding_rx) {
        bufid_rx = buffer_id;
    } else {
        bufid_tx = buffer_id;
    }
}

static void sp_notification_from_driver(struct net_queue_manager_binding *b,
       uint64_t queueid, uint64_t type, uint64_t rts)
{
    //printf("sp_notification_from_driver\n");
}

static struct net_queue_manager_rx_vtbl rx_vtbl = {
    .new_buffer_id = new_buffer_id,
    .sp_notification_from_driver = sp_notification_from_driver,
    //.get_mac_address_response = get_mac_address_response,
};

static void bind_cb(void *st, errval_t err, struct net_queue_manager_binding *b)
{
    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;

    if (binding_rx == NULL) {
        binding_rx = b;
    } else {
        binding_tx = b;
    }
}

static void connect_to_driver(const char *cname, uint64_t qid)
{
    errval_t err;
    iref_t iref;
    char qm_name[MAX_SERVICE_NAME_LEN] = { 0 };

    snprintf(qm_name, sizeof(qm_name), "%s_%"PRIu64, cname, qid);
    err = nameservice_blocking_lookup(qm_name, &iref);
    assert(err_is_ok(err));

    err = net_queue_manager_bind(iref, bind_cb, NULL, get_default_waitset(),
                                 IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

}

void terminate_benchmark(void)
{
    vspace_unmap(buffer_base);
    cap_delete(buffer_frame);
    exit(-1);
}

static void process_cmdline(int argc, char* argv[])
{
    int i;
    for (i = 1; i < argc; i++) {
        benchmark_argument(argv[i]);
    }
}

static void eventloop(void)
{
    struct waitset *ws = get_default_waitset();

    while (1) {
        event_dispatch_non_block(ws);
        benchmark_do_pending_work();
        check_rx_ring();
        check_tx_ring();
    }
}

int main(int argc, char* argv[])
{
    struct waitset *ws = get_default_waitset();

    printf("elb_app: Started, v2\n");
    process_cmdline(argc, argv);

    char *cardname = get_cardname();
    if (cardname == NULL) {
        cardname = "e10k";
    }
    queue = get_cmdline_queueid();
    printf("Using [%s] as cardname and %"PRIu64"\n", cardname,
            queue);
    ELB1_DEBUG("Using [%s] as cardname\n", cardname);
    // Connect RX path
    connect_to_driver(cardname, queue);
    while (binding_rx == NULL) { event_dispatch(ws); }

    // Connect TX path
    connect_to_driver(cardname, queue);
    while (binding_rx == NULL) { event_dispatch(ws); }

    buffers_init(32);
    benchmark_init(32);

    eventloop();
}

