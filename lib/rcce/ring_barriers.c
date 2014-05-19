/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include "internal.h"

enum role_type {
    MSG_WAIT_MSG_WAIT,
    WAIT_MSG_WAIT_MSG,
};

#define COORDINATOR (bsp_id)
static enum role_type my_role;
static bool round;
static coreid_t experiment_max_cpus;

static void barrier_init(void)
{
    round = false;

    /* Determine roles as per my_core_id */
    if (my_core_id == COORDINATOR) {
        my_role = MSG_WAIT_MSG_WAIT;
    } else {
        my_role = WAIT_MSG_WAIT_MSG;
    }
}

static void ring_reply(struct rcce_binding *st)
{
    /* nop */
}

static void ring_request(struct rcce_binding *st)
{
    assert(!round);
    round = true;

    errval_t err = st->tx_vtbl.ring_reply(st, NOP_CONT);
    if (err_is_fail(err)){
        DEBUG_ERR(err, "send ring reply");
        abort();
    }
}

static void ring_request_cont(void *arg)
{
    struct rcce_binding *b = arg;
    errval_t err = b->tx_vtbl.ring_request(b, NOP_CONT);
    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            err = b->register_send(b, get_default_waitset(),
                                   MKCONT(ring_request_cont,b));
            assert(err_is_ok(err));
            return;
        }
        DEBUG_ERR(err, "send ring request");
        abort();
    }
}

static void message(void)
{
    for (coreid_t i = bsp_id + ((my_core_id - bsp_id + 1) % experiment_max_cpus);
         i != my_core_id;
         i = bsp_id + ((i - bsp_id + 1) % experiment_max_cpus)) {
        if (barray[i] != NULL) {
            ring_request_cont(barray[i]);
            return;
        }
    }
    assert(!"Should not get here");
}

struct msg_buf msgbuf[MAX_CPUS];

static void message_request(struct rcce_binding *st, uint16_t coreid,
                            uint8_t *msg, size_t size)
{
    assert(!msgbuf[coreid].pending);
    struct msg_buf *m = &msgbuf[coreid];

    m->msg = (char *)msg;
    m->length = size;
    m->pending = true;
    m->bulk = false;
    dprintf("%d: msg arrived, (%d, %lu)\n", my_core_id, coreid, size);
}

#ifdef BULK_TRANSFER_ENABLED
static void bulk_message_request(struct rcce_binding *b, uint16_t coreid,
                                 uint64_t id, uint64_t size,
                                 uint8_t last_fragment)
{
    struct rcce_state *st = b->st;
    assert(!msgbuf[coreid].pending);
    struct msg_buf *m = &msgbuf[coreid];
    assert(m->bulk_ready);
    assert(m->length == size);
    void *buf = bulk_slave_buf_get_mem(&st->btr, id, NULL);
    size_t copysize = last_fragment ? size - m->current : BLOCK_SIZE;

    char *bf = buf;
    /* printf("current = %lu, msg[0] = %d (%p), buf[0] = %d (%p), size = %llu, copysize = %lu\n", */
    /*        m->current, m->msg[0], m->msg, bf[0], buf, size, copysize); */
    /* for(int i = 0; i < 64; i++) { */
    /*     printf("%d ", bf[i]); */
    /* } */
    /* printf("\n"); */
    /* static int iter = 0; */
    /* if(++iter >= 2) { */
    /*     while(bf[0] == 0); */
    /* } */
    memcpy(m->msg + m->current, buf, copysize);
    /* m->msg = bulk_slave_buf_get_mem(&st->btr, id); */
    /* m->length = size; */
    m->current += copysize;

    if(last_fragment) {
        m->pending = true;
        m->bulk = true;
        m->id = id;
        m->bulk_ready = false;
    }

    errval_t err = barray[coreid]->tx_vtbl.
        bulk_message_reply(barray[coreid], NOP_CONT, my_core_id, id);
    assert(err_is_ok(err));
}
#endif

static void message_reply(struct rcce_binding *b, uint16_t coreid)
{
    struct rcce_state *st = b->st;
    assert(st->waitmsg == true);
    st->waitmsg = false;
    /* printf("%d: msg ack'd %p, %d\n", my_core_id, st, st->waitmsg); */
}

#ifdef BULK_TRANSFER_ENABLED
static void bulk_message_reply(struct rcce_binding *b, uint16_t coreid,
                               uint64_t id)
{
    struct rcce_state *st = b->st;
    errval_t err = bulk_free(&st->bt, id);
    assert(err_is_ok(err));
    assert(st->bulk_waitmsg == true);
    st->bulk_waitmsg = false;
}
#endif

static void message_request_cont(void *arg)
{
    struct rcce_state *st = arg;
    assert(!st->request_done);
    st->request_done = true;
    /* printf("%d: msg delivered, %p\n", my_core_id, st); */
}

#ifdef BULK_TRANSFER_ENABLED
static void bulk_recv_ready(struct rcce_binding *b, uint16_t coreid,
                            uint64_t size)
{
    struct rcce_state *st = b->st;

    assert(!st->recv_ready);
    st->recv_ready = true;
    dprintf("bulk_recv_ready\n");
}
#endif

#ifdef RCCE_PERF_MEASURE
#       include <barrelfish/dispatcher_arch.h>
#       include <barrelfish/curdispatcher_arch.h>
#       define PERF(x)  d->timestamp[x] = rdtsc()
#       define PERFM(x) x
#else
#       define PERF(x)
#       define PERFM(x)
#endif

errval_t send_message(char *msg, size_t size, coreid_t dest)
{
    assert(barray[dest] != NULL);
    struct rcce_state *st = barray[dest]->st;
    errval_t err;

#ifdef RCCE_PERF_MEASURE
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* d =
        get_dispatcher_shared_generic(handle);
#endif

    dprintf("%d: S(%lu,%d,%p,%d)\n", my_core_id, size, dest, st, st->waitmsg);

#ifdef BULK_TRANSFER_ENABLED
    // XXX: Assert we can always send a big buffer as bulk data for performance
    // reasons
    if(size > BLOCK_SIZE) {
        /* printf("size = %lu, BLOCK_SIZE = %u\n", size, BLOCK_SIZE); */
    }
    //    assert(size <= BLOCK_SIZE);
#endif

    PERF(0);

    // Wait til previous message has been processed by receiver
#ifdef BULK_TRANSFER_ENABLED
    while(st->waitmsg || st->bulk_waitmsg || !st->recv_ready) {
#else
    while(st->waitmsg) {
#endif
        dprintf("waiting\n");
        messages_wait_and_handle_next();
    }
    st->recv_ready = false;

    PERF(1);

#ifndef BULK_TRANSFER_ENABLED
    st->waitmsg = true;
    // Send via UMP
    st->request_done = false;
    PERF(2);
    err = barray[dest]->
        tx_vtbl.message_request(barray[dest], MKCONT(message_request_cont,st),
                                my_core_id, (uint8_t *)msg, size);
    assert(err_is_ok(err));
    PERF(16);
    while(!st->request_done) {
        /* printf("%d: handling\n", my_core_id); */
        messages_wait_and_handle_next();
    }
    PERF(17);
#else
    /* printf("recv ready, sending %d\n", msg[0]); */
    // Send via bulk transfer
    for(size_t i = 0; i < size; i += BLOCK_SIZE) {
        struct bulk_buf *bb = bulk_alloc(&st->bt);
        assert(bb != NULL);
        void *buf = bulk_buf_get_mem(bb);
        size_t sendsize = i + BLOCK_SIZE < size ? BLOCK_SIZE : size - i;
        bool last_fragment = i + BLOCK_SIZE < size ? false : true;

        memcpy(buf, msg + i, sendsize);
        char *bf = buf;
        /* printf("send to %p (%d), msg = %p, i = %lu, sendsize = %lu\n", buf, bf[0], msg, i, sendsize); */
        uintptr_t id = bulk_prepare_send(bb);
        st->bulk_waitmsg = true;
        err = barray[dest]->tx_vtbl.
            bulk_message_request(barray[dest], NOP_CONT, my_core_id, id,
                                 size, last_fragment);
        assert(err_is_ok(err));
        while(st->bulk_waitmsg) {
            dprintf("waiting for bulk reply\n");
            messages_wait_and_handle_next();
        }
    }
#endif

    return SYS_ERR_OK;
}

static void wait(void)
{
    while (!round) {
        messages_wait_and_handle_next();
    }
    round = false;
}

void barrier_wait(void)
{
    switch(my_role) {
    case MSG_WAIT_MSG_WAIT:
        message();
        wait();
        message();
        wait();
        break;

    case WAIT_MSG_WAIT_MSG:
        wait();
        message();
        wait();
        message();
        break;

    default:
        assert(!"should not get here");
    }
}

void barrier_binding_init(struct rcce_binding *binding)
{
    binding->rx_vtbl.ring_request = ring_request;
    binding->rx_vtbl.ring_reply   = ring_reply;
    binding->rx_vtbl.message_request = message_request;
    binding->rx_vtbl.message_reply = message_reply;
#ifdef BULK_TRANSFER_ENABLED
    binding->rx_vtbl.bulk_message_request = bulk_message_request;
    binding->rx_vtbl.bulk_message_reply = bulk_message_reply;
    binding->rx_vtbl.bulk_recv_ready = bulk_recv_ready;
#endif
}

void barriers_init(coreid_t max_cpus)
{
    experiment_max_cpus = max_cpus;

    barrier_init();
}
