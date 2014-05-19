/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/sys_debug.h>
#include <if/bulkbench_defs.h>
#include <bench/bench.h>

#define MAXROUND        1000
#define INIT_ROUNDS     10
#define MINBUFSIZE      (1 << 16)
#define MAXBUFSIZE      (1 << 24)
#define BULKSIZE        65536

#define BULK_PAGE_MAP VREGION_FLAGS_READ_WRITE

static char buffer[BULKSIZE];
static bool request_done = false;
static struct bulkbench_binding *peer = NULL;
static size_t block_size, bulk_inflight = 0;
static struct bulk_transfer bt;
static struct bulk_transfer_slave btr;
static bool client = false, use_memcpy = false;
static char *mode = "server";

/* done by client */
/* server sends the msg once he has created a cap and mapped it locally.
 * Server passes that cap to client so that client can also mount it. */
static void bulk_sys_init(struct bulkbench_binding *b, struct capref shared_mem)
{
    errval_t err;

    // Map the frame in local memory
    void *pool;
    err = vspace_map_one_frame_attr(&pool, BULKSIZE, shared_mem,
                                    BULK_PAGE_MAP, NULL, NULL);
    assert(pool != NULL);
    assert(err_is_ok(err));

    // Init receiver
    err = bulk_slave_init(pool, BULKSIZE, &btr);
    assert(err_is_ok(err));
    printf("%s: bulk_sys_init: done\n", mode);

    err = peer->tx_vtbl.bulk_init_reply(peer, NOP_CONT);
    assert(err_is_ok(err));
}

/* Done by server */
static void bulk_init_reply(struct bulkbench_binding *b)
{
    assert(!request_done);
    request_done = true;
    printf("%s: bulk_init_reply: done\n", mode);

}

static void bulk_message_request(struct bulkbench_binding *b, uint64_t id,
                                 uint64_t size, uint8_t last_fragment)
{
    void *buf = bulk_slave_buf_get_mem(&btr, id, NULL);
    static int iter = 0;
    static bool startup_round = true;
    static uint64_t timestamp[MAXROUND];

    printf("%s: bulk_message_request: started for id %"PRIu64"\n", mode, id);

    assert(last_fragment);

    if(use_memcpy) {
        memcpy(buffer, buf, size);
    }

    timestamp[iter] = bench_tsc();
    iter++;
    if(iter == MAXROUND) {
        iter = 0;
        if(startup_round) {
            startup_round = false;
        } else {
            for(int i = 1; i < MAXROUND; i++) {
                uint64_t diff = timestamp[i] - timestamp[i - 1];
                printf("rawresult %" PRIu64 "\n", diff);
            }
            printf("client done.\n");
            abort();
        }
    }

    printf("%s: bulk_message_request: almost done for id %"PRIu64", sending reply\n",
					mode, id);

    errval_t err =
        peer->tx_vtbl.bulk_message_reply(peer, NOP_CONT, id, last_fragment);
    assert(err_is_ok(err));
    printf("%s: bulk_message_request: done for id %"PRIu64", with sending reply\n",
					mode, id);

}

/* in server */
static void bulk_message_reply(struct bulkbench_binding *b, uint64_t id,
                               uint8_t last_fragment)
{
    errval_t err = bulk_free(&bt, id);
    assert(err_is_ok(err));
    bulk_inflight--;
    printf("%s: bulk_msg_reply: %"PRIu64" done\n", mode, id);
}

static struct bulkbench_rx_vtbl bulkbench_vtbl = {
    .bulk_init = bulk_sys_init,
    .bulk_init_reply = bulk_init_reply,
    .bulk_message_request = bulk_message_request,
    .bulk_message_reply = bulk_message_reply
};

static void _listening(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    /* Register the service with the nameserver */
    err = nameservice_register("bulkbench", iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
    printf("%s: _listening:nameservice bulkbench registered\n", mode);
}

static errval_t _connected(void *st, struct bulkbench_binding *b)
{
    b->rx_vtbl = bulkbench_vtbl;
    peer = b;
    assert(!request_done);
    request_done = true;
    printf("%s: _connected: connection arrived\n", mode);
    return SYS_ERR_OK;
}

static void client_connected(void *st, errval_t err,
                             struct bulkbench_binding *b)
{
    assert(err_is_ok(err));
    b->rx_vtbl = bulkbench_vtbl;
    peer = b;
    printf("%s: client_connected: done\n", mode);
}

/* in  server */
static void send(char *buf, size_t size)
{
    errval_t err;

    printf("%s: send: started\n", mode);
    for(size_t i = 0; i < size; i += block_size) {
        struct bulk_buf *bb;
        /* get memory chunk from shared memory */
        do {
            bb = bulk_alloc(&bt);
            if(bb == NULL) {
                // dispatch one
                event_dispatch(get_default_waitset());
            }
        } while(bb == NULL);

        bulk_inflight++;
        void *bbuf = bulk_buf_get_mem(bb);
        size_t sendsize = i + block_size < size ? block_size : size - i;
        bool last_fragment = i + block_size < size ? false : true;

        memcpy(bbuf, buf, sendsize);
        uintptr_t id = bulk_prepare_send(bb);

    retry:
        err = peer->tx_vtbl.bulk_message_request(peer, NOP_CONT, id, size,
                                                 last_fragment);
        if(err_is_fail(err)) {
            if(err_no(err) == FLOUNDER_ERR_TX_BUSY) {
                // Dispatch one
                event_dispatch(get_default_waitset());
                goto retry;
            } else {
                DEBUG_ERR(err, "Failure in send()");
                abort();
            }
        }
    }
    printf("%s: send: done\n", mode);
}

int main(int argc, char *argv[])
{
    /* int round; */
    /* size_t bufsize; */
    errval_t err;

    if(argc < 4) {
        printf("Usage: %s blocksize send/recv memcpy/nomemcpy\n",
               argv[0]);
        exit(EXIT_FAILURE);
    }

    bench_init();

    block_size = atoi(argv[1]);
    assert(block_size <= BULKSIZE);
    if(!strcmp(argv[2], "recv")) {
        client = true;
        mode = "client";
    }
    if(!strcmp(argv[3], "memcpy")) {
        use_memcpy = true;
    }

    // Runs only on the peer
    if(client) {
        iref_t iref;
        err = nameservice_blocking_lookup("bulkbench", &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_blocking_lookup failed");
            abort();
        }
        assert(iref != 0);

        err = bulkbench_bind(iref, client_connected, NULL, get_default_waitset(),
                             IDC_BIND_FLAGS_DEFAULT);
        assert(err_is_ok(err));

        for(;;) {
            event_dispatch(get_default_waitset());
        }
    }

    // Export service
    request_done = false;
    err = bulkbench_export(NULL, _listening, _connected, get_default_waitset(),
                           IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "rcce_export failed");
        abort();
    }
    while (!request_done) {
        event_dispatch(get_default_waitset());
    }

    // Init sender
    struct capref frame;
    err = bulk_create(BULKSIZE, block_size, &frame, &bt, false);
    assert(err_is_ok(err));

    err = peer->tx_vtbl.bulk_init(peer, NOP_CONT, frame);
    assert(err_is_ok(err));

    request_done = false;
    while (!request_done) {
        event_dispatch(get_default_waitset());
    }

    for(;;) {
        send(buffer, block_size);
    }

    // Run the benchmark
    /* for(bufsize = MINBUFSIZE; bufsize <= MAXBUFSIZE; bufsize *= 2) { */
    /*     for (round = 0; round < MAXROUND; round++) { */
            /* send(buffer, bufsize); */
        /* } */

        // Let the benchmark cool down
    /*     while(bulk_inflight > 0) { */
    /*         event_dispatch(get_default_waitset()); */
    /*     } */
    /* } */

    printf("client done\n");
    return 0;
}
