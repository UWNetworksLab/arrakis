/**
 * \file
 * \brief RCCE library
 */

/*
 * Copyright (c) 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <ctype.h>
#include <RCCE_lib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#ifdef __scc__
#       include <barrelfish_kpi/shared_mem_arch.h>
#endif

#include "internal.h"

static bool core_present[MAX_CPUS] = {false};
coreid_t bsp_id;
static bool request_done = false;
coreid_t my_core_id;
coreid_t num_cores = 0;
static char my_name[100];
struct rcce_binding *barray[MAX_CPUS] = {NULL};
static struct rcce_binding *connect_request = NULL;
static uintptr_t connect_state = 0;

static void error_reply(struct rcce_binding *st, errval_t err, uint64_t state);
static void init_request(struct rcce_binding *st, coreid_t id, coreid_t bspid,
                         uint64_t state, struct capref shared_mem);

static struct rcce_rx_vtbl rcce_vtbl = {
    .init_request = init_request,
    .error_reply = error_reply,
};

static void client_connected(void *st, errval_t err, struct rcce_binding *b)
{
    struct rcce_state *cs = st;
    assert(err_is_ok(err));

    /* printf("%s: Am connected to client\n", my_name); */

    b->rx_vtbl = rcce_vtbl;
    b->st = cs;

    // Create a Frame Capability 
    size_t allocated_size;
    struct capref shared_mem;
#ifdef __scc__
    ram_set_affinity(SHARED_MEM_MIN + (PERCORE_MEM_SIZE * disp_get_core_id()),
                     SHARED_MEM_MIN + (PERCORE_MEM_SIZE * (disp_get_core_id() + 1)));
#endif
    errval_t r = frame_alloc(&shared_mem, BULK_SIZE * 2, &allocated_size);
    assert(err_is_ok(r));
#ifdef __scc__
    ram_set_affinity(0, 0);
#endif

    // Map the frame in local memory
    void *pool;
    r = vspace_map_one_frame_attr(&pool, allocated_size, shared_mem,
                                  BULK_PAGE_MAP, NULL, NULL);
    assert(pool != NULL);
    assert(err_is_ok(r));
    assert(allocated_size >= BULK_SIZE * 2);

    // Init sender
    err = bulk_init(pool, BULK_SIZE, BLOCK_SIZE, &cs->bt);
    assert(err_is_ok(err));

    // Init receiver
    err = bulk_slave_init(pool + BULK_SIZE, BULK_SIZE, &cs->btr);
    assert(err_is_ok(err));

    barrier_binding_init(b);
    barray[cs->index] = b;

    err = barray[cs->index]->tx_vtbl.init_request(barray[cs->index],
                                                  NOP_CONT, my_core_id, bsp_id,
                                                  (uint64_t)(uintptr_t)cs,
                                                  shared_mem);
    assert(err_is_ok(err));
}

static void connect(coreid_t idx)
{
    errval_t err;
    char id[100];
    snprintf(id, sizeof(id), "%s%d", my_name, idx);

    iref_t iref;
    err = nameservice_blocking_lookup(id, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_blocking_lookup failed");
        abort();
    }
    assert(iref != 0);

    struct rcce_state *st = malloc(sizeof(struct rcce_state));
    assert(st != NULL);
    memset(st, 0, sizeof(struct rcce_state));
    st->index = idx;
    st->request_done = false;

    /* printf("%s: rcce_bind\n", my_name); */

    err = rcce_bind(iref, client_connected, st, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    /* printf("%s: waiting\n", my_name); */

    while (!st->request_done) {
        messages_wait_and_handle_next();
    }

    /* printf("%s: done\n", my_name); */
}

static void init_request(struct rcce_binding *st, coreid_t id, coreid_t bspid,
                         uint64_t state, struct capref shared_mem)
{
    errval_t err;
    struct rcce_state *rs = st->st;

    // Initialize local state for incoming connection
    barray[id] = st;
    bsp_id = bspid;

    // Map the frame in local memory
    void *pool;
    err = vspace_map_one_frame_attr(&pool, BULK_SIZE * 2, shared_mem,
                                    BULK_PAGE_MAP, NULL, NULL);
    assert(pool != NULL);
    assert(err_is_ok(err));

    // Init receiver
    err = bulk_slave_init(pool, BULK_SIZE, &rs->btr);
    assert(err_is_ok(err));

    // Init sender
    err = bulk_init(pool + BULK_SIZE, BULK_SIZE, BLOCK_SIZE, &rs->bt);
    assert(err_is_ok(err));

    if(connect_request == NULL && my_core_id != bspid) {
        connect_request = st;
        connect_state = state;
    } else {
        err = st->tx_vtbl.error_reply(st, NOP_CONT, SYS_ERR_OK, state);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "init_reply failed");
            abort();
        }
    }
}

static void error_reply(struct rcce_binding *st, errval_t err, uint64_t state)
{
    struct rcce_state *cs = (struct rcce_state *)(uintptr_t)state;
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "error_reply");
    }
    assert(err_is_ok(err));
    if(cs != NULL) {
        assert(!cs->request_done);
        cs->request_done = true;
    } else {
        assert(!request_done);
        request_done = true;
    }
}

static void _listening(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    /* printf("%s: listening\n", my_name); */

    /* Register the service with the nameserver */
    char serv[100];
    snprintf(serv, sizeof(serv), "%s%d", my_name, my_core_id);

    err = nameservice_register(serv, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }

    /* printf("%s: registered '%s'\n", my_name, serv); */

    assert(!request_done);
    request_done = true;
}

static errval_t _connected(void *st, struct rcce_binding *b)
{
    /* printf("%s: connected\n", my_name); */
    b->st = malloc(sizeof(struct rcce_state));
    assert(b->st != NULL);
    memset(b->st, 0, sizeof(struct rcce_state));
    b->rx_vtbl = rcce_vtbl;
    barrier_binding_init(b);
    return SYS_ERR_OK;
}

static void monitor_reply(struct monitor_binding *st, errval_t msgerr)
{
    assert(!request_done);
    assert(err_is_ok(msgerr));
    request_done = true;
}

static void set_present(char *str)
{
    while (*str != '\0') {
        if (!isdigit((int)*str)) {
            str++;
            continue;
        }
        int num = strtol(str, &str, 10);
        if (num < MAX_CPUS) {
            num_cores++;
            core_present[num] = true;
        }
    }
}

void setup_routes(int argc, char **argv)
{
    errval_t err;
    struct monitor_binding *st = get_monitor_binding();

    /* printf("%s: setup_routes\n", argv[0]); */

    /* Set core id */
    my_core_id = disp_get_core_id();
    strcpy(my_name, argv[0]);

    // Get number of cores
    coreid_t cores = atoi(argv[1]);

    // Get list of present cores
    for(int i = 3; i < argc; i++) {
        set_present(argv[i]);
    }

    if (strcmp(argv[argc - 1], "dummy")) { /* bsp core */
        // Spawn all copies
        bsp_id = my_core_id;

        /* Spawn on all cores */
        char *spawnargv[argc + 2];
        for (int i = 0; i < argc; i++) {
            spawnargv[i] = argv[i];
        }
        spawnargv[argc] = "dummy";
        spawnargv[argc + 1] = NULL;
        for(coreid_t i = 0; i < MAX_CPUS; i++) {
            if(core_present[i] && i != my_core_id) {
                err = spawn_program(i, my_name, spawnargv, NULL,
                                    SPAWN_FLAGS_DEFAULT, NULL);
                assert(err_is_ok(err));
            }
        }
    }

    /* printf("%s: exporting service\n", argv[0]); */
    /* Setup a server */
    request_done = false;
    err = rcce_export(NULL, _listening, _connected, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "rcce_export failed");
        abort();
    }
    while (!request_done) {
        event_dispatch(get_default_waitset());
    }

    if (strcmp(argv[argc - 1], "dummy")) { /* bsp core */
        for (coreid_t i = 0; i < MAX_CPUS; i++) {
            /* Connect to all cores */
            if (core_present[i] && i != my_core_id && barray[i] == NULL) {
                /* printf("%s: connecting to core %d\n", argv[0], i); */
                connect(i);
            }
        }
    } else {
        /* printf("%s: waiting for connection\n", argv[0]); */
        // Wait for an incoming connection request
        while(connect_request == NULL) {
            event_dispatch(get_default_waitset());
        }

        /* Connect to all cores to which we have not connected already */
        for (coreid_t i = 0; i < MAX_CPUS; i++) {
            if (core_present[i] && i != my_core_id && barray[i] == NULL) {
                /* printf("%s: slave connecting to core %d\n", argv[0], i); */
                connect(i);
            }
        }

        /* printf("%s: sending connect reply\n", argv[0]); */
        // Send the reply back
        err = connect_request->tx_vtbl.
            error_reply(connect_request, NOP_CONT, SYS_ERR_OK, connect_state);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "init_reply failed");
            abort();
        }
    }

    /* printf("%s: done\n", argv[0]); */

    // Determine maximum core ID
    coreid_t maxcore = 0;
    for(coreid_t i = 0; i < MAX_CPUS; i++) {
        if(core_present[i]) {
            maxcore = i;
        }
    }

    barriers_init(maxcore + 1);
}
