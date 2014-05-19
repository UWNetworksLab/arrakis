/**
 * Tests cross core cap management
 *
 *
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <if/xcorecapbench_defs.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <trace/trace.h>
#include <string.h>
#include <bench/bench.h>

#define PARENT_BITS   16
#define CHILD_BITS    9
#define CAPS_PER_CORE (1 << (PARENT_BITS - CHILD_BITS))

/* --- Globals ---*/

static bool is_bsp;
static bool exported;
static bool connected;
static bool connect;
static bool start_sending_caps;
static bool start_retyping_caps;
static struct xcorecapbench_binding *bindings[MAX_CPUS];
static coreid_t my_coreid; 
static coreid_t num_cores;
static coreid_t wait_for;
static struct capref my_caps[CAPS_PER_CORE];
static struct capref retyped_caps[CAPS_PER_CORE];
static cycles_t total_cycles;

/* --- Binding handlers --- */

static void connect_handler(struct xcorecapbench_binding *b);
static void start_retyping_handler(struct xcorecapbench_binding *b);
static void start_sending_handler(struct xcorecapbench_binding *b);
static void send_cap_handler(struct xcorecapbench_binding *b, 
                             struct capref ram_cap); 
static void barrier_done_handler(struct xcorecapbench_binding *b, uint64_t cycles);

struct xcorecapbench_rx_vtbl rx_vtbl = {
    .start_retyping= start_retyping_handler,
    .start_sending = start_sending_handler,
    .send_cap      = send_cap_handler,
    .barrier_done  = barrier_done_handler,
    .connect       = connect_handler,
};


static void create_caps(void) 
{
    errval_t err;

    // allocate a bunch of ramcaps
    for (int i=0; i<CAPS_PER_CORE; i++) {
        err = ram_alloc(&my_caps[i], CHILD_BITS);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "xcorecap: RAM alloc failed\n");   
            abort(); 
        }
        err = slot_alloc(&retyped_caps[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "slot alloc err\n");   
            abort(); 
        }
    }
    printf("core %i caps created\n", my_coreid);
}

static inline bool redo_message(errval_t err) {
    if (err == FLOUNDER_ERR_TX_BUSY) {
        messages_wait_and_handle_next();
        return true;
    } else {
        return false;  
    }
}

static cycles_t send_caps(void)
{
    errval_t err;
    cycles_t time_taken = 0;

    srand(bench_tsc());  // random starting seed

    for (int i=0; i<CAPS_PER_CORE; i++) {
        coreid_t to_core;
        do {
            to_core = rand() % num_cores;
        } while(to_core == my_coreid);

        do {
            cycles_t start =  bench_tsc();
            err = bindings[to_core]->tx_vtbl.send_cap(
                       bindings[to_core], NOP_CONT, my_caps[i]);
            if (i >= 20 && i <= (CAPS_PER_CORE - 20)) { // avoid warmup / cooldown
                time_taken += (bench_tsc() - start);
            }
        } while(redo_message(err));

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "xcorecap: cap send failed\n");   
            abort(); 
        }
    }

    return time_taken / (CAPS_PER_CORE - 40);
}

static cycles_t retype_caps(void)
{
    errval_t err;
    cycles_t time_taken = 0;
    for (int i=0; i<CAPS_PER_CORE; i++) {
        cycles_t start =  bench_tsc();
        err = cap_retype(retyped_caps[i], my_caps[i], ObjType_Frame, CHILD_BITS);
        if (i >= 20 && i <= (CAPS_PER_CORE - 20)) { // avoid warmup / cooldown
            time_taken += (bench_tsc() - start);
        }
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "xcorecap: Retype to frame failed\n");    
        }
    }

    return time_taken / (CAPS_PER_CORE - 40);
}

static void destroy_caps(void)
{
    errval_t err;
    for (int i=0; i<CAPS_PER_CORE; i++) {
        err = cap_destroy(retyped_caps[i]);
        err = cap_revoke(my_caps[i]);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "xcorecap: Retype to frame failed\n");    
        }
    }
}

static void start_sending_handler(struct xcorecapbench_binding *b)
{
    start_sending_caps = true;
}

static void start_retyping_handler(struct xcorecapbench_binding *b)
{
    start_retyping_caps = true;
}

static void connect_handler(struct xcorecapbench_binding *b)
{
    connect = true;
}
static void barrier_done_handler(struct xcorecapbench_binding *b,
                                 uint64_t cycles)
{
    total_cycles += (cycles_t)cycles;
    assert(wait_for != 0);
    wait_for--;
}

static void send_cap_handler(struct xcorecapbench_binding *b, 
                             struct capref ram_cap)
{
    // don't do anything
}

/* ------------------------- Domain spawning code -------------------------- */

static errval_t spawn_other_cores(int argc, char *argv[]) {
    errval_t err;

    char core_id_char[32];
    snprintf(core_id_char, sizeof(core_id_char), "%d", num_cores);
    core_id_char[sizeof(core_id_char) - 1] = '\0';

    char *xargv[] = {argv[0], "client", core_id_char, NULL};

    for (int i=1; i<num_cores; i++) {
        /* XXX: assumes core IDs are 0-based and contiguous */
        err = spawn_program(i, xargv[0], xargv, NULL,
                            SPAWN_FLAGS_DEFAULT, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "error spawning other core");
            abort();
        }
    }

    return SYS_ERR_OK;
}


/* ---------------------------- Binding Setup Callbacks ------------------- */


static inline void get_service_name(char * service_name, size_t size,
                                    coreid_t coreid) 
{
    memset(service_name, 0, size);
    strncpy(service_name, "xcorecapbench_",
            sizeof("xcorecapbench_"));
    char local_id[32];
    snprintf(local_id, sizeof(local_id), "%d", 
             coreid);
    strcat(service_name, local_id);
}


static errval_t connect_cb(void *st, struct xcorecapbench_binding *b)
{
    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection 
    return SYS_ERR_OK;
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    // register this iref with the name service
    char my_service_name[128];
    get_service_name(my_service_name, 128, disp_get_core_id());
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
    exported = true;
}

static void bind_cb(void *st, errval_t err, struct xcorecapbench_binding *b)
{
    assert(err_is_ok(err));

    b->rx_vtbl = rx_vtbl;
    
    coreid_t core = (coreid_t)(uintptr_t) st;
    bindings[core] = b;

    // check if we are finished
    for (int i=0; i<num_cores; i++) {
        if (i != my_coreid && bindings[i] == NULL) {
            return;  // not finished yet
        }
    }
    connected = true;
}

static void bind_core(coreid_t core) {
    errval_t err;
    iref_t iref;
    char core_service_name[128];
    get_service_name(core_service_name, 128, core);
    err = nameservice_blocking_lookup(core_service_name, &iref);
    if (err_is_fail(err)) {
        fprintf(stderr, "could not connect to the xcorecapbench.\n"
                "Terminating.\n");
        abort();
    }
    err = xcorecapbench_bind(iref, bind_cb, (void*)(uintptr_t)core, 
                             get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
}

/* ----- Experiment ------- */
static void do_experiment_bsp(void)
{
    errval_t err;

    printf("Sending Caps\n");
    wait_for = num_cores;
    total_cycles = 0;

    for (int i=1; i<num_cores; i++) {
        do {
            err = bindings[i]->tx_vtbl.start_sending(bindings[i], NOP_CONT);
        } while (redo_message(err));
        
        assert(err_is_ok(err));
    }
    total_cycles += send_caps();
    wait_for--;
    
    // wait for other cores to finish
    while(wait_for) {
        messages_wait_and_handle_next();
    }

    printf("Retyping Caps\n");
    for(int iter=0; iter<10; iter++) {
        wait_for = num_cores;
        total_cycles = 0;
        
        for (int i=1; i<num_cores; i++) {
            do {
                err = bindings[i]->tx_vtbl.start_retyping(bindings[i], NOP_CONT);
            } while (redo_message(err));
            
            assert(err_is_ok(err));
        }
        total_cycles += retype_caps();
        wait_for--;
        
        // wait for other cores to finish
        while(wait_for) {
            messages_wait_and_handle_next();
        }

        destroy_caps();
        
        printf("%" PRIuCYCLES "\n", total_cycles / num_cores);
    }
    printf("Done\n");
}


static void do_experiment_client(void)
{
    errval_t err;
    cycles_t cycles;

    while(!start_sending_caps) {
        messages_wait_and_handle_next();
    }
    cycles = send_caps();
    do {
        err = bindings[0]->tx_vtbl.barrier_done(bindings[0], NOP_CONT, cycles);
    } while (redo_message(err));
    assert(err_is_ok(err));

    while (true) {
        start_retyping_caps = false;
        while(!start_retyping_caps) {
            messages_wait_and_handle_next();
        }
        cycles = retype_caps();
        
        destroy_caps();
        do {
            err = bindings[0]->tx_vtbl.barrier_done(bindings[0], NOP_CONT, cycles);
        } while (redo_message(err));
        assert(err_is_ok(err));
    }
}
/* ------------------------------------ MAIN ----------------------------- */

#define NUM_RAM_CAPS 100
static struct capref ram_caps[NUM_RAM_CAPS];

static errval_t my_ram_alloc(struct capref *ret, uint8_t size_bits,
                             uint64_t minbase, uint64_t maxlimit)
{
    if (size_bits != BASE_PAGE_BITS) {
        printf("%d I cannot allocate this frame %d %d\n", my_coreid, size_bits, BASE_PAGE_BITS);
       abort();
    }
    static int i = 0;
    if (i == NUM_RAM_CAPS) {
        printf("%d, out of ram caps\n", my_coreid);
        abort();
    }
    *ret = ram_caps[i++];
    return SYS_ERR_OK;
}

static void ram_hack(void)
{
    errval_t err;

    for (int i = 0; i < 100; i++) {
        err = ram_alloc(&ram_caps[i], BASE_PAGE_BITS);
        assert(err_is_ok(err));
    }

    err = ram_alloc_set(my_ram_alloc);
    assert(err_is_ok(err));
}

int main (int argc, char* argv[]) 
{    
    errval_t err;
    my_coreid = disp_get_core_id();

    exported  = false;
    connected = false;
    start_sending_caps  = false;
    start_retyping_caps = false;

    // munge up a bunch of caps
    create_caps();

    assert (argc >= 2);
    if (!strncmp(argv[1], "client", sizeof("client"))) {
        is_bsp = false;
        assert (argc >= 3);
        num_cores = atoi(argv[2]);
        ram_hack();
    } else {
        is_bsp = true;
        num_cores = atoi(argv[1]);
    }

    // export our binding
    exported = false;
    err = xcorecapbench_export(NULL, export_cb, connect_cb, 
                               get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    while(!exported) {
        messages_wait_and_handle_next();
    }

    if (is_bsp) {
        wait_for = num_cores;
        // spawn other cores
        printf("Core %d starting xcorecapbench on %i cores\n", my_coreid, 
               num_cores);
        assert(disp_get_core_id() == 0);
        spawn_other_cores(argc, argv);
    } else {
        printf("Starting xcorecapbench on core %i \n", my_coreid);
    }

    // connect to other cores
    connected = false;
    for(int i=0; i<num_cores; i++) {
        if (i != my_coreid) {
            bind_core(i);
        }
    }
    while(!connected) {
        messages_wait_and_handle_next();
    }

    if (is_bsp) {
        wait_for--;
        // wait for cores to connect
        while(wait_for) {
            messages_wait_and_handle_next();
        }
    } else {
        err = bindings[0]->tx_vtbl.barrier_done(bindings[0], NOP_CONT, 0);
        assert(err_is_ok(err));
    }

    if (is_bsp) {
        do_experiment_bsp();
    } else {
        do_experiment_client();
    }

    messages_handler_loop();

    return 0;
}

