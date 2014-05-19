/**
 * \file
 * \brief init process.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "init.h"
#include <stdlib.h>
#include <trace/trace.h>
#include <barrelfish/morecore.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/lmp_chan.h>

static coreid_t my_core_id;

#ifdef __x86_64__
#       define MONITOR_NAME  "x86_64/sbin/monitor"
#       define MEM_SERV_NAME "x86_64/sbin/mem_serv"
#elif defined(__scc__)
#       define MONITOR_NAME  "scc/sbin/monitor"
#       define MEM_SERV_NAME "scc/sbin/mem_serv"
#elif defined(__i386__)
#       define MONITOR_NAME  "x86_32/sbin/monitor"
#       define MEM_SERV_NAME "x86_32/sbin/mem_serv"
#elif defined(__ARM_ARCH_7A__)
#       define MONITOR_NAME  "armv7/sbin/monitor"
#       define MEM_SERV_NAME "armv7/sbin/mem_serv"
#elif defined(__ARM_ARCH_7M__)
#       define MONITOR_NAME  "armv7-m/sbin/monitor"
#       define MEM_SERV_NAME "armv7-m/sbin/mem_serv"
#elif defined(__arm__)
#       define MONITOR_NAME  "armv5/sbin/monitor"
#       define MEM_SERV_NAME "armv5/sbin/mem_serv"
#else
#       error Unknown architecture
#endif

const char *multiboot_strings;

struct bootinfo *bi;

/// Channels to monitor and mem_serv
static struct lmp_chan monitor_chan, mem_serv_chan;

/// State of caps received from monitor and mem_serv
static struct {
    int pos;
    bool done;
} recv_state;

/* handler function that runs when we get a message from either the mem_serv or
 * monitor channels. once we've received caps from both, we send them to each
 * other's opposite to allow them to construct a proper point-to-point channel
 */
static void init_recv_handler(void *arg)
{
    struct lmp_chan *lc = arg;
    struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
    struct capref cap;
    errval_t err;

    err = lmp_chan_recv(lc, &msg, &cap);
    if (err_is_fail(err)) {
        if (lmp_err_is_transient(err)) {
            // re-register
            struct event_closure recv_handler = {
                .handler = init_recv_handler,
                .arg = arg,
            };
            struct waitset *ws = get_default_waitset();
            err = lmp_chan_register_recv(lc, ws, recv_handler);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in lmp_chan_register_recv");
                abort();
            }
        } else {
            DEBUG_ERR(err, "in lmp_chan_recv");
            abort();
        }
    }

    assert(!capref_is_null(cap));
    assert(recv_state.pos < 2);

    // store cap
    lc->remote_cap = cap;

    // if we now have both caps, send them to each other
    if (++recv_state.pos == 2) {
        err = lmp_chan_send0(&monitor_chan, 0, mem_serv_chan.remote_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "sending cap to monitor");
            abort();
        }

        err = lmp_chan_send0(&mem_serv_chan, 0, monitor_chan.remote_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "sending cap to mem_serv");
            abort();
        }

        recv_state.done = true;
    }
}

/**
 * \brief Spawn essential initial domains.
 */
static errval_t bootstrap(int argc, char *argv[])
{
    errval_t err;

    /* Initialize tracing */
    err = trace_init();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error initializing trace buffer");
        printf("Warning: tracing not available\n");
    }
    #if defined(CONFIG_TRACE)
    err = trace_my_setup();
    if (err_is_fail(err)) {
		DEBUG_ERR(err, "error setting up tracing in init");
		printf("Warning: tracing not available\n");
	} else {
		// Initialize the pointers
		trace_reset_all();
		// Enable all subsystems by default.
		trace_set_all_subsys_enabled(true);
	}
    #endif

    /* Load mem_serv */
    printf("Spawning memory server (%s)...\n", MEM_SERV_NAME);
    struct spawninfo mem_serv_si;
    err = spawn_load_with_bootinfo(&mem_serv_si, bi, MEM_SERV_NAME, my_core_id);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_SPAWN_MEM_SERV);
    }

    /* Initialize mem_serv */
    err = initialize_mem_serv(&mem_serv_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_INIT_MEM_SERV);
    }

    /* Load monitor */
    printf("Spawning monitor (%s)...\n", MONITOR_NAME);
    struct spawninfo monitor_si;
#ifdef __scc__
    if(argc > 2) {
        assert(argc == 5);
        char appargs[1024];
        snprintf(appargs, 1024, "%s %s %s", argv[2], argv[3], argv[4]);
        monitor_si.codeword = 0xcafebabe;
        monitor_si.append_args = appargs;
    }
#endif
    err = spawn_load_with_bootinfo(&monitor_si, bi, MONITOR_NAME, my_core_id);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_SPAWN_MONITOR);
    }

    /* unmap bootinfo mem */
    err = multiboot_cleanup_mapping();
    if (err_is_fail(err)) {
        //return err_push(err, INIT_ERR_UNMAP_BOOTINFO);
        return err;
    }

    /* Initialize monitor */
    err = initialize_monitor(&monitor_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_INIT_MONITOR);
    }

    // Allocate an incoming LMP endpoint for each spawned domain
    err = lmp_chan_accept(&monitor_chan, DEFAULT_LMP_BUF_WORDS, NULL_CAP);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_SETUP_MONITOR_CHAN);
    }

    err = lmp_chan_alloc_recv_slot(&monitor_chan);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT);
    }

    err = lmp_chan_accept(&mem_serv_chan, DEFAULT_LMP_BUF_WORDS, NULL_CAP);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_SETUP_MEM_SERV_CHAN);
    }

    err = lmp_chan_alloc_recv_slot(&mem_serv_chan);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_LMP_ALLOC_RECV_SLOT);
    }

    // Register receive handlers for these channels
    struct waitset *ws = get_default_waitset();

    struct event_closure recv_handler = {
        .handler = init_recv_handler,
        .arg = &monitor_chan,
    };
    err = lmp_chan_register_recv(&monitor_chan, ws, recv_handler);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CHAN_REGISTER_RECV);
    }

    recv_handler.arg = &mem_serv_chan;
    err = lmp_chan_register_recv(&mem_serv_chan, ws, recv_handler);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CHAN_REGISTER_RECV);
    }

    /* Give EP to monitor */
    struct capref cap_dest;
    cap_dest.cnode = monitor_si.taskcn;
    cap_dest.slot  = TASKCN_SLOT_INITEP;
    err = cap_copy(cap_dest, monitor_chan.local_cap);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_EP_TO_MEM_SERV);
    }

    /* Give EP to mem_serv */
    cap_dest.cnode = mem_serv_si.taskcn;
    cap_dest.slot  = TASKCN_SLOT_INITEP;
    err = cap_copy(cap_dest, mem_serv_chan.local_cap);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_EP_TO_MONITOR);
    }

    /* Make monitor runnable */
    err = spawn_run(&monitor_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_RUN_MONITOR);
    }

    /* Make mem_serv runnable */
    err = spawn_run(&mem_serv_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_RUN_MEM_SERV);
    }

    err = spawn_free(&monitor_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_FREE_MONITOR);
    }

    err = spawn_free(&mem_serv_si);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_FREE_MEM_SERV);
    }

    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    printf("init: invoked as:");
    for (int i = 0; i < argc; i++) {
        printf(" %s", argv[i]);
    }
    printf("\n");

    errval_t err;

    // First argument contains the bootinfo location
    bi = (struct bootinfo*)strtol(argv[1], NULL, 10);

    /* Set the core id in the disp_priv struct */
    err = invoke_kernel_get_core_id(cap_kernel, &my_core_id);
    assert(err_is_ok(err));
    disp_set_core_id(my_core_id);

    /* Create our endpoint to self */
    err = cap_retype(cap_selfep, cap_dispatcher, ObjType_EndPoint, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to create our endpoint to self");
        abort();
    }

    /* Initialize local memory allocator */
    err = initialize_ram_alloc();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to init ram alloc");
        abort();
    }

    /* Spawn essential initial domains */
    err = bootstrap(argc, argv);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed bootstrap");
        abort();
    }

    /* hang around only until we proxy the endpoint caps */
    struct waitset *default_ws = get_default_waitset();
    while (!recv_state.done) {
        err = event_dispatch(default_ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            abort();
        }
    }

    return EXIT_SUCCESS;
}
