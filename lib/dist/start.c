/**
 * \file
 * \brief Startup code for distributed services
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/spawn_client.h>

#include <dist/args.h>
#include <dist/barrier.h>
#include <dist/start.h>


static int run_master_wrapper(coreid_t core, struct dist_args *d_args,
                              void *m_args, char *name)
{
    assert(d_args != NULL);
    assert(name != NULL);

    errval_t err;
    int i;

    debug_printf("rmw: %s master on core %d\n", name, core);

    debug_printf("rmw: spawning on %d cores\n", d_args->cores_len);
    
    int num_m_args;
    char **m_args_string;
    err = worker_args(m_args, &m_args_string, &num_m_args);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "preparing worker args");
        return EXIT_FAILURE;
    }

    debug_printf("rmw: after worker_args\n");

    // set up args for the spawn
    // -w
    // -c <core list>
    char *new_argv[4 + num_m_args + 1];
    new_argv[0] = d_args->path;
    new_argv[1] = "-w"; 
    new_argv[2] = "-c";
    new_argv[3] = list_to_string(d_args->cores, d_args->cores_len);
    assert(new_argv[3] != NULL);
    if (new_argv[3] == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "out of memory");
        return EXIT_FAILURE;
    }
    // worker-specific args
    for (i = 0; i < num_m_args; i++) {
        new_argv[4+i] = m_args_string[i];
    }
    new_argv[4+i] = NULL;

    debug_printf("rmw: after setting up new_argv\n");

    for (i = 0; i < d_args->cores_len; i++) {
        err = spawn_program(d_args->cores[i], new_argv[0], new_argv, 
                            NULL, 0, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "spawning percore %s worker on core %d", name, i);
            return EXIT_FAILURE;
        }

        debug_printf("rmw: after worker %d spawned\n", i);

    }

    debug_printf("rmw: after all workers spawned\n");

    free(m_args_string);
    
    // wait for all the spawned workers to start up
    err = nsb_master_l(d_args->cores, d_args->cores_len, name);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "barrier master failed");
        return EXIT_FAILURE;
    }

    debug_printf("rmw: after all workers started\n");

    err = run_master(core, d_args->cores, d_args->cores_len, m_args, name); 
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "run_master failed");
        return EXIT_FAILURE;
    }

    debug_printf("rmw: after run_master\n");

    return EXIT_SUCCESS;
}

static int run_worker_wrapper(coreid_t core, struct dist_args *d_args,
                              void *m_args, char *name)
{

    assert(d_args != NULL);
    assert(name != NULL);

    errval_t err;

    debug_printf("rww: %s percore worker on core %d\n", name, core);

    err = run_worker(core, d_args->cores, d_args->cores_len, m_args, name);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "run_worker");
        return EXIT_FAILURE; 
    }

    debug_printf("rww: after run_worker\n");

    // register worker barrier
    nsb_register_n(core, name);

    debug_printf("rww: registered %s.%d.ready\n", name, core);

    // TODO: not sure if this dispatch loop should be in the wrapper
    //       in case different waitset(s) should be used
    //       in case THC is used
    //       but it seems silly to make the user do it...

    // dispatch loop
    struct waitset *ws = get_default_waitset();
    while (true) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in main event_dispatch loop");
            return EXIT_FAILURE;
        }
    }

    // should never really get here.

    debug_printf("rww: after event dispatch loop\n");
    return EXIT_SUCCESS;
}


int dist_main(struct dist_args *d_args, void *m_args, char *name)
{
    coreid_t core = disp_get_core_id();

    if (d_args->master) {
        return run_master_wrapper(core, d_args, m_args, name);
    } else {
        return run_worker_wrapper(core, d_args, m_args, name);
    } 

    return EXIT_SUCCESS;
}
