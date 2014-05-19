/**
 * \file
 * \brief Gathers different information from every single core
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
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
#include <barrelfish/spawn_client.h>
#include <skb/skb.h>
#include <barrelfish/dispatch.h>
#include "datagatherer.h"

#define SPAWN_YOUR_SELF

//prototypes
#ifdef SPAWN_YOUR_SELF
static void spawnmyself(void);
#endif


static bool bsp_datagatherer = false;
static coreid_t core_id;

extern bool rtt_done, nr_cores_done;
extern int nr_of_running_cores;



int main(int argc, char **argv)
{
 //this is the bootstrap copy of the domain
     if (strcmp(argv[argc - 1], "SpAwNeD") != 0) {
        bsp_datagatherer = true;
    } else {
        bsp_datagatherer = false;
    }

    core_id = disp_get_core_id();
    skb_client_connect();

#ifdef SPAWN_YOUR_SELF
    if (bsp_datagatherer) {
        spawnmyself();
    }
#endif

//gather different types of data

//run cpuid
    gather_cpuid_data(core_id);

//get the number of running cores and their APIC IDs from the monitor
    if (bsp_datagatherer) {
        gather_nr_running_cores(get_monitor_binding());
    } else {
        nr_cores_done = true;
    }

//adding the numer of cores is the last operation performed by the datagatherer.
//therefore the domain can exit after this. process events as long as the number
//of cores has not yet been added to the SKB.
    struct waitset *ws = get_default_waitset();
    while (!nr_cores_done) {
        errval_t err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    skb_add_fact("datagatherer_done.");

    if (bsp_datagatherer) {
        int length = nr_of_running_cores + 1;
        while (length != nr_of_running_cores) {
            skb_execute_query("findall(X, datagatherer_done, L),length(L,Len),write(Len).");
            skb_read_output("%d", &length);
            thread_yield();
        }


        errval_t err = nameservice_register("datagatherer_done", 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_register failed");
        }
    }
    return 0;
}




#ifdef SPAWN_YOUR_SELF
//*********** everything needed to spawn myself to all other cores *************

static void spawnmyself(void)
{
    errval_t err;
    char *argv[] = {"datagatherer", "SpAwNeD", NULL};
    err = spawn_program_on_all_cores(false, argv[0], argv, NULL,
                                     SPAWN_FLAGS_DEFAULT, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error spawning other core");
    }
}
#endif
