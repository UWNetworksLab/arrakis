/**
 * \file
 * \brief Startup daemon for Barrelfish.
 * At boot, after spawnd has started, startd decides which domains to spawn.
 * There are two phases to service startup.
 * 1) startup distributed services.  This proceeds in lockstep, with 
 *    each service being fully started before the next is started.
 * 2) startup applications. After all distributed services are started then 
 *    the applications are started.  Here the startd does not wait for a 
 *    previous domain to be started before continuing with the next one.
 */

/*
 * Copyright (c) 2010-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish_kpi/cpu.h> // for cpu_type_to_archstr()
#include <barrelfish/cpu_arch.h> // for CURRENT_CPU_TYPE

#include <vfs/vfs.h>

#include <dist/barrier.h>

#include "internal.h"

#define ALL_SPAWNDS_UP "all_spawnds_up" // this comes from spawnd/internal.h

coreid_t my_core_id;
bool is_bsp_core;
const char *gbootmodules;

/* set an initial default environment for our boot-time children */
static void init_environ(void)
{
    int r;

    /* PATH=/arch/sbin */
    char pathstr[64];
    snprintf(pathstr, sizeof(pathstr), "/%s/sbin",
             cpu_type_to_archstr(CURRENT_CPU_TYPE));
    pathstr[sizeof(pathstr) - 1] = '\0';
    r = setenv("PATH", pathstr, 0);
    if (r != 0) {
        USER_PANIC("failed to set PATH");
    }

    /* HOME=/ */
    r = setenv("HOME", "/", 0);
    if (r != 0) {
        USER_PANIC("failed to set HOME");
    }
}


/* open bootmodules file and read it in */
static void get_bootmodules(void)
{
    errval_t err;

    vfs_handle_t vh;
    err = vfs_open("/bootmodules", &vh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to open /bootmodules");
    }

    struct vfs_fileinfo info;
    err = vfs_stat(vh, &info);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to stat /bootmodules");
    }

    char *bootmodules = malloc(info.size + 1);
    if (bootmodules == NULL) {
        USER_PANIC_ERR(LIB_ERR_MALLOC_FAIL, 
                       "failed to allocate memory for bootmodules");
    }

    size_t bootmodules_len;
    err = vfs_read(vh, bootmodules, info.size, &bootmodules_len);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "unable to read /bootmodules");
    } else if (bootmodules_len == 0) {
        USER_PANIC("/bootmodules is empty");
    } else if (bootmodules_len != info.size) {
        USER_PANIC("unexpected short read of /bootmodules");
    }

    vfs_close(vh);

    // terminate as a string
    bootmodules[bootmodules_len] = '\0';
    gbootmodules = bootmodules;
}

int main(int argc, const char *argv[])
{
    errval_t err;

    vfs_init();
    
    my_core_id = disp_get_core_id();

    // read in the bootmodules file so that we know what to start
    get_bootmodules();
    
    // construct sane inital environment
    init_environ();

    // wait for spawnd boot to finish
    err = nsb_wait(ALL_SPAWNDS_UP);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed ns barrier wait for %s", ALL_SPAWNDS_UP);
    }
    // debug_printf("got \"%s\", continuing\n", ALL_SPAWNDS_UP);

    // XXX: wait for spawnd on same core to register itself
    // not sure why, but without this there is a race on bootup -AB 20110526

    /*char namebuf[16];
    snprintf(namebuf, sizeof(namebuf), "spawn.%u", my_core_id);
    namebuf[sizeof(namebuf) - 1] = '\0';

    iref_t iref;
    err = nameservice_blocking_lookup(namebuf, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "unexpected error waiting for '%s'\n", namebuf);
        return -1;
    }*/

    // startup distributed services
    spawn_dist_domains();

    // startup regular apps
    spawn_app_domains();

    // startup apps listed in bootscript
    spawn_bootscript_domains();

    spawn_arrakis_domains();

    return EXIT_SUCCESS;
}
