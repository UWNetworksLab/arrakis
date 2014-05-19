/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <angler/angler.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/debug.h>
#include <barrelfish/spawn_client.h>
#include <posixcompat.h>
#include <vfs/vfs.h>

#include <assert.h>
#include <stdio.h>
#include <unistd.h>

#define SHELL "fish"
#define BUFFER_SIZE 128

static int openpty(int *amaster, int *aslave)
{
    int ptm;
    char *pts;

    /* open master side */
    ptm = posix_openpt(O_RDWR | O_NOCTTY);
    if (ptm < 0) {
        return -1;
    }
    if (grantpt(ptm) < 0) {
        return -1;
    }
    if (unlockpt(ptm) < 0) {
        return -1;
    }
    *amaster = ptm;

    pts = ptsname(ptm);
    if (pts == NULL) {
        return -1;
    }

    /* open slave side */
    *aslave = open(pts, O_RDWR | O_NOCTTY);
    if (*aslave < 0) {
        close(*amaster);
        return -1;
    }

    return 0;
}

static void spawn_shell(int ttyfd)
{
    struct capref session_id;
    errval_t err;
    coreid_t my_core_id = disp_get_core_id();
    char *shell = SHELL;

    /* establish a new session */
    iref_t iref = posixcompat_pts_get_iref(ttyfd);
    err = angler_new_session_with_iref(iref, &session_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error starting session.");
    }
    debug_printf("Created a new session.\n");

    /* setup argv of the shell */
    char *shell_argv[2];
    shell_argv[0] = SHELL;
    shell_argv[1] = NULL;

    /* inherit the session capability */
    struct capref inheritcn_cap;
    err = alloc_inheritcn_with_sidcap(&inheritcn_cap, session_id);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error allocating inherit CNode with session cap.");
    }

    /* spawn shell on the same core */
    debug_printf("About to spawn fish.\n");
    err = spawn_program_with_caps(my_core_id, shell, shell_argv, NULL,
                                  inheritcn_cap, NULL_CAP, SPAWN_NEW_DOMAIN,
                                  NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error spawning shell.");
    }
}

int main(int argc, char *argv[])
{
    int ret = 0;
    int ptm = 0;
    int pts = 0;

    vfs_init();

    // allocate new pseudo-terminal
    ret = openpty(&ptm, &pts);
    debug_printf("Allocated pseudo-terminal with fds %d and %d.\n", ptm, pts);
    assert(ret == 0);

    // spawn shell
    spawn_shell(pts);

    // read from master side
    char buffer[BUFFER_SIZE + 1];
    ssize_t r = 0;
    while (true) {
        debug_printf("Waiting for characters at pseudo-terminal master.\n");
        r = read(ptm, buffer, BUFFER_SIZE);
        if (r > 0) {
            buffer[r] = '\0';
            printf("read %zd characters: %s\n.", r, buffer);
        } else {
            printf("error: read returned %zd\n", r);
        }
    }

    return 0;
}
