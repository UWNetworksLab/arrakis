/*
 * Copyright (c) 2011, 2013, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/wait.h>
#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>
#include "posixcompat.h"

#define MAX_CHILDREN    64

static pid_t children[MAX_CHILDREN];

int _posixcompat_add_child(pid_t pid);

pid_t wait(int *status)
{
    return waitpid(-1, status, 0);
}

pid_t wait3(int *status, int options, struct rusage *rusage)
{
    // XXX: Won't touch rusage at all
    return waitpid(-1, status, options);
}

pid_t waitpid(pid_t pid, int *status, int options)
{
    int i;
    bool nohang = options & WNOHANG ? true : false;

    if(pid <= 0) {
        // XXX: We don't have process groups, so all these cases are the same
        for(i = 0; i < MAX_CHILDREN; i++) {
            if(children[i] != 0) {
                break;
            }
        }

        if(i == MAX_CHILDREN) {
            errno = ECHILD;
            return -1;
        }
    } else {
        for(i = 0; i < MAX_CHILDREN; i++) {
            if(children[i] == pid) {
                break;
            }
        }

        if(i == MAX_CHILDREN) {
            errno = ECHILD;
            return -1;
        }
    }

    if(!nohang && pid <= 0) {
        printf("Warning: waitpid(<= 0, _, _) waits for the first child, "
               "not all\n");
    }

    // We got a child to wait for
    uint8_t exitcode;
    errval_t err = spawn_wait(children[i], &exitcode, nohang);
    if(err_is_fail(err)) {
        if(err_no(err) == SPAWN_ERR_DOMAIN_RUNNING) {
            return 0;
        } else {
            DEBUG_ERR(err, "spawn_wait");
        }
        return -1;
    }

    *status = exitcode;
    pid_t wpid = children[i];
    children[i] = 0;

    POSIXCOMPAT_DEBUG("waitpid(%d, %d, %d) = %d\n",
                      pid, *status, options, wpid);

    return wpid;
}

/**
 * \brief This is a hack to add a child process, while we don't have fork() or exec().
 */
int _posixcompat_add_child(pid_t pid)
{
    for(int i = 0; i < MAX_CHILDREN; i++) {
        if(children[i] == 0) {
            children[i] = pid;
            return 0;
        }
    }

    return -1;
}
