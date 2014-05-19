/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <assert.h>
#include "posixcompat.h"

pid_t getpid(void)
{
    domainid_t pid = disp_get_domain_id();

    // If we don't have a valid domainid (if we're a boot domain),
    // we fake a pid
    if(pid == 0) {
        pid = 3;
    }

    POSIXCOMPAT_DEBUG("getpid() = %d\n", pid);
    return pid;
}

pid_t getppid(void)
{
    char *sppid = getenv("PPID");
    int ppid;

    if(sppid == NULL) {
        // Fake parent ID if not in environment
        ppid = 2;
    } else {
        ppid = atoi(sppid);
    }

    POSIXCOMPAT_DEBUG("getppid() = %d\n", ppid);
    return ppid;
}
