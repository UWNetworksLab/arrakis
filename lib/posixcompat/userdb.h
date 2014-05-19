/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_USERDB_H
#define POSIXCOMPAT_USERDB_H

#include <pwd.h>
#include <barrelfish/cpu_arch.h> // for CURRENT_CPU_TYPE
#include <barrelfish_kpi/cpu.h>

#if CURRENT_CPU_TYPE == CPU_X86_64
# define CURRENT_CPU_TYPE_STR "x86_64"
#elif CURRENT_CPU_TYPE == CPU_X86_32
# define CURRENT_CPU_TYPE_STR "x86_32"
#elif CURRENT_CPU_TYPE == CPU_SCC
# define CURRENT_CPU_TYPE_STR "scc"
#elif CURRENT_CPU_TYPE == CPU_ARM5
# define CURRENT_CPU_TYPE_STR "armv5"
#elif CURRENT_CPU_TYPE == CPU_ARM7
# define CURRENT_CPU_TYPE_STR "armv7"
#else
# error "unknown CURRENT_CPU_TYPE"
#endif

#define DEFAULT_SHELL "/" CURRENT_CPU_TYPE_STR "/sbin/fish"

/*
 * Static user database akin /etc/passwd.
 */

static struct passwd userdb[] = {
    { // dummyuser
        .pw_name = "user",
        .pw_passwd = "abcd",
        .pw_uid = 1000,
        .pw_gid = 100,
        .pw_gecos = "John Doe",
        .pw_dir = "/",
        .pw_shell = DEFAULT_SHELL,
    },
    { // non-privileged user for sshd
        .pw_name = "sshd",
        .pw_passwd = "*",
        .pw_uid = 1001,
        .pw_gid = 1001,
        .pw_gecos = "sshd user",
        .pw_dir = "/",
        .pw_shell = DEFAULT_SHELL,
    }
};

#endif // POSIXCOMPAT_USERDB_H
