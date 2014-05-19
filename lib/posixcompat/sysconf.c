/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <errno.h>
#include <unistd.h>
#include <vfs/fdtab.h> /* For MAX_FD */

#include "posixcompat.h"

/**
 * \brief Get configurable system variables.
 */
long sysconf(int name)
{
    switch(name) {
    case _SC_OPEN_MAX:
        return MAX_FD;

    default:
        POSIXCOMPAT_DEBUG("sysconf(%d): No implementation for this "
                          "configuration information.\n", name);
        errno = EINVAL;
        return -1;
    }
}
