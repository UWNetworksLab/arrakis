/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <errno.h>
#include <string.h>
#include <unistd.h>

#include "posixcompat.h"

static char dummy_hostname[] = "barrelfish.local";

/**
 * \brief Get name of current host.
 */
int gethostname(char *name, size_t len)
{
    if (name == NULL) {
        errno = EFAULT;
        return -1;
    }

    if (sizeof(dummy_hostname) > len) {
        errno = ENAMETOOLONG;
        return -1;
    }

    POSIXCOMPAT_DEBUG("gethostname() returned dummy hostname \"%s\"\n",
                      dummy_hostname);
    memcpy(name, dummy_hostname, sizeof(dummy_hostname));
    return 0;
}
