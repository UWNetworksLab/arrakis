/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/stat.h>
#include <barrelfish/barrelfish.h>
#include <unistd.h>
#include "posixcompat.h"

int chmod(const char *path, mode_t mode)
{
    POSIXCOMPAT_DEBUG("Warning: chmod(\"%s\", %o) ignored\n", path, mode);
    return 0;
}

int chown(const char *path, uid_t owner, gid_t group)
{
    POSIXCOMPAT_DEBUG("Warning: chown(\"%s\", %d, %d) ignored\n",
                      path, owner, group);
    return 0;
}
