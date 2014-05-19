/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <unistd.h>

#include "posixcompat.h"

int getgroups(int size, gid_t grouplist[])
{
    assert(!"NYI");
    return -1;
}

int setgroups(int size, gid_t grouplist[])
{
    POSIXCOMPAT_DEBUG("Warning: setgroups(%d, ...) ignored\n", size);
    return 0;
}

int initgroups(const char *username, gid_t group)
{
    POSIXCOMPAT_DEBUG("Warning: initgroups(%s, %d) ignored\n", username, group);
    return 0;
}
