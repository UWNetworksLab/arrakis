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

/**
 * \brief Read the contents of a symbolic link relative to a directory file
 *        descriptor.
 */
ssize_t readlink(const char *path, char *buf, size_t bufsize)
{
    assert(!"NYI");
    return -1;
}
