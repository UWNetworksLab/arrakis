/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <string.h>
#include <vfs/vfs_path.h>
#include "posixcompat.h"

char *getcwd(char *buf, size_t size)
{
    char *cwd = getenv("PWD");
    if(cwd == NULL) {
        return NULL;
    }
    strncpy(buf, cwd, size);
    return buf;
}
