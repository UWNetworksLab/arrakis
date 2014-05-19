/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <utime.h>
#include <barrelfish/barrelfish.h>

int utime(const char *filename, const struct utimbuf *times)
{
    USER_PANIC("utime() NYI");
    return -1;
}
