/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <grp.h>
#include <assert.h>

struct group *getgrgid(gid_t gid)
{
    assert(!"NYI");
    return NULL;
}

struct group *getgrnam(const char *name)
{
    assert(!"NYI");
    return NULL;
}

struct group *getgrent(void)
{
    assert(!"NYI");
    return NULL;
}

void setgrent(void)
{
    assert(!"NYI");
    return;
}

void endgrent(void)
{
    assert(!"NYI");
    return;
}
