/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdio_file.h>
#include <barrelfish/barrelfish.h>
#include <vfs/fdtab.h>

int fileno(FILE *f)
{
    assert(f != NULL);
    /* XXX: seedy! */
    if (f == stdin) {
        return 0;
    } else if (f == stdout) {
        return 1;
    } else if (f == stderr) {
        return 2;
    }

    struct fdtab_entry e = {
        .type = FDTAB_TYPE_FILE,
        .handle = f->handle,
        .epoll_fd = -1,
    };
    return fdtab_search_alloc(&e);
}
