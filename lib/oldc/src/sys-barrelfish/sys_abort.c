/**
 * \file
 * \brief Standard libc abort function.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>

/* XXX: FIXME: this introduces a link dependency on libbarrelfish! */
extern void sys_print(const char *, size_t);

void abort(void)
{
    // Do not use stderr here.  It relies on too much to be working.
    sys_print("Aborted\n", 8);
    exit(EXIT_FAILURE);

    // Can't assert() here (would re-enter abort())
    sys_print("FATAL: exit() returned in abort()!\n", 36);
    for(;;);
}
