/**
 * \file
 * \brief Low-level exit domain.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>

void (*_libc_exit_func)(int); // set by libbarrelfish

void _exit(int status)
{
    _libc_exit_func(status);
}
