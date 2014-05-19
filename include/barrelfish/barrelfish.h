/**
 * \file
 * \brief Top-level header for convenient inclusion of standard
 * libbarrelfish headers.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_BARRELFISH_H
#define LIBBARRELFISH_BARRELFISH_H

/* standard libc types and assert */
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

/* barrelfish kernel interface definitions */
#include <errors/errno.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/cpu.h> // XXX!?
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/init.h> // kernel-defined part of cspace
#include <barrelfish_kpi/registers_arch.h>
#include <barrelfish_kpi/dispatcher_handle.h>

/* libbarrelfish API */
#include <barrelfish/types.h>
#include <barrelfish/capabilities.h>
#include <barrelfish/slab.h>
#include <barrelfish/vspace_common.h>
#include <barrelfish/threads.h>
#include <barrelfish/slot_alloc.h>
#include <barrelfish/ram_alloc.h>
#include <barrelfish/syscalls.h>
#include <barrelfish/cspace.h>
#include <barrelfish/domain.h>
#include <barrelfish/debug.h>
#include <barrelfish/static_assert.h>

/* XXX: utility macros. not sure where to put these */

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

/// Divide n by size, rounding up
#define DIVIDE_ROUND_UP(n, size)    (((n) + (size) - 1) / (size))

/// Round n down to the nearest multiple of size
#define ROUND_DOWN(n, size)         ((n) & (~((size) - 1)))

/** Macro to return the number of entries in a statically-allocated array. */
#define ARRAY_LENGTH(x) (sizeof(x) / sizeof((x)[0]))

/// Computes the floor of log_2 of the given number
static inline uint8_t log2floor(uintptr_t num)
{
    uint8_t l = 0;
    uintptr_t n;
    for (n = num; n > 1; n >>= 1, l++);
    return l;
}

/// Computes the ceiling of log_2 of the given number
static inline uint8_t log2ceil(uintptr_t num)
{
    uint8_t l = log2floor(num);
    if (num == ((uintptr_t)1) << l) { /* fencepost case */
        return l;
    } else {
        return l + 1;
    }
}

/* XXX: glue junk for old IDC system, to be removed!! */

void messages_wait_and_handle_next(void);
void __attribute__((noreturn)) messages_handler_loop(void);

#endif
