/**
 * \file
 * \brief Define generics for arch specific definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_KPI_PAGING_H
#define ARCH_X86_32_BARRELFISH_KPI_PAGING_H

#include <target/x86_32/barrelfish_kpi/paging_target.h>

// A number of configuration assertions that have to hold
#if !defined(CONFIG_PAE) && defined(CONFIG_NXE)
#       error NXE is only supported with PAE!
#endif

/**
 * Information about page sizes
 */
#define BASE_PAGE_BITS      X86_32_BASE_PAGE_BITS
#define BASE_PAGE_SIZE      X86_32_BASE_PAGE_SIZE
#define BASE_PAGE_MASK      X86_32_BASE_PAGE_MASK
#define BASE_PAGE_OFFSET    X86_32_BASE_PAGE_OFFSET

#define LARGE_PAGE_BITS      X86_32_LARGE_PAGE_BITS
#define LARGE_PAGE_SIZE      X86_32_LARGE_PAGE_SIZE
#define LARGE_PAGE_MASK      X86_32_LARGE_PAGE_MASK
#define LARGE_PAGE_OFFSET    X86_32_LARGE_PAGE_OFFSET

/**
 * Bits within the various page directories and tables.
 */
#define PTABLE_EXECUTE_DISABLE  X86_32_PTABLE_EXECUTE_DISABLE
#define PTABLE_GLOBAL_PAGE      X86_32_PTABLE_GLOBAL_PAGE
#define PTABLE_ATTR_INDEX       X86_32_PTABLE_ATTR_INDEX
#define PTABLE_DIRTY            X86_32_PTABLE_DIRTY
#define PTABLE_ACCESSED         X86_32_PTABLE_ACCESSED
#define PTABLE_CACHE_DISABLED   X86_32_PTABLE_CACHE_DISABLED
#define PTABLE_WRITE_THROUGH    X86_32_PTABLE_WRITE_THROUGH
#define PTABLE_USER_SUPERVISOR  X86_32_PTABLE_USER_SUPERVISOR
#define PTABLE_READ_WRITE       X86_32_PTABLE_READ_WRITE
#define PTABLE_PRESENT          X86_32_PTABLE_PRESENT

#define PTABLE_SIZE             X86_32_PTABLE_SIZE
#define PTABLE_MASK             X86_32_PTABLE_MASK
#define PTABLE_CLEAR            X86_32_PTABLE_CLEAR

#define PTABLE_ACCESS_DEFAULT   X86_32_PTABLE_ACCESS_DEFAULT
#define PTABLE_ACCESS_READONLY  X86_32_PTABLE_ACCESS_READONLY

#define PTABLE_ENTRY_SIZE       X86_32_PTABLE_ENTRY_SIZE

#endif // ARCH_X86_32_BARRELFISH_KPI_PAGING_H
