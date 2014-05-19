/**
 * \file
 * \brief Arch specific definitions, can be included by others.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_64_BARRELFISH_KPI_PAGING_H
#define TARGET_X86_64_BARRELFISH_KPI_PAGING_H

#ifndef __ASSEMBLER__
typedef uint64_t paging_x86_64_flags_t;
#endif

/** The system's base page size is 4kB */
#define X86_64_BASE_PAGE_BITS                  12
#define X86_64_BASE_PAGE_SIZE                  0x1000
#define X86_64_BASE_PAGE_MASK                  (X86_64_BASE_PAGE_SIZE - 1)
#define X86_64_BASE_PAGE_OFFSET(a)             ((a) & X86_64_BASE_PAGE_MASK)

/** The system's large page size is 2MB */
#define X86_64_LARGE_PAGE_BITS                  21
#define X86_64_LARGE_PAGE_SIZE                  0x200000
#define X86_64_LARGE_PAGE_MASK                  (X86_64_LARGE_PAGE_SIZE - 1)
#define X86_64_LARGE_PAGE_OFFSET(a)             ((a) & X86_64_LARGE_PAGE_MASK)

/**
 * Bits within the various page directories and tables.
 */
#define X86_64_PTABLE_EXECUTE_DISABLE  (((paging_x86_64_flags_t)1) << 63)
#define X86_64_VTD_PAGE_SNOOP          (((paging_x86_64_flags_t)1) << 11)
#define X86_64_PTABLE_GLOBAL_PAGE      (((paging_x86_64_flags_t)1) << 8)
#define X86_64_PTABLE_ATTR_INDEX       (((paging_x86_64_flags_t)1) << 7)
#define X86_64_PTABLE_DIRTY            (((paging_x86_64_flags_t)1) << 6)
#define X86_64_PTABLE_ACCESSED         (((paging_x86_64_flags_t)1) << 5)
#define X86_64_PTABLE_CACHE_DISABLED   (((paging_x86_64_flags_t)1) << 4)
#define X86_64_PTABLE_WRITE_THROUGH    (((paging_x86_64_flags_t)1) << 3)
#define X86_64_PTABLE_USER_SUPERVISOR  (((paging_x86_64_flags_t)1) << 2)
#define X86_64_PTABLE_READ_WRITE       (((paging_x86_64_flags_t)1) << 1)
#define X86_64_PTABLE_PRESENT          (((paging_x86_64_flags_t)1) << 0)

#define X86_64_PTABLE_SIZE         512     /**< Page directory/table size */
#define X86_64_PTABLE_MASK         0x1ff   /**< Page dir/table address mask */
#define X86_64_PTABLE_CLEAR        0       /**< Bitmap of a clear table entry */

#define X86_64_PTABLE_ENTRY_SIZE   sizeof(union x86_64_pdir_entry)

/// Default access is read/write, but not execute
#define X86_64_PTABLE_ACCESS_DEFAULT \
    (X86_64_PTABLE_EXECUTE_DISABLE | X86_64_PTABLE_USER_SUPERVISOR | \
     X86_64_PTABLE_READ_WRITE)
#define X86_64_PTABLE_ACCESS_READONLY \
    (X86_64_PTABLE_EXECUTE_DISABLE | X86_64_PTABLE_USER_SUPERVISOR)

/* Macros to compute the corresponding portions of the vaddr */
#define X86_64_PML4_BASE(base)         (((uint64_t)(base) >> 39) & X86_64_PTABLE_MASK)
#define X86_64_PDPT_BASE(base)         (((uint64_t)(base) >> 30) & X86_64_PTABLE_MASK)
#define X86_64_PDIR_BASE(base)         (((uint64_t)(base) >> 21) & X86_64_PTABLE_MASK)
#define X86_64_PTABLE_BASE(base)       (((uint64_t)(base) >> 12) & X86_64_PTABLE_MASK)

#endif // TARGET_X86_64_BARRELFISH_KPI_PAGING_H
