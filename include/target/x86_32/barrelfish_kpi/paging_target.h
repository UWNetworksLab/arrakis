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

#ifndef TARGET_X86_32_BARRELFISH_KPI_PAGING_H
#define TARGET_X86_32_BARRELFISH_KPI_PAGING_H

#ifndef __ASSEMBLER__
#ifdef CONFIG_PAE
typedef uint64_t paging_x86_32_flags_t;
#else
typedef uint32_t paging_x86_32_flags_t;
#endif
#endif

/** The system's base page size is 4kB */
#define X86_32_BASE_PAGE_BITS                  12
#define X86_32_BASE_PAGE_SIZE                  0x1000
#define X86_32_BASE_PAGE_MASK                  (X86_32_BASE_PAGE_SIZE - 1)
#define X86_32_BASE_PAGE_OFFSET(a)             ((a) & X86_32_BASE_PAGE_MASK)

/** The system's large page size is 2MB or 4MB */
#ifdef CONFIG_PAE
#define X86_32_LARGE_PAGE_BITS                 21
#define X86_32_LARGE_PAGE_SIZE                 0x200000

#else
#define X86_32_LARGE_PAGE_BITS                 22
#define X86_32_LARGE_PAGE_SIZE                 0x400000

#endif

#define X86_32_LARGE_PAGE_MASK                  (X86_32_LARGE_PAGE_SIZE - 1)
#define X86_32_LARGE_PAGE_OFFSET(a)             ((a) & X86_32_LARGE_PAGE_MASK)

/**
 * Bits within the various page directories and tables.
 */
#ifdef CONFIG_NXE
#       define X86_32_PTABLE_EXECUTE_DISABLE  (((paging_x86_32_flags_t)1) << 63)
#else
#       define X86_32_PTABLE_EXECUTE_DISABLE  ((paging_x86_32_flags_t)0)
#endif
#ifndef __scc__
#       define X86_32_PTABLE_GLOBAL_PAGE      (((paging_x86_32_flags_t)1) << 8)
#       define X86_32_PTABLE_ATTR_INDEX       (((paging_x86_32_flags_t)1) << 7)
#else
#       define X86_32_PTABLE_GLOBAL_PAGE      ((paging_x86_32_flags_t)0)
#       define SCC_PTABLE_MESSAGE_BUFFER      (((paging_x86_32_flags_t)1) << 7)
#       define X86_32_PTABLE_ATTR_INDEX       (((paging_x86_32_flags_t)1) << 7)
#endif
#define X86_32_PTABLE_DIRTY            (((paging_x86_32_flags_t)1) << 6)
#define X86_32_PTABLE_ACCESSED         (((paging_x86_32_flags_t)1) << 5)
#define X86_32_PTABLE_CACHE_DISABLED   (((paging_x86_32_flags_t)1) << 4)
#define X86_32_PTABLE_WRITE_THROUGH    (((paging_x86_32_flags_t)1) << 3)
#define X86_32_PTABLE_USER_SUPERVISOR  (((paging_x86_32_flags_t)1) << 2)
#define X86_32_PTABLE_READ_WRITE       (((paging_x86_32_flags_t)1) << 1)
#define X86_32_PTABLE_PRESENT          (((paging_x86_32_flags_t)1) << 0)

#ifdef CONFIG_PAE
#define X86_32_PDPTE_SIZE              4
#define X86_32_PDPTE_MASK              3
#define X86_32_PDPTE_CLEAR             0

#define X86_32_PTABLE_SIZE         512     /**< Page directory/table size */
#define X86_32_PTABLE_MASK         0x1ff   /**< Page dir/table address mask */
#define X86_32_PTABLE_CLEAR        0       /**< Bitmap of a clear table entry */

#else
#define X86_32_PDIR_SIZE           1024
#define X86_32_PDIR_MASK           0x3ff
#define X86_32_PDIR_CLEAR          0

#define X86_32_PTABLE_SIZE         1024    /**< Page directory/table size */
#define X86_32_PTABLE_MASK         0x3ff   /**< Page dir/table address mask */
#define X86_32_PTABLE_CLEAR        0       /**< Bitmap of a clear table entry */

#endif

#define X86_32_PTABLE_ENTRY_SIZE   sizeof(union x86_32_pdir_entry)

#ifdef CONFIG_PAE
#define X86_32_PDPTE_BASE(base)        (((uint32_t)(base) >> 30) & X86_32_PDPTE_MASK)
#define X86_32_PDIR_BASE(base)         (((uint32_t)(base) >> 21) & X86_32_PTABLE_MASK)
#define X86_32_PTABLE_BASE(base)       (((uint32_t)(base) >> 12) & X86_32_PTABLE_MASK)
#else
#define X86_32_PDIR_BASE(base)         (((uint32_t)(base) >> 22) & X86_32_PTABLE_MASK)
#define X86_32_PTABLE_BASE(base)       (((uint32_t)(base) >> 12) & X86_32_PTABLE_MASK)
#endif

/// Default access is read/write, but not execute
#define X86_32_PTABLE_ACCESS_DEFAULT \
    (X86_32_PTABLE_EXECUTE_DISABLE | X86_32_PTABLE_USER_SUPERVISOR | \
     X86_32_PTABLE_READ_WRITE)
#define X86_32_PTABLE_ACCESS_READONLY \
    (X86_32_PTABLE_EXECUTE_DISABLE | X86_32_PTABLE_USER_SUPERVISOR)

#endif // TARGET_X86_32_BARRELFISH_KPI_PAGING_H
