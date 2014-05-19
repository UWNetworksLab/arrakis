/**
 * \file
 * \brief Paging definitions for arm_v5.
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V5_H
#define TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V5_H

/* Default page size is 4K */
#define BASE_PAGE_BITS          12
#define BASE_PAGE_SIZE          (1u << BASE_PAGE_BITS)
#define BASE_PAGE_MASK          (BASE_PAGE_SIZE - 1)
#define BASE_PAGE_OFFSET(a)     ((a) & BASE_PAGE_MASK)

/* 1MB large pages */
#define LARGE_PAGE_BITS         20
#define LARGE_PAGE_SIZE         (1u << PAGE_LARGE_BITS)
#define LARGE_PAGE_MASK         (LARGE_PAGE_SIZE - 1)
#define LARGE_PAGE_OFFSET(a)    ((a) & LARGE_PAGE_MASK)

#define ARM_L1_OFFSET(addr)       ((((uintptr_t)addr) >> 20) & 0xfff) // 12 bits
#define ARM_L2_OFFSET(addr)       ((((uintptr_t)addr) >> 12) & 0xff)  // 8 bits
#define ARM_PAGE_OFFSET(addr)     ((uintptr_t)addr & 0xfff)           // 12 bits

// L1 Alignment determined by TTBR register (bits 13:0 ignored by hardware)
#define ARM_L1_ALIGN                    16384u

#define ARM_L1_MAX_ENTRIES              4096u
#define ARM_L1_BYTES_PER_ENTRY          4u
#define ARM_L1_SECTION_BYTES            (1024u * 1024u)
#define ARM_L1_TABLE_BYTES              (ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY)

#define ARM_L2_ALIGN                    1024u
#define ARM_L2_MAX_ENTRIES              256u
#define ARM_L2_BYTES_PER_ENTRY          4u
#define ARM_L2_TABLE_BYTES              ARM_L2_ALIGN

/* Page type independent page options */
#define KPI_PAGING_FLAGS_READ    0x01
#define KPI_PAGING_FLAGS_WRITE   0x02
#define KPI_PAGING_FLAGS_EXECUTE 0x04
#define KPI_PAGING_FLAGS_NOCACHE 0x08
#define KPI_PAGING_FLAGS_MASK    0x0f

#endif // TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V5_H
