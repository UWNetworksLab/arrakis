/**
 * \file
 * \brief Paging definitions for arm_v7.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7_H
#define TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7_H

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

#define ARM_L1_OFFSET(addr)       (((uintptr_t)addr) >> 20)
#define ARM_L2_OFFSET(addr)       ((((uintptr_t)addr) >> 12) & 0xff)
#define ARM_PAGE_OFFSET(addr)     ((uintptr_t)addr & 0xfff)

#define ARM_L1_ALIGN                    16384u

#define ARM_L1_MAX_ENTRIES              4096u
#define ARM_L1_BYTES_PER_ENTRY          4u
#define ARM_L1_SECTION_BYTES            (1024u * 1024u)
#define ARM_L1_SECTION_MASK             0x000FFFFF

#define ARM_L2_ALIGN                    1024u
#define ARM_L2_MAX_ENTRIES              256u
#define ARM_L2_BYTES_PER_ENTRY          4u
#define ARM_L2_TABLE_BYTES              ARM_L2_ALIGN

#define ARM_L2_SMALL_CACHEABLE          0x008
#define ARM_L2_SMALL_BUFFERABLE         0x004
#define ARM_L2_SMALL_USR_RO             0x20
#define ARM_L2_SMALL_USR_RW             0x30
#define ARM_L2_SMALL_USR_NONE            0x10

/* Page type independent page options */
#define KPI_PAGING_FLAGS_READ    0x01
#define KPI_PAGING_FLAGS_WRITE   0x02
#define KPI_PAGING_FLAGS_EXECUTE 0x04
#define KPI_PAGING_FLAGS_NOCACHE 0x08
#define KPI_PAGING_FLAGS_MASK    0x0f

union arm_l1_entry {
    uint32_t raw;

    /// Invalid L1 entry
    struct {
        uint32_t        type            :2;     // == 0
    } invalid;

    /// L1 entry for 256 4K L2 entries
    struct {
        uint32_t        type            :2;     // == 1
        uint32_t        pxn             :1;     // PXN
        uint32_t        ns              :1;
        uint32_t        sbz0            :1;     // Should-be-zero
        uint32_t        domain          :4;
        uint32_t        sbz1            :1;     // Should-be-zero
        uint32_t        base_address    :22;
    } page_table;

    /// L1 entry for 1MB mapped section
    struct {
        uint32_t        type            :2;     // == 2
        uint32_t        bufferable      :1;
        uint32_t        cacheable       :1;
        uint32_t        execute_never   :1;
        uint32_t        domain          :4;
        uint32_t        sbz0            :1;
        uint32_t        ap10            :2;        // AP[1:0]
        uint32_t        tex             :3;        // type extension
        uint32_t        ap2             :1;        // AP[2]
        uint32_t        shareable       :1;
        uint32_t        not_global      :1;
        uint32_t        mbz0            :1;        //must be zero
        uint32_t        ns              :1;
        uint32_t        base_address    :12;
    } section;

    /// L1 entry for 16MB mapped super section
    struct {
        uint32_t        type            :2;     // == 3
        uint32_t        bufferable      :1;
        uint32_t        cacheable       :1;
        uint32_t        execute_never   :1;
        uint32_t        domain          :4;
        uint32_t        sbz0            :1;
        uint32_t        ap10            :2;        // AP[1:0]
        uint32_t        tex             :3;        // type extension
        uint32_t        ap2             :1;        // AP[2]
        uint32_t        shareable       :1;
        uint32_t        not_global      :1;
        uint32_t        mbo0            :1;        //must be one
        uint32_t        ns              :1;
        uint32_t        base_address    :12;
    } super_section;

};

#define L1_TYPE_INVALID_ENTRY           0
#define L1_TYPE_PAGE_TABLE_ENTRY        1
#define L1_TYPE_SECTION_ENTRY           2
#define L1_TYPE_SUPER_SECTION_ENTRY     3
#define L1_TYPE(x)              ((x) & 3)

union arm_l2_entry {
    uint32_t raw;

    /// Invalid L2 entry
    struct {
        uint32_t        type            :2;     // == 0
    } invalid;

    /// Descriptior for a 64K page
    struct {
        uint32_t        type            :2;     // == 1
        uint32_t        bufferable      :1;
        uint32_t        cacheable       :1;
        uint32_t        ap10            :2;        // AP[1:0]
        uint32_t        sbz0            :3;        // should be zero
        uint32_t        ap2             :1;        // AP[2]
        uint32_t        shareable       :1;
        uint32_t        not_global      :1;
        uint32_t        tex             :3;        // type extension TEX[2:0]
        uint32_t        execute_never   :1;
        uint32_t        base_address    :16;
    } large_page;

    /// Descriptor for a 4K page
    struct {
        uint32_t        type            :2;        // == 2 or 3
        uint32_t        bufferable      :1;
        uint32_t        cacheable       :1;
        uint32_t        ap10            :2;        // AP[1:0]
        uint32_t        tex             :3;        // type extension TEX[2:0]
        uint32_t        ap2             :1;        // AP[2]
        uint32_t        shareable       :1;
        uint32_t        not_global      :1;
        uint32_t        base_address    :20;
    } small_page;

};


#define L2_TYPE_INVALID_PAGE    0
#define L2_TYPE_LARGE_PAGE      1
#define L2_TYPE_SMALL_PAGE      2
#define L2_TYPE_SMALL_PAGE_XN   3
#define L2_TYPE(x)              ((x) & 3)

#define BYTES_PER_SECTION       0x100000
#define BYTES_PER_LARGE_PAGE    0x10000
#define BYTES_PER_PAGE          0x1000
#define BYTES_PER_SMALL_PAGE    0x400

#endif // TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7_H
