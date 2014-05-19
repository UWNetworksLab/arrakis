/**
 * \file
 * \brief Paging definitions for arm_v7m.
 * this reflects the page table entry format on the cortex-M3 on the pandaboard, which
 * may not be the same as that of other armv7-m implementations (as the pure armv7-m
 * specification does not define any kind of address translation mechanism)
 * Alignment and size of tables is the same as for other armv7 architectures,
 * but it lacks all permission bits, providing endianness control instead
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
//TODO: heteropanda: maybe use the ignored bits in each entry to store metadata
//  at least put a bit for cacheability in there
#ifndef TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7M_H
#define TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7M_H


/* Default page size is 4K */
#define BASE_PAGE_BITS          12
#define BASE_PAGE_SIZE          (1u << BASE_PAGE_BITS)
#define BASE_PAGE_MASK          (BASE_PAGE_SIZE - 1)
#define BASE_PAGE_OFFSET(a)     ((a) & BASE_PAGE_MASK)

#define LARGE_PAGE_MASK                 0x0000FFFF  //64KB large page

#define ARM_L1_OFFSET(addr)       (((uintptr_t)addr) >> 20)
#define ARM_L2_OFFSET(addr)       ((((uintptr_t)addr) >> 12) & 0xff)
#define ARM_PAGE_OFFSET(addr)     ((uintptr_t)addr & 0xfff)

#define ARM_L1_ALIGN                    16384u

#define ARM_L1_MAX_ENTRIES              4096u
#define ARM_L1_BYTES_PER_ENTRY          4u
#define ARM_L1_SECTION_BYTES            (1024u * 1024u)
#define ARM_L1_SECTION_MASK             0x000FFFFF

#define ARM_L1_SUPERSECTION_MASK        0x00FFFFFF

#define ARM_L2_ALIGN                    1024u
#define ARM_L2_MAX_ENTRIES              256u
#define ARM_L2_BYTES_PER_ENTRY          4u
#define ARM_L2_TABLE_BYTES              ARM_L2_ALIGN

//#define ARM_L2_SMALL_CACHEABLE          0x008
//#define ARM_L2_SMALL_BUFFERABLE         0x004
//#define ARM_L2_SMALL_USR_RO             0x20
//#define ARM_L2_SMALL_USR_RW             0x30
//#define ARM_L2_SMALL_USR_NONE            0x10

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
        uint32_t        ignored3        :8;     //XXX: we currently use this byte to keep version numbers for the table
        uint32_t        base_address    :22;
    } page_table;

    /// L1 entry for 1MB mapped section
    struct {
        uint32_t        type            :2;     // == 2
        uint32_t        ignored3        :8;
        uint32_t        element_size    :2;        // for endianness conversion (should be irrelevant) - use "3" if in doubt
        uint32_t        ignored2        :3;        // type extension
        uint32_t        endianness      :1;        // locked on "0", little endian
        uint32_t        ignored1        :1;
        uint32_t        mixed_region    :1;
        uint32_t        mbz0            :1;        //must be zero
        uint32_t        ignored0        :1;
        uint32_t        base_address    :12;
    } section;

    /// L1 entry for 16MB mapped super section
    struct {
        uint32_t        type            :2;     // == 3
        uint32_t        ignored3        :8;
        uint32_t        element_size    :2;        // for endianness conversion (should be irrelevant)
        uint32_t        ignored2        :3;        // type extension
        uint32_t        endianness      :1;        // locked on "0", little endian
        uint32_t        ignored1        :1;
        uint32_t        mixed_region    :1;
        uint32_t        mbo0            :1;        //must be one
        uint32_t        ignored0        :5;
        uint32_t        base_address    :8;
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
        uint32_t        ignored3        :2;
        uint32_t        element_size    :2;        // for endianness conversion (should be irrelevant)
        uint32_t        ignored2        :3;        // type extension
        uint32_t        endianness      :1;        // locked on "0", little endian
        uint32_t        ignored1        :1;
        uint32_t        mixed_region    :1;
        uint32_t        ignored0        :4;
        uint32_t        base_address    :16;
    } large_page;

    /// Descriptor for a 4K page
    struct {
        uint32_t        type            :2;        // == 2 or 3
        uint32_t        ignored3        :2;
        uint32_t        element_size    :2;        // for endianness conversion (should be irrelevant)
        uint32_t        ignored2        :3;        // type extension
        uint32_t        endianness      :1;        // locked on "0", little endian
        uint32_t        ignored1        :1;
        uint32_t        mixed_region    :1;
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



#endif // TARGET_ARM_BARRELFISH_KPI_PAGING_ARM_V7M_H
