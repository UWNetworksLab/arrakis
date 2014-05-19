/**
 * \file
 * \brief init user-space domain special structures.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_INIT_H
#define BARRELFISH_KPI_INIT_H

#include <stdbool.h>

/**
 * Size of bootinfo mapping.
 */ 
#define BOOTINFO_SIZEBITS       (BASE_PAGE_BITS + 2)
#define BOOTINFO_SIZE           (1UL << BOOTINFO_SIZEBITS)

/**
 * Address (in spawned domain) of page with command-line arguments and environment
 */
#define ARGS_FRAME_BITS    17 // 128 kB
#define ARGS_SIZE          ((genpaddr_t)1 << ARGS_FRAME_BITS)
#define MAX_CMDLINE_ARGS   128
#define MAX_ENVIRON_VARS   128

/**
 * Size of page with inherited file descriptors
 */
#define FDS_SIZE          (32 * 1024)  // estimate worst case here.


/**
 * Size of dispatcher frame
 */
#define DISPATCHER_SIZE         ((genpaddr_t)1 << DISPATCHER_FRAME_BITS)

/**
 * Size of initial page to carry out monitor URPC
 */
// Supports at least #MON_URPC_CHANNEL_LEN
// Change #MON_URPC_CHANNEL_LEN if changing this
#define MON_URPC_SIZE           (2 * BASE_PAGE_SIZE)

/**

 * Maximum possible number of entries in the memory regions array. This is
 * limited by the page size minus the size of the initial part of the bootinfo
 * structure.
 */
#define MAX_MEM_REGIONS         ((BOOTINFO_SIZE - sizeof(struct bootinfo)) / \
                                 sizeof(struct mem_region))

/// Default number of entries in a cnode (in bits)
#define DEFAULT_CNODE_BITS    (BASE_PAGE_BITS - OBJBITS_CTE)

/// Default number of entries in a cnode
#define DEFAULT_CNODE_SLOTS      (1UL << DEFAULT_CNODE_BITS)

/// Number of entries in page cnode (in bits)
#define PAGE_CNODE_BITS         (DEFAULT_CNODE_BITS + 2)

/// Number of entries in page cnode
#define PAGE_CNODE_SLOTS        (1UL << PAGE_CNODE_BITS)

/// Remainder of guard size when subtracting bits from capaddr_t bitsize
#define GUARD_REMAINDER(bits)   (CPTR_BITS - (bits))

/* Root CNode */
#define ROOTCN_SLOT_TASKCN       0   ///< Taskcn slot in root cnode
#define ROOTCN_SLOT_PAGECN       1   ///< Pagecn slot in root cnode
#define ROOTCN_SLOT_BASE_PAGE_CN 2   ///< Slot for a cnode of BASE_PAGE_SIZE frames
#define ROOTCN_SLOT_SUPERCN      3   ///< Slot for a cnode of SUPER frames
#define ROOTCN_SLOT_SEGCN        4   ///< SegCN slot in root cnode
#define ROOTCN_SLOT_PACN         5   ///< PhysAddr cnode slot in root cnode
#define ROOTCN_SLOT_MODULECN     6   ///< Multiboot modules cnode slot in root cnode
#define ROOTCN_SLOT_SLOT_ALLOC0  7   ///< Root of slot alloc0
#define ROOTCN_SLOT_SLOT_ALLOC1  8   ///< Root of slot alloc1
#define ROOTCN_SLOT_SLOT_ALLOC2  9   ///< Root of slot alloc2
#define ROOTCN_SLOT_ARGCN        10  ///< Argcn slot in root cnode
#define ROOTCN_SLOTS_USER        11  ///< First free slot in root cnode for user

/* Size of CNodes in Root CNode if not the default size */
#define SLOT_ALLOC_CNODE_BITS   (DEFAULT_CNODE_BITS * 2)
#define SLOT_ALLOC_CNODE_SLOTS  (1UL << SLOT_ALLOC_CNODE_BITS)

/* Task CNode */
#define TASKCN_SLOT_DISPATCHER  1   ///< Dispatcher cap in task cnode
#define TASKCN_SLOT_ROOTCN      2   ///< RootCN slot in task cnode
#define TASKCN_SLOT_DISPFRAME   4   ///< Dispatcher frame cap in task cnode
#define TASKCN_SLOT_IRQ         5   ///< IRQ cap in task cnode
#define TASKCN_SLOT_IO          6   ///< IO cap in task cnode
#define TASKCN_SLOT_BOOTINFO    7   ///< Bootinfo frame slot in task cnode
#define TASKCN_SLOT_KERNELCAP   8   ///< Kernel cap in task cnode
#define TASKCN_SLOT_TRACEBUF    9   ///< Trace buffer cap in task cnode
#define TASKCN_SLOT_ARGSPAGE    10  ///< ?
#define TASKCN_SLOT_MON_URPC    11  ///< Frame cap for urpc comm.
#define TASKCN_SLOT_SESSIONID   12  ///< Session ID domain belongs to
#define TASKCN_SLOT_FDSPAGE     13  ///< cap for inherited file descriptors
#define TASKCN_SLOT_PERF_MON    14  ///< cap for performance monitoring
#define TASKCN_SLOT_DISPFRAME2  15  ///< Copy of dispatcher frame cap (mapped into spawn vspace)
#define TASKCN_SLOT_ARGSPAGE2   16  ///< Copy of environment cap (mapped into spawn vspace)
#define TASKCN_SLOTS_USER       17  ///< First free slot in taskcn for user

/// Address bits resolved for the standard CNodes (taskcn, supercn, base_page_cn)
#define DEFAULT_CN_ADDR_BITS    (CPTR_BITS - DEFAULT_CNODE_BITS)

#define CPTR_BASE_PAGE_CN_BASE  (ROOTCN_SLOT_BASE_PAGE_CN << DEFAULT_CN_ADDR_BITS)
#define CPTR_SUPERCN_BASE       (ROOTCN_SLOT_SUPERCN << DEFAULT_CN_ADDR_BITS)
#define CPTR_PHYADDRCN_BASE     (ROOTCN_SLOT_PACN << DEFAULT_CN_ADDR_BITS)
#define CPTR_MODULECN_BASE      (ROOTCN_SLOT_MODULECN << DEFAULT_CN_ADDR_BITS)
#define CPTR_PML4_BASE          (ROOTCN_SLOT_PAGECN << (CPTR_BITS - PAGE_CNODE_BITS))
#define MODULECN_SIZE_BITS      14  ///< Size of module cnode (in bits)

/**
 * Memory region types.
 */
enum region_type {
    /// Empty memory: describes a RAM cap in supercn
    RegionType_Empty,
    /// Code/Data of init itself: describes a Frame cap in segcn
    RegionType_RootTask,
    /// Physical address range (not RAM): describes a PhysAddr cap in physaddrcn
    RegionType_PhyAddr,
    /// BIOS tables and platform-specific data: describes a PhysAddr cap in physaddrcn
    RegionType_PlatformData,
    /// Multiboot module: describes multiple Frame caps in modulecn
    RegionType_Module,
    RegionType_Max ///< Must be last
};

/**
 * A memory region.
 */
struct mem_region {
    genpaddr_t       mr_base;///< Address of the start of the region
    enum region_type mr_type;///< Type of region
    uint8_t          mr_bits;///< Size as a power of two shift (not module type)
    bool             mr_consumed;///< Flag for user code to mark region consumed
    size_t           mrmod_size;///< Size in bytes (module type only)
    ptrdiff_t        mrmod_data;///< Offset of module string (module type only)
    int              mrmod_slot;///< First slot containing caps (module only)
};

/**
 * This structure holds essential information for the init process to
 * allocate and manage its address space.
 */
struct bootinfo {
    /// Number of entries in regions array
    size_t              regions_length;
    /// Amount of memory required to spawn another core
    size_t              mem_spawn_core;
    /// Memory regions array
    struct mem_region   regions[];
};

#endif
