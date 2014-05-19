/**
 * \file
 * \brief Essential capability definitions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_CAPABILITIES_H
#define BARRELFISH_CAPABILITIES_H

/* FIXME: OBJBITS defines must match sizes in Hamlet's capabilities/caps.hl */

// Size of CNode entry
#define OBJBITS_CTE             7

// Size of dispatcher
#define OBJBITS_DISPATCHER     10

#ifndef __ASSEMBLER__

#define CAPRIGHTS_READ          (1 << 0)
#define CAPRIGHTS_WRITE         (1 << 1)
#define CAPRIGHTS_EXECUTE       (1 << 2)
#define CAPRIGHTS_GRANT         (1 << 3)
#define CAPRIGHTS_IDENTIFY      (1 << 4)
#define CAPRIGHTS_NUM           5

#define CAPRIGHTS_ALLRIGHTS     ((1 << CAPRIGHTS_NUM) - 1)
#define CAPRIGHTS_READ_WRITE    (CAPRIGHTS_READ | CAPRIGHTS_WRITE)
#define CAPRIGHTS_NORIGHTS      0

typedef uint8_t         CapRights;
#define PRIuCAPRIGHTS PRIu8
#define PRIxCAPRIGHTS PRIx8

struct dcb;

#include <barrelfish_kpi/capbits.h>
#include <assert.h>

static inline bool type_is_vnode(enum objtype type)
{
    STATIC_ASSERT(25 == ObjType_Num, "Check VNode definitions");

    return (type == ObjType_VNode_x86_64_pml4 ||
            type == ObjType_VNode_x86_64_pdpt ||
            type == ObjType_VNode_x86_64_pdir ||
            type == ObjType_VNode_x86_64_ptable ||
            type == ObjType_VNode_x86_32_pdpt ||
            type == ObjType_VNode_x86_32_pdir ||
            type == ObjType_VNode_x86_32_ptable ||
            type == ObjType_VNode_ARM_l2 ||
            type == ObjType_VNode_ARM_l1
           );
}

/**
 * Return size of vnode in bits. This is the size of a page table page.
 *
 * @param type Object type.
 *
 * @return Number of bits represented by a VNode.
 */
static inline size_t vnode_objbits(enum objtype type)
{
    // This function should be emitted by hamlet or somesuch.
    STATIC_ASSERT(25 == ObjType_Num, "Check VNode definitions");

    if (type == ObjType_VNode_x86_64_pml4 ||
        type == ObjType_VNode_x86_64_pdpt ||
        type == ObjType_VNode_x86_64_pdir ||
        type == ObjType_VNode_x86_64_ptable ||
        type == ObjType_VNode_x86_32_pdpt ||
        type == ObjType_VNode_x86_32_pdir ||
        type == ObjType_VNode_x86_32_ptable)
    {
        return 12;      // BASE_PAGE_BITS
    }
    else if (type == ObjType_VNode_ARM_l1)
    {
        return 14;
    }
    else if (type == ObjType_VNode_ARM_l2)
    {
        return 12;
    }

    assert(0 && !"Page table size unknown.");
    return 0;
}

/**
 * Return number of page table entries for vnode in bits.
 * @param type Object type.
 * @return Number of page table entries in bits
 */
static inline size_t vnode_entry_bits(enum objtype type) {
    // This function should be emitted by hamlet or somesuch.
    STATIC_ASSERT(25 == ObjType_Num, "Check VNode definitions");

    if (type == ObjType_VNode_x86_64_pml4 ||
        type == ObjType_VNode_x86_64_pdpt ||
        type == ObjType_VNode_x86_64_pdir ||
        type == ObjType_VNode_x86_64_ptable)
    {
        return 9;      // log2(X86_64_PTABLE_SIZE)
    }
#ifdef CONFIG_PAE
    if (type == ObjType_VNode_x86_32_pdpt)
    {
        return 2;       // log2(X86_32_PDPTE_SIZE)
    }
    else if (type == ObjType_VNode_x86_32_pdir ||
             type == ObjType_VNode_x86_32_ptable)
    {
        return 9;       // log2(X86_32_PTABLE_SIZE) == log2(X86_32_PDIR_SIZE)
    }
#else
    if (type == ObjType_VNode_x86_32_pdir ||
        type == ObjType_VNode_x86_32_ptable)
    {
        return 10;      // log2(X86_32_PTABLE_SIZE) == log2(X86_32_PDIR_SIZE)
    }
#endif
    if (type == ObjType_VNode_ARM_l2)
    {
        return 9;       // log2(ARM_L2_MAX_ENTRIES)
    }
    else if (type == ObjType_VNode_ARM_l1)
    {
        return 12;      // log2(ARM_L1_MAX_ENTRIES)
    }

    assert(!"unknown page table type");
    return 0;
}


/**
 * CNode capability commands.
 */
enum cnode_cmd {
    CNodeCmd_Copy,      ///< Copy capability
    CNodeCmd_Mint,      ///< Mint capability
    CNodeCmd_Retype,    ///< Retype capability
    CNodeCmd_Delete,    ///< Delete capability
    CNodeCmd_Revoke,    ///< Revoke capability
    CNodeCmd_Create,    ///< Create capability
};

enum vnode_cmd {
    VNodeCmd_Map,
    VNodeCmd_Unmap,
    VNodeCmd_Identify,   ///< Return the physical address of the VNode
};

/**
 * Kernel capabilities commands.
 * Monitor's invocations of capability operations
 * which the kernel will not subject to cross core checks
 */
enum kernel_cmd {
    KernelCmd_Spawn_core,         ///< Spawn a new kernel
    KernelCmd_Identify_cap,       ///< Return the meta data of a capability
    KernelCmd_Identify_domains_cap,  ///< Return the meta data of another domain's capability
    KernelCmd_Remote_cap,         ///< Set capability as being remote
    KernelCmd_Create_cap,         ///< Create a new capability
    KernelCmd_Iden_cnode_get_cap, ///< Look up cnode, return cap within
    KernelCmd_Get_core_id,        ///< Returns the id of the core the domain is on
    KernelCmd_Get_arch_id,        ///< Returns arch id of caller's core
    KernelCmd_Nullify_cap,        ///< Set the capability to NULL allowed it to be reused
    KernelCmd_Unmap_vaddr,
    KernelCmd_Setup_trace,        ///< Set up trace buffer
    KernelCmd_Register,           ///< Register monitor notify endpoint
    KernelCmd_Domain_Id,          ///< Set domain ID of dispatcher
    MonitorCmd_Retype,
    MonitorCmd_Delete,
    MonitorCmd_Revoke,
    KernelCmd_Sync_timer,
    KernelCmd_Spawn_SCC_Core,
    KernelCmd_IPI_Register,
    KernelCmd_IPI_Delete,
    KernelCmd_Count
};

/**
 * Specific commands for dispatcher capabilities.
 */
enum dispatcher_cmd {
    DispatcherCmd_Setup,            ///< Set dispatcher parameters
    DispatcherCmd_Properties,       ///< Set dispatcher properties
    DispatcherCmd_PerfMon,          ///< Performance monitoring
    DispatcherCmd_SetupGuest,       ///< Set up the DCB of a guest domain
    DispatcherCmd_DumpPTables       ///< Dump hw page tables of dispatcher
};

/**
 * Frame capability commands.
 */
enum frame_cmd {
    FrameCmd_Identify,      ///< Return physical address of frame
    FrameCmd_SCC_Identify,  ///< Return MC route to frame
    FrameCmd_ModifyFlags,   ///< Modify flags for (part of) the mapped region of frame
};

/**
 * IRQ Table capability commands.
 */
enum irqtable_cmd {
    IRQTableCmd_Set,    ///< Set endpoint for IRQ# notifications
    IRQTableCmd_Delete  ///< Remove notification endpoint for IRQ#
};

/**
 * IO capability commands.
 */
enum io_cmd {
    IOCmd_Outb,         ///< Output byte to port
    IOCmd_Outw,         ///< Output word to port
    IOCmd_Outd,         ///< Output double word to port
    IOCmd_Inb,          ///< Input byte from port
    IOCmd_Inw,          ///< Input word from port
    IOCmd_Ind           ///< Input double word from port
};

/**
 * Notify capability commands.
 */
enum notify_cmd {
    NotifyCmd_Send
};


/**
 * Performance monitoring commands.
 * Seems to be already included in the Dispatcher capability.
 */
enum perfmon_cmd {
    PerfmonCmd_Activate,    ///< Activate performance counters
    PerfmonCmd_Deactivate,  ///< Deactivate performance counters 
    PerfmonCmd_Write        ///< Read current performance counter values
};

/**
 * ID capability commands.
 */
enum id_cmd {
    IDCmd_Identify  ///< Return system-wide unique ID
};

/**
 * Maximum command ordinal.
 */
#define CAP_MAX_CMD KernelCmd_Count

/**
 * \brief Values returned from frame identify invocation
 */
struct frame_identity {
    genpaddr_t base;   ///< Physical base address of frame
    uint8_t bits;      ///< Size of frame, in bits
};

/**
 * \brief Values returned from the VNode identify invocation
 */
struct vnode_identity {
    genpaddr_t base;   ///< Physical base address of the VNode
    uint8_t type;      ///< Type of VNode
};

#ifdef __scc__
struct scc_frame_identity {
    uint8_t route, subdest;
    uint16_t addrbits;
};
#endif

#endif // __ASSEMBLER__

#endif // BARRELFISH_CAPABILITIES_H
