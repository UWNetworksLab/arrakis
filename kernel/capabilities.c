/**
 * \file
 * \brief Kernel capability management implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <kernel.h>
#include <barrelfish_kpi/syscalls.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/lmp.h>
#include <offsets.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <mdb/mdb.h>
#include <mdb/mdb_tree.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <wakeup.h>

/// Ignore remote capabilities if this is defined
#define RCAPDB_NULL

/// Sets the specified number of low-order bits to 1
#define MASK(bits)      ((1UL << bits) - 1)

struct capability monitor_ep;

/**
 * ID capability core_local_id counter.
 */
static uint32_t id_cap_counter = 1;

/**
 *  Sets #dest equal to #src
 *
 * #dest cannot be in use.
 */
static errval_t set_cap(struct capability *dest, struct capability *src)
{
    /* Parameter checking */
    assert(src  != NULL);
    assert(dest != NULL);

    // Reserved object bits must always be greater/equal to actual object size
    assert((1UL << OBJBITS_CTE) >= sizeof(struct cte));

    // Cannot overwrite an already existing cap
    if (dest->type != ObjType_Null) {
        return SYS_ERR_SLOT_IN_USE;
    }

    memcpy(dest, src, sizeof(struct capability));
    return SYS_ERR_OK;
}

/**
 * \brief Determine how many objects can be created in a specified region.
 *
 * This function computes the number of objects that can be created by a call
 * to caps_create().
 *
 * \param type          Type of objects to create.
 * \param bits          Size of memory area as 2^bits.
 * \param objbits       For variable-sized objects, size multiplier as 2^bits.
 *
 * \return Number of objects to be created, or zero on error
 */

// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(ObjType_Num == 25, "Knowledge of all cap types");

static size_t caps_numobjs(enum objtype type, uint8_t bits, uint8_t objbits)
{
    switch(type) {
    case ObjType_PhysAddr:
    case ObjType_RAM:
    case ObjType_Frame:
    case ObjType_DevFrame:
        if (objbits > bits) {
            return 0;
        } else {
            return 1UL << (bits - objbits);
        }

    case ObjType_CNode:
        if (bits < OBJBITS_CTE || objbits > bits - OBJBITS_CTE) {
            return 0;
        } else {
            return 1UL << (bits - OBJBITS_CTE - objbits);
        }

    case ObjType_VNode_x86_64_pml4:
    case ObjType_VNode_x86_64_pdpt:
    case ObjType_VNode_x86_64_pdir:
    case ObjType_VNode_x86_64_ptable:
    case ObjType_VNode_x86_32_pdpt:
    case ObjType_VNode_x86_32_pdir:
    case ObjType_VNode_x86_32_ptable:
    case ObjType_VNode_ARM_l1:
    case ObjType_VNode_ARM_l2:
    {
        size_t objbits_vnode = vnode_objbits(type);
        if (bits < objbits_vnode) {
            return 0;
        } else {
            return 1UL << (bits - objbits_vnode);
        }
    }

    case ObjType_Dispatcher:
        if (bits < OBJBITS_DISPATCHER) {
            return 0;
        } else {
            return 1UL << (bits - OBJBITS_DISPATCHER);
        }

    case ObjType_Kernel:
    case ObjType_IRQTable:
    case ObjType_IO:
    case ObjType_EndPoint:
    case ObjType_ID:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
        return 1;

    default:
        panic("invalid type");
        return 0;
    }
}

/**
 * \brief Create capabilities to kernel objects.
 *
 * This function creates kernel objects of 'type' into the memory
 * area, based at 'addr' and of size 2^'bits', so they completely fill the
 * area. For each created kernel object, a capability is created to it and
 * put consecutively into the array of CTEs pointed to by 'caps'. The array
 * needs to have the appropriate size to hold all created caps. Some kernel
 * objects can have a variable size. In that case, 'objbits' should be non-zero
 * and give the a size multiplier as 2^'objbits'.
 *
 * \param type          Type of objects to create.
 * \param addr          Base address in the local address space.
 * \param bits          Size of memory area as 2^bits.
 * \param objbits       For variable-sized objects, size multiplier as 2^bits.
 * \param numobjs       Number of objects to be created, from caps_numobjs()
 * \param dest_caps     Pointer to array of CTEs to hold created caps.
 *
 * \return Error code
 */
// If you create more capability types you need to deal with them
// in the table below.
STATIC_ASSERT(ObjType_Num == 25, "Knowledge of all cap types");

static errval_t caps_create(enum objtype type, lpaddr_t lpaddr, uint8_t bits,
                            uint8_t objbits, size_t numobjs,
                            struct cte *dest_caps)
{
    errval_t err;

    /* Parameter checking */
    assert(dest_caps != NULL);
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);
    assert(numobjs > 0);

    genpaddr_t genpaddr = local_phys_to_gen_phys(lpaddr);

    // Virtual address of the memory the kernel object resides in
    // XXX: A better of doing this,
    // this is creating caps that the kernel cannot address.
    // It assumes that the cap is not of the type which will have to zeroed out.
    lvaddr_t lvaddr;
    if(lpaddr < PADDR_SPACE_LIMIT) {
        lvaddr = local_phys_to_mem(lpaddr);
    } else {
        lvaddr = 0;
    }

    /* Initialize the created capability */
    struct capability src_cap;
    memset(&src_cap, 0, sizeof(struct capability));
    src_cap.type = type;
    // XXX: Handle rights!
    src_cap.rights = CAPRIGHTS_ALLRIGHTS;

    /* Set the type specific fields and insert into #dest_caps */
    switch(type) {
    case ObjType_Frame:
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        // XXX: SCC hack, while we don't have a devframe allocator
        if(lpaddr + ((lpaddr_t)1 << bits) < PADDR_SPACE_LIMIT) {
            memset((void*)lvaddr, 0, (lvaddr_t)1 << bits);
        } else {
            printk(LOG_WARN, "Allocating RAM at 0x%" PRIxLPADDR
                   " uninitialized\n", lpaddr);
        }
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);
        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.frame.base = genpaddr + i * ((genpaddr_t)1 << objbits);
            src_cap.u.frame.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;

    case ObjType_PhysAddr:
        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.physaddr.base = genpaddr + i * ((genpaddr_t)1 << objbits);
            src_cap.u.physaddr.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    case ObjType_RAM:
        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.ram.base = genpaddr + i * ((genpaddr_t)1 << objbits);
            src_cap.u.ram.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    case ObjType_DevFrame:
        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.devframe.base = genpaddr + i * ((genpaddr_t)1 << objbits);
            src_cap.u.devframe.bits = objbits;
            // Insert the capabilities
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;

    case ObjType_CNode:
        assert((1UL << OBJBITS_CTE) >= sizeof(struct cte));
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.cnode.cnode =
                lpaddr + i * ((lpaddr_t)1 << (objbits + OBJBITS_CTE));
            src_cap.u.cnode.bits = objbits;
            src_cap.u.cnode.guard = 0;
            src_cap.u.cnode.guard_size = 0;
            // XXX: Handle rights!
            src_cap.u.cnode.rightsmask = CAPRIGHTS_ALLRIGHTS;
            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;

    case ObjType_VNode_ARM_l1:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_arm_l1.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

#ifdef __arm__
            // Insert kernel/mem mappings into new table.
            paging_make_good(
                gen_phys_to_local_phys(
                    local_phys_to_mem(src_cap.u.vnode_arm_l1.base)
                ),
                1u << objbits_vnode
                );
#endif

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }

        return SYS_ERR_OK;
    }

    case ObjType_VNode_ARM_l2:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_arm_l2.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_32_ptable:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_ptable.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_32_pdir:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_pdir.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

#if defined(__i386__) && !defined(CONFIG_PAE)
            // Make it a good PDE by inserting kernel/mem VSpaces
            lpaddr = gen_phys_to_local_phys(src_cap.u.vnode_x86_32_pdir.base);
            paging_x86_32_make_good_pdir(lpaddr);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_32_pdpt:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_32_pdir.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

#if defined(__i386__) && defined(CONFIG_PAE)
            // Make it a good PDPTE by inserting kernel/mem VSpaces
            lpaddr_t var =
                gen_phys_to_local_phys(src_cap.u.vnode_x86_32_pdpt.base);
            paging_x86_32_make_good_pdpte(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_64_ptable:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_ptable.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_64_pdir:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pdir.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_64_pdpt:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pdpt.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;
    }

    case ObjType_VNode_x86_64_pml4:
    {
        size_t objbits_vnode = vnode_objbits(type);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.vnode_x86_64_pml4.base =
                genpaddr + i * ((genpaddr_t)1 << objbits_vnode);

#ifdef __x86_64__
            // Make it a good PML4 by inserting kernel/mem VSpaces
            lpaddr_t var =
                gen_phys_to_local_phys(src_cap.u.vnode_x86_64_pml4.base);
            paging_x86_64_make_good_pml4(var);
#endif

            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }

        return SYS_ERR_OK;
    }

    case ObjType_Dispatcher:
        assert((1UL << OBJBITS_DISPATCHER) >= sizeof(struct dcb));
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 1);
        memset((void*)lvaddr, 0, 1UL << bits);
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_BZERO, 0);

        for(size_t i = 0; i < numobjs; i++) {
            // Initialize type specific fields
            src_cap.u.dispatcher.dcb = (struct dcb *)
                (lvaddr + i * (1UL << OBJBITS_DISPATCHER));
            // Insert the capability
            err = set_cap(&dest_caps[i].cap, &src_cap);
            if (err_is_fail(err)) {
                return err;
            }
        }
        return SYS_ERR_OK;

    case ObjType_ID:
        // ID type does not refer to a kernel object
        assert(lpaddr  == 0);
        assert(bits    == 0);
        assert(objbits == 0);
        assert(numobjs == 1);

        // Prevent wrap around
        if (id_cap_counter >= UINT32_MAX) {
            return SYS_ERR_ID_SPACE_EXHAUSTED;
        }

        // Generate a new ID, core_local_id monotonically increases
        src_cap.u.id.coreid = my_core_id;
        src_cap.u.id.core_local_id = id_cap_counter++;

        // Insert the capability
        return set_cap(&dest_caps->cap, &src_cap);

    case ObjType_IO:
        src_cap.u.io.start = 0;
        src_cap.u.io.end   = 65535;
        /* fall through */

    case ObjType_Kernel:
    case ObjType_IRQTable:
    case ObjType_EndPoint:
    case ObjType_Notify_RCK:
    case ObjType_Notify_IPI:
    case ObjType_PerfMon:
        // These types do not refer to a kernel object
        assert(lpaddr  == 0);
        assert(bits    == 0);
        assert(objbits == 0);
        assert(numobjs == 1);

        // Insert the capability
        return set_cap(&dest_caps->cap, &src_cap);

    default:
        panic("Unhandled capability type or capability of this type cannot"
              " be created");
    }
}

/**
 * Look up a capability.
 *
 * Starting from #cnode_cap, recursively lookup the capability at #cptr
 * with #vbits.
 *
 * \bug Handle rights
 */
errval_t caps_lookup_slot(struct capability *cnode_cap, capaddr_t cptr,
                          uint8_t vbits, struct cte **ret, CapRights rights)
{
    /* parameter checking */
    assert(cnode_cap != NULL);

    /* Can only resolve CNode type */
    if (cnode_cap->type != ObjType_CNode) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Cap to lookup not of type CNode\n"
              "cnode_cap->type = %u\n", cnode_cap->type);
        return SYS_ERR_CNODE_TYPE;
    }

    /* Apply rights to this CNode */
    if ((cnode_cap->rights & rights) != rights) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Rights mismatch\n"
              "Passed rights = %u, cnode_cap->rights = %u\n",
              rights, cnode_cap->rights);
        return SYS_ERR_CNODE_RIGHTS;
    }

    /* Number of bits resolved by this cnode (guard and bits) */
    uint8_t bits_resolved = cnode_cap->u.cnode.bits +
        cnode_cap->u.cnode.guard_size;
    // All CNodes must resolve at least one bit
    assert(bits_resolved > 0);
    // If lookup exceeded expected depth then table is malformed
    if (bits_resolved > vbits) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: Lookup exceeded valid bits\n"
              "Cnode bits = %u, guard size = %u, valid bits = %u\n",
              cnode_cap->u.cnode.bits, cnode_cap->u.cnode.guard_size,
              vbits);
        return SYS_ERR_DEPTH_EXCEEDED;
    }

    /* Guard-check (bit-mask of guard in cptr must match guard in cnode cap) */
    capaddr_t cptr_guard = (cptr >> (vbits - cnode_cap->u.cnode.guard_size))
        & MASK(cnode_cap->u.cnode.guard_size);
    if (cptr_guard != cnode_cap->u.cnode.guard) {
        debug(SUBSYS_CAPS, "caps_lookup_slot: guard check failed\n"
              "Computed guard = %"PRIuCADDR", "
              "Cnode guard = %"PRIxCADDR", bits = %u\n",
              cptr_guard, cnode_cap->u.cnode.guard,
              cnode_cap->u.cnode.guard_size);
        return SYS_ERR_GUARD_MISMATCH;
    }

    /* Locate capability in this cnode */
    // Offset into the cnode
    size_t offset = (cptr >> (vbits - bits_resolved)) &
        MASK(cnode_cap->u.cnode.bits);
    // The capability at the offset
    struct cte *next_slot = caps_locate_slot(cnode_cap->u.cnode.cnode, offset);
    // Do not return NULL type capability
    if (next_slot->cap.type == ObjType_Null) {
        return SYS_ERR_CAP_NOT_FOUND;
    }

    /* Number of bits left to resolve */
    int bitsleft = vbits - bits_resolved;
    // If all bits have been resolved, return the capability
    if(bitsleft == 0) {
        *ret = next_slot;
        return SYS_ERR_OK;
    }

    /* If next capability is not of type cnode, return it */
    // XXX: Is this consistent?
    if (next_slot->cap.type != ObjType_CNode) {
        *ret = next_slot;
        return SYS_ERR_OK;
    }

    /* Descend to next level */
    return caps_lookup_slot(&next_slot->cap, cptr, bitsleft, ret, rights);
}

/**
 * Wrapper for caps_lookup_slot returning capability instead of cte.
 */
errval_t caps_lookup_cap(struct capability *cnode_cap, capaddr_t cptr,
                         uint8_t vbits, struct capability **ret, CapRights rights)
{
    struct cte *ret_cte;
    errval_t err = caps_lookup_slot(cnode_cap, cptr, vbits, &ret_cte, rights);
    if (err_is_fail(err)) {
        return err;
    }
    *ret = &ret_cte->cap;
    return SYS_ERR_OK;
}

/**
 * \brief Create a capability from an existing capability metadata.
 *
 * Used when sending capabilities across cores. The metadata is sent across
 * cores and the receiving monitor can create the new capability on its core.
 */
errval_t caps_create_from_existing(struct capability *root, capaddr_t cnode_cptr,
                                   int cnode_vbits, cslot_t dest_slot,
                                   struct capability *src)
{
    errval_t err;
    struct capability *cnode;
    err = caps_lookup_cap(root, cnode_cptr, cnode_vbits, &cnode,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_SLOT_LOOKUP_FAIL);
    }
    if (cnode->type != ObjType_CNode) {
        return SYS_ERR_CNODE_TYPE;
    }

    struct cte *dest = caps_locate_slot(cnode->u.cnode.cnode, dest_slot);

    err = set_cap(&dest->cap, src);
    if (err_is_fail(err)) {
        return err;
    }

    set_init_mapping(dest, 1);
    return SYS_ERR_OK;
}

/// Create caps to new kernel objects.
errval_t caps_create_new(enum objtype type, lpaddr_t addr, size_t bits,
                         size_t objbits, struct cte *caps)
{
    /* Parameter checking */
    assert(type != ObjType_EndPoint); // Cap of this type cannot be created

    size_t numobjs = caps_numobjs(type, bits, objbits);
    assert(numobjs > 0);

    /* Create the new capabilities */
    errval_t err = caps_create(type, addr, bits, objbits, numobjs, caps);
    if (err_is_fail(err)) {
        return err;
    }

    // Handle the mapping database
    set_init_mapping(caps, numobjs);
    return SYS_ERR_OK;
}


/// Retype caps
errval_t caps_retype(enum objtype type, size_t objbits,
                     struct capability *dest_cnode, cslot_t dest_slot,
                     struct cte *src_cte, bool from_monitor)
{
    size_t numobjs;
    uint8_t bits = 0;
    genpaddr_t base = 0;
    errval_t err;

    /* Parameter checking */
    assert(type != ObjType_Null);
    assert(type < ObjType_Num);

    struct capability *src_cap = &src_cte->cap;

    /* Check retypability */
    err = is_retypeable(src_cte, src_cap->type, type, from_monitor);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: is_retypeable failed\n");
        return err;
    }

    /* Create Destination caps as per source and destination type */
    switch(src_cap->type) {
    case ObjType_PhysAddr:
        bits = src_cap->u.physaddr.bits;
        base = src_cap->u.physaddr.base;
        break;

    case ObjType_RAM:
        bits = src_cap->u.ram.bits;
        base = src_cap->u.ram.base;
        break;

    case ObjType_Dispatcher:
        bits = base = 0;
        break;

    case ObjType_Frame:
        bits = src_cap->u.frame.bits;
        base = src_cap->u.frame.base;
        break;

    case ObjType_DevFrame:
        bits = src_cap->u.devframe.bits;
        base = src_cap->u.devframe.base;
        break;

    default:
        panic("Unreachable case");
    }

    /* determine number of objects to be created */
    numobjs = caps_numobjs(type, bits, objbits);

    if (numobjs == 0) {
        debug(SUBSYS_CAPS, "caps_retype: numobjs == 0\n");
        return SYS_ERR_INVALID_SIZE_BITS;
    }
   // debug(SUBSYS_CAPS, "caps_retype: numobjs == %d\n", (int)numobjs);

    /* check that destination slots all fit within target cnode */
    if (dest_slot + numobjs > (1UL << dest_cnode->u.cnode.bits)) {
        debug(SUBSYS_CAPS, "caps_retype: dest slots don't fit in cnode\n");
        return SYS_ERR_SLOTS_INVALID;
    }

    /* check that destination slots are all empty */
    debug(SUBSYS_CAPS, "caps_retype: dest cnode is %#" PRIxLPADDR
          " dest_slot %d\n",
          dest_cnode->u.cnode.cnode, (int)dest_slot);
    for (cslot_t i = 0; i < numobjs; i++) {
        if (caps_locate_slot(dest_cnode->u.cnode.cnode, dest_slot + i)->cap.type
            != ObjType_Null) {
            debug(SUBSYS_CAPS, "caps_retype: dest slot %d in use\n",
                  (int)(dest_slot + i));
            return SYS_ERR_SLOTS_IN_USE;
        }
    }

    /* create new caps */
    struct cte *dest_cte =
        caps_locate_slot(dest_cnode->u.cnode.cnode, dest_slot);
    err = caps_create(type, base, bits, objbits, numobjs, dest_cte);
    if (err_is_fail(err)) {
        debug(SUBSYS_CAPS, "caps_retype: failed to create a dest cap\n");
        return err_push(err, SYS_ERR_RETYPE_CREATE);
    }

    /* special initialisation for endpoint caps */
    if (type == ObjType_EndPoint) {
        assert(src_cap->type == ObjType_Dispatcher);
        assert(numobjs == 1);
        struct capability *dest_cap = &dest_cte->cap;
        dest_cap->u.endpoint.listener = src_cap->u.dispatcher.dcb;
    }

    /* Handle mapping */
    for (size_t i = 0; i < numobjs; i++) {
        mdb_insert(&dest_cte[i]);
    }

    return SYS_ERR_OK;
}

/// Check the validity of a retype operation
errval_t is_retypeable(struct cte *src_cte, enum objtype src_type,
                       enum objtype dest_type, bool from_monitor)
{
    if (!is_well_founded(src_type, dest_type)) {
        return SYS_ERR_INVALID_RETYPE;
    } else if (!is_revoked_first(src_cte, src_type)){
        printf("err_revoke_first: (%p, %d, %d)\n", src_cte, src_type, dest_type);
        return SYS_ERR_REVOKE_FIRST;
#ifndef RCAPDB_NULL
    } else if (!from_monitor && is_cap_remote(src_cte)) {
        return SYS_ERR_RETRY_THROUGH_MONITOR;
#endif
    } else {
        return SYS_ERR_OK;
    }
}

/// Create copies to a slot within a cnode
errval_t caps_copy_to_cnode(struct cte *dest_cnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, bool mint, uintptr_t param1,
                            uintptr_t param2)
{
    /* Parameter Checking */
    assert(dest_cnode_cte->cap.type == ObjType_CNode);

    struct cte *dest_cte;
    dest_cte = caps_locate_slot(dest_cnode_cte->cap.u.cnode.cnode, dest_slot);
    return caps_copy_to_cte(dest_cte, src_cte, mint, param1, param2);

}

/// Create copies to a cte
errval_t caps_copy_to_cte(struct cte *dest_cte, struct cte *src_cte, bool mint,
                          uintptr_t param1, uintptr_t param2)
{
    errval_t err;
    /* Parameter checking */
    // Null checking
    assert(dest_cte != NULL);
    assert(src_cte != NULL);

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_cte->cap;
    // NULL caps cannot be copied/minted
    if (src_cap->type == ObjType_Null) {
        return SYS_ERR_CAP_NOT_FOUND;
    }
    // Parameters should be 0 if not minting
    if (!mint) {
        assert(param1 == 0);
        assert(param2 == 0);
    }

    /* Insert #source_cap into #dest_cap */
    err = set_cap(dest_cap, src_cap);
    if (err_is_fail(err)) {
        return err;
    }


    /* Copy is done */
    if(!mint) {
        // Handle mapping here only for non-mint operations
        // (mint can change eq fields which would make the early insertion
        // invalid in some cases)
        mdb_insert(dest_cte);
        return SYS_ERR_OK;
    }

    /* For minting, set the specified parameters */
    // Process source-specific parameters for minting
    // XXX: If failure, revert the insertion
    switch(src_cap->type) {
    case ObjType_CNode:
        if (param2 > CPTR_BITS) {
            return SYS_ERR_GUARD_SIZE_OVERFLOW;
        }
        dest_cap->u.cnode.guard      = param1;
        dest_cap->u.cnode.guard_size = param2;
        break;

    case ObjType_EndPoint:
        // XXX: FIXME: check that buffer offset lies wholly within the disp frame
        // can't easily enforce this here, because the dispatcher frame may not
        // yet be setup
/*        if (param1 < sizeof(struct dispatcher) ||
            dest_cap->u.endpoint.listener->disp == NULL ||
            param2 < IDC_RECV_LENGTH ||
            param1 + sizeof(struct idc_endpoint) + param2 * sizeof(uintptr_t) >
            (1UL << dest_cap->u.endpoint.listener->disp_cte.cap.u.frame.bits)) {
            return SYS_ERR_INVALID_EPBUF;
        }*/
        if (param2 < LMP_RECV_HEADER_LENGTH) {
            return SYS_ERR_INVALID_EPLEN;
        }
        dest_cap->u.endpoint.epoffset = param1;
        dest_cap->u.endpoint.epbuflen = param2;
        break;

    case ObjType_IO:
        if(src_cap->u.io.start  <= param1) {
            dest_cap->u.io.start = param1;
        }
        if(src_cap->u.io.end  >= param2) {
            dest_cap->u.io.end = param2;
        }
        break;

    default:
        // Unhandled source type for mint
        return SYS_ERR_INVALID_SOURCE_TYPE;
    }

    // Handle mapping after doing minting operation
    mdb_insert(dest_cte);

    return SYS_ERR_OK;
}

/// Handle deletion of a cnode or dcb cap
static void delete_cnode_or_dcb(struct capability *cap, bool from_monitor)
{
    assert(cap != NULL);
    assert((cap->type == ObjType_CNode) || (cap->type == ObjType_Dispatcher));

    if(cap->type == ObjType_CNode) {
        // Number of slots in the cnode
        cslot_t max_slots = 1UL << cap->u.cnode.bits;

        // Delete each non Null slot
        for (cslot_t slot_no = 0; slot_no < max_slots; slot_no++) {
            struct cte *cte_in_cnode =
                caps_locate_slot(cap->u.cnode.cnode, slot_no);
            if (cte_in_cnode->cap.type != ObjType_Null) {
                caps_delete(cte_in_cnode, from_monitor);
            }
        }
    } else {
        struct dcb *dcb = cap->u.dispatcher.dcb;
        // Delete each slot in dispatcher
        if (dcb->cspace.cap.type != ObjType_Null) {
            caps_delete(&dcb->cspace, from_monitor);
        }
        if (dcb->disp_cte.cap.type != ObjType_Null) {
            caps_delete(&dcb->disp_cte, from_monitor);
        }

        // Remove from queue
        scheduler_remove(dcb);
        // Reset curent if it was deleted
        if (dcb_current == dcb) {
            dcb_current = NULL;
        }

        // Remove from wakeup queue
        wakeup_remove(dcb);

        // Notify monitor
        if (monitor_ep.u.endpoint.listener == dcb) {
            printk(LOG_ERR, "monitor terminated; expect badness!\n");
            monitor_ep.u.endpoint.listener = NULL;
        } else if (monitor_ep.u.endpoint.listener != NULL) {
            errval_t err;
            uintptr_t payload = dcb->domain_id;
            err = lmp_deliver_payload(&monitor_ep, NULL, &payload, 1, false);
            assert(err_is_ok(err));
        }
    }
}

/**
 * \brief Delete a capability from a table entry.
 *
 * Deletes the capability from the table entry pointed to by
 * 'cte'.
 *
 * \param cte   Pointer to table entry to delete cap from.
 *
 * \bug If deleting the last copy of a cnode or dispatcher, recursively delete
 */
errval_t caps_delete(struct cte *cte, bool from_monitor)
{
    assert(cte != NULL);

#ifndef RCAPDB_NULL
    if (!from_monitor && is_cap_remote(cte) && !has_copies(cte)) {
        // delete on the last copy of a remote cap, do this through the monitor
        // so we can inform other cores
        return SYS_ERR_RETRY_THROUGH_MONITOR;
    }
#endif

    struct capability *cap = &cte->cap;
    // special handling for last copy of cnode and dispatcher types
    if ((cap->type == ObjType_CNode) || (cap->type == ObjType_Dispatcher)) {
        if (!has_copies(cte)) {
            delete_cnode_or_dcb(cap, from_monitor);
        }
    }

    // If this was the last reference to an object, we might have to
    // resurrect the RAM and send it back to the monitor
    if(!has_copies(cte) && !has_descendants(cte) && !has_ancestors(cte)
       && !is_cap_remote(cte) && monitor_ep.u.endpoint.listener != NULL) {
        struct RAM ram = { .bits = 0 };
        size_t len = sizeof(struct RAM) / sizeof(uintptr_t) + 1;

        // List all RAM-backed capabilities here
        // NB: ObjType_PhysAddr and ObjType_DevFrame caps are *not* RAM-backed!
        switch(cap->type) {
        case ObjType_RAM:
            ram.base = cap->u.ram.base;
            ram.bits = cap->u.ram.bits;
            break;

        case ObjType_Frame:
            ram.base = cap->u.frame.base;
            ram.bits = cap->u.frame.bits;
            break;

        case ObjType_CNode:
            ram.base = cap->u.cnode.cnode;
            ram.bits = cap->u.cnode.bits + OBJBITS_CTE;
            break;

        case ObjType_Dispatcher:
            // Convert to genpaddr
            ram.base = local_phys_to_gen_phys(mem_to_local_phys((lvaddr_t)cap->u.dispatcher.dcb));
            ram.bits = OBJBITS_DISPATCHER;
            break;

        default:
            // Handle VNodes here
            if(type_is_vnode(cap->type)) {
                ram.base = get_address(cap);
                ram.bits = vnode_objbits(cap->type);
            }
            break;
        }

        if(ram.bits > 0) {
            // Send back as RAM cap to monitor
            // XXX: This looks pretty ugly. We need an interface.
            // FIXME: why exactly the return value of this call is ignored?
            //errval_t err =
                lmp_deliver_payload(&monitor_ep, NULL,
                                      (uintptr_t *)&ram,
                                      len, false);
            //assert(err_is_ok(err));
        }
    }

    // unmap if mapped
    if (type_is_vnode(cap->type) || cap->type == ObjType_Frame || cap->type == ObjType_DevFrame) {
        unmap_capability(cte);
    }

    // Remove from mapping database
    remove_mapping(cte);
    // Initialize the cap
    memset(cte, 0, sizeof(struct cte));

    return SYS_ERR_OK;
}

/**
 * \brief Revoke a cap
 *
 * Find all copies and descendants and delete them.
 * When seeing a capability with no relations, stop traversing.
 */
errval_t caps_revoke(struct cte *cte, bool from_monitor)
{
    assert(cte != NULL);

#ifndef RCAPDB_NULL
    if (!from_monitor && is_cap_remote(cte)) {
        return SYS_ERR_RETRY_THROUGH_MONITOR;
    }
#endif

    struct cte *walk;
    errval_t err = SYS_ERR_OK;
    // Traverse forward
    walk = mdb_successor(cte);
    while(walk && walk != cte && err_is_ok(err)) {
        struct cte *next = mdb_successor(walk);
        if (is_ancestor(&walk->cap, &cte->cap)) {
            err = caps_delete(walk, from_monitor);
        } else if(is_copy(&walk->cap, &cte->cap)) {
            err = caps_delete(walk, from_monitor);
        } else {
            break;
        }
        walk = next;
    }

    // Traverse backwards
    walk = mdb_predecessor(cte);
    while(walk && walk != cte && err_is_ok(err)) {
        struct cte *prev = mdb_predecessor(walk);
        if (is_ancestor(&walk->cap, &cte->cap)) {
            err = caps_delete(walk, from_monitor);
        } else if(is_copy(&walk->cap, &cte->cap)) {
            err = caps_delete(walk, from_monitor);
        } else {
            break;
        }
        walk = prev;
    }

    return err;
}
