/**
 * \file
 * \brief Capability system user code
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/cspace.h>
#include <barrelfish/caddr.h>
#include <barrelfish/lmp_endpoints.h>
#include <if/monitor_defs.h>
#include <if/monitor_blocking_rpcclient_defs.h>
#include <barrelfish/monitor_client.h>
#include <trace/trace.h>

/// Root CNode
struct cnoderef cnode_root = {
    .address = CPTR_ROOTCN,
    .address_bits = CPTR_BITS,
    .size_bits = DEFAULT_CNODE_BITS,
    .guard_size = 0
};

#define TASK_CNODE_INIT { \
    .address = 0, \
    .address_bits = DEFAULT_CNODE_BITS, \
    .size_bits = DEFAULT_CNODE_BITS, \
    .guard_size = GUARD_REMAINDER(2 * DEFAULT_CNODE_BITS) }

#define PAGE_CNODE_INIT { \
    .address = ROOTCN_SLOT_PAGECN << DEFAULT_CN_ADDR_BITS, \
    .address_bits = DEFAULT_CNODE_BITS, \
    .size_bits = PAGE_CNODE_BITS, \
    .guard_size = 0 }

/// Task CNode
struct cnoderef cnode_task = TASK_CNODE_INIT;

/// Base CNode
struct cnoderef cnode_base = {
    .address = CPTR_BASE_PAGE_CN_BASE,
    .address_bits = DEFAULT_CNODE_BITS,
    .size_bits = DEFAULT_CNODE_BITS,
    .guard_size = 0
};

/// Super CNode
struct cnoderef cnode_super = {
    .address = CPTR_SUPERCN_BASE,
    .address_bits = DEFAULT_CNODE_BITS,
    .size_bits = DEFAULT_CNODE_BITS,
    .guard_size = 0
};

/// Page CNode
struct cnoderef cnode_page = PAGE_CNODE_INIT;

/// Module CNode
struct cnoderef cnode_module = {
    .address = CPTR_MODULECN_BASE,
    .address_bits = DEFAULT_CNODE_BITS,
    .size_bits = MODULECN_SIZE_BITS,
    .guard_size = 0
};

/// Capability to Root CNode
struct capref cap_root = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_ROOTCN
};

/// Capability for IRQ table
struct capref cap_irq = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_IRQ
};

/// Capability for legacy IO
struct capref cap_io = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_IO
};

/// Capability for endpoint to self
struct capref cap_selfep = {
    .cnode = TASK_CNODE_INIT,
    .slot = TASKCN_SLOT_SELFEP
};

/// Capability for dispatcher
struct capref cap_dispatcher = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_DISPATCHER
};

/// Capability for dispatcher
struct capref cap_dispframe = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_DISPFRAME
};

#define ROOT_CNODE_INIT { \
    .address = CPTR_ROOTCN, \
    .address_bits = CPTR_BITS, \
    .size_bits = DEFAULT_CNODE_BITS, \
    .guard_size = 0 }

/// Capability for monitor endpoint
struct capref cap_monitorep = {
    .cnode = ROOT_CNODE_INIT,
    .slot  = ROOTCN_SLOT_MONITOREP
};

/// Capability for kernel (only in monitor)
struct capref cap_kernel = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_KERNELCAP
};

/// PerfMon CNode
struct capref cap_perfmon = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_PERF_MON
};

/// Capability for endpoint to init (only in monitor/mem_serv)
struct capref cap_initep = {
    .cnode = TASK_CNODE_INIT,
    .slot  = TASKCN_SLOT_INITEP
};

/// Session ID
struct capref cap_sessionid = {
    .cnode = TASK_CNODE_INIT,
    .slot = TASKCN_SLOT_SESSIONID
};

/// Root PML4 VNode
struct capref cap_vroot = {
    .cnode = PAGE_CNODE_INIT,
    .slot = CPTR_PML4_BASE
};

static inline bool backoff(int count)
{
    // very crude exponential backoff based upon core id
    int yieldcnt = 2^count * disp_get_core_id();
    for (int i=0; i<yieldcnt; i++) {
        thread_yield();
    }
    return true;
}

/**
 * \brief Retype a capability into one or more new capabilities, going through
 * the monitor to ensure consistancy with other cores.  Only necessary for
 * caps that have been sent remotely.
 */
static errval_t cap_retype_remote(capaddr_t src, enum objtype new_type,
                                  uint8_t size_bits, capaddr_t to, capaddr_t slot,
                                  int dcn_vbits)
{
    struct monitor_blocking_rpc_client *mrc = get_monitor_blocking_rpc_client();
    errval_t err, remote_cap_err;
    int count = 0;
    do {
        err = mrc->vtbl.remote_cap_retype(mrc, cap_root, src,
                                          (uint64_t)new_type,
                                          size_bits, to, slot,
                                          dcn_vbits, &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap retype\n");
        }
    } while (remote_cap_err == MON_ERR_REMOTE_CAP_RETRY && backoff(++count));

    return remote_cap_err;

}


/**
 * \brief Delete the given capability, going through  the monitor to ensure
 * consistancy with other cores.  Only necessary for caps that have been sent
 * remotely.
 *
 * \param cap Capability to be deleted
 *
 * Deletes (but does not revoke) the given capability, allowing the CNode slot
 * to be reused.
 */
static errval_t cap_delete_remote(capaddr_t src, uint8_t vbits)
{
    struct monitor_blocking_rpc_client *mrc = get_monitor_blocking_rpc_client();
    errval_t err, remote_cap_err;
    int count = 0;
    do {
        err = mrc->vtbl.remote_cap_delete(mrc, cap_root, src, vbits,
                                          &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap delete\n");
        }
    } while (remote_cap_err == MON_ERR_REMOTE_CAP_RETRY && backoff(++count));

    return remote_cap_err;
}

/**
 * \brief Revoke (delete all copies and descendants of) the given capability,
 * going through the monitor to ensure consistancy with other cores.  Only
 * necessary for caps that have been sent remotely.
 *
 * \param cap Capability to be revoked
 *
 * Deletes all copies and descendants of the given capability, but not the
 * capability itself. If this succeeds, the capability is guaranteed to be
 * the only copy in the system.
 */
static errval_t cap_revoke_remote(capaddr_t src, uint8_t vbits)
{
    struct monitor_blocking_rpc_client *mrc = get_monitor_blocking_rpc_client();
    errval_t err, remote_cap_err;
    int count = 0;
    do {
        err = mrc->vtbl.remote_cap_revoke(mrc, cap_root, src, vbits,
                                          &remote_cap_err);
        if (err_is_fail(err)){
            DEBUG_ERR(err, "remote cap delete\n");
        }
    } while (remote_cap_err == MON_ERR_REMOTE_CAP_RETRY && backoff(++count));

    return remote_cap_err;
}

/**
 * \brief Retype a capability into one or more new capabilities
 *
 * \param dest_start    Location of first desination slot, which must be empty
 * \param src           Source capability to retype
 * \param new_type      Kernel object type to retype to.
 * \param size_bits     Size of created objects as a power of two
 *                      (ignored for fixed-size objects)
 *
 * Retypes the given source capability into a number of new capabilities, which
 * may be of the same or of different type. The new capabilities are created
 * in the slots starting from dest_start, which must all be empty and lie in the
 * same CNode. The number of objects created is determined by the size of the
 * source object divided by the size of the destination objects.
 */
errval_t cap_retype(struct capref dest_start, struct capref src,
                    enum objtype new_type, uint8_t size_bits)
{
    errval_t err;
    // Number of valid bits in destination CNode address
    uint8_t dcn_vbits = get_cnode_valid_bits(dest_start);
    // Address of the cap to the destination CNode
    capaddr_t dcn_addr = get_cnode_addr(dest_start);
    // Address of source capability
    capaddr_t scp_addr = get_cap_addr(src);

    err = invoke_cnode_retype(cap_root, scp_addr, new_type, size_bits,
                              dcn_addr, dest_start.slot, dcn_vbits);

    if (err == SYS_ERR_RETRY_THROUGH_MONITOR) {
        return cap_retype_remote(scp_addr, new_type, size_bits,
                                 dcn_addr, dest_start.slot, dcn_vbits);
    } else {
        return err;
    }
}


/**
 * \brief Create a capability
 *
 * \param dest      Location where to create the cap, which must be empty.
 * \param type      Kernel object type to create.
 * \param size_bits Size of the created capability as a power of two.
 *                  (ignored for fixed-size objects)
 *
 * Only certain types of capabilities can be created this way. If invoked on
 * a capability type, that is not creatable at runtime the error
 * SYS_ERR_TYPE_NOT_CREATABLE is returned. Most capabilities have to be retyped
 * from other capabilities with cap_retype().
 */
errval_t cap_create(struct capref dest, enum objtype type, uint8_t size_bits)
{
    errval_t err;

    // Number of valid bits in the destination CNode address
    uint8_t dest_vbits = get_cnode_valid_bits(dest);
    // Address of the cap to the destination CNode
    capaddr_t dest_cnode_cptr = get_cnode_addr(dest);

    err = invoke_cnode_create(cap_root, type, size_bits, dest_cnode_cptr,
                              dest.slot, dest_vbits);

    return err;
}

/**
 * \brief Delete the given capability
 *
 * \param cap Capability to be deleted
 *
 * Deletes (but does not revoke) the given capability, allowing the CNode slot
 * to be reused.
 */
errval_t cap_delete(struct capref cap)
{
    errval_t err;
    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);

    err = invoke_cnode_delete(cap_root, caddr, vbits);

    if (err == SYS_ERR_RETRY_THROUGH_MONITOR) {
        return cap_delete_remote(caddr, vbits);
    } else {
        return err;
    }
}

/**
 * \brief Revoke (delete all copies and descendants of) the given capability
 *
 * \param cap Capability to be revoked
 *
 * Deletes all copies and descendants of the given capability, but not the
 * capability itself. If this succeeds, the capability is guaranteed to be
 * the only copy in the system.
 */
errval_t cap_revoke(struct capref cap)
{
    errval_t err;
    uint8_t vbits = get_cap_valid_bits(cap);
    capaddr_t caddr = get_cap_addr(cap) >> (CPTR_BITS - vbits);

    err = invoke_cnode_revoke(cap_root, caddr, vbits);

    if (err == SYS_ERR_RETRY_THROUGH_MONITOR) {
        return cap_revoke_remote(caddr, vbits);
    } else {
        return err;
    }
}

/**
 * \brief Destroy a capability, i.e. delete it and free the slot.
 *
 * \param cap           Capability to be destroyed
 */
errval_t cap_destroy(struct capref cap)
{
    errval_t err;
    err = cap_delete(cap);
    if (err_is_fail(err)) {
        return err;
    }

    err = slot_free(cap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_FREEING_SLOT);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a CNode from a given RAM capability in a specific slot
 *
 * \param dest location in which to place newly-created CNode cap
 * \param src  location of RAM capability to be retyped to new CNode
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param slot_bits number of slots in created CNode as a power of two.
 *                  must match size of RAM capability.
 *
 * This function requires that dest refer to an existing but empty slot. It
 * retypes the given memory to a new CNode.
 */
errval_t cnode_create_from_mem(struct capref dest, struct capref src,
                               struct cnoderef *cnoderef, uint8_t slot_bits)
{
    errval_t err;

    // Retype it to the destination
    err = cap_retype(dest, src, ObjType_CNode, slot_bits);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    // Construct the cnoderef to return
    if (cnoderef != NULL) {
        *cnoderef = build_cnoderef(dest, slot_bits);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a CNode from newly-allocated RAM in a newly-allocated slot
 *
 * \param ret_dest capref struct to be filled-in with location of CNode
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 */
errval_t cnode_create(struct capref *ret_dest, struct cnoderef *cnoderef,
                      cslot_t slots, cslot_t *retslots)
{
    errval_t err;

    // Allocate a slot for destination.
    assert(ret_dest != NULL);
    err = slot_alloc(ret_dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    // Use cnode_create_raw
    return cnode_create_raw(*ret_dest, cnoderef, slots, retslots);
}

/**
 * \brief Create a CNode from newly-allocated RAM in the given slot
 *
 * \param dest location in which to place CNode cap
 * \param cnoderef cnoderef struct, filled-in if non-NULL with relevant info
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 *
 * This function requires that dest refer to an existing but empty slot. It
 * allocates memory (using #ram_alloc), and retypes that memory to a new CNode.
 * The intermediate ram cap is destroyed.
 */
errval_t cnode_create_raw(struct capref dest, struct cnoderef *cnoderef,
                          cslot_t slots, cslot_t *retslots)
{
    errval_t err;
    struct capref ram;

    assert(slots > 0);
    uint8_t bits = log2ceil(slots);
    assert((1UL << bits) >= slots);
    if (bits < DEFAULT_CNODE_BITS) {
        bits = DEFAULT_CNODE_BITS;
    }

    if (retslots != NULL) {
        *retslots = 1UL << bits;
    }

    // Allocate some memory
    err = ram_alloc(&ram, bits + OBJBITS_CTE);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cnode_create_from_mem(dest, ram, cnoderef, bits);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE_FROM_MEM);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create CNode with a given guard
 *
 * \param dest         Location where to place the cnode
 * \param cnoderef   Filled in cnoderef struct if non-NULL
 * \param slots Minimum number of slots in created CNode
 * \param retslots If non-NULL, filled in with the  number of slots in created CNode
 * \param guard        The guard value to set
 * \param guard_size   The length of the guard in bits
 *
 * This function requires that dest refer to an existing but empty slot. It
 * allocates memory (using #ram_alloc), and retypes that memory to a new CNode
 * with the given guard value and size. An intermediate slot is used in order to
 * set the guard value.
 */
errval_t cnode_create_with_guard(struct capref dest, struct cnoderef *cnoderef,
                                 cslot_t slots, cslot_t *retslots,
                                 uint64_t guard, uint8_t guard_size)
{
    errval_t err;

    /* Create an intermediate cnode cap */
    struct capref inter;
    err = cnode_create(&inter, NULL, slots, retslots);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CNODE_CREATE);
    }

    /* Mint it to the new destination setting the guard */
    err = cap_mint(dest, inter, guard, guard_size);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_MINT);
    }

    /* Free the intermediate cnode cap and slot */
    err = cap_delete(inter);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_DELETING);
    }
    err = slot_free(inter);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_WHILE_FREEING_SLOT);
    }

    /* Build the cnoderef */
    if (cnoderef != NULL) {
        assert(slots > 0);
        uint8_t bits = log2ceil(slots);
        assert((1UL << bits) >= slots);
        if (bits < DEFAULT_CNODE_BITS) {
            bits = DEFAULT_CNODE_BITS;
        }
        *cnoderef = build_cnoderef(dest, bits);
        cnoderef->guard_size = guard_size;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a VNode in newly-allocated memory
 *
 * \param dest location to place new VNode cap
 * \param type VNode type to create
 *
 * This function requires that dest refer to an existing but empty slot.
 * The intermidiate ram cap is destroyed.
 */
errval_t vnode_create(struct capref dest, enum objtype type)
{
    errval_t err;

    struct capref ram;

    size_t objbits_vnode = vnode_objbits(type);
    err = ram_alloc(&ram, objbits_vnode);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    assert(type_is_vnode(type));
    err = cap_retype(dest, ram, type, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a Frame cap referring to newly-allocated RAM in a given slot
 *
 * \param dest  Location to place new frame cap
 * \param bytes Minimum size of frame to create
 * \param retbytes If non-NULL, filled in with size of created frame
 *
 * This function requires that dest refer to an existing but empty slot.
 * #ram_alloc is used to allocate memory. After retyping the intermediate
 * ram cap is destroyed.
 *
 * This function will returns a special error code if ram_alloc fails
 * due to the constrains on the memory server (size of cap or region
 * of memory). This is to facilitate retrying with different
 * constraints.
 */
errval_t frame_create(struct capref dest, size_t bytes, size_t *retbytes)
{
    assert(bytes > 0);
    uint8_t bits = log2ceil(bytes);
    assert((1UL << bits) >= bytes);
    errval_t err;

    if (bits < BASE_PAGE_BITS) {
        bits = BASE_PAGE_BITS;
    }

    struct capref ram;
    err = ram_alloc(&ram, bits);
    if (err_is_fail(err)) {
        if (err_no(err) == MM_ERR_NOT_FOUND ||
            err_no(err) == LIB_ERR_RAM_ALLOC_WRONG_SIZE) {
            return err_push(err, LIB_ERR_RAM_ALLOC_MS_CONSTRAINTS);
        }
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cap_retype(dest, ram, ObjType_Frame, bits);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err;
    }

    if (retbytes != NULL) {
        *retbytes = 1UL << bits;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Create a Dispatcher in newly-allocated memory
 *
 * \param dest location to place new dispatcher cap
 *
 * This function requires that dest refer to an existing but empty slot. It does
 * not map in nor initialise the Dispatcher.
 * The intermediate ram cap is destroyed.
 */
errval_t dispatcher_create(struct capref dest)
{
    errval_t err;

    struct capref ram;
    err = ram_alloc(&ram, OBJBITS_DISPATCHER);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC);
    }

    err = cap_retype(dest, ram, ObjType_Dispatcher, 0);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_RETYPE);
    }

    err = cap_destroy(ram);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_CAP_DESTROY);
    }
    return SYS_ERR_OK;
}

/**
 * \brief Create endpoint to caller on current dispatcher.
 *
 * \param buflen  Length of incoming LMP buffer, in words
 * \param retcap  Pointer to capref struct, filled-in with location of cap
 * \param retep   Double pointer to LMP endpoint, filled-in with allocated EP
 */
errval_t endpoint_create(size_t buflen, struct capref *retcap,
                         struct lmp_endpoint **retep)
{
    errval_t err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return lmp_endpoint_create_in_slot(buflen, *retcap, retep);
}

/**
 * \brief Create a Frame cap referring to newly-allocated RAM in an allocated slot
 *
 * \param dest  Pointer to capref struct, filled-in with location of new cap
 * \param bytes Minimum size of frame to create
 * \param retbytes If non-NULL, filled in with size of created frame
 */
errval_t frame_alloc(struct capref *dest, size_t bytes, size_t *retbytes)
{
    errval_t err = slot_alloc(dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return frame_create(*dest, bytes, retbytes);
}

/**
 * \brief Create a DevFrame cap by retyping out of given source PhysAddr cap
 *
 * \param dest          Pointer to capref struct, filled-in with location of new cap
 * \param src           Cap_info struct for the source PhysAddr cap
 * \param size_bits     Size of created objects as a power of two
 *                      (ignored for fixed-size objects)
 */
errval_t devframe_type(struct capref *dest, struct capref src, uint8_t bits)
{
    errval_t err = slot_alloc(dest);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return cap_retype(*dest, src, ObjType_DevFrame, bits);
}

/**
 * \brief Create an ID cap in a newly allocated slot.
 *
 * \param dest  Pointer to capref struct, filld-in with location of new cap.
 *
 * The caller is responsible for revoking the cap after using it.
 */
errval_t idcap_alloc(struct capref *dest)
{
    errval_t err = slot_alloc(dest);

    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    return idcap_create(*dest);
}

/**
 * \brief Create an ID cap in the specified slot.
 *
 * \param dest  Capref, where ID cap should be created.
 *
 * The caller is responsible for revoking the cap after using it.
 */
errval_t idcap_create(struct capref dest)
{
    return cap_create(dest, ObjType_ID, 0);
}

/**
 * \brief Builds a #cnoderef struct from a #capref struct using cap
 *        identification.
 *
 * \param cnoder Pointer to a cnoderef struct, fill-in by function.
 * \param capr   Capref to a CNode capability.
 */
errval_t cnode_build_cnoderef(struct cnoderef *cnoder, struct capref capr)
{
    struct capability cap;
    errval_t err = debug_cap_identify(capr, &cap);
    if (err_is_fail(err)) {
        return err;
    }

    if (cap.type != ObjType_CNode) {
        return LIB_ERR_NOT_CNODE;
    }

    cnoder->address = get_cap_addr(capr);
    cnoder->address_bits = get_cap_valid_bits(capr);
    cnoder->size_bits = cap.u.cnode.bits;
    cnoder->guard_size = cap.u.cnode.guard_size;

    return SYS_ERR_OK;
}
