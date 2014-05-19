/**
 * \file
 * \brief Low-level capability invocations
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INVOCATIONS_ARCH_H
#define INVOCATIONS_ARCH_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/paging_arch.h>

static inline struct sysret cap_invoke(struct capref to, uintptr_t arg1,
                                       uintptr_t arg2, uintptr_t arg3,
                                       uintptr_t arg4, uintptr_t arg5,
                                       uintptr_t arg6, uintptr_t arg7,
                                       uintptr_t arg8, uintptr_t arg9,
                                       uintptr_t arg10)
{
    uint8_t invoke_bits = get_cap_valid_bits(to);
    capaddr_t invoke_cptr = get_cap_addr(to) >> (CPTR_BITS - invoke_bits);

    return syscall(SYSCALL_INVOKE, (uint64_t)invoke_cptr << 32 |
                   (uint64_t)invoke_bits << 16 | 10 << 8, 0,
                   arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
                   arg10);
}

#define cap_invoke10(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j)   \
    cap_invoke(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, _j)
#define cap_invoke9(to, _a, _b, _c, _d, _e, _f, _g, _h, _i)        \
    cap_invoke10(to, _a, _b, _c, _d, _e, _f, _g, _h, _i, 0)
#define cap_invoke8(to, _a, _b, _c, _d, _e, _f, _g, _h)    \
    cap_invoke9(to, _a, _b, _c, _d, _e, _f, _g, _h, 0)
#define cap_invoke7(to, _a, _b, _c, _d, _e, _f, _g)    \
    cap_invoke8(to, _a, _b, _c, _d, _e, _f, _g, 0)
#define cap_invoke6(to, _a, _b, _c, _d, _e, _f)        \
    cap_invoke7(to, _a, _b, _c, _d, _e, _f, 0)
#define cap_invoke5(to, _a, _b, _c, _d, _e)            \
    cap_invoke6(to, _a, _b, _c, _d, _e, 0)
#define cap_invoke4(to, _a, _b, _c, _d)                \
    cap_invoke5(to, _a, _b, _c, _d, 0)
#define cap_invoke3(to, _a, _b, _c)                    \
    cap_invoke4(to, _a, _b, _c, 0)
#define cap_invoke2(to, _a, _b)                        \
    cap_invoke3(to, _a, _b, 0)
#define cap_invoke1(to, _a)                            \
    cap_invoke2(to, _a, 0)


/**
 * \brief Retype a capability.
 *
 * Retypes CPtr 'cap' into 2^'objbits' caps of type 'newtype' and places them
 * into slots starting at slot 'slot' in the CNode, addressed by 'to', with
 * 'bits' address bits of 'to' valid.
 *
 * See also cap_retype(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param cap           Address of cap to retype.
 * \param newtype       Kernel object type to retype to.
 * \param objbits       Size of created objects, for variable-sized types
 * \param to            Address of CNode cap to place retyped caps into.
 * \param slot          Slot in CNode cap to start placement.
 * \param bits          Number of valid address bits in 'to'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_retype(struct capref root, capaddr_t cap,
                                           enum objtype newtype, int objbits,
                                           capaddr_t to, capaddr_t slot, int bits)
{
    assert(cap != CPTR_NULL);
    return cap_invoke7(root, CNodeCmd_Retype, cap, newtype, objbits, to,
                       slot, bits).error;
}

/**
 * \brief Create a capability.
 *
 * Create a new capability of type 'type' and size 'objbits'. The new cap will
 * be placed in the slot 'dest_slot' of the CNode located at 'dest_cnode_cptr'
 * in the address space rooted at 'root'.
 *
 * See also cap_create(), which wraps this.
 *
 * \param root            Capability of the CNode to invoke.
 * \param type            Kernel object type to create.
 * \param objbits         Size of created object
 *                        (ignored for fixed-size objects)
 * \param dest_cnode_cptr Address of CNode cap, where newly created cap will be
 *                        placed into.
 * \param dest_slot       Slot in CNode cap to place new cap.
 * \param dest_vbits      Number of valid address bits in 'dest_cnode_cptr'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_create(struct capref root,
                                           enum objtype type, uint8_t objbits,
                                           capaddr_t dest_cnode_cptr,
                                           capaddr_t dest_slot,
                                           uint8_t dest_vbits)
{
    assert(dest_cnode_cptr != CPTR_NULL);
    return cap_invoke6(root, CNodeCmd_Create, type, objbits, dest_cnode_cptr,
                       dest_slot, dest_vbits).error;
}

/**
 * \brief "Mint" a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_mint(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param to            CNode to place copy into.
 * \param slot          Slot in CNode cap to place copy into.
 * \param from          Address of cap to copy.
 * \param tobits        Number of valid address bits in 'to'.
 * \param frombits      Number of valid address bits in 'from'.
 * \param param1        1st cap-dependent parameter.
 * \param param2        2nd cap-dependent parameter.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_mint(struct capref root, capaddr_t to,
                                         capaddr_t slot, capaddr_t from, int tobits,
                                         int frombits, uint64_t param1,
                                         uint64_t param2)
{
    return cap_invoke8(root, CNodeCmd_Mint, to, slot, from, tobits, frombits,
                       param1, param2).error;
}

/**
 * \brief Copy a capability.
 *
 * Copies CPtr 'from' into slot 'slot' in the CNode, addressed by 'to', within
 * the address space, rooted at 'root' and with 'tobits' and 'frombits' address
 * bits of 'to' and 'from' valid, respectively.
 *
 * See also cap_copy(), which wraps this.
 *
 * \param root          Capability of the CNode to invoke
 * \param to            CNode to place copy into.
 * \param slot          Slot in CNode cap to place copy into.
 * \param from          Address of cap to copy.
 * \param tobits        Number of valid address bits in 'to'.
 * \param frombits      Number of valid address bits in 'from'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_copy(struct capref root, capaddr_t to,
                                         capaddr_t slot, capaddr_t from, int tobits,
                                         int frombits)
{
    return cap_invoke6(root, CNodeCmd_Copy, to, slot, from,
                       tobits, frombits).error;
}

/**
 * \brief Delete a capability.
 *
 * Delete the capability pointed to by 'cap', with 'bits' address bits
 * of it valid, from the address space rooted at 'root'.
 *
 * \param root  Capability of the CNode to invoke
 * \param cap   Address of cap to delete.
 * \param bits  Number of valid bits within 'cap'.
 *
 * \return Error code
 */
static inline errval_t invoke_cnode_delete(struct capref root, capaddr_t cap,
                                           int bits)
{
    return cap_invoke3(root, CNodeCmd_Delete, cap, bits).error;
}

static inline errval_t invoke_cnode_revoke(struct capref root, capaddr_t cap,
                                           int bits)
{
    return cap_invoke3(root, CNodeCmd_Revoke, cap, bits).error;
}

static inline errval_t invoke_vnode_map(struct capref ptable, capaddr_t slot,
                                        capaddr_t src, int frombits, size_t flags,
                                        size_t offset, size_t pte_count)
{
    return cap_invoke7(ptable, VNodeCmd_Map, slot, src, frombits, flags, offset, pte_count).error;
}

static inline errval_t invoke_vnode_unmap(struct capref cap, capaddr_t mapping_addr,
                                          int bits, size_t entry, size_t num_pages)
{
    return cap_invoke5(cap, VNodeCmd_Unmap, mapping_addr, bits, entry, num_pages).error;
}

/**
 * \brief Return the physical address and size of a frame capability
 *
 * \param frame    CSpace address of frame capability
 * \param ret      frame_identity struct filled in with relevant data
 *
 * \return Error code
 */
static inline errval_t invoke_frame_identify(struct capref frame,
                                             struct frame_identity *ret)
{
    struct sysret sysret = cap_invoke1(frame, FrameCmd_Identify);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        ret->base = sysret.value & (~BASE_PAGE_MASK);
        ret->bits = sysret.value & BASE_PAGE_MASK;
        return sysret.error;
    }

    ret->base = 0;
    ret->bits = 0;
    return sysret.error;
}

static inline errval_t invoke_vnode_identify(struct capref vnode,
					     struct vnode_identity *ret)
{
    struct sysret sysret = cap_invoke1(vnode, VNodeCmd_Identify);

    assert(ret != NULL);
    if (err_is_ok(sysret.error)) {
        ret->base = sysret.value & (~BASE_PAGE_MASK);
	ret->type = sysret.value & BASE_PAGE_MASK;
        return sysret.error;
    }

    ret->base = 0;
    ret->type = 0;
    return sysret.error;
}

/**
 * \brief Modify mapping flags on parts of a mapped frame
 *
 * \param frame    CSpace address of frame capability
 * \param off      Offset (in #pages) of the first page to get new set of flags
 *                 from the first page in the mapping identified by `frame`
 * \param pages    Number of pages that should get new set of flags
 * \param flags    New set of flags
 *
 * \return Error code
 */
static inline errval_t invoke_frame_modify_flags(struct capref frame,
                                                 size_t offset,
                                                 size_t pages,
                                                 size_t flags)
{
    return cap_invoke4(frame, FrameCmd_ModifyFlags, offset, pages, flags).error;
}

static inline errval_t invoke_iocap_in(struct capref iocap, enum io_cmd cmd,
                                       uint16_t port, uint32_t *data)
{
    struct sysret sysret = cap_invoke2(iocap, cmd, port);

    if (err_is_ok(sysret.error)) {
        assert(data != NULL);
        *data = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_iocap_out(struct capref iocap, enum io_cmd cmd,
                                        uint16_t port, uint32_t data)
{
    return cap_invoke3(iocap, cmd, port, data).error;
}

/**
 * \brief Setup a dispatcher, possibly making it runnable
 *
 * \param dispatcher    Address of dispatcher capability
 * \param domdispatcher Address of existing dispatcher for domain ID
 * \param cspace_root   Root of CSpace for new dispatcher
 * \param cspace_root_bits  Number of valid bits in cspace_root
 * \param vspace_root   Root of VSpace for new dispatcher
 * \param dispatcher_frame Frame capability for dispatcher structure
 * \param run           Make runnable if true
 *
 * Any arguments of CPTR_NULL are ignored.
 *
 * \return Error code
 */
static inline errval_t
invoke_dispatcher(struct capref dispatcher, struct capref domdispatcher,
                  struct capref cspace, struct capref vspace,
                  struct capref dispframe, bool run)
{
    uint8_t root_vbits = get_cap_valid_bits(cspace);
    capaddr_t root_caddr = get_cap_addr(cspace) >> (CPTR_BITS - root_vbits);
    capaddr_t vtree_caddr = get_cap_addr(vspace);
    capaddr_t disp_caddr = get_cap_addr(dispframe);
    capaddr_t dd_caddr = get_cap_addr(domdispatcher);

    return cap_invoke7(dispatcher, DispatcherCmd_Setup, root_caddr,
                       root_vbits, vtree_caddr, disp_caddr, run,
                       dd_caddr).error;
}

/**
 * \brief Setup a VM guest DCB
 *
 * \param dcb       Dispatcher capability
 */
static inline errval_t
invoke_dispatcher_setup_guest(struct capref dispatcher,
                              struct capref ep_cap,
                              struct capref vnode,
                              struct capref vmkit_guest,
                              struct capref guest_control_cap)
{
    return cap_invoke5(dispatcher, DispatcherCmd_SetupGuest,
                       get_cap_addr(ep_cap), get_cap_addr(vnode),
                       get_cap_addr(vmkit_guest),
                       get_cap_addr(guest_control_cap)).error;
}

static inline errval_t invoke_irqtable_set(struct capref irqcap, int irq,
                                           struct capref ep)
{
    return cap_invoke3(irqcap, IRQTableCmd_Set, irq, get_cap_addr(ep)).error;
}

static inline errval_t invoke_irqtable_delete(struct capref irqcap, int irq)
{
    return cap_invoke2(irqcap, IRQTableCmd_Delete, irq).error;
}

/**
 * \brief do a kernel cap invocation to get the core id
 */
static inline errval_t invoke_kernel_get_core_id(struct capref kern_cap,
                                                 coreid_t *core_id)
{
    assert(core_id != NULL);

    struct sysret sysret = cap_invoke1(kern_cap, KernelCmd_Get_core_id);
    if (sysret.error == SYS_ERR_OK) {
        *core_id = sysret.value;
    }
    return sysret.error;
}

static inline errval_t invoke_dispatcher_dump_ptables(struct capref dispcap)
{
    return cap_invoke1(dispcap, DispatcherCmd_DumpPTables).error;
}

static inline errval_t invoke_perfmon_activate(struct capref perfmon_cap,
                                               uint8_t event, uint8_t perf_umask, 
                                               bool kernel, uint8_t counter_id,
                                               uint64_t counter_value, 
                                               capaddr_t ep_addr)
{
    return cap_invoke7(perfmon_cap, PerfmonCmd_Activate, 
                       event, perf_umask, counter_id, kernel, 
                       counter_value, ep_addr).error;
}

static inline errval_t invoke_perfmon_write(struct capref perfmon_cap,
                                                  uint8_t counter_id,
                                                  uint64_t counter_value)
{
    return cap_invoke3(perfmon_cap, PerfmonCmd_Write, counter_id, counter_value).error;
}

static inline errval_t invoke_perfmon_deactivate(struct capref perfmon_cap)
{
    return cap_invoke1(perfmon_cap, PerfmonCmd_Deactivate).error;
}

static inline errval_t
invoke_dispatcher_properties(struct capref dispatcher,
                             enum task_type type, unsigned long deadline,
                             unsigned long wcet, unsigned long period,
                             unsigned long release, unsigned short weight)
{
    return cap_invoke7(dispatcher, DispatcherCmd_Properties, type, deadline,
                       wcet, period, release, weight).error;
}

static inline errval_t invoke_ipi_notify_send(struct capref notify_cap)
{
    return cap_invoke1(notify_cap, NotifyCmd_Send).error;
}

/**
 * \brief Return the system-wide unique ID of the passed ID capability.
 *
 * \param idcap ID capability to invoke.
 * \param id    Filled-in with system-wide unique ID of ID cap.
 *
 * \return      Error code
 */
static inline errval_t invoke_idcap_identify(struct capref idcap,
                                             idcap_id_t *id)
{
    assert(id != NULL);

    struct sysret sysret = cap_invoke1(idcap, IDCmd_Identify);

    if (err_is_ok(sysret.error)) {
        *id = sysret.value;
    }

    return sysret.error;
}

#endif
