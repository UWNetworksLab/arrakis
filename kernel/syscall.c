/**
 * \file
 * \brief Arch-generic system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb.h>
#include <dispatch.h>
#include <wakeup.h>
#include <paging_kernel_helper.h>
#include <paging_kernel_arch.h>
#include <exec.h>
#include <irq.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

errval_t sys_print(const char *str, size_t length)
{
    /* FIXME: check that string is mapped and accessible to caller! */
    printf("%.*s", (int)length, str);
    return SYS_ERR_OK;
}

/* FIXME: lots of missing argument checks in this function */
struct sysret
sys_dispatcher_setup(struct capability *to, capaddr_t cptr, int depth,
                     capaddr_t vptr, capaddr_t dptr, bool run, capaddr_t odptr)
{
    errval_t err = SYS_ERR_OK;
    assert(to->type == ObjType_Dispatcher);
    struct dcb *dcb = to->u.dispatcher.dcb;

    lpaddr_t lpaddr;

    /* 1. set cspace root */
    if (cptr != CPTR_NULL) {
        struct cte *root;
        err = caps_lookup_slot(&dcb_current->cspace.cap, cptr, depth,
                               &root, CAPRIGHTS_READ);
        if (err_is_fail(err)) {
            return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_ROOT));
        }
        if (root->cap.type != ObjType_CNode) {
            return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_INVALID));
        }
        err = caps_copy_to_cte(&dcb->cspace, root, false, 0, 0);
        if (err_is_fail(err)) {
            return SYSRET(err_push(err, SYS_ERR_DISP_CSPACE_ROOT));
        }
    }

    /* 2. set vspace root */
    if (vptr != CPTR_NULL) {
        struct capability *vroot;
        err = caps_lookup_cap(&dcb_current->cspace.cap, vptr, CPTR_BITS,
                              &vroot, CAPRIGHTS_WRITE);
        if (err_is_fail(err)) {
            return SYSRET(err_push(err, SYS_ERR_DISP_VSPACE_ROOT));
        }

        // Insert as dispatcher's VSpace root
        switch(vroot->type) {
        case ObjType_VNode_x86_64_pml4:
            dcb->vspace =
                (lvaddr_t)gen_phys_to_local_phys(vroot->u.vnode_x86_64_pml4.base);
            break;
#ifdef CONFIG_PAE
        case ObjType_VNode_x86_32_pdpt:
            dcb->vspace =
                (lvaddr_t)gen_phys_to_local_phys(vroot->u.vnode_x86_32_pdpt.base);
            break;
#else
        case ObjType_VNode_x86_32_pdir:
            dcb->vspace =
                (lvaddr_t)gen_phys_to_local_phys(vroot->u.vnode_x86_32_pdir.base);
            break;
#endif
        case ObjType_VNode_ARM_l1:
            dcb->vspace =
                (lvaddr_t)gen_phys_to_local_phys(vroot->u.vnode_arm_l1.base);
            break;

        default:
            return SYSRET(err_push(err, SYS_ERR_DISP_VSPACE_INVALID));
        }
    }

    /* 3. set dispatcher frame pointer */
    if (dptr != CPTR_NULL) {
        struct cte *dispcte;
        err = caps_lookup_slot(&dcb_current->cspace.cap, dptr, CPTR_BITS,
                               &dispcte, CAPRIGHTS_WRITE);
        if (err_is_fail(err)) {
            return SYSRET(err_push(err, SYS_ERR_DISP_FRAME));
        }
        struct capability *dispcap = &dispcte->cap;
        if (dispcap->type != ObjType_Frame) {
            return SYSRET(err_push(err, SYS_ERR_DISP_FRAME_INVALID));
        }

        /* FIXME: check rights, check size */

        lpaddr = gen_phys_to_local_phys(dispcap->u.frame.base);
        dcb->disp = local_phys_to_mem(lpaddr);
        // Copy the cap to dcb also
        err = caps_copy_to_cte(&dcb->disp_cte, dispcte, false, 0, 0);
        // If copy fails, something wrong in kernel
        assert(err_is_ok(err));
    }

    /* 5. Make runnable if desired -- Set pointer to ipi_data */
    if (run) {
        if (dcb->vspace == 0 ||
        (!dcb->is_vm_guest &&
        (dcb->disp == 0 || dcb->cspace.cap.type != ObjType_CNode))) {
            return SYSRET(err_push(err, SYS_ERR_DISP_NOT_RUNNABLE));
        }

        // XXX: dispatchers run disabled the first time they start
        dcb->disabled = 1;
        //printf("DCB: %p %.*s\n", dcb, DISP_NAME_LEN, dcb->disp->name);
        make_runnable(dcb);
    }

    /* 6. Copy domain ID off given dispatcher */
    if(odptr != CPTR_NULL) {
        struct capability *odisp;
        err = caps_lookup_cap(&dcb_current->cspace.cap, odptr, CPTR_BITS,
                              &odisp, CAPRIGHTS_READ_WRITE);
        if (err_is_fail(err)) {
            return SYSRET(err_push(err, SYS_ERR_DISP_OCAP_LOOKUP));
        }
        dcb->domain_id = odisp->u.dispatcher.dcb->domain_id;
    }

    if(!dcb->is_vm_guest) {
        struct dispatcher_shared_generic *disp =
                    get_dispatcher_shared_generic(dcb->disp);
        err = trace_new_application(disp->name, (uintptr_t) dcb);

        if (err == TRACE_ERR_NO_BUFFER) {
            // Try to use the boot buffer.
            trace_new_boot_application(disp->name, (uintptr_t) dcb);
        }
    }

    return SYSRET(SYS_ERR_OK);
}

struct sysret
sys_dispatcher_properties(struct capability *to,
                          enum task_type type, unsigned long deadline,
                          unsigned long wcet, unsigned long period,
                          unsigned long release, unsigned short weight)
{
    assert(to->type == ObjType_Dispatcher);

#ifdef CONFIG_SCHEDULER_RBED
    struct dcb *dcb = to->u.dispatcher.dcb;

    assert(type >= TASK_TYPE_BEST_EFFORT && type <= TASK_TYPE_HARD_REALTIME);
    assert(wcet <= deadline);
    assert(wcet <= period);
    assert(type != TASK_TYPE_BEST_EFFORT || weight > 0);

    trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_REMOVE,
                152);
    scheduler_remove(dcb);

    /* Set task properties */
    dcb->type = type;
    dcb->deadline = deadline;
    dcb->wcet = wcet;
    dcb->period = period;
    dcb->release_time = (release == 0) ? kernel_now : release;
    dcb->weight = weight;

    make_runnable(dcb);
#endif

    return SYSRET(SYS_ERR_OK);
}

/**
 * \param root                  Root CNode to invoke
 * \param source_cptr           Source capability cptr
 * \param type                  Type to retype to
 * \param objbits               Object bits for variable-sized types
 * \param dest_cnode_cptr       Destination cnode cptr
 * \param dest_slot             Destination slot number
 * \param dest_vbits            Valid bits in destination cnode cptr
 */
struct sysret
sys_retype(struct capability *root, capaddr_t source_cptr, enum objtype type,
           uint8_t objbits, capaddr_t dest_cnode_cptr, cslot_t dest_slot,
           uint8_t dest_vbits, bool from_monitor)
{
    errval_t err;

    /* Parameter checking */
    if (type == ObjType_Null || type >= ObjType_Num) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Source capability */
    struct cte *source_cap;
    err = caps_lookup_slot(root, source_cptr, CPTR_BITS, &source_cap,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }
    assert(source_cap != NULL);

    /* Destination cnode */
    struct capability *dest_cnode_cap;
    err = caps_lookup_cap(root, dest_cnode_cptr, dest_vbits,
                          &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }
    if (dest_cnode_cap->type != ObjType_CNode) {
        return SYSRET(SYS_ERR_DEST_CNODE_INVALID);
    }

    return SYSRET(caps_retype(type, objbits, dest_cnode_cap, dest_slot,
                              source_cap, from_monitor));
}

struct sysret sys_create(struct capability *root, enum objtype type,
                         uint8_t objbits, capaddr_t dest_cnode_cptr,
                         cslot_t dest_slot, int dest_vbits)
{
    errval_t err;
    uint8_t bits = 0;
    genpaddr_t base = 0;

    /* Paramter checking */
    if (type == ObjType_Null || type >= ObjType_Num) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Destination CNode */
    struct capability *dest_cnode_cap;
    err = caps_lookup_cap(root, dest_cnode_cptr, dest_vbits,
                          &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    /* Destination slot */
    struct cte *dest_cte;
    dest_cte = caps_locate_slot(dest_cnode_cap->u.cnode.cnode, dest_slot);
    if (dest_cte->cap.type != ObjType_Null) {
        return SYSRET(SYS_ERR_SLOTS_IN_USE);
    }

    /* List capabilities allowed to be created at runtime. */
    switch(type) {

    case ObjType_ID:
        break;

    // only certain types of capabilities can be created at runtime
    default:
        return SYSRET(SYS_ERR_TYPE_NOT_CREATABLE);
    }

    return SYSRET(caps_create_new(type, base, bits, objbits, dest_cte));
}

/**
 * Common code for copying and minting except the mint flag and param passing
 */
struct sysret
sys_copy_or_mint(struct capability *root, capaddr_t destcn_cptr, cslot_t dest_slot,
             capaddr_t source_cptr, int destcn_vbits, int source_vbits,
             uintptr_t param1, uintptr_t param2, bool mint)
{
    errval_t err;

    if (!mint) {
        param1 = param2 = 0;
    }

    /* Lookup source cap */
    struct cte *src_cap;
    err = caps_lookup_slot(root, source_cptr, source_vbits,
                           &src_cap, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }

    /* Lookup destination cnode cap */
    struct cte *dest_cnode_cap;
    err = caps_lookup_slot(root, destcn_cptr, destcn_vbits,
                           &dest_cnode_cap, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_DEST_CNODE_LOOKUP));
    }

    /* Perform copy */
    if (dest_cnode_cap->cap.type == ObjType_CNode) {
        return SYSRET(caps_copy_to_cnode(dest_cnode_cap, dest_slot, src_cap,
                                         mint, param1, param2));
    } else {
        return SYSRET(SYS_ERR_DEST_TYPE_INVALID);
    }
}

static inline struct cte *cte_for_cap(struct capability *cap)
{
    return (struct cte *) (cap - offsetof(struct cte, cap));
}

struct sysret
sys_map(struct capability *ptable, cslot_t slot, capaddr_t source_cptr,
        int source_vbits, uintptr_t flags, uintptr_t offset,
        uintptr_t pte_count)
{
    assert (type_is_vnode(ptable->type));

    errval_t err;

    /* Lookup source cap */
    struct capability *root = &dcb_current->cspace.cap;
    struct cte *src_cte;
    err = caps_lookup_slot(root, source_cptr, source_vbits, &src_cte,
                           CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_SOURCE_CAP_LOOKUP));
    }

    /* Perform map */
    // XXX: this does not check if we do have CAPRIGHTS_READ_WRITE on
    // the destination cap (the page table we're inserting into)
    return SYSRET(caps_copy_to_vnode(cte_for_cap(ptable), slot, src_cte, flags,
                                     offset, pte_count));
}

struct sysret sys_delete(struct capability *root, capaddr_t cptr, uint8_t bits,
                         bool from_monitor)
{
    errval_t err;
    struct cte *slot;
    err = caps_lookup_slot(root, cptr, bits, &slot, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    err = caps_delete(slot, from_monitor);
    return SYSRET(err);
}

struct sysret sys_revoke(struct capability *root, capaddr_t cptr, uint8_t bits,
                         bool from_monitor)
{
    errval_t err;
    struct cte *slot;
    err = caps_lookup_slot(root, cptr, bits, &slot, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    err = caps_revoke(slot, from_monitor);
    return SYSRET(err);
}

struct sysret sys_monitor_register(capaddr_t ep_caddr)
{
    errval_t err;
    struct capability *ep;
    err = caps_lookup_cap(&dcb_current->cspace.cap, ep_caddr, CPTR_BITS, &ep,
                          CAPRIGHTS_READ);

    if(err_is_fail(err)) {
        printf("Failure looking up endpoint!\n");
        return SYSRET(err);
    }

    monitor_ep = *ep;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_identify_cap(struct capability *root,
                                       capaddr_t cptr, uint8_t bits,
                                       struct capability *retbuf)
{
    struct capability *cap;
    errval_t err = caps_lookup_cap(root, cptr, bits, &cap, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    *retbuf = *cap;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_nullify_cap(capaddr_t cptr, uint8_t bits)
{
    struct capability *root = &dcb_current->cspace.cap;
    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, bits, &cte,
                                    CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    // remove from MDB
    remove_mapping(cte);

    // zero-out cap entry
    memset(cte, 0, sizeof(struct cte));

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_monitor_domain_id(capaddr_t cptr, domainid_t domain_id)
{
    struct capability *root = &dcb_current->cspace.cap;
    struct capability *disp;

    errval_t err = caps_lookup_cap(root, cptr, CPTR_BITS, &disp,
                                   CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    disp->u.dispatcher.dcb->domain_id = domain_id;

    return SYSRET(SYS_ERR_OK);
}

struct sysret sys_yield(capaddr_t target)
{
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);


    debug(SUBSYS_DISPATCH, "%.*s yields%s\n", DISP_NAME_LEN, disp->name,
          !disp->haswork && disp->lmp_delivered == disp->lmp_seen
           ? " and is removed from the runq" : "");

    if (!disp->disabled) {
        printk(LOG_ERR, "SYSCALL_YIELD while enabled\n");
        return SYSRET(SYS_ERR_CALLER_ENABLED);
    }

    struct capability *yield_to = NULL;
    if (target != CPTR_NULL) {
        errval_t err;

        /* directed yield */
        err = caps_lookup_cap(&dcb_current->cspace.cap, target, CPTR_BITS,
                              &yield_to, CAPRIGHTS_READ);
        if (err_is_fail(err)) {
            return SYSRET(err);
        } else if (yield_to == NULL ||
                   (yield_to->type != ObjType_EndPoint
                    && yield_to->type != ObjType_Dispatcher)) {
            return SYSRET(SYS_ERR_INVALID_YIELD_TARGET);
        }
        /* FIXME: check rights? */
    }

    disp->disabled = false;
    dcb_current->disabled = false;

    // Remove from queue when no work and no more messages and no missed wakeup
    systime_t wakeup = disp->wakeup;
    if (!disp->haswork && disp->lmp_delivered == disp->lmp_seen
        && (wakeup == 0 || wakeup > kernel_now)) {

         trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_SCHED_REMOVE,
            (uint32_t)(lvaddr_t)dcb_current & 0xFFFFFFFF);

        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_SCHED_REMOVE,
                151);

    /*
        if (disp->name[0] == 'e' && disp->name[1] == '1' ) {
            printk(LOG_ERR, "%s has been removed from scheduler queue\n",
                disp->name);
        }
*/

        scheduler_remove(dcb_current);
        if (wakeup != 0) {
            wakeup_set(dcb_current, wakeup);
        }
    } else {
        // Otherwise yield for the timeslice
        scheduler_yield(dcb_current);
    }

    if (yield_to != NULL) {
        struct dcb *target_dcb = NULL;
        if (yield_to->type == ObjType_EndPoint) {
            target_dcb = yield_to->u.endpoint.listener;
        } else if (yield_to->type == ObjType_Dispatcher) {
            target_dcb = yield_to->u.dispatcher.dcb;
        } else {
            panic("invalid type in yield cap");
        }

        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_YIELD,
            (uint32_t)(lvaddr_t)target_dcb & 0xFFFFFFFF);
        make_runnable(target_dcb);
        dispatch(target_dcb);
    } else {
//        trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_YIELD,
//            0);

        /* undirected yield */
        dispatch(schedule());
    }

    panic("Yield returned!");
}

/**
 * The format of the returned ID is:
 *
 * --------------------------------------------------------------------
 * |             0 (unused) | coreid |         core_local_id          |
 * --------------------------------------------------------------------
 * 63                        39       31                              0 Bit
 *
 */
struct sysret sys_idcap_identify(struct capability *cap, idcap_id_t *id)
{
    STATIC_ASSERT_SIZEOF(coreid_t, 1);

    idcap_id_t coreid = (idcap_id_t) cap->u.id.coreid;
    *id = coreid << 32 | cap->u.id.core_local_id;

    return SYSRET(SYS_ERR_OK);
}
