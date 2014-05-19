/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "init.h"

/**
 * Initialize mem_serv while spawning it.
 */
errval_t initialize_mem_serv(struct spawninfo *si)
{
    errval_t err;

    /* copy supercn to memory server */;
    struct capref init_supercn_cap = {
        .cnode = cnode_root,
        .slot  = ROOTCN_SLOT_SUPERCN
    };
    struct capref child_supercn_cap = {
        .cnode = si->rootcn,
        .slot  = ROOTCN_SLOT_SUPERCN
    };
    err = cap_copy(child_supercn_cap, init_supercn_cap);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_SUPERCN_CAP);
    }

    return SYS_ERR_OK;
}

errval_t initialize_monitor(struct spawninfo *si)
{
    errval_t err;

    /* Give monitor the kernel capability */
    struct capref dest, src;
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_KERNELCAP;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_KERNELCAP;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_KERNEL_CAP);
    }

    /* Give monitor the perfmon capability */
    dest.cnode = si->taskcn;
    dest.slot = TASKCN_SLOT_PERF_MON;
    src.cnode = cnode_task;
    src.slot = TASKCN_SLOT_PERF_MON;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PERF_MON);
    }

    /* Give monitor modulecn */
    dest.cnode = si->rootcn;
    dest.slot  = ROOTCN_SLOT_MODULECN;
    src.cnode = cnode_root;
    src.slot  = ROOTCN_SLOT_MODULECN;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_MODULECN_CAP);
    }

    /* Give monitor physaddr cn */
    dest.cnode = si->rootcn;
    dest.slot  = ROOTCN_SLOT_PACN;
    src.cnode = cnode_root;
    src.slot  = ROOTCN_SLOT_PACN;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_PACN_CAP);
    }

#if __x86_64__ || __i386__
    /* Give monitor IRQ */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_IRQ;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_IRQ;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IRQ_CAP);
    }

    /* Give monitor IO */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_IO;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_IO;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
        return err_push(err, INIT_ERR_COPY_IO_CAP);
    }
#endif // __x86_64__ || __i386__

#if __arm__
    /* Give monitor IO */
       dest.cnode = si->taskcn;
       dest.slot  = TASKCN_SLOT_IO;
       src.cnode = cnode_task;
       src.slot  = TASKCN_SLOT_IO;
       err = cap_copy(dest, src);
       if (err_is_fail(err)) {
           return err_push(err, INIT_ERR_COPY_IO_CAP);
       }
       /* Give monitor IRQ */
           dest.cnode = si->taskcn;
           dest.slot  = TASKCN_SLOT_IRQ;
           src.cnode = cnode_task;
           src.slot  = TASKCN_SLOT_IRQ;
           err = cap_copy(dest, src);
           if (err_is_fail(err)) {
               return err_push(err, INIT_ERR_COPY_IRQ_CAP);
           }
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
#if 0   // XXX: Disabled until SCC has a decent memory allocator
    /* Give monitor the foreign frame capability */
    dest.cnode = si->taskcn;
    dest.slot  = TASKCN_SLOT_MON_URPC;
    src.cnode = cnode_task;
    src.slot  = TASKCN_SLOT_MON_URPC;
    err = cap_copy(dest, src);
    if (err_is_fail(err)) {
	return err_push(err, INIT_ERR_COPY_UMP_CAP);
    }
#endif
#endif // CONFIG_INTERCONNECT_DRIVER_UMP

    return SYS_ERR_OK;
}
