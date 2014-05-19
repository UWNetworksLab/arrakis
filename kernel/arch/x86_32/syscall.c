/**
 * \file
 * \brief System calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <capabilities.h>
#include <mdb/mdb.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <exec.h>
#include <arch/x86/apic.h>
#include <arch/x86/perfmon_intel.h>
#include <arch/x86/global.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <barrelfish_kpi/syscall_overflows_arch.h>
#include <trace/trace.h>
#include <arch/x86/debugregs.h>
#include <arch/x86/syscall.h>
#include <arch/x86/timing.h>
#include <fpu.h>
#include <useraccess.h>
#ifdef __scc__
#       include <rck.h>
#else
#       include <arch/x86/perfmon_amd.h>
#       include <arch/x86/ipi_notify.h>
#endif

/* FIXME: lots of missing argument checks in this function */
static struct sysret handle_dispatcher_setup(struct capability *to,
                                             int cmd, uintptr_t *args)
{
    capaddr_t odptr = args[0];
    capaddr_t cptr = args[1];
    uintptr_t rundepth = args[2];
    int depth = rundepth & 0xff;
    bool run = rundepth >> 8;
    capaddr_t vptr = args[3];
    capaddr_t dptr = args[4];

    return sys_dispatcher_setup(to, cptr, depth, vptr, dptr, run, odptr);

}

static struct sysret handle_dispatcher_properties(struct capability *to,
                                                  int cmd, uintptr_t *args)
{
    enum task_type type = args[0] >> 16;
    unsigned short weight = args[0] & 0xffff;
    unsigned long deadline = args[1];
    unsigned long wcet = args[2];
    unsigned long period = args[3];
    unsigned long release = args[4];

    return sys_dispatcher_properties(to, type, deadline, wcet, period,
                                     release, weight);
}

// XXX: FIXME: cleanup and handle errors!
static struct sysret handle_dispatcher_perfmon(struct capability *to,
                                               int cmd, uintptr_t *args)
{
#if 1
    return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);

#else
    perfmon_counter_t idx = (perfmon_counter_t)(args[0]);

    // Currently only AMD perfmon is supported
    if(!perfmon_amd_supported()) {
        return SYSRET(SYS_ERR_PERFMON_NOT_AVAILABLE);
    }

    uint64_t operation = args[1];
    switch(operation) {
    case 0: {
        perfmon_event_t event = args[2];
        perfmon_mask_t umask = args[3];
        bool os = args[4];
        perfmon_amd_measure_start(event, umask, os ? true : false, idx);
        break;
    }

    case 1: {
        uint64_t val = args[2];
        perfmon_amd_measure_write(val, idx);
        break;
    }

    default:
        panic("Unknown performance monitoring function!");
    }

    return SYSRET(SYS_ERR_OK);
#endif
}

static struct sysret handle_retype_common(struct capability *root,
                                          uintptr_t *args,
                                          bool from_monitor)
{
    // Source capability cptr
    capaddr_t source_cptr      = args[0];
    // Type to retype to
    enum objtype type        = args[1] >> 16;
    // Object bits for variable-sized types
    uint8_t objbits          = (args[1] >> 8) & 0xff;
    // Destination cnode cptr
    capaddr_t  dest_cnode_cptr = args[2];
    // Destination slot number
    capaddr_t dest_slot        = args[3];
    // Valid bits in destination cnode cptr
    uint64_t dest_vbits      = args[1] & 0xff;

    return sys_retype(root, source_cptr, type, objbits, dest_cnode_cptr,
                      dest_slot, dest_vbits, from_monitor);
}

static struct sysret handle_retype(struct capability *root, int cmd, uintptr_t *args)
{
    return handle_retype_common(root, args, false);
}

static struct sysret handle_create(struct capability *root, int cmd,
                                   uintptr_t *args)
{
    /* Retrieve arguments */
    enum objtype type         = args[0] >> 16;
    uint8_t objbits           = (args[0] >> 8) & 0xff;
    capaddr_t dest_cnode_cptr = args[1];
    capaddr_t dest_slot       = args[2];
    uint8_t dest_vbits        = args[0] & 0xff;

    return sys_create(root, type, objbits, dest_cnode_cptr, dest_slot,
                      dest_vbits);
}

/**
 * Common code for copying and minting except the mint flag and param passing
 */
static struct sysret copy_or_mint(struct capability *root,
                                  uintptr_t *args, bool mint)
{
    /* Retrive arguments */
    capaddr_t  destcn_cptr   = args[0];
    capaddr_t  source_cptr   = args[1];
    capaddr_t dest_slot      = args[2] >> 16;
    int      destcn_vbits  = (args[2] >> 8) & 0xff;
    int      source_vbits  = args[2] & 0xff;
    uintptr_t param1, param2;
    // params only sent if mint operation
    if (mint) {
        param1 = args[3];
        param2 = args[4];
    } else {
        param1 = param2 = 0;
    }

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, param1, param2, mint);
}

static struct sysret handle_mint(struct capability *root, int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, true);
}

static struct sysret handle_copy(struct capability *root,
                                 int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, false);
}

static struct sysret handle_delete_common(struct capability *root,
                                          uintptr_t *args, bool from_monitor)
{
    capaddr_t cptr = args[0];
    int bits     = args[1];
    return sys_delete(root, cptr, bits, from_monitor);
}

static struct sysret handle_delete(struct capability *root, int cmd, uintptr_t *args)
{
    return  handle_delete_common(root, args, false);
}

static struct sysret handle_revoke_common(struct capability *root,
                                          uintptr_t *args,
                                          bool from_monitor)
{
    capaddr_t cptr = args[0];
    int bits     = args[1];
    return sys_revoke(root, cptr, bits, from_monitor);
}

static struct sysret handle_revoke(struct capability *root,
                                   int cmd, uintptr_t *args)
{
    return  handle_revoke_common(root, args, false);
}

static struct sysret handle_map(struct capability *pgtable,
                                int cmd, uintptr_t *args)
{
    /* Retrive arguments */
    capaddr_t  source_cptr   = args[0];
    capaddr_t dest_slot      = args[1] >> 16;
    int      source_vbits  = args[1] & 0xff;
    uintptr_t flags, offset,pte_count;
    flags = args[2];
    offset = args[3];
    pte_count = args[4];

    return sys_map(pgtable, dest_slot, source_cptr, source_vbits,
                   flags, offset, pte_count);
}

static struct sysret handle_unmap(struct capability *pgtable,
                                  int cmd, uintptr_t *args)
{
    size_t mapping_caddr = args[0];
    size_t entry = args[1] & 0x3ff;
    size_t pte_count = (args[1]>>10) & 0x3ff;
    pte_count += 1;
    int mapping_bits = (args[1]>>20) & 0xff;

    errval_t err;
    struct cte *mapping = NULL;
    err = caps_lookup_slot(&dcb_current->cspace.cap, mapping_caddr, mapping_bits,
                           &mapping, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_CAP_NOT_FOUND));
    }

    err = page_mappings_unmap(pgtable, mapping, entry, pte_count);
    return SYSRET(err);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_retype(struct capability *kernel_cap,
                                           int cmd, uintptr_t *args)
{
    errval_t err;

    struct remote_retype_syscall_overflow * overflow =
        (struct remote_retype_syscall_overflow *) args[0];

    assert(overflow != NULL);

    capaddr_t root_caddr = overflow->rootcap_addr;
    capaddr_t root_vbits = overflow->rootcap_vbits;

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: conceal first word of arguments */
    return handle_retype_common(root, &args[1], true);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_delete(struct capability *kernel_cap,
                                           int cmd, uintptr_t *args)
{
    errval_t err;

    capaddr_t root_caddr = args[0];
    capaddr_t root_vbits = args[1];

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: conceal first two words of arguments */
    return handle_delete_common(root, &args[2], true);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_revoke(struct capability *kernel_cap,
                                           int cmd, uintptr_t *args)
{
    errval_t err;

    capaddr_t root_caddr = args[0];
    capaddr_t root_vbits = args[1];

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: conceal first two words of arguments */
    return handle_revoke_common(root, &args[2], true);
}

static struct sysret monitor_handle_register(struct capability *kernel_cap,
                                             int cmd, uintptr_t *args)
{
    capaddr_t ep_caddr = args[0];
    return sys_monitor_register(ep_caddr);
}

#ifndef __scc__
/**
 * \brief Spawn a new core and create a kernel cap for it.
 */
static struct sysret monitor_spawn_core(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    coreid_t core_id        = args[0];
    enum cpu_type cpu_type  = args[1];
    uintptr_t entry0 = args[2];
    uintptr_t entry1 = args[3];
    forvaddr_t entry = (forvaddr_t)entry0 << 32 | entry1;

    return sys_monitor_spawn_core(core_id, cpu_type, entry);
}

#else

static struct sysret monitor_spawn_scc_core(struct capability *kernel_cap,
                                            int cmd, uintptr_t *args)
{
    uint8_t id                  = args[1] >> 24;
    genpaddr_t urpcframe_base   = args[0];
    uint8_t urpcframe_bits      = (args[1] >> 16) & 0xff;
    int chanid                  = args[1] & 0xffff;

    int r = rck_start_core(id, urpcframe_base, urpcframe_bits, chanid);

    if (r != 0) {
        return SYSRET(SYS_ERR_CORE_NOT_FOUND);
    }

    return SYSRET(SYS_ERR_OK);
}
#endif

static struct sysret monitor_get_core_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = my_core_id
    };
}

static struct sysret monitor_get_arch_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = apic_id
    };
}

static struct sysret monitor_identify_cap_common(struct capability *kernel_cap,
                                                 struct capability *root,
                                                 uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits = args[1];
    struct capability *retbuf = (void *)args[2];

    return sys_monitor_identify_cap(root, cptr, bits, retbuf);
}

static struct sysret monitor_identify_cap(struct capability *kernel_cap,
                                          int cmd, uintptr_t *args)
{
    return monitor_identify_cap_common(kernel_cap, &dcb_current->cspace.cap, args);
}

static struct sysret monitor_identify_domains_cap(struct capability *kernel_cap,
                                                  int cmd, uintptr_t *args)
{
    errval_t err;

    capaddr_t root_caddr = args[0];
    capaddr_t root_vbits = args[1];

    struct capability *root;
    err = caps_lookup_cap(&dcb_current->cspace.cap, root_caddr, root_vbits,
                          &root, CAPRIGHTS_READ);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_ROOT_CAP_LOOKUP));
    }

    /* XXX: conceal first two words of arguments */
    return monitor_identify_cap_common(kernel_cap, root, &args[2]);
}

static struct sysret monitor_remote_cap(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    struct capability *root = &dcb_current->cspace.cap;
    capaddr_t cptr = args[0];
    int bits = args[1];
    bool remote = (bool)args[2];

    struct cte *cte;
    errval_t err = caps_lookup_slot(root, cptr, bits, &cte, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_IDENTIFY_LOOKUP));
    }

    set_cap_remote(cte, remote);
    bool has_desc = has_descendants(cte);

    return (struct sysret){ .error = SYS_ERR_OK, .value = has_desc };
}


static struct sysret monitor_create_cap(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    /* Create the cap in the destination */
    capaddr_t cnode_cptr = args[0];
    int cnode_vbits    = args[1];
    size_t slot        = args[2];
    struct capability *src =
        (struct capability*)args[3];

    /* Certain types cannot be created here */
    if ((src->type == ObjType_Null) || (src->type == ObjType_EndPoint)
        || (src->type == ObjType_Dispatcher) || (src->type == ObjType_Kernel)
        || (src->type == ObjType_IRQTable)) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, src));
}

static struct sysret monitor_nullify_cap(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits = args[1];

    return sys_monitor_nullify_cap(cptr, bits);
}

static struct sysret monitor_iden_cnode_get_cap(struct capability *kern_cap,
                                                int cmd, uintptr_t *args)
{
#if 0 /* not called on user side! */
    errval_t err;

    // deal with strict aliasing rules, also there is no specific
    // reason that a capability is a multiple in size of 64-bits.
    union {
        struct capability cap;
        uint64_t raw[(sizeof(struct capability) + sizeof(uint64_t) -1)/sizeof(uint64_t)];
    } u;

    /* Get the raw metadata of the cnode */
    for(int i = 0; i < sizeof(u) / sizeof(uint64_t); i++) {
        u.raw[i] = args[i];
    }

    struct capability *cnode = &u.cap;
    assert(cnode->type == ObjType_CNode);

    struct capability *cnode_copy;
    err = mdb_get_copy(cnode, &cnode_copy);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    capaddr_t slot = args[];
    struct cte* cte = caps_locate_slot(cnode_copy->u.cnode.cnode, slot);

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    struct capability *retbuf = (void *)args[xxx];
    *retbuf = cte->cap;

    return SYSRET(SYS_ERR_OK);
#else
    return SYSRET(ERR_NOTIMP);
#endif
}


static struct sysret monitor_handle_sync_timer(struct capability *kern_cap,
                                               int cmd, uintptr_t *args)
{
    uint64_t synctime = (uint64_t)args[0] << 32;
    synctime |= (uint64_t)args[1];
    return sys_monitor_handle_sync_timer(synctime);
}

static struct sysret handle_frame_identify(struct capability *to,
                                           int cmd, uintptr_t *args)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    assert(to->u.frame.bits < BASE_PAGE_SIZE);
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = to->u.frame.base | to->u.frame.bits,
    };
}

static struct sysret handle_frame_modify_flags(struct capability *to,
                                               int cmd, uintptr_t *args)
{
    // Modify flags of (part of) mapped region of frame
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);

    // unpack arguments
    size_t offset = args[0]; // in pages; of first page to modify from first
                             // page in mapped region
    size_t pages  = args[1]; // #pages to modify
    size_t flags  = args[2]; // new flags

    page_mappings_modify_flags(to, offset, pages, flags);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = 0,
    };
}

#ifdef __scc__
static struct sysret handle_frame_scc_identify(struct capability *to,
                                               int cmd, uintptr_t *args)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    assert(to->u.frame.bits < BASE_PAGE_SIZE);

    uint8_t route, subdest;
    uint16_t addrbits;

    errval_t err = rck_get_route(to->u.frame.base, 1 << to->u.frame.bits,
                                 &route, &subdest, &addrbits);
    if(err_is_fail(err)) {
        return SYSRET(err);
    }

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = (addrbits << 16) | (subdest << 8) | route,
    };
}
#endif

static struct sysret handle_io(struct capability *to, int cmd, uintptr_t *args)
{
    uint32_t    port = args[0];
    uint32_t    data = args[1];

    return sys_io(to, cmd, port, data);
}

static struct sysret monitor_handle_domain_id(struct capability *monitor_cap,
                                              int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    domainid_t domain_id = args[1];

    return sys_monitor_domain_id(cptr, domain_id);
}

/**
 * \brief Set up tracing in the kernel
 */
static struct sysret handle_trace_setup(struct capability *cap,
                                        int cmd, uintptr_t *args)
{
    struct capability *frame;
    errval_t err;

    /* lookup passed cap */
    capaddr_t cptr = args[0];
    err = caps_lookup_cap(&dcb_current->cspace.cap, cptr, CPTR_BITS, &frame,
                          CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    lpaddr_t lpaddr = gen_phys_to_local_phys(frame->u.frame.base);
    kernel_trace_buf = local_phys_to_mem(lpaddr);
    //printf("kernel.%u: handle_trace_setup at %lx\n", apic_id, kernel_trace_buf);

    // Copy boot applications.
	trace_copy_boot_applications();

    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_irq_table_set(struct capability *to, int cmd, uintptr_t *args)
{
    return SYSRET(irq_table_set(args[0], args[1]));
}

static struct sysret handle_irq_table_delete(struct capability *to, int cmd, uintptr_t *args)
{
    return SYSRET(irq_table_delete(args[0]));
}

/**
 * \brief Return system-wide unique ID of this ID cap.
 */
static struct sysret handle_idcap_identify(struct capability *cap, int cmd,
                                           uintptr_t *args)
{
    idcap_id_t *idp = (idcap_id_t *) args[0];

    // Check validity of user space pointer
    if (!access_ok(ACCESS_WRITE, (lvaddr_t) idp, sizeof(*idp)))  {
        return SYSRET(SYS_ERR_INVALID_USER_BUFFER);
    }

    return sys_idcap_identify(cap, idp);
}

#ifdef __scc__
static struct sysret kernel_rck_register(struct capability *cap,
                                         int cmd, uintptr_t *args)
{
    capaddr_t ep = args[0];
    int chanid = args[1];
    return SYSRET(rck_register_notification(ep, chanid));
}

static struct sysret kernel_rck_delete(struct capability *cap,
                                       int cmd, uintptr_t *args)
{
    int chanid = args[0];
    return SYSRET(rck_delete_notification(chanid));
}

static struct sysret handle_rck_notify_send(struct capability *cap,
                                            int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Notify_RCK);
    rck_send_notification(cap->u.notify_rck.coreid, cap->u.notify_rck.chanid);
    return SYSRET(SYS_ERR_OK);
}

#else

static struct sysret kernel_ipi_register(struct capability *cap,
                                         int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Kernel);
    capaddr_t ep = args[0];
    int chanid = args[1];
    return SYSRET(ipi_register_notification(ep, chanid));
}

static struct sysret kernel_ipi_delete(struct capability *cap,
                                       int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Kernel);
    assert(!"NYI");
    return SYSRET(SYS_ERR_OK);
}

static struct sysret handle_ipi_notify_send(struct capability *cap,
                                            int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Notify_IPI);
    return ipi_raise_notify(cap->u.notify_ipi.coreid, cap->u.notify_ipi.chanid);
}
#endif

static struct sysret dispatcher_dump_ptables(struct capability *cap,
                                             int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Dispatcher);

    printf("kernel_dump_ptables\n");

    struct dcb *dispatcher = cap->u.dispatcher.dcb;

    paging_dump_tables(dispatcher);

    return SYSRET(SYS_ERR_OK);
}

typedef struct sysret (*invocation_handler_t)(struct capability *to,
                                              int cmd, uintptr_t *args);

static invocation_handler_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup]        = handle_dispatcher_setup,
        [DispatcherCmd_Properties]   = handle_dispatcher_properties,
        [DispatcherCmd_PerfMon]      = handle_dispatcher_perfmon,
        [DispatcherCmd_DumpPTables]  = dispatcher_dump_ptables,
    },
    [ObjType_Frame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
#ifdef __scc__
        [FrameCmd_SCC_Identify] = handle_frame_scc_identify
#endif
    },
    [ObjType_DevFrame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
#ifdef __scc__
        [FrameCmd_SCC_Identify] = handle_frame_scc_identify
#endif
    },
    [ObjType_CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Create] = handle_create,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
    },
    [ObjType_VNode_x86_32_pdpt] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_32_pdir] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_32_ptable] = {
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_Kernel] = {
#ifndef __scc__
        [KernelCmd_Spawn_core]   = monitor_spawn_core,
#endif
        [KernelCmd_Get_core_id]  = monitor_get_core_id,
        [KernelCmd_Get_arch_id]  = monitor_get_arch_id,
        [KernelCmd_Identify_cap] = monitor_identify_cap,
        [KernelCmd_Identify_domains_cap] = monitor_identify_domains_cap,
        [KernelCmd_Remote_cap]   = monitor_remote_cap,
        [KernelCmd_Iden_cnode_get_cap] = monitor_iden_cnode_get_cap,
        [KernelCmd_Create_cap]   = monitor_create_cap,
        [KernelCmd_Nullify_cap]  = monitor_nullify_cap,
        [KernelCmd_Setup_trace]  = handle_trace_setup,
        [KernelCmd_Register]     = monitor_handle_register,
        [KernelCmd_Domain_Id]    = monitor_handle_domain_id,
        [MonitorCmd_Retype]      = monitor_handle_retype,
        [MonitorCmd_Delete]      = monitor_handle_delete,
        [MonitorCmd_Revoke]      = monitor_handle_revoke,
        [KernelCmd_Sync_timer]   = monitor_handle_sync_timer,
#ifdef __scc__
        [KernelCmd_Spawn_SCC_Core]   = monitor_spawn_scc_core,
        [KernelCmd_IPI_Register] = kernel_rck_register,
        [KernelCmd_IPI_Delete]   = kernel_rck_delete,
#else
        [KernelCmd_IPI_Register] = kernel_ipi_register,
        [KernelCmd_IPI_Delete]   = kernel_ipi_delete,
#endif
    },
    [ObjType_IRQTable] = {
        [IRQTableCmd_Set] = handle_irq_table_set,
        [IRQTableCmd_Delete] = handle_irq_table_delete
    },
    [ObjType_IO] = {
        [IOCmd_Outb] = handle_io,
        [IOCmd_Outw] = handle_io,
        [IOCmd_Outd] = handle_io,
        [IOCmd_Inb] = handle_io,
        [IOCmd_Inw] = handle_io,
        [IOCmd_Ind] = handle_io
    },
    [ObjType_ID] = {
        [IDCmd_Identify] = handle_idcap_identify
    },
#ifdef __scc__
    [ObjType_Notify_RCK] = {
        [NotifyCmd_Send] = handle_rck_notify_send
    }
#else
    [ObjType_Notify_IPI] = {
        [NotifyCmd_Send] = handle_ipi_notify_send
    }
#endif
};

/* syscall C entry point; called only from entry.S so no prototype in header */
struct sysret sys_syscall(uintptr_t arg0, uintptr_t arg1, uintptr_t *args,
                          uintptr_t *cpu_save_frame);
struct sysret sys_syscall(uintptr_t arg0, uintptr_t arg1, uintptr_t *args,
                          uintptr_t *cpu_save_frame)
{
    struct sysret retval = { .error = SYS_ERR_OK, .value = 0 };
    uint8_t syscall = arg0 & 0xff;

    switch(syscall) {
    case SYSCALL_INVOKE: ; /* Handle capability invocation */
        uint8_t flags = (arg0 >> 24) & 0xf;
        uint8_t invoke_bits = (arg0 >> 16) & 0xff;
        capaddr_t invoke_cptr = arg1;

        debug(SUBSYS_SYSCALL, "sys_invoke(0x%"PRIxCADDR"(%d))\n",
              invoke_cptr, invoke_bits);

        // Capability to invoke
        struct capability *to = NULL;
        retval.error = caps_lookup_cap(&dcb_current->cspace.cap, invoke_cptr,
                                       invoke_bits, &to, CAPRIGHTS_READ);
        if (err_is_fail(retval.error)) {
            break;
        }
        assert(to != NULL);
        assert(to->type < ObjType_Num);

        // Endpoint cap, do LMP
        if (to->type == ObjType_EndPoint) {
            struct dcb *listener = to->u.endpoint.listener;
            assert(listener != NULL);

            if (listener->disp == 0) {
                retval.error = SYS_ERR_LMP_NO_TARGET;
                break;
            }

            uint8_t length_words = (arg0 >> 28) & 0xf;
            uint8_t send_bits = (arg0 >> 8) & 0xff;
            capaddr_t send_cptr = args[0];

            /* limit length of message from buggy/malicious sender */
            length_words = min(length_words, LMP_MSG_LENGTH);

            // does the sender want to yield their timeslice on success?
            bool sync = flags & LMP_FLAG_SYNC;
            // does the sender want to yield to the target if undeliverable?
            bool yield = flags & LMP_FLAG_YIELD;

            // try to deliver message
            retval.error = lmp_deliver(to, dcb_current, &args[1], length_words,
                                       send_cptr, send_bits);

            /* Switch to reciever upon successful delivery with sync flag,
             * or (some cases of) unsuccessful delivery with yield flag */
            enum err_code err_code = err_no(retval.error);
            if ((sync && err_is_ok(retval.error)) ||
                (yield && (err_code == SYS_ERR_LMP_BUF_OVERFLOW
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_LOOKUP
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID
                           || err_code == SYS_ERR_LMP_CAPTRANSFER_DST_SLOT_OCCUPIED))
                    ) {
                if (err_is_fail(retval.error)) {
                    struct dispatcher_shared_generic *current_disp =
                        get_dispatcher_shared_generic(dcb_current->disp);
                    struct dispatcher_shared_generic *listener_disp =
                        get_dispatcher_shared_generic(listener->disp);
                    debug(SUBSYS_DISPATCH, "LMP failed; %.*s yields to %.*s: %u\n",
                          DISP_NAME_LEN, current_disp->name,
                          DISP_NAME_LEN, listener_disp->name, err_code);
                }

                // special-case context switch: ensure correct state in current DCB
                dispatcher_handle_t handle = dcb_current->disp;
                struct dispatcher_shared_x86_32 *disp =
                    get_dispatcher_shared_x86_32(handle);
                dcb_current->disabled = dispatcher_is_disabled_ip(handle, cpu_save_frame[0]);
                struct registers_x86_32 *save_area;
                if (dcb_current->disabled) {
                    save_area = &disp->disabled_save_area;
                } else {
                    save_area = &disp->enabled_save_area;
                }

                // save calling dispatcher's registers, so that when the dispatcher
                // next runs, it has a valid state in the relevant save area.
                // Save EIP, EFLAGS, ESP and set EAX (return value) for later resume
                save_area->eax = retval.error; // x86 1st return register
                // save frame contains: eip, cs, eflags, esp, ss
                save_area->eip = cpu_save_frame[0];
                save_area->cs = cpu_save_frame[1];
                save_area->eflags = cpu_save_frame[2];
                save_area->esp = cpu_save_frame[3];
                save_area->ss = cpu_save_frame[4];

                /* save FS/GS selectors (they're unmodified by the syscall path) */
                __asm ("mov     %%fs, %[fs]     \n\t"
                       "mov     %%gs, %[gs]     \n\t"
                       : /* No output */
                       :
                       [fs] "m" (save_area->fs),
                       [gs] "m" (save_area->gs)
                       );

                dispatch(to->u.endpoint.listener);
                panic("dispatch returned");
            }
        } else { // not endpoint cap, call kernel handler through dispatch table
            uint8_t cmd = arg0 >> 8;
            if (cmd >= CAP_MAX_CMD) {
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
                break;
            }

            // Call the invocation
            invocation_handler_t invocation = invocations[to->type][cmd];
            if(invocation == NULL) {
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
                break;
            } else {
                retval = invocation(to, cmd, args);
            }
        }
        break;

        // Yield the CPU to the next dispatcher
    case SYSCALL_YIELD:
        retval = sys_yield((capaddr_t)arg1);
        break;

        // NOP system call for benchmarking purposes
    case SYSCALL_NOP:
        break;

        // Debug print system call
    case SYSCALL_PRINT:
        retval.error = sys_print((char *)arg1, args[0]);
        break;

        // Reboot!
        // FIXME: this should be a kernel cap invocation or similarly restricted
    case SYSCALL_REBOOT:
        reboot();
        break;

    case SYSCALL_X86_FPU_TRAP_ON:
        fpu_trap_on();
        break;

    case SYSCALL_DEBUG:
        switch(arg1) {
        case DEBUG_CONTEXT_COUNTER_RESET:
            dispatch_csc_reset();
            break;

        case DEBUG_CONTEXT_COUNTER_READ:
            retval.value = dispatch_get_csc();
            break;

        case DEBUG_TIMESLICE_COUNTER_READ:
            retval.value = kernel_now;
            break;

        case DEBUG_FLUSH_CACHE:
            wbinvd();
            break;

        case DEBUG_SEND_IPI:
            apic_send_std_ipi(args[0], args[1], args[2]);
            break;

        case DEBUG_SET_BREAKPOINT:
            debugregs_set_breakpoint(args[0], args[1], args[2]);
            break;

        case DEBUG_GET_TSC_PER_MS:
            retval.value = timing_get_tsc_per_ms();
            break;

        case DEBUG_FEIGN_FRAME_CAP:
            {
                uint8_t bits = args[2] & 0xff;
                uint8_t cap_bits = (args[2] >> 8) & 0xff;
                uint8_t recv_slot = (args[2] >> 16) & 0xff;
                struct cte *slot;
                struct capability *recv_cnode_cap;

/*                printf("arg1 = %" PRIx64 ", arg2 = %" PRIx64 "\n",
                        (uint64_t)args[1], (uint64_t)args[2]);
*/
                errval_t err = caps_lookup_cap(&dcb_current->cspace.cap,
                      args[0], cap_bits, &recv_cnode_cap, CAPRIGHTS_READ_WRITE);
                if(err_is_fail(err)) {
                    retval.error = err;
                    break;
                }

                // Check for cnode type
                if (recv_cnode_cap->type != ObjType_CNode) {
                    retval.error = SYS_ERR_LMP_CAPTRANSFER_DST_CNODE_INVALID;
                    break;
                }
                // The slot within the cnode
                slot = caps_locate_slot(recv_cnode_cap->u.cnode.cnode,
                                        recv_slot);

                retval.error = caps_create_new(ObjType_DevFrame, args[1], bits,
                        bits, slot);
            }
            break;

        default:
            printk(LOG_ERR, "invalid sys_debug msg type\n");
        }
        break;

    default:
        printk(LOG_ERR, "sys_syscall: Illegal system call! "
               "(0x%x, 0x%"PRIxPTR")\n", syscall, arg1);
        retval.error = SYS_ERR_ILLEGAL_SYSCALL;
        break;
    }

    // If dcb_current got removed, dispatch someone else
    if (dcb_current == NULL) {
        assert(err_is_ok(retval.error));
        dispatch(schedule());
    }

    if (syscall == SYSCALL_INVOKE) {
        debug(SUBSYS_SYSCALL, "invoke returning 0x%"PRIxERRV" 0x%"PRIxPTR"\n",
              retval.error, retval.value);
    }

    return retval;
}
