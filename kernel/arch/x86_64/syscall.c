/**
 * \file
 * \brief System calls implementation.
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
#include <syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <mdb/mdb.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <paging_generic.h>
#include <exec.h>
#include <arch/x86/apic.h>
#include <arch/x86/global.h>
#include <arch/x86/perfmon.h>
#include <vmkit.h>
#include <barrelfish_kpi/sys_debug.h>
#include <barrelfish_kpi/lmp.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <arch/x86/debugregs.h>
#include <trace/trace.h>
#include <arch/x86/syscall.h>
#include <arch/x86/timing.h>
#include <fpu.h>
#include <arch/x86/ipi_notify.h>
#include <amd_vmcb_dev.h>

#define MIN(a,b)        ((a) < (b) ? (a) : (b))

extern uint64_t user_stack_save;

/* FIXME: lots of missing argument checks in this function */
static struct sysret handle_dispatcher_setup(struct capability *to,
                                             int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int depth    = args[1];
    capaddr_t vptr = args[2];
    capaddr_t dptr = args[3];
    bool run = args[4];
    capaddr_t odptr = args[5];

    return sys_dispatcher_setup(to, cptr, depth, vptr, dptr, run, odptr);
}

static struct sysret handle_dispatcher_properties(struct capability *to,
                                                  int cmd, uintptr_t *args)
{
    enum task_type type = args[0];
    unsigned long deadline = args[1];
    unsigned long wcet = args[2];
    unsigned long period = args[3];
    unsigned long release = args[4];
    unsigned short weight = args[5];

    return sys_dispatcher_properties(to, type, deadline, wcet, period,
                                     release, weight);
}

static struct sysret handle_retype_common(struct capability *root,
                                          uintptr_t *args,
                                          bool from_monitor)
{
    uint64_t source_cptr     = args[0];
    uint64_t type            = args[1];
    uint64_t objbits         = args[2];
    uint64_t  dest_cnode_cptr = args[3];
    uint64_t dest_slot       = args[4];
    uint64_t dest_vbits      = args[5];

    return sys_retype(root, source_cptr, type, objbits, dest_cnode_cptr,
                      dest_slot, dest_vbits, from_monitor);
}

static struct sysret handle_retype(struct capability *root,
                                   int cmd, uintptr_t *args)
{
    return handle_retype_common(root, args, false);
}

static struct sysret handle_create(struct capability *root,
                                   int cmd, uintptr_t *args)
{
    /* Retrieve arguments */
    enum objtype type         = args[0];
    uint8_t objbits           = args[1];
    capaddr_t dest_cnode_cptr = args[2];
    cslot_t dest_slot         = args[3];
    uint8_t dest_vbits        = args[4];

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
    uint64_t dest_slot     = args[1];
    capaddr_t  source_cptr   = args[2];
    int      destcn_vbits  = args[3];
    int      source_vbits  = args[4];
    uint64_t param1, param2;
    // params only sent if mint operation
    if (mint) {
        param1 = args[5];
        param2 = args[6];
    } else {
        param1 = param2 = 0;
    }

    return sys_copy_or_mint(root, destcn_cptr, dest_slot, source_cptr,
                            destcn_vbits, source_vbits, param1, param2, mint);
}

static struct sysret handle_map(struct capability *ptable,
                                int cmd, uintptr_t *args)
{
    /* Retrieve arguments */
    uint64_t  slot          = args[0];
    capaddr_t source_cptr   = args[1];
    int       source_vbits  = args[2];
    uint64_t  flags         = args[3];
    uint64_t  offset        = args[4];
    uint64_t  pte_count     = args[5];

    return sys_map(ptable, slot, source_cptr, source_vbits, flags, offset,
                   pte_count);
}

static struct sysret handle_mint(struct capability *root,
                                 int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, true);
}

static struct sysret handle_copy(struct capability *root,
                                 int cmd, uintptr_t *args)
{
    return copy_or_mint(root, args, false);
}

static struct sysret handle_delete_common(struct capability *root,
                                   uintptr_t *args,
                                   bool from_monitor)
{
    capaddr_t cptr = args[0];
    int bits     = args[1];
    return sys_delete(root, cptr, bits, from_monitor);
}

static struct sysret handle_delete(struct capability *root,
                                   int cmd, uintptr_t *args)
{
    return handle_delete_common(root, args, false);
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
    return handle_revoke_common(root, args, false);
}


static struct sysret handle_unmap(struct capability *pgtable,
                                  int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    int bits       = args[1];
    size_t entry   = args[2];
    size_t pages   = args[3];

    errval_t err;
    struct cte *mapping;
    err = caps_lookup_slot(&dcb_current->cspace.cap, cptr, bits,
                                    &mapping, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_CAP_NOT_FOUND));
    }

    err = page_mappings_unmap(pgtable, mapping, entry, pages);
    return SYSRET(err);
}

/// Different handler for cap operations performed by the monitor
static struct sysret monitor_handle_retype(struct capability *kernel_cap,
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

    /* XXX: this hides the first two arguments */
    return handle_retype_common(root, &args[2], true);
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

    /* XXX: this hides the first two arguments */
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

    /* XXX: this hides the first two arguments */
    return handle_revoke_common(root, &args[2], true);
}

static struct sysret monitor_handle_register(struct capability *kernel_cap,
                                             int cmd, uintptr_t *args)
{
    capaddr_t ep_caddr = args[0];
    return sys_monitor_register(ep_caddr);
}

/**
 * \brief Spawn a new core and create a kernel cap for it.
 */
static struct sysret monitor_spawn_core(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    coreid_t core_id       = args[0];
    enum cpu_type cpu_type = args[1];
    genvaddr_t entry       = args[2];

    return sys_monitor_spawn_core(core_id, cpu_type, entry);
}

static struct sysret monitor_get_core_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret){.error = SYS_ERR_OK, .value = my_core_id};
}

static struct sysret monitor_get_arch_id(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    return (struct sysret){.error = SYS_ERR_OK, .value = apic_id};
}

static struct sysret monitor_identify_cap_common(struct capability *kernel_cap,
                                                 struct capability *root,
                                                 uintptr_t *args)
{
    capaddr_t cptr = args[0];
    uint8_t bits = args[1];
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

    /* XXX: this hides the first two arguments */
    return monitor_identify_cap_common(kernel_cap, root, &args[2]);
}

static struct sysret monitor_remote_cap(struct capability *kernel_cap,
                                        int cmd, uintptr_t *args)
{
    struct capability *root = &dcb_current->cspace.cap;
    capaddr_t cptr = args[0];
    int bits = args[1];
    bool remote = args[2];

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
    /* XXX: Get the raw metadata of the capability to create */
    struct capability *src = (struct capability *)args;
    int pos = sizeof(struct capability) / sizeof(uint64_t);

    /* Certain types cannot be created here */
    if ((src->type == ObjType_Null) || (src->type == ObjType_EndPoint)
        || (src->type == ObjType_Dispatcher) || (src->type == ObjType_Kernel)
        || (src->type == ObjType_IRQTable)) {
        return SYSRET(SYS_ERR_ILLEGAL_DEST_TYPE);
    }

    /* Create the cap in the destination */
    capaddr_t cnode_cptr = args[pos];
    int cnode_vbits    = args[pos + 1];
    size_t slot        = args[pos + 2];

    return SYSRET(caps_create_from_existing(&dcb_current->cspace.cap,
                                            cnode_cptr, cnode_vbits,
                                            slot, src));
}

static struct sysret monitor_nullify_cap(struct capability *kernel_cap,
                                         int cmd, uintptr_t *args)
{
    capaddr_t cptr = args[0];
    uint8_t bits = args[1];

    return sys_monitor_nullify_cap(cptr, bits);
}

static struct sysret monitor_iden_cnode_get_cap(struct capability *kern_cap,
                                                int cmd, uintptr_t *args)
{
    errval_t err;

    /* XXX: Get the raw metadata of the cnode */
    int pos = sizeof(struct capability) / sizeof(uint64_t);
    struct capability *cnode = (struct capability *)args;
    assert(cnode->type == ObjType_CNode);

    struct capability *cnode_copy;
    err = mdb_get_copy(cnode, &cnode_copy);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }

    capaddr_t slot = args[pos];
    struct cte* cte = caps_locate_slot(cnode_copy->u.cnode.cnode, slot);

    // XXX: Write cap data directly back to user-space
    // FIXME: this should involve a pointer/range check for reliability,
    // but because the monitor is inherently trusted it's not a security hole
    struct capability *retbuf = (void *)args[pos + 1];
    *retbuf = cte->cap;

    return SYSRET(SYS_ERR_OK);
}

static struct sysret monitor_handle_sync_timer(struct capability *kern_cap,
                                               int cmd, uintptr_t *args)
{
    uint64_t synctime = args[0];
    return sys_monitor_handle_sync_timer(synctime);
}

static struct sysret handle_frame_identify(struct capability *to,
                                           int cmd, uintptr_t *args)
{
    // Return with physical base address of frame
    // XXX: pack size into bottom bits of base address
    assert(to->type == ObjType_Frame || to->type == ObjType_DevFrame);
    assert((to->u.frame.base & BASE_PAGE_MASK) == 0);
    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = to->u.frame.base | to->u.frame.bits,
    };
}

static struct sysret handle_vnode_identify(struct capability *to,
					   int cmd, uintptr_t *args)
{
    // Return with physical base address of the VNode
    // XXX: pack type into bottom bits of base address
    assert(to->type == ObjType_VNode_x86_64_pml4 ||
	   to->type == ObjType_VNode_x86_64_pdpt ||
	   to->type == ObjType_VNode_x86_64_pdir ||
	   to->type == ObjType_VNode_x86_64_ptable);
    
    uint64_t base_addr = 0;
    switch (to->type) {
    case ObjType_VNode_x86_64_pml4:
        base_addr = (uint64_t)(to->u.vnode_x86_64_pml4.base);
	break;
    case ObjType_VNode_x86_64_pdpt:
	base_addr = (uint64_t)(to->u.vnode_x86_64_pdpt.base);
	break;
    case ObjType_VNode_x86_64_pdir:
	base_addr = (uint64_t)(to->u.vnode_x86_64_pdir.base);
	break;
    case ObjType_VNode_x86_64_ptable:
	base_addr = (uint64_t)(to->u.vnode_x86_64_ptable.base);
	break;
    default:
        break;
    }
    assert((base_addr & BASE_PAGE_MASK) == 0);

    return (struct sysret) {
        .error = SYS_ERR_OK,
        .value = (genpaddr_t)base_addr | ((uint8_t)to->type),
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


static struct sysret handle_io(struct capability *to, int cmd, uintptr_t *args)
{
    uint64_t    port = args[0];
    uint64_t    data = args[1]; // ignored for input

    return sys_io(to, cmd, port, data);
}

static struct sysret
handle_dispatcher_setup_guest (struct capability *to, int cmd, uintptr_t *args)
{
    errval_t err;
    struct dcb *dcb = to->u.dispatcher.dcb;

    capaddr_t epp = args[0];
    capaddr_t vnodep = args[1];
    capaddr_t vmcbp = args[2];
    capaddr_t ctrlp = args[3];

    // 0. Enable VM extensions
    err = vmkit_enable_virtualization();
    if (err != SYS_ERR_OK) {
        return SYSRET(err);
    }

    // 1. Check arguments
    // Monitor endpoint for exits of this geust
    struct cte *ep_cte;

    err = caps_lookup_slot(&dcb_current->cspace.cap, epp, CPTR_BITS,
                           &ep_cte, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    if (ep_cte->cap.type != ObjType_EndPoint) {
        return SYSRET(SYS_ERR_VMKIT_ENDPOINT_INVALID);
    }
    err = caps_copy_to_cte(&dcb->guest_desc.monitor_ep, ep_cte, false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_VMKIT_ENDPOINT));
    }

    // Domain vspace
    struct capability *vnode_cap;
    err = caps_lookup_cap(&dcb_current->cspace.cap, vnodep, CPTR_BITS,
                          &vnode_cap, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    if (vnode_cap->type != ObjType_VNode_x86_64_pml4) {
        return SYSRET(SYS_ERR_DISP_VSPACE_INVALID);
    }

    assert(vnode_cap->type == ObjType_VNode_x86_64_pml4);

    // VMCB
    struct cte *vmcb_cte;
    err = caps_lookup_slot(&dcb_current->cspace.cap, vmcbp, CPTR_BITS,
                           &vmcb_cte, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    if (vmcb_cte->cap.type != ObjType_Frame ||
        vmcb_cte->cap.u.frame.bits < BASE_PAGE_BITS) {
        return SYSRET(SYS_ERR_VMKIT_VMCB_INVALID);
    }
    err = caps_copy_to_cte(&dcb->guest_desc.vmcb, vmcb_cte, false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_VMKIT_VMCB));
    }

    // guest control
    struct cte *ctrl_cte;
    err = caps_lookup_slot(&dcb_current->cspace.cap, ctrlp, CPTR_BITS,
                           &ctrl_cte, CAPRIGHTS_READ_WRITE);
    if (err_is_fail(err)) {
        return SYSRET(err);
    }
    if (ctrl_cte->cap.type != ObjType_Frame ||
        ctrl_cte->cap.u.frame.bits < BASE_PAGE_BITS) {
        return SYSRET(SYS_ERR_VMKIT_CTRL_INVALID);
    }
    err = caps_copy_to_cte(&dcb->guest_desc.ctrl, ctrl_cte, false, 0, 0);
    if (err_is_fail(err)) {
        return SYSRET(err_push(err, SYS_ERR_VMKIT_CTRL));
    }

    // 2. Set up the target DCB
/*     dcb->guest_desc.monitor_ep = ep_cap; */
    dcb->vspace = vnode_cap->u.vnode_x86_64_pml4.base;
    dcb->is_vm_guest = true;
/*     dcb->guest_desc.vmcb = vmcb_cap->u.frame.base; */
/*     dcb->guest_desc.ctrl = (void *)x86_64_phys_to_mem(ctrl_cap->u.frame.base); */

    return SYSRET(SYS_ERR_OK);
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

static struct sysret handle_irq_table_set(struct capability *to, int cmd,
                                          uintptr_t *args)
{
    return SYSRET(irq_table_set(args[0], args[1]));
}

static struct sysret handle_irq_table_delete(struct capability *to, int cmd,
                                             uintptr_t *args)
{
    return SYSRET(irq_table_delete(args[0]));
}

static struct sysret handle_ipi_notify_send(struct capability *cap,
                                            int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Notify_IPI);
    return ipi_raise_notify(cap->u.notify_ipi.coreid, cap->u.notify_ipi.chanid);
}

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

static struct sysret dispatcher_dump_ptables(struct capability *cap,
                                             int cmd, uintptr_t *args)
{
    assert(cap->type == ObjType_Dispatcher);

    printf("kernel_dump_ptables\n");

    struct dcb *dispatcher = cap->u.dispatcher.dcb;

    paging_dump_tables(dispatcher);

    return SYSRET(SYS_ERR_OK);
}

/* 
 * \brief Activate performance monitoring
 * 
 * Activates performance monitoring.
 * \param xargs Expected parameters in args:
 * - performance monitoring type
 * - mask for given type
 * - Counter id
 * - Also count in privileged mode
 * - Number of counts before overflow. This parameter may be used to
 *   set tradeoff between accuracy and overhead. Set the counter to 0
 *   to deactivate the usage of APIC.
 * - Endpoint capability to be invoked when the counter overflows.
 *   The buffer associated with the endpoint needs to be large enough
 *   to hold several overflow notifications depending on the overflow 
 *   frequency.
 */
static struct sysret performance_counter_activate(struct capability *cap,
                                                  int cmd, uintptr_t *args)
{
    uint8_t event = args[0];
    uint8_t umask = args[1];
    uint8_t counter_id = args[2];
    bool kernel = args[3];
    uint64_t counter_value = args[4];
    capaddr_t ep_addr = args[5];

    errval_t err;
    struct capability *ep;
    extern struct capability perfmon_callback_ep;

    // Make sure that 
    assert(ep_addr!=0 || counter_value==0);

    perfmon_init();
    perfmon_measure_start(event, umask, counter_id, kernel, counter_value);

    if(ep_addr!=0) {
        
        err = caps_lookup_cap(&dcb_current->cspace.cap, ep_addr, CPTR_BITS, &ep,
                               CAPRIGHTS_READ);
        if(err_is_fail(err)) {
            return SYSRET(err);
        }
        
        perfmon_callback_ep = *ep; 
    }

    return SYSRET(SYS_ERR_OK);
}

/* 
 * \brief Write counter values.
 */
static struct sysret performance_counter_write(struct capability *cap,
                                               int cmd, uintptr_t *args)
{
    uint8_t counter_id = args[0];
    uint64_t counter_value = args[1];

    perfmon_measure_write(counter_id, counter_value);
    return SYSRET(SYS_ERR_OK);
}

/* 
 * \brief Deactivate performance counters again.
 */
static struct sysret performance_counter_deactivate(struct capability *cap,
                                                  int cmd, uintptr_t *args)
{
    perfmon_measure_stop();
    return SYSRET(SYS_ERR_OK);
}

/*
 * \brief Return system-wide unique ID of this ID cap.
 */
static struct sysret handle_idcap_identify(struct capability *cap, int cmd,
                                           uintptr_t *args)
{
    idcap_id_t id;
    struct sysret sysret = sys_idcap_identify(cap, &id);
    sysret.value = id;

    return sysret;
}

typedef struct sysret (*invocation_handler_t)(struct capability *to,
                                              int cmd, uintptr_t *args);

static invocation_handler_t invocations[ObjType_Num][CAP_MAX_CMD] = {
    [ObjType_Dispatcher] = {
        [DispatcherCmd_Setup] = handle_dispatcher_setup,
        [DispatcherCmd_Properties] = handle_dispatcher_properties,
        [DispatcherCmd_SetupGuest] = handle_dispatcher_setup_guest,
        [DispatcherCmd_DumpPTables]  = dispatcher_dump_ptables,
    },
    [ObjType_Frame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
    },
    [ObjType_DevFrame] = {
        [FrameCmd_Identify] = handle_frame_identify,
        [FrameCmd_ModifyFlags] = handle_frame_modify_flags,
    },
    [ObjType_CNode] = {
        [CNodeCmd_Copy]   = handle_copy,
        [CNodeCmd_Mint]   = handle_mint,
        [CNodeCmd_Retype] = handle_retype,
        [CNodeCmd_Create] = handle_create,
        [CNodeCmd_Delete] = handle_delete,
        [CNodeCmd_Revoke] = handle_revoke,
    },
    [ObjType_VNode_x86_64_pml4] = {
        [VNodeCmd_Identify] = handle_vnode_identify,
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_64_pdpt] = {
        [VNodeCmd_Identify] = handle_vnode_identify,
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_64_pdir] = {
        [VNodeCmd_Identify] = handle_vnode_identify,
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_VNode_x86_64_ptable] = {
        [VNodeCmd_Identify] = handle_vnode_identify,
        [VNodeCmd_Map]   = handle_map,
        [VNodeCmd_Unmap] = handle_unmap,
    },
    [ObjType_Kernel] = {
        [KernelCmd_Spawn_core]   = monitor_spawn_core,
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
        [KernelCmd_IPI_Register] = kernel_ipi_register,
        [KernelCmd_IPI_Delete]   = kernel_ipi_delete,
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
    [ObjType_Notify_IPI] = {
        [NotifyCmd_Send] = handle_ipi_notify_send
    },
    [ObjType_PerfMon] = {
        [PerfmonCmd_Activate] = performance_counter_activate,
        [PerfmonCmd_Deactivate] = performance_counter_deactivate,
        [PerfmonCmd_Write] = performance_counter_write,
    },
    [ObjType_ID] = {
        [IDCmd_Identify] = handle_idcap_identify,
    }
};

/* syscall C entry point; called only from entry.S so no prototype in header */
struct sysret sys_syscall(uint64_t syscall, uint64_t arg0, uint64_t arg1,
                          uint64_t *args, uint64_t rflags, uint64_t rip);
struct sysret sys_syscall(uint64_t syscall, uint64_t arg0, uint64_t arg1,
                          uint64_t *args, uint64_t rflags, uint64_t rip)
{
    struct sysret retval = { .error = SYS_ERR_OK, .value = 0 };

    switch(syscall) {
    case SYSCALL_INVOKE: /* Handle capability invocation */
    {
        // unpack "header" word
        capaddr_t invoke_cptr = arg0 >> 32;
        uint8_t send_bits = arg0 >> 24;
        uint8_t invoke_bits = arg0 >> 16;
        uint8_t length_words = arg0 >> 8;
        uint8_t flags = arg0;

        debug(SUBSYS_SYSCALL, "sys_invoke(0x%x(%d), 0x%lx)\n",
              invoke_cptr, invoke_bits, arg1);

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

            /* limit length of message from buggy/malicious sender */
            length_words = MIN(length_words, LMP_MSG_LENGTH);

            // does the sender want to yield their timeslice on success?
            bool sync = flags & LMP_FLAG_SYNC;
            // does the sender want to yield to the target if undeliverable?
            bool yield = flags & LMP_FLAG_YIELD;

            // try to deliver message
            retval.error = lmp_deliver(to, dcb_current, args, length_words,
                                       arg1, send_bits);

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
                struct dispatcher_shared_x86_64 *disp =
                    get_dispatcher_shared_x86_64(handle);
                dcb_current->disabled = dispatcher_is_disabled_ip(handle, rip);
                struct registers_x86_64 *save_area;
                if (dcb_current->disabled) {
                    save_area = &disp->disabled_save_area;
                } else {
                    save_area = &disp->enabled_save_area;
                }

		// Should be enabled. Else, how do we do an invocation??
		if(dcb_current->disabled) {
		  panic("Dispatcher needs to be enabled for this invocation");
		}

                // save calling dispatcher's registers, so that when the dispatcher
                // next runs, it has a valid state in the relevant save area.
                // Save RIP, RFLAGS, RSP and set RAX (return value) for later resume
                save_area->rax = retval.error; // XXX: x86 1st return register
                save_area->rip = rip;
                save_area->eflags = rflags;
                save_area->rsp = user_stack_save;

		if(!dcb_current->is_vm_guest) {
		  /* save and zero FS/GS selectors (they're unmodified by the syscall path) */
		  __asm ("mov     %%fs, %[fs]     \n\t"
			 "mov     %%gs, %[gs]     \n\t"
			 "mov     %[zero], %%fs   \n\t"
			 "mov     %[zero], %%gs   \n\t"
			 : /* No output */
			 :
			 [fs] "m" (save_area->fs),
			 [gs] "m" (save_area->gs),
			 [zero] "r" (0)
			 );
		} else {
		  lpaddr_t lpaddr = gen_phys_to_local_phys(dcb_current->guest_desc.vmcb.cap.u.frame.base);
		  amd_vmcb_t vmcb;
		  amd_vmcb_initialize(&vmcb, (void *)local_phys_to_mem(lpaddr));
		  save_area->fs = amd_vmcb_fs_selector_rd(&vmcb);
		  save_area->gs = amd_vmcb_gs_selector_rd(&vmcb);
		}

                dispatch(to->u.endpoint.listener);
                panic("dispatch returned");
            }
        } else { // not endpoint cap, call kernel handler through dispatch table
            uint64_t cmd = args[0];
            if (cmd >= CAP_MAX_CMD) {
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
                break;
            }

            // Call the invocation
            invocation_handler_t invocation = invocations[to->type][cmd];
            if(invocation == NULL) {
                retval.error = SYS_ERR_ILLEGAL_INVOCATION;
            } else {
                retval = invocation(to, cmd, &args[1]);
            }
        }
        break;
    }

        // Yield the CPU to the next dispatcher
    case SYSCALL_YIELD:
        retval = sys_yield((capaddr_t)arg0);
        break;

        // NOP system call for benchmarking purposes
    case SYSCALL_NOP:
        break;

        // Debug print system call
    case SYSCALL_PRINT:
        retval.error = sys_print((char *)arg0, arg1);
        break;

        // Reboot!
        // FIXME: this should be a kernel cap invocation or similarly restricted
    case SYSCALL_REBOOT:
        reboot();
        break;

    case SYSCALL_X86_FPU_TRAP_ON:
        fpu_trap_on();
        break;

    case SYSCALL_X86_RELOAD_LDT:
        maybe_reload_ldt(dcb_current, true);
        break;

    case SYSCALL_DEBUG:
        switch(arg0) {
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
            apic_send_std_ipi(arg1, args[0], args[1]);
            break;

        case DEBUG_SET_BREAKPOINT:
            debugregs_set_breakpoint(arg1, args[0], args[1]);
            break;

        case DEBUG_GET_TSC_PER_MS:
            retval.value = timing_get_tsc_per_ms();
            break;

        case DEBUG_GET_APIC_TIMER:
            retval.value = apic_timer_get_count();
            break;

        case DEBUG_GET_APIC_TICKS_PER_SEC:
            retval.value = timing_get_apic_ticks_per_sec();
            break;

        case DEBUG_GET_APIC_ID:
            retval.value = apic_get_id();
            break;

        default:
            printk(LOG_ERR, "invalid sys_debug msg type\n");
        }
        break;

    default:
        printk(LOG_ERR, "sys_syscall: Illegal system call! "
               "(0x%lx, 0x%lx, 0x%lx)\n", syscall, arg0, arg1);
        retval.error = SYS_ERR_ILLEGAL_SYSCALL;
        break;
    }

    // If dcb_current got removed, dispatch someone else
    if (dcb_current == NULL) {
        assert(err_is_ok(retval.error));
        dispatch(schedule());
    }

    if (syscall == SYSCALL_INVOKE) {
        debug(SUBSYS_SYSCALL, "invoke returning 0x%lx 0x%lx\n",
              retval.error, retval.value);
    }

    return retval;
}
