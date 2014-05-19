/**
 * \file
 * \brief Architecture-independent bootstrap code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <kernel.h>
#include <startup.h>
#include <exec.h>
#include <dispatch.h>
#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/domain_params.h>
#include <trace/trace.h>

coreid_t my_core_id;

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

/// Size of the physaddrcn (in terms of cspace bits resolved, ie. 2^n slots)
#define PHYSADDRCN_BITS (DEFAULT_CNODE_BITS + 2)

/**
 * \brief Create caps in 'cnode'
 *
 * This function creates untyped caps to the RAM at physical address 'base_addr'
 * and size 'size' and adds them to a cnode for the init task. The bootinfo is
 * updated accordingly.
 *
 * \param base_addr The physical base address of the RAM for which caps have to
 *                  be created
 * \param size      The size of the physical region
 * \param type      Region type to create
 * \param st        spawn_state structure to update
 * \param bootinfo  bootinfo structure to update
 */
errval_t create_caps_to_cnode(lpaddr_t base_addr, size_t size,
                              enum region_type type,
                              struct spawn_state *st, struct bootinfo *bootinfo)
{
    size_t remain = size;
    struct mem_region *regions = bootinfo->regions;
    size_t *regions_index = &bootinfo->regions_length;
    struct capability *cnode;
    cslot_t *slot;
    enum objtype cap_type;
    errval_t err;

    // determine destination and cap type
    switch(type) {
    case RegionType_Empty:
        cap_type = ObjType_RAM;
        cnode = &st->supercn->cap;
        slot = &st->supercn_slot;
        break;

    case RegionType_PhyAddr:
    case RegionType_PlatformData:
        cap_type = ObjType_PhysAddr;
        cnode = &st->physaddrcn->cap;
        slot = &st->physaddrcn_slot;
        break;

    case RegionType_RootTask:
        cap_type = ObjType_Frame;
        cnode = &st->segcn->cap;
        slot = &st->segcn_slot;
        break;

    default:
        panic("Cannot handle bootinfo region type!");
    }

    while (remain > 0) {
        /* Cannot insert anymore into this cnode */
        if (*slot >= 1UL << cnode->u.cnode.bits) {
            printk(LOG_WARN, "create_caps_to_cnode: Cannot create more caps "
                   "in CNode\n");
            return -1;
        }
        /* Cannot insert anymore into the mem_region */
        if (*regions_index >= MAX_MEM_REGIONS) {
            printk(LOG_WARN, "create_caps_to_cnode: mem_region out of space\n");
            return -1;
        }

        uint8_t block_size = bitaddralign(remain, base_addr);

        /* Create the capability */
        err = caps_create_new(cap_type, base_addr, block_size, block_size,
                            caps_locate_slot(cnode->u.cnode.cnode, (*slot)++));
        if (err_is_fail(err)) {
            return err;
        }

        assert(regions != NULL);
        regions[*regions_index].mr_base = base_addr;
        regions[*regions_index].mr_type = type;
        regions[*regions_index].mr_bits = block_size;
        regions[*regions_index].mr_consumed = false;
        regions[*regions_index].mrmod_size = 0;
        regions[*regions_index].mrmod_data = 0;
        (*regions_index)++;

        // Advance physical memory pointer
        base_addr += (1UL << block_size);
        remain -= (1UL << block_size);
    }

    return SYS_ERR_OK;
}


struct dcb *spawn_module(struct spawn_state *st,
                         const char *name, int argc, const char** argv,
                         lpaddr_t bootinfo, lvaddr_t args_base,
                         alloc_phys_func alloc_phys, lvaddr_t *retparamaddr)
{
    errval_t err;

    printf("spawn module: %s\n", name);

    // check for reuse of static state
#ifndef NDEBUG
    static bool once_only;
    assert(!once_only);
    once_only = true;
#endif

    /* Set up root cnode and the caps it contains */
    // must be static, because this CTE will be entered into the MDB!
    static struct cte rootcn;
    err = caps_create_new(ObjType_CNode, alloc_phys(BASE_PAGE_SIZE),
                        BASE_PAGE_BITS, DEFAULT_CNODE_BITS, &rootcn);
    assert(err_is_ok(err));

    // Task cnode in root cnode
    st->taskcn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_TASKCN);
    err = caps_create_new(ObjType_CNode, alloc_phys(BASE_PAGE_SIZE),
                          BASE_PAGE_BITS, DEFAULT_CNODE_BITS, st->taskcn);
    assert(err_is_ok(err));
    st->taskcn->cap.u.cnode.guard_size = GUARD_REMAINDER(2 * DEFAULT_CNODE_BITS);

    // Page cnode in root cnode
    st->pagecn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_PAGECN);
    err = caps_create_new(ObjType_CNode,
                          alloc_phys(1UL << (OBJBITS_CTE + PAGE_CNODE_BITS)),
                          PAGE_CNODE_BITS + OBJBITS_CTE,
                          PAGE_CNODE_BITS, st->pagecn);
    assert(err_is_ok(err));

    // Base page cnode in root cnode
    st->basepagecn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_BASE_PAGE_CN);
    err = caps_create_new(ObjType_CNode, alloc_phys(BASE_PAGE_SIZE),
                          BASE_PAGE_BITS, DEFAULT_CNODE_BITS, st->basepagecn);
    assert(err_is_ok(err));

    // Super cnode in root cnode
    st->supercn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_SUPERCN);
    err = caps_create_new(ObjType_CNode, alloc_phys(BASE_PAGE_SIZE),
                          BASE_PAGE_BITS, DEFAULT_CNODE_BITS, st->supercn);
    assert(err_is_ok(err));

    // slot_alloc cnodes in root cnode
    st->slot_alloc_cn0 = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_SLOT_ALLOC0);
    err = caps_create_new(ObjType_CNode,
                          alloc_phys(1UL << (OBJBITS_CTE + SLOT_ALLOC_CNODE_BITS)),
                          SLOT_ALLOC_CNODE_BITS + OBJBITS_CTE,
                          SLOT_ALLOC_CNODE_BITS,
                          st->slot_alloc_cn0);
    assert(err_is_ok(err));

    st->slot_alloc_cn1 = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_SLOT_ALLOC1);
    err = caps_create_new(ObjType_CNode,
                          alloc_phys(1UL << (OBJBITS_CTE + SLOT_ALLOC_CNODE_BITS)),
                          SLOT_ALLOC_CNODE_BITS + OBJBITS_CTE,
                          SLOT_ALLOC_CNODE_BITS,
                          st->slot_alloc_cn1);
    assert(err_is_ok(err));

    st->slot_alloc_cn2 = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_SLOT_ALLOC2);
    err = caps_create_new(ObjType_CNode,
                          alloc_phys(1UL << (OBJBITS_CTE + SLOT_ALLOC_CNODE_BITS)),
                          SLOT_ALLOC_CNODE_BITS + OBJBITS_CTE,
                          SLOT_ALLOC_CNODE_BITS,
                          st->slot_alloc_cn2);
    assert(err_is_ok(err));

    // Seg cnode in root cnode
    st->segcn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_SEGCN);
    err = caps_create_new(ObjType_CNode, alloc_phys(BASE_PAGE_SIZE),
                        BASE_PAGE_BITS, DEFAULT_CNODE_BITS, st->segcn);
    assert(err_is_ok(err));

    // Physaddr cnode in root cnode
    st->physaddrcn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_PACN);
    err = caps_create_new(ObjType_CNode,
                        alloc_phys(1UL << (OBJBITS_CTE + PHYSADDRCN_BITS)),
                        OBJBITS_CTE + PHYSADDRCN_BITS,
                        PHYSADDRCN_BITS, st->physaddrcn);
    assert(err_is_ok(err));

    if (arch_core_is_bsp()) {
        // Cnode for Boot loaded modules
        st->modulecn = caps_locate_slot(CNODE(&rootcn), ROOTCN_SLOT_MODULECN);
        err = caps_create_new(ObjType_CNode,
                              alloc_phys(1UL << (OBJBITS_CTE + MODULECN_SIZE_BITS)),
                              MODULECN_SIZE_BITS + OBJBITS_CTE, MODULECN_SIZE_BITS,
                              st->modulecn);
        assert(err_is_ok(err));
    }

    /* Managing caps in task cnode */
    // Dcb cap
    struct cte *init_dcb_cte = caps_locate_slot(CNODE(st->taskcn),
                                                TASKCN_SLOT_DISPATCHER);
    err = caps_create_new(ObjType_Dispatcher,
                          alloc_phys(1UL << OBJBITS_DISPATCHER),
                          OBJBITS_DISPATCHER, 0, init_dcb_cte);
    assert(err_is_ok(err));
    struct dcb *init_dcb = init_dcb_cte->cap.u.dispatcher.dcb;

    // Copy root cnode to task cnode
    err = caps_copy_to_cnode(st->taskcn, TASKCN_SLOT_ROOTCN, &rootcn, 0, 0, 0);
    assert(err_is_ok(err));

    // Dispatcher frame in task cnode
    struct cte *init_dispframe_cte = caps_locate_slot(CNODE(st->taskcn),
                                                      TASKCN_SLOT_DISPFRAME);
    err = caps_create_new(ObjType_Frame, alloc_phys(1 << DISPATCHER_FRAME_BITS),
                        DISPATCHER_FRAME_BITS, DISPATCHER_FRAME_BITS,
                        init_dispframe_cte);
    assert(err_is_ok(err));

    // Copy dispatcher frame to the dcb struct
    err = caps_copy_to_cte(&init_dcb->disp_cte, init_dispframe_cte, false, 0, 0);
    assert(err_is_ok(err));

    // Argspage in task cnode
    struct cte *init_args_cte = caps_locate_slot(CNODE(st->taskcn),
                                                 TASKCN_SLOT_ARGSPAGE);
    err = caps_create_new(ObjType_Frame, alloc_phys(ARGS_SIZE),
                          ARGS_FRAME_BITS, ARGS_FRAME_BITS, init_args_cte);
    st->args_page = gen_phys_to_local_phys(init_args_cte->cap.u.frame.base);

    if (arch_core_is_bsp()) {
        assert(bootinfo != 0);

        // Map bootinfo (in task cnode)
        struct cte *bootinfo_cte = caps_locate_slot(CNODE(st->taskcn),
                                                    TASKCN_SLOT_BOOTINFO);
        /* DevFrame to prevent zeroing! */
        err = caps_create_new(ObjType_DevFrame, bootinfo,
                              BOOTINFO_SIZEBITS, BOOTINFO_SIZEBITS, bootinfo_cte);
        assert(err_is_ok(err));
    }

    // Map kernel Cap in task cnode
    struct cte *kernelcap_cte = caps_locate_slot(CNODE(st->taskcn),
                                                 TASKCN_SLOT_KERNELCAP);
    err = caps_create_new(ObjType_Kernel, 0, 0, 0, kernelcap_cte);
    assert(err_is_ok(err));

    // Create capability for performance monitoring
    struct cte *perfmoncap_cte = caps_locate_slot(CNODE(st->taskcn),
                                                   TASKCN_SLOT_PERF_MON);
    err = caps_create_new(ObjType_PerfMon, 0, 0, 0, perfmoncap_cte);
    assert(err_is_ok(err));

    // Map IRQ table in task cnode
    err = caps_create_new(ObjType_IRQTable, 0, 0, 0,
                          caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_IRQ));
    assert(err_is_ok(err));

    /* Initialize dispatcher */
    dispatcher_handle_t init_handle
        = local_phys_to_mem(init_dispframe_cte->cap.u.frame.base);
    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_handle);
    init_disp->disabled = true;
    init_disp->fpu_trap = 1;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* Set fields in DCB */
    // Set cspace
    err = caps_copy_to_cte(&init_dcb->cspace, &rootcn, 0, 0, 0);
    assert(err_is_ok(err));

    // Set disp and add to run queue
    init_dcb->disp = init_handle;
    init_dcb->disabled = true;
    make_runnable(init_dcb);

    // XXX: hack for 1:1 mapping
    if (args_base == 0) {
        args_base = st->args_page;
    }

    /* Construct args page */
    struct spawn_domain_params *params = (void *)local_phys_to_mem(st->args_page);
    memset(params, 0, sizeof(*params));
    char *buf = (char *)local_phys_to_mem(st->args_page
                                          + sizeof(struct spawn_domain_params));
    size_t buflen = ARGS_SIZE - sizeof(struct spawn_domain_params);
    assert(argc < MAX_CMDLINE_ARGS);
    params->argc = argc;
    for (int i = 0; i < argc; i++) {
        size_t arglen = strlen(argv[i]);
        assert(arglen < buflen);
        params->argv[i] = (void *)(args_base + mem_to_local_phys((lvaddr_t)buf)
                                   - st->args_page);
        strcpy(buf, argv[i]);
        buf += arglen + 1;
        buflen -= arglen + 1;
    }

    assert(retparamaddr != NULL);
    *retparamaddr = args_base;

    /* Fill up base page CN (pre-allocated 4K pages) */
    for(size_t i = 0; i < (1UL << (BASE_PAGE_BITS - OBJBITS_CTE)); i++) {
        err = caps_create_new(ObjType_RAM, alloc_phys(BASE_PAGE_SIZE),
                              BASE_PAGE_BITS, BASE_PAGE_BITS,
                              caps_locate_slot(CNODE(st->basepagecn), i));
        assert(err_is_ok(err));
    }

    // Store the application in the boot applications.
	trace_new_boot_application((char*) name, (uintptr_t) init_dcb);

    return init_dcb;
}
