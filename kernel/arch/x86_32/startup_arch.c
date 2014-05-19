/**
 * \file
 * \brief x86_32 kernel bootup code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <paging_kernel_arch.h>
#include <elf/elf.h>
#include <kernel_multiboot.h>
#include <irq.h>
#include <init.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <getopt/getopt.h>
#include <dispatch.h>
#include <barrelfish_kpi/init.h>
#include <arch/x86/apic.h>
#include <kputchar.h>
#include <startup.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/syscalls.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#ifdef __scc__
#       include <rck.h>
#endif
#include <arch/x86/startup_x86.h>

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

/**
 * init's needed boot pages.
 */
#define INIT_PDIR_SIZE          X86_32_PDIR_ENTRIES(X86_32_INIT_SPACE_LIMIT)
#define INIT_PTABLE_SIZE        X86_32_PTABLE_ENTRIES(X86_32_INIT_SPACE_LIMIT)
#define INIT_PAGE_BITMAP        X86_32_PTABLE_PRESENT

/// Pointer to bootinfo structure for init
static struct bootinfo *bootinfo = (struct bootinfo *)BOOTINFO_BASE;

static struct spawn_state spawn_state;

#ifdef CONFIG_PAE
/**
 * Page directory pointer table for init user address space.
 */
static union x86_32_pdpte_entry *init_pdpte; //[INIT_PDPT_SIZE][PTABLE_SIZE]
#endif

/**
 * Page directory for init user address space.
 */
static union x86_32_pdir_entry *init_pdir; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][PTABLE_SIZE]

/**
 * Page tables for init user address space.
 */
static union x86_32_ptable_entry *init_ptable; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][INIT_PTABLE_SIZE][PTABLE_SIZE]

/**
 * \brief Convert elf flags to page flags
 *
 * \param flags ELF64 program segment flags.
 *
 * \return page flags.
 *
 * Not all combinations may be supported by an architecture
 */
static paging_x86_32_flags_t paging_elf_to_page_flags(uint32_t flags)
{
    paging_x86_32_flags_t pageflags = 0;

    pageflags |= flags & PF_R ? PTABLE_USER_SUPERVISOR : 0;
    pageflags |= flags & PF_W ? PTABLE_READ_WRITE : 0;
    pageflags |= flags & PF_X ? 0 : PTABLE_EXECUTE_DISABLE;

    return pageflags;
}

/**
 * \brief Map init user-space memory.
 *
 * This function maps pages of the init user-space module. It expects
 * the virtual base address 'vbase' of a program segment of the init executable,
 * its size 'size' and its ELF64 access control flags. It maps pages
 * to the sequential area of physical memory, given by 'base'. If you
 * want to allocate physical memory frames as you go, you better use
 * startup_alloc_init().
 *
 * \param vbase Virtual base address of program segment.
 * \param base  Physical base address of program segment.
 * \param size  Size of program segment in bytes.
 * \param flags ELF64 access control flags of program segment.
 */
errval_t startup_map_init(lvaddr_t vbase, lpaddr_t base, size_t size,
                          uint32_t flags)
{
    lvaddr_t vaddr;

    paging_align(&vbase, &base, &size, BASE_PAGE_SIZE);
    assert(vbase + size < X86_32_INIT_SPACE_LIMIT);

    // Map pages
    for(vaddr = vbase; vaddr < vbase + size;
        vaddr += BASE_PAGE_SIZE, base += BASE_PAGE_SIZE) {
#ifdef CONFIG_PAE
        union x86_32_ptable_entry *ptable_base = &init_ptable[
                    + X86_32_PDPTE_BASE(vaddr) * X86_32_PTABLE_SIZE * X86_32_PTABLE_SIZE
                    + X86_32_PDIR_BASE(vaddr) * X86_32_PTABLE_SIZE
                    + X86_32_PTABLE_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4K page: vaddr = 0x%x, base = 0x%x, "
              "PDPTE_BASE = %u, PDIR_BASE = %u, "
              "PTABLE_BASE = %u -- ", vaddr, base, X86_32_PDPTE_BASE(vaddr),
              X86_32_PDIR_BASE(vaddr), X86_32_PTABLE_BASE(vaddr));
#else
        union x86_32_ptable_entry *ptable_base = &init_ptable[
                    X86_32_PDIR_BASE(vaddr) * X86_32_PTABLE_SIZE
                    + X86_32_PTABLE_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4K page: vaddr = 0x%"PRIxLVADDR
                             ", base = 0x%"PRIxLPADDR", "
              "PDIR_BASE = %"PRIuLPADDR", "
              "PTABLE_BASE = %"PRIuLPADDR" -- ", vaddr, base,
              X86_32_PDIR_BASE(vaddr), X86_32_PTABLE_BASE(vaddr));
#endif

        if(!X86_32_IS_PRESENT(ptable_base)) {
            debug(SUBSYS_PAGING, "mapped!\n");
            paging_x86_32_map(ptable_base, base,
                       INIT_PAGE_BITMAP | paging_elf_to_page_flags(flags));
        } else {
            debug(SUBSYS_PAGING, "already existing!\n");
        }
    }

    return SYS_ERR_OK;
}

/// Create physical address range or RAM caps to unused physical memory
static void create_phys_caps(lpaddr_t init_alloc_addr)
{
    errval_t err;

#ifndef __scc__
    // map first meg of RAM, which contains lots of crazy BIOS tables
    err = create_caps_to_cnode(0, X86_32_START_KERNEL_PHYS,
                               RegionType_PlatformData, &spawn_state, bootinfo);
    assert(err_is_ok(err));
#endif

    /* Walk multiboot MMAP structure, and create appropriate caps for memory */
    char *mmap_addr = MBADDR_ASSTRING(glbl_core_data->mmap_addr);
    genpaddr_t last_end_addr = 0;

    for(char *m = mmap_addr; m < mmap_addr + glbl_core_data->mmap_length;) {
        struct multiboot_mmap *mmap = (struct multiboot_mmap * SAFE)TC(m);

        debug(SUBSYS_STARTUP, "MMAP %llx--%llx Type %"PRIu32"\n",
              mmap->base_addr, mmap->base_addr + mmap->length,
              mmap->type);

#if 0
        // XXX: Remove intersecting regions
        bool skip = false;
        for(int i = 0; i < bootinfo->regions_length; i++) {
            struct mem_region *r = &bootinfo->regions[i];

            // Remove intersecting regions (earlier additions take precedence)
            if((r->base + (1 << r->bits) >= mmap->base_addr
                && r->base + (1 << r->bits) <= mmap->base_addr + mmap->length)
               || (r->base >= mmap->base_addr
                   && r->base <= mmap->base_addr + mmap->length)) {
                skip = true;
                break;
            }
        }

        if(skip) {
            continue;
        }
#endif

        if (last_end_addr >= init_alloc_addr
            && mmap->base_addr > last_end_addr) {
            /* we have a gap between regions. add this as a physaddr range */
            debug(SUBSYS_STARTUP, "physical address range %llx--%llx\n",
                  last_end_addr, mmap->base_addr);

            err = create_caps_to_cnode(last_end_addr,
                                       mmap->base_addr - last_end_addr,
                                       RegionType_PhyAddr, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        if (mmap->type == MULTIBOOT_MEM_TYPE_RAM) {
            genpaddr_t base_addr = mmap->base_addr;
            genpaddr_t end_addr  = base_addr + mmap->length;

            // only map RAM which is greater than init_alloc_addr
            if (end_addr > local_phys_to_gen_phys(init_alloc_addr)) {
                if (base_addr < local_phys_to_gen_phys(init_alloc_addr)) {
                    base_addr = local_phys_to_gen_phys(init_alloc_addr);
                }

#ifndef CONFIG_PAE
                if(base_addr >= X86_32_PADDR_SPACE_SIZE) {
                    printk(LOG_NOTE, "skipping RAM [%llx--%llx] out of "
                           "mappable space\n", base_addr, end_addr);
                    last_end_addr = mmap->base_addr + mmap->length;
                    m += mmap->size + 4;
                    continue;
                }
                if(end_addr > X86_32_PADDR_SPACE_SIZE) {
                    printk(LOG_NOTE, "shortening RAM [%llx--%llx] to mappable "
                           "space [0--%llx]\n", base_addr, end_addr,
                           X86_32_PADDR_SPACE_LIMIT);
                    end_addr = X86_32_PADDR_SPACE_SIZE;
                }
#endif

#ifndef __scc__
                // XXX: Do not create ram caps for memory the kernel cannot
                // address to prevent kernel objects from being created there
                if(base_addr >= PADDR_SPACE_LIMIT) {
                    last_end_addr = mmap->base_addr + mmap->length;
                    m += mmap->size + 4;
                    continue;
                }
                if (end_addr > PADDR_SPACE_LIMIT) {
                    end_addr = PADDR_SPACE_LIMIT;
                }
#endif

                debug(SUBSYS_STARTUP, "RAM %llx--%llx\n", base_addr, end_addr);

                assert(end_addr >= base_addr);
                err = create_caps_to_cnode(base_addr, end_addr - base_addr,
                                           RegionType_Empty, &spawn_state, bootinfo);
                assert(err_is_ok(err));
            }
        } else if (mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr)) {
            /* XXX: The multiboot spec just says that mapping types other than
             * RAM are "reserved", but GRUB always maps the ACPI tables as type
             * 3, and things like the IOAPIC tend to show up as type 2 or 4,
             * so we map all these regions as platform data
             */
            debug(SUBSYS_STARTUP, "platform %llx--%llx\n", mmap->base_addr,
                  mmap->base_addr + mmap->length);
            assert(mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr));
            err = create_caps_to_cnode(mmap->base_addr, mmap->length,
                                       RegionType_PlatformData, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        last_end_addr = mmap->base_addr + mmap->length;
        m += mmap->size + 4;
    }

    // Assert that we have some physical address space
    assert(last_end_addr != 0);

    if (last_end_addr < X86_32_PADDR_SPACE_SIZE) {
        /*
         * FIXME: adding the full range results in too many caps to add
         * to the cnode (and we can't handle such big caps in user-space
         * yet anyway) so instead we limit it to something much smaller
         */
        genpaddr_t size = X86_32_PADDR_SPACE_SIZE - last_end_addr;
        const genpaddr_t phys_region_limit = 1ULL << 32; // PCI implementation limit
        if (last_end_addr > phys_region_limit) {
            size = 0; // end of RAM is already too high!
        } else if (last_end_addr + size > phys_region_limit) {
            size = phys_region_limit - last_end_addr;
        }
        debug(SUBSYS_STARTUP, "end physical address range %llx--%llx\n",
              last_end_addr, last_end_addr + size);
        err = create_caps_to_cnode(last_end_addr, size,
                                   RegionType_PhyAddr, &spawn_state, bootinfo);
        assert(err_is_ok(err));
    }
}

#define NEEDED_KERNEL_SPACE \
    ((SIZE_KERNEL_IMAGE & 0x1000 ) == SIZE_KERNEL_IMAGE ? \
    SIZE_KERNEL_IMAGE : \
    (SIZE_KERNEL_IMAGE & 0xfffffffffffff000) + 0x1000)

#define OBJSPERPAGE_CTE         (1 << (BASE_PAGE_BITS - OBJBITS_CTE))


static void init_page_tables(struct spawn_state *st, alloc_phys_func alloc_phys)
{
    /* Allocate memory for init's page tables */
#ifdef CONFIG_PAE
    init_pdpte = (void *)local_phys_to_mem(alloc_phys(X86_32_PDPTE_SIZE
                                           * sizeof(union x86_32_pdpte_entry)));
#endif
    init_pdir = (void *)local_phys_to_mem(
                alloc_phys(X86_32_PTABLE_SIZE * INIT_PDIR_SIZE
                           * sizeof(union x86_32_pdir_entry)));
    init_ptable = (void *)local_phys_to_mem(
                alloc_phys(X86_32_PTABLE_SIZE * INIT_PDIR_SIZE
                           * INIT_PTABLE_SIZE * sizeof(union x86_32_ptable_entry)));

    /* Page table setup */
    /* Initialize init page tables */
    for(size_t j = 0; j < INIT_PDIR_SIZE; j++) {
        paging_x86_32_clear_pdir(&init_pdir[j]);
        for(size_t k = 0; k < INIT_PTABLE_SIZE; k++) {
            paging_x86_32_clear_ptable(&init_ptable[j * X86_32_PTABLE_SIZE + k]);
        }
    }
    /* Map pagetables into pageCN */
    int     pagecn_pagemap = 0;
#ifdef CONFIG_PAE
    // Map PDPTE into first slot in pagecn
    caps_create_new(ObjType_VNode_x86_32_pdpt,
                    mem_to_local_phys((lvaddr_t)init_pdpte),
                    BASE_PAGE_BITS, 0,
                    caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
#endif
    // Map PDIR into successive slots in pagecn
    for(size_t i = 0; i < INIT_PDIR_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_32_pdir,
                        mem_to_local_phys((lvaddr_t)init_pdir) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_BITS, 0,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Map page tables into successive slots in pagecn
    for(size_t i = 0; i < INIT_PTABLE_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_32_ptable,
                        mem_to_local_phys((lvaddr_t)init_ptable) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_BITS, 0,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Connect all page tables to page directories.
    // init's memory manager expects page tables within the pagecn to
    // already be connected to the corresponding directories. To avoid
    // unneccessary special cases, we connect them here.
    for(lvaddr_t vaddr = 0; vaddr < X86_32_INIT_SPACE_LIMIT;
        vaddr += BASE_PAGE_SIZE) {
#ifdef CONFIG_PAE
        union x86_32_pdpte_entry *pdpte_base =
            &init_pdpte[X86_32_PDPTE_BASE(vaddr)];
        union x86_32_pdir_entry *pdir_base =
            &init_pdir[X86_32_PDPTE_BASE(vaddr) * X86_32_PTABLE_SIZE +
                       X86_32_PDIR_BASE(vaddr)];
        union x86_32_ptable_entry *ptable_base =
            &init_ptable[X86_32_PDPTE_BASE(vaddr) * X86_32_PTABLE_SIZE *
                         X86_32_PTABLE_SIZE + X86_32_PDIR_BASE(vaddr) *
                         X86_32_PTABLE_SIZE + X86_32_PTABLE_BASE(vaddr)];

        paging_x86_32_map_pdpte(pdpte_base, mem_to_local_phys((lvaddr_t)pdir_base));
#else
        union x86_32_pdir_entry *pdir_base =
            &init_pdir[X86_32_PDIR_BASE(vaddr)];
        union x86_32_ptable_entry *ptable_base =
            &init_ptable[X86_32_PDIR_BASE(vaddr) * X86_32_PTABLE_SIZE +
                         X86_32_PTABLE_BASE(vaddr)];
#endif
        paging_x86_32_map_table(pdir_base,
                                mem_to_local_phys((lvaddr_t)ptable_base));
    }

    /* Switch to init's VSpace */
#ifdef CONFIG_PAE
    paging_x86_32_context_switch(mem_to_local_phys((lvaddr_t)init_pdpte));
#else
    paging_x86_32_context_switch(mem_to_local_phys((lvaddr_t)init_pdir));
#endif

    /***** VSpace available *****/

    /* Map cmdline args R/W into VSpace at ARGS_BASE */
#ifdef CONFIG_PAE
    paging_x86_32_map_pdpte(&init_pdpte[X86_32_PDPTE_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
#endif
    paging_x86_32_map_table(&init_pdir[X86_32_PDIR_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < ARGS_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_32_map(&init_ptable[X86_32_PTABLE_BASE(ARGS_BASE) + i],
                   st->args_page + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R | PF_W));
    }
}

static struct dcb *spawn_init_common(struct spawn_state *st, const char *name,
                                     int argc, const char *argv[],
                                     lpaddr_t bootinfo_phys,
                                     alloc_phys_func alloc_phys)
{
    errval_t err;

    /* Perform arch-independent spawn */
    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(st, name, argc, argv, bootinfo_phys,
                                        ARGS_BASE, alloc_phys, &paramaddr);

    /* Init page tables */
    init_page_tables(st, alloc_phys);

    /* Map dispatcher R/W into VSpace starting at vaddr 0x204000
     * (Starting after Bootinfo pages)*/
#ifdef CONFIG_PAE
    paging_x86_32_map_pdpte(&init_pdpte[X86_32_PDPTE_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
#endif
    paging_x86_32_map_table(&init_pdir[X86_32_PDIR_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < DISPATCHER_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_32_map(&init_ptable[X86_32_PTABLE_BASE(DISPATCHER_BASE) + i],
                   mem_to_local_phys(init_dcb->disp) + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R | PF_W));
    }

    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_x86_32 *init_disp_x86_32 =
        get_dispatcher_shared_x86_32(init_dcb->disp);

    registers_set_param(&init_disp_x86_32->enabled_save_area, paramaddr);

    // Map IO cap in task cnode
    struct cte *iocap = caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_IO);
    err = caps_create_new(ObjType_IO, 0, 0, 0, iocap);
    assert(err_is_ok(err));

    /* Set fields in DCB */
    // Set Vspace
#ifdef CONFIG_PAE
    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_pdpte);
#else
    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_pdir);
#endif

    /* Initialize dispatcher */
    init_disp->disabled = true;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* tell init the vspace addr of its dispatcher */
    init_disp->udisp = DISPATCHER_BASE;

    init_disp_x86_32->disabled_save_area.edi = DISPATCHER_BASE;
    init_disp_x86_32->disabled_save_area.fs = 0;
    init_disp_x86_32->disabled_save_area.gs = 0;
    init_disp_x86_32->disabled_save_area.cs = USER_CS;
    init_disp_x86_32->disabled_save_area.ss = USER_SS;
    init_disp_x86_32->disabled_save_area.eflags = USER_EFLAGS;
    
    return init_dcb;
}

struct dcb *spawn_bsp_init(const char *name, alloc_phys_func alloc_phys)
{
    errval_t err;

    /* Only the first core can run this code */
    assert(apic_is_bsp());
    
    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = alloc_phys(BOOTINFO_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%"PRIuLPADDR, BOOTINFO_BASE);

    const char *argv[6] = { "init", bootinfochar };
    int argc = 2;

#ifdef __scc__
    if(glbl_core_data->urpc_frame_base != 0) {
        char coreidchar[10];
        snprintf(coreidchar, sizeof(coreidchar), "%d",
                 glbl_core_data->src_core_id);
        argv[argc++] = coreidchar;

        char chan_id_char[30];
        snprintf(chan_id_char, sizeof(chan_id_char), "chanid=%"PRIu32,
                 glbl_core_data->chan_id);
        argv[argc++] = chan_id_char;

        char urpc_frame_base_char[30];
        snprintf(urpc_frame_base_char, sizeof(urpc_frame_base_char),
                 "frame=%" PRIuGENPADDR, glbl_core_data->urpc_frame_base);
        argv[argc++] = urpc_frame_base_char;
    }
#endif

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name, argc, argv,
                                             bootinfo_phys, alloc_phys);

    /* Map bootinfo R/W into VSpace at vaddr 0x200000 (BOOTINFO_BASE) */
#ifdef CONFIG_PAE
    paging_x86_32_map_pdpte(&init_pdpte[0], mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_32_map_table(&init_pdir[1], mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < BOOTINFO_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_32_map(&init_ptable[i], bootinfo_phys + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }
#else
    paging_x86_32_map_table(&init_pdir[0], mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < BOOTINFO_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_32_map(&init_ptable[i + 512], bootinfo_phys + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }
#endif

    /* Load init ELF32 binary */
    struct multiboot_modinfo *module = multiboot_find_module(name);
    if (module == NULL) {
        panic("Could not find init module!");
    }
    genvaddr_t init_ep;
    err = elf_load(EM_386, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(module->mod_start),
                   MULTIBOOT_MODULE_SIZE(*module), &init_ep);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_32 *init_disp_x86_32 =
        get_dispatcher_shared_x86_32(init_dcb->disp);
    init_disp_x86_32->disabled_save_area.eip = init_ep;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    lpaddr_t init_alloc_end = alloc_phys(0); // XXX
    create_phys_caps(init_alloc_end);

    /* Fill bootinfo struct */
    bootinfo->mem_spawn_core = NEEDED_KERNEL_SPACE; // Size of kernel

    /* for (int i = 0; i < bootinfo->regions_length; i++) { */
    /*     printf("%d region %d: 0x%09" PRIxPTR " - 0x%09lx (%lu MB, %u bits)\n", */
    /*            bootinfo->regions[i].mr_type, i, bootinfo->regions[i].mr_base, */
    /*            bootinfo->regions[i].mr_base + (1UL<<bootinfo->regions[i].mr_bits), */
    /*            bootinfo->regions[i].mr_bits >= 20 */
    /*            ? 1UL << (bootinfo->regions[i].mr_bits - 20) : 0, */
    /*            bootinfo->regions[i].mr_bits); */
    /* } */

#if 0
    // If app core, map (static) URPC channel
    if(kernel_scckernel != 0) {
        printf("SCC app kernel, frame at: 0x%x\n", kernel_scckernel);
#define TASKCN_SLOT_MON_URPC    (TASKCN_SLOTS_USER+6)   ///< Frame cap for urpc comm.

        err = caps_create_new(ObjType_Frame, kernel_scckernel, 13, 13,
                              caps_locate_slot(CNODE(taskcn), TASKCN_SLOT_MON_URPC));
        assert(err_is_ok(err));
    }
#endif

    return init_dcb;
}

struct dcb *spawn_app_init(struct x86_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys)
{
    errval_t err;

    /* Construct cmdline args */
    // Core id of the core that booted this core
    char coreidchar[10];
    snprintf(coreidchar, sizeof(coreidchar), "%d", core_data->src_core_id);

    // IPI channel id of core that booted this core
    char chanidchar[30];
    snprintf(chanidchar, sizeof(chanidchar), "chanid=%"PRIu32, core_data->chan_id);

    // Arch id of the core that booted this core
    char archidchar[30];
    snprintf(archidchar, sizeof(archidchar), "archid=%d",
             core_data->src_arch_id);

    const char *argv[5] = { name, coreidchar, chanidchar, archidchar };
    int argc = 4;

#ifdef __scc__
    char urpc_frame_base_char[30];
    snprintf(urpc_frame_base_char, sizeof(urpc_frame_base_char),
             "frame=%" PRIuGENPADDR, core_data->urpc_frame_base);
    argv[argc++] = urpc_frame_base_char;
#endif

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name, argc, argv,
                                             0, alloc_phys);

    // Urpc frame cap
    struct cte *urpc_frame_cte = caps_locate_slot(CNODE(spawn_state.taskcn),
                                                  TASKCN_SLOT_MON_URPC);
    // XXX: Create as devframe so the memory is not zeroed out
    err = caps_create_new(ObjType_DevFrame, core_data->urpc_frame_base,
                          core_data->urpc_frame_bits,
                          core_data->urpc_frame_bits, urpc_frame_cte);
    assert(err_is_ok(err));
    urpc_frame_cte->cap.type = ObjType_Frame;
    lpaddr_t urpc_ptr = gen_phys_to_local_phys(urpc_frame_cte->cap.u.frame.base);

    /* Map urpc frame at MON_URPC_BASE */
#ifdef CONFIG_PAE
    paging_x86_32_map_pdpte(&init_pdpte[X86_32_PDPTE_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
#endif
    paging_x86_32_map_table(&init_pdir[X86_32_PDIR_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < MON_URPC_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_32_map(&init_ptable[X86_32_PTABLE_BASE(MON_URPC_BASE) + i],
                   urpc_ptr + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R | PF_W));
    }

    // elf load the domain
    genvaddr_t entry_point;
    err = elf_load(EM_386, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(core_data->monitor_binary),
                   core_data->monitor_binary_size, &entry_point);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_32 *init_disp_x86_32 =
        get_dispatcher_shared_x86_32(init_dcb->disp);
    init_disp_x86_32->disabled_save_area.eip = entry_point;

    return init_dcb;
}
