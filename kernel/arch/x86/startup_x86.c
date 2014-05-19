/**
 * \file
 * \brief x86 kernel bootup code.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
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
#ifdef CONFIG_MICROBENCHMARKS
#include <microbenchmarks.h>
#endif
#include <irq.h>
#include <init.h>
#include <barrelfish_kpi/cpu.h>
#include <exec.h>
#include <getopt/getopt.h>
#include <dispatch.h>
#include <barrelfish_kpi/init.h>
#include <arch/x86/apic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/syscalls.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <kputchar.h>
#include <startup.h>
#include <arch/x86/startup_x86.h>
#ifdef __scc__
#       include <rck.h>
#endif

/// Optional core ID to use for the BSP core (command-line argument)
static int bsp_coreid;

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

/// Pointer to bootinfo structure for init
static struct bootinfo *bootinfo = (struct bootinfo *)BOOTINFO_BASE;

/**
 * Each kernel has a local copy of global and locks. However, during booting and
 * kernel relocation, these are set to point to global of the pristine kernel,
 * so that all the kernels can share it.
 */
static  struct global myglobal;
struct global *global = &myglobal;

// Physical memory allocator for spawn_app_init
static lpaddr_t app_alloc_phys_start, app_alloc_phys_end;
static lpaddr_t app_alloc_phys(size_t size)
{
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    lpaddr_t addr = app_alloc_phys_start;
    app_alloc_phys_start += npages * BASE_PAGE_SIZE;

    if (app_alloc_phys_start >= app_alloc_phys_end) {
        panic("Out of memory, increase CORE_DATA_PAGES");
    }

    return addr;
}

/**
 * The address from where bsp_alloc_phys will start allocating memory
 */
static lpaddr_t bsp_init_alloc_addr = 0;

/**
 * \brief Linear physical memory allocator.
 *
 * This function allocates a linear region of addresses of size 'size' from
 * physical memory.
 *
 * \param size  Number of bytes to allocate.
 *
 * \return Base physical address of memory region.
 */
static lpaddr_t bsp_alloc_phys(size_t size)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    assert(bsp_init_alloc_addr != 0);
    lpaddr_t addr = bsp_init_alloc_addr;

    bsp_init_alloc_addr += npages * BASE_PAGE_SIZE;
    return addr;
}

/**
 * \brief Map init user-space memory.
 *
 * This function maps pages of the init user-space module. It expects
 * the virtual base address 'vbase' of a program segment of the init executable,
 * its size 'size' and its ELF64 access control flags. It maps pages
 * into physical memory that is allocated on the fly and puts
 * corresponding frame caps into init's segcn.
 *
 * \param vbase Virtual base address of program segment.
 * \param size  Size of program segment in bytes.
 * \param flags ELF64 access control flags of program segment.
 * \param ret   Used to return base region pointer
 */
errval_t startup_alloc_init(void *state, genvaddr_t gvbase, size_t size,
                            uint32_t flags, void **ret)
{

    errval_t err;

    struct spawn_state *spawn_state = state;

    lvaddr_t vbase = (lvaddr_t)gvbase; /* XXX */
    lvaddr_t offset = BASE_PAGE_OFFSET(vbase);

    /* Page align the parameters */
    paging_align(&vbase, NULL, &size, BASE_PAGE_SIZE);

    lpaddr_t pbase = 0, paddr = 0;
    for(lvaddr_t i = vbase; i < vbase + size; i += BASE_PAGE_SIZE) {
        if (apic_is_bsp()) {
            paddr = bsp_alloc_phys(BASE_PAGE_SIZE);
        } else {
            paddr = app_alloc_phys(BASE_PAGE_SIZE);
        }

        if(pbase == 0) {
            pbase = paddr;
        }

        err = startup_map_init(i, paddr, BASE_PAGE_SIZE, flags);
        assert(err_is_ok(err));
    }

    if (apic_is_bsp()) {
        // Create frame caps for segcn
        paddr += BASE_PAGE_SIZE;

        debug(SUBSYS_STARTUP,
              "Allocated physical memory [0x%"PRIxLPADDR", 0x%"PRIxLPADDR"]\n",
              pbase, paddr - pbase);

        err = create_caps_to_cnode(pbase, paddr - pbase,
                                   RegionType_RootTask, spawn_state, bootinfo);
        if (err_is_fail(err)) {
            return err;
        }
    }

    assert(ret != NULL);
    *ret = (void *)(vbase + offset);

    return SYS_ERR_OK;
}

/// Setup the module cnode, which contains frame caps to all multiboot modules
void create_module_caps(struct spawn_state *st)
{
    errval_t err;

    /* Create caps for multiboot modules */
    struct multiboot_modinfo *module =
        (struct multiboot_modinfo *)local_phys_to_mem(glbl_core_data->mods_addr);

    // Allocate strings area
    lpaddr_t mmstrings_phys = bsp_alloc_phys(BASE_PAGE_SIZE);
    lvaddr_t mmstrings_base = local_phys_to_mem(mmstrings_phys);
    lvaddr_t mmstrings = mmstrings_base;

    // create cap for strings area in first slot of modulecn
    assert(st->modulecn_slot == 0);
    err = caps_create_new(ObjType_Frame, mmstrings_phys, BASE_PAGE_BITS,
                          BASE_PAGE_BITS,
                          caps_locate_slot(CNODE(st->modulecn),
                                           st->modulecn_slot++));
    assert(err_is_ok(err));

    /* Walk over multiboot modules, creating frame caps */
    for (int i = 0; i < glbl_core_data->mods_count; i++) {
        struct multiboot_modinfo *m = &module[i];

        // Set memory regions within bootinfo
        struct mem_region *region =
            &bootinfo->regions[bootinfo->regions_length++];

        genpaddr_t remain = MULTIBOOT_MODULE_SIZE(*m);
        genpaddr_t base_addr = local_phys_to_gen_phys(m->mod_start);

        region->mr_type = RegionType_Module;
        region->mr_base = base_addr;
        region->mrmod_slot = st->modulecn_slot;  // first slot containing caps
        region->mrmod_size = remain;  // size of image _in bytes_
        region->mrmod_data = mmstrings - mmstrings_base; // offset of string in area

        // round up to page size for caps
        remain = ROUND_UP(remain, BASE_PAGE_SIZE);

        // Create max-sized caps to multiboot module in module cnode
        while (remain > 0) {
            assert((base_addr & BASE_PAGE_MASK) == 0);
            assert((remain & BASE_PAGE_MASK) == 0);

            // determine size of next chunk
            uint8_t block_size = bitaddralign(remain, base_addr);

            assert(st->modulecn_slot < (1UL << st->modulecn->cap.u.cnode.bits));
            // create as DevFrame cap to avoid zeroing memory contents
            err = caps_create_new(ObjType_DevFrame, base_addr, block_size,
                                  block_size,
                                  caps_locate_slot(CNODE(st->modulecn),
                                                   st->modulecn_slot++));
            assert(err_is_ok(err));

            // Advance by that chunk
            base_addr += ((genpaddr_t)1 << block_size);
            remain -= ((genpaddr_t)1 << block_size);
        }

        // Copy multiboot module string to mmstrings area
        strcpy((char *)mmstrings, MBADDR_ASSTRING(m->string));
        mmstrings += strlen(MBADDR_ASSTRING(m->string)) + 1;
        assert(mmstrings < mmstrings_base + BASE_PAGE_SIZE);
    }
}

// XXX from serial.c
extern int serial_portbase;

static struct cmdarg cmdargs[] = {
    {"loglevel", ArgType_Int, { .integer = &kernel_loglevel }},
    {"logmask", ArgType_Int, { .integer = &kernel_log_subsystem_mask }},
    {"ticks", ArgType_Bool, { .boolean = &kernel_ticks_enabled }},
    {"timeslice", ArgType_Int, { .integer = &kernel_timeslice }},
#ifndef __scc__ // FIXME: why not?
    {"serial", ArgType_Int, { .integer = &serial_portbase }},
#endif
    {"bsp_coreid", ArgType_Int, { .integer = &bsp_coreid }},
    {NULL, 0, {NULL}}
};

/**
 * Name of multiboot module containing program for init domains.
 */
#if defined(__x86_64__)
#       define BSP_INIT_MODULE_PATH     "x86_64/sbin/init"
#elif defined(__scc__)
#       define BSP_INIT_MODULE_PATH     "scc/sbin/init"
#elif defined(__i386__)
#       define BSP_INIT_MODULE_PATH     "x86_32/sbin/init"
#else
#       error "Unknown x86"
#endif
#define BSP_INIT_PROG_NAME       "init"
#define APP_INIT_PROG_NAME       "monitor"

/**
 * \brief Kernel's early startup code, called from arch-specific bootstrap.
 */
void kernel_startup_early(void)
{
    const char *cmdline;
    assert(glbl_core_data != NULL);
    cmdline = MBADDR_ASSTRING(glbl_core_data->cmdline);
    parse_commandline(cmdline, cmdargs);
}

/**
 * \brief Kernel's main startup code, called from arch-specific bootstrap.
 *
 * This function never returns.
 */
void kernel_startup(void)
{
#ifdef CONFIG_MICROBENCHMARKS
    printk(LOG_NOTE, "\nRunning microbenchmarks...\n");
    microbenchmarks_run_all();
#endif

    /* Initialize the core_data */
    /* Used when bringing up other cores, must be at consistent global address
     * seen by all cores */
    struct x86_core_data *core_data
        = (void *)((lvaddr_t)&_start_kernel - BASE_PAGE_SIZE);

    struct dcb *init_dcb;

    if (apic_is_bsp()) {
        if (bsp_coreid != 0) {
            my_core_id = bsp_coreid;
        }

        /* Initialize the location to allocate phys memory from */
        bsp_init_alloc_addr = glbl_core_data->start_free_ram;

        /* spawn init */
        init_dcb = spawn_bsp_init(BSP_INIT_MODULE_PATH, bsp_alloc_phys);
    } else {
        my_core_id = core_data->dst_core_id;

        /* Initialize the allocator */
        app_alloc_phys_start = core_data->memory_base_start;
        app_alloc_phys_end   = ((lpaddr_t)1 << core_data->memory_bits) +
                                    app_alloc_phys_start;

        init_dcb = spawn_app_init(core_data, APP_INIT_PROG_NAME, app_alloc_phys);
    }

    // Should not return
    dispatch(init_dcb);
    panic("Error spawning init!");
}
