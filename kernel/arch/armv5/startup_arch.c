/*
 * Copyright (c) 2009, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <string.h>
#include <stdio.h>

#include <barrelfish_kpi/init.h>
#include <barrelfish_kpi/syscalls.h>
#include <elf/elf.h>

#include <arm_hal.h>
#include <paging_kernel_arch.h>
#include <exceptions.h>
#include <cp15.h>
#include <cpiobin.h>
#include <init.h>
#include <phys_mmap.h>
#include <barrelfish_kpi/paging_arm_v5.h>
#include <startup.h>

#include <romfs_size.h>

#define CNODE(cte)              (cte)->cap.u.cnode.cnode
#define UNUSED(x)               (x) = (x)

#define STARTUP_PROGRESS()      debug(SUBSYS_STARTUP, "%s:%d\n",          \
                                      __FUNCTION__, __LINE__);

#define BSP_INIT_MODULE_NAME    "armv5/sbin/init"

#define INIT_L1_BYTES           (ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY)

#define INIT_L2_PAGES           ((INIT_SPACE_LIMIT - INIT_VBASE) / BASE_PAGE_SIZE)
#define INIT_L2_BYTES           INIT_L2_PAGES * ARM_L2_BYTES_PER_ENTRY

#define INIT_BOOTINFO_VBASE     INIT_VBASE
#define INIT_ARGS_VBASE         (INIT_BOOTINFO_VBASE + BOOTINFO_SIZE)
#define INIT_DISPATCHER_VBASE   (INIT_ARGS_VBASE + ARGS_SIZE)

#define INIT_PERM_RO            (ARM_L2_SMALL_CACHEABLE  | \
                                 ARM_L2_SMALL_BUFFERABLE | \
                                 ARM_L2_SMALL_USR_RO)

#define INIT_PERM_RW            (ARM_L2_SMALL_CACHEABLE  | \
                                 ARM_L2_SMALL_BUFFERABLE | \
                                 ARM_L2_SMALL_USR_RW)

static phys_mmap_t* g_phys_mmap;        // Physical memory map
static uintptr_t* init_l1;              // L1 page table for init
static uintptr_t* init_l2;              // L2 page tables for init

static struct spawn_state spawn_state;

static inline uintptr_t round_up(uintptr_t value, size_t unit)
{
    assert(0 == (unit & (unit - 1)));
    size_t m = unit - 1;
    return (value + m) & ~m;
}

static inline uintptr_t round_down(uintptr_t value, size_t unit)
{
    assert(0 == (unit & (unit - 1)));
    size_t m = unit - 1;
    return value & ~m;
}

static lpaddr_t alloc_phys_aligned(size_t bytes, size_t align)
{
    bytes = round_up(bytes, align);
    lpaddr_t a = phys_mmap_alloc(g_phys_mmap, bytes, align);
    assert(0 == (a & (align - 1)));
    return a;
}

static lpaddr_t alloc_phys(size_t bytes)
{
    return alloc_phys_aligned(bytes, BASE_PAGE_SIZE);
}

static lvaddr_t alloc_mem_aligned(size_t bytes, size_t align)
{
    return local_phys_to_mem(alloc_phys_aligned(bytes, align));
}

static lvaddr_t alloc_mem(size_t bytes)
{
    return local_phys_to_mem(alloc_phys_aligned(bytes, BASE_PAGE_SIZE));
}

/**
 * Map frames into init process address space. Init has a contiguous set of
 * l2 entries so this is straightforward.
 *
 * @param l2_table      pointer to init's L2 table.
 * @param l2_base       virtual address represented by first L2 table entry
 * @param va_base       virtual address to map.
 * @param pa_base       physical address to associate with virtual address.
 * @param bytes        number of bytes to map.
 * @param l2_flags      ARM L2 small page flags for mapped pages.
 */
static void
spawn_init_map(uintptr_t* l2_table,
               lvaddr_t   l2_base,
               lvaddr_t   va_base,
               lpaddr_t   pa_base,
               size_t     bytes,
               uintptr_t  l2_flags)
{
    assert(va_base >= l2_base);
    assert(0 == (va_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (pa_base & (BASE_PAGE_SIZE - 1)));
    assert(0 == (bytes & (BASE_PAGE_SIZE - 1)));

    int bi = (va_base - l2_base) / BASE_PAGE_SIZE;
    int li = bi + bytes / BASE_PAGE_SIZE;

    while (bi < li)
    {
        paging_set_l2_entry(&l2_table[bi], pa_base, l2_flags);
        pa_base += BASE_PAGE_SIZE;
        bi++;
    }
}

static uint32_t elf_to_l2_flags(uint32_t eflags)
{
    switch (eflags & (PF_W|PF_R))
    {
      case PF_W|PF_R:
        return (ARM_L2_SMALL_USR_RW |
                ARM_L2_SMALL_CACHEABLE |
                ARM_L2_SMALL_BUFFERABLE);
      case PF_R:
        return (ARM_L2_SMALL_USR_RO |
                ARM_L2_SMALL_CACHEABLE |
                ARM_L2_SMALL_BUFFERABLE);
      default:
        panic("Unknown ELF flags combination.");
    }
}

struct startup_l2_info
{
    uintptr_t* l2_table;
    lvaddr_t   l2_base;
};

static errval_t
startup_alloc_init(
    void*      state,
    genvaddr_t gvbase,
    size_t     bytes,
    uint32_t   flags,
    void**     ret
    )
{
    const struct startup_l2_info* s2i = (const struct startup_l2_info*)state;

    lvaddr_t sv = round_down((lvaddr_t)gvbase, BASE_PAGE_SIZE);
    size_t   off = (lvaddr_t)gvbase - sv;
    lvaddr_t lv = round_up((lvaddr_t)gvbase + bytes, BASE_PAGE_SIZE);
    lpaddr_t pa;

    STARTUP_PROGRESS();
    if (lv > sv && ((pa = alloc_phys(lv - sv)) != 0))
    {
        spawn_init_map(s2i->l2_table, s2i->l2_base, sv,
                       pa, lv - sv, elf_to_l2_flags(flags));
        *ret = (void*)(local_phys_to_mem(pa) + off);
    }
    else
    {
        *ret = 0;
    }
    return SYS_ERR_OK;
}

static void
load_init_image(
    struct startup_l2_info* l2i,
    const uint8_t*          initrd_base,
    size_t                  initrd_bytes,
    genvaddr_t*             init_ep,
    genvaddr_t*             got_base
    )
{
    const uint8_t* elf_base;
    size_t elf_bytes;
    int found;

    STARTUP_PROGRESS();
    *init_ep = *got_base = 0;

    found = cpio_get_file_by_name(initrd_base,
                                  initrd_bytes,
                                  BSP_INIT_MODULE_NAME,
                                  &elf_base, &elf_bytes);
    if (!found)
    {
        panic("Failed to find " BSP_INIT_MODULE_NAME "\n");
    }

    debug(SUBSYS_STARTUP, "load_init_image %p %08x\n", initrd_base, initrd_bytes);

    errval_t err = elf_load(EM_ARM, startup_alloc_init, l2i,
                            (lvaddr_t)elf_base, elf_bytes, init_ep);
    if (err_is_fail(err))
    {
        panic("ELF load of " BSP_INIT_MODULE_NAME " failed!\n");
    }

    // TODO: Fix application linkage so that it's non-PIC.
    struct Elf32_Shdr* got_shdr =
        elf32_find_section_header_name((lvaddr_t)elf_base, elf_bytes, ".got");
    if (got_shdr)
    {
        *got_base = got_shdr->sh_addr;
    }
}

static void
create_modules_from_initrd(struct bootinfo* bi,
                           const uint8_t*   initrd_base,
                           size_t           initrd_bytes)
{
    errval_t err;
    lvaddr_t mmstrings_base = 0;
    lvaddr_t mmstrings      = 0;

    // CPIO archive is crafted such that first file is
    // command-line strings for "modules" - ie menu.lst. The
    // subsequent file follow in the order they appear in
    // menu.lst.arm.
    const uint8_t* data;
    size_t bytes;

    if (cpio_get_file_by_name(initrd_base, initrd_bytes,
                              "armv5/menu.lst.modules",
                              &data, &bytes))
    {
        assert(bytes < BASE_PAGE_SIZE);

        mmstrings_base = alloc_mem(BASE_PAGE_SIZE);
        mmstrings      = mmstrings_base;

        STARTUP_PROGRESS();

        // Create cap for strings area in first slot of modulecn
        err = caps_create_new(
                  ObjType_Frame,
                  mem_to_local_phys(mmstrings_base),
                  BASE_PAGE_BITS, BASE_PAGE_BITS,
                  caps_locate_slot(
                      CNODE(spawn_state.modulecn),
                      spawn_state.modulecn_slot++)
                  );
        assert(err_is_ok(err));

        STARTUP_PROGRESS();

        // Copy strings from file into allocated page
        memcpy((void*)mmstrings_base, data, bytes);
        ((char*)mmstrings_base)[bytes] = '\0';

        STARTUP_PROGRESS();

        // Skip first line (corresponds to bootscript in archive)
        strtok((char*)mmstrings_base, "\r\n");

        STARTUP_PROGRESS();

        assert(bi->regions_length == 0);
        int ord = 1;
        const char* name;
        while ((mmstrings = (lvaddr_t)strtok(NULL, "\r\n")) != 0)
        {
            if (!cpio_get_file_by_ordinal(initrd_base, initrd_bytes, ord,
                                          &name, &data, &bytes))
            {
                panic("Failed to find file\n");
            }
            ord++;

            debug(SUBSYS_STARTUP,
                  "Creating caps for \"%s\" (Command-line \"%s\")\n",
                   name, (char*)mmstrings);

            // Copy file from archive into RAM.
            // TODO: Give up archive space.
            size_t   pa_bytes = round_up(bytes, BASE_PAGE_SIZE);
            lpaddr_t pa       = alloc_phys(pa_bytes);
            memcpy((void*)local_phys_to_mem(pa), data, bytes);

            struct mem_region* region = &bi->regions[bi->regions_length++];
            region->mr_type    = RegionType_Module;
            region->mrmod_slot = spawn_state.modulecn_slot;
            region->mrmod_size = pa_bytes;
            region->mrmod_data = mmstrings - mmstrings_base;

            assert((pa & BASE_PAGE_MASK) == 0);
            assert((pa_bytes & BASE_PAGE_MASK) == 0);

            while (pa_bytes != 0)
            {
                assert(spawn_state.modulecn_slot
                       < (1UL << spawn_state.modulecn->cap.u.cnode.bits));
                // create as DevFrame cap to avoid zeroing memory contents
                err = caps_create_new(
                          ObjType_DevFrame, pa, BASE_PAGE_BITS,
                          BASE_PAGE_BITS,
                          caps_locate_slot(
                              CNODE(spawn_state.modulecn),
                              spawn_state.modulecn_slot++)
                          );
                assert(err_is_ok(err));
                pa       += BASE_PAGE_SIZE;
                pa_bytes -= BASE_PAGE_SIZE;
            }
        }
    }
    else
    {
        panic("No command-line file.\n");
    }
}

/// Create physical address range or RAM caps to unused physical memory
static void
create_phys_caps(struct capability *physaddrcn_cap, struct bootinfo* bi)
{
    STARTUP_PROGRESS();
    int i;
    for (i = 0; i < g_phys_mmap->region_count; i++)
    {
        // TODO: Add RegionType_PhyAddr entries for memory mapped I/O
        // regions.
        const phys_region_t* r = &g_phys_mmap->regions[i];
        if (r->limit - r->start >= BASE_PAGE_SIZE) {
            create_caps_to_cnode(r->start, r->limit - r->start,
                                 RegionType_Empty, &spawn_state, bi);
        }
    }
    g_phys_mmap->region_count = 0;
    STARTUP_PROGRESS();
}

static void __attribute__ ((noreturn))
spawn_init(const char*      name,
           int32_t          kernel_id,
           const uint8_t*   initrd_base,
           size_t           initrd_bytes)
{
    assert(0 == kernel_id);

    // Create page table for init

    init_l1 =  (uintptr_t*)alloc_mem_aligned(INIT_L1_BYTES, ARM_L1_ALIGN);
    memset(init_l1, 0, INIT_L1_BYTES);

    init_l2 = (uintptr_t*)alloc_mem_aligned(INIT_L2_BYTES, ARM_L2_ALIGN);
    memset(init_l2, 0, INIT_L2_BYTES);

    STARTUP_PROGRESS();

    /* Allocate bootinfo */
    lpaddr_t bootinfo_phys = alloc_phys(BOOTINFO_SIZE);
    memset((void *)local_phys_to_mem(bootinfo_phys), 0, BOOTINFO_SIZE);

    STARTUP_PROGRESS();

    /* Construct cmdline args */
    char bootinfochar[16];
    snprintf(bootinfochar, sizeof(bootinfochar), "%u", INIT_BOOTINFO_VBASE);
    const char *argv[] = { "init", bootinfochar };

    lvaddr_t paramaddr;
    struct dcb *init_dcb = spawn_module(&spawn_state, name,
                                        ARRAY_LENGTH(argv), argv,
                                        bootinfo_phys, INIT_ARGS_VBASE,
                                        alloc_phys, &paramaddr);

    STARTUP_PROGRESS();

    /*
     * Create a capability that allows user-level applications to
     * access device memory. This capability will be passed to Kaluga,
     * split up into smaller pieces and distributed to among device
     * drivers.
     *
     * For armv5, this is currently a dummy capability. We do not
     * have support for user-level device drivers in gem5 yet, so we
     * do not allocate any memory as device memory. Some cap_copy
     * operations in the bootup code fail if this capability is not
     * present.
     */
    struct cte *iocap = caps_locate_slot(CNODE(spawn_state.taskcn), TASKCN_SLOT_IO);
    errval_t  err = caps_create_new(ObjType_IO, 0, 0, 0, iocap);
    assert(err_is_ok(err));

    struct dispatcher_shared_generic *disp
        = get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_arm *disp_arm
        = get_dispatcher_shared_arm(init_dcb->disp);
    assert(NULL != disp);

    STARTUP_PROGRESS();

    /* Initialize dispatcher */
    disp->udisp = INIT_DISPATCHER_VBASE;

    STARTUP_PROGRESS();
    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_l1);

    STARTUP_PROGRESS();

    /* Page table setup */

    /* Map pagetables into page CN */
    int pagecn_pagemap = 0;

    /*
     * ARM has:
     *
     * L1 has 4096 entries (16KB).
     * L2 Coarse has 256 entries (256 * 4B = 1KB).
     *
     * CPU driver currently fakes having 1024 entries in L1 and
     * L2 with 1024 entries by treating a page as 4 consecutive
     * L2 tables and mapping this as a unit in L1.
     */
    caps_create_new(
        ObjType_VNode_ARM_l1,
        mem_to_local_phys((lvaddr_t)init_l1),
            vnode_objbits(ObjType_VNode_ARM_l1), 0,
            caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
        );

    STARTUP_PROGRESS();

    // Map L2 into successive slots in pagecn
    size_t i;
    for (i = 0; i < INIT_L2_BYTES / BASE_PAGE_SIZE; i++) {
        size_t objbits_vnode = vnode_objbits(ObjType_VNode_ARM_l2);
        assert(objbits_vnode == BASE_PAGE_BITS);
        caps_create_new(
            ObjType_VNode_ARM_l2,
            mem_to_local_phys((lvaddr_t)init_l2) + (i << objbits_vnode),
            objbits_vnode, 0,
            caps_locate_slot(CNODE(spawn_state.pagecn), pagecn_pagemap++)
            );
    }

    /*
     * Initialize init page tables - this just wires the L1
     * entries through to the corresponding L2 entries.
     */
    STATIC_ASSERT(0 == (INIT_VBASE % ARM_L1_SECTION_BYTES), "");
    for (lvaddr_t vaddr = INIT_VBASE; vaddr < INIT_SPACE_LIMIT; vaddr += ARM_L1_SECTION_BYTES)
    {
        uintptr_t section = (vaddr - INIT_VBASE) / ARM_L1_SECTION_BYTES;
        uintptr_t l2_off = section * ARM_L2_TABLE_BYTES;
        lpaddr_t paddr = mem_to_local_phys((lvaddr_t)init_l2) + l2_off;
        paging_map_user_pages_l1((lvaddr_t)init_l1, vaddr, paddr);
    }

    paging_make_good((lvaddr_t)init_l1, INIT_L1_BYTES);

    STARTUP_PROGRESS();

    printf("XXX: Debug print to make Bram's code work\n");

    paging_context_switch(mem_to_local_phys((lvaddr_t)init_l1));

    STARTUP_PROGRESS();

    // Map cmdline arguments in VSpace at ARGS_BASE
    STATIC_ASSERT(0 == (ARGS_SIZE % BASE_PAGE_SIZE), "");

    STARTUP_PROGRESS();

    spawn_init_map(init_l2, INIT_VBASE, INIT_ARGS_VBASE,
                   spawn_state.args_page, ARGS_SIZE, INIT_PERM_RW);

    STARTUP_PROGRESS();

    // Map bootinfo
    spawn_init_map(init_l2, INIT_VBASE, INIT_BOOTINFO_VBASE,
                   bootinfo_phys, BOOTINFO_SIZE, INIT_PERM_RW);

    struct startup_l2_info l2_info = { init_l2, INIT_VBASE };

    genvaddr_t init_ep, got_base;
    load_init_image(&l2_info, initrd_base, initrd_bytes, &init_ep, &got_base);

    // Set startup arguments (argc, argv)
    disp_arm->enabled_save_area.named.r0   = paramaddr;
    disp_arm->enabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    disp_arm->enabled_save_area.named.rtls = INIT_DISPATCHER_VBASE;
    disp_arm->enabled_save_area.named.r10  = got_base;

    disp_arm->got_base = got_base;

    struct bootinfo* bootinfo = (struct bootinfo*)INIT_BOOTINFO_VBASE;
    bootinfo->regions_length = 0;

    STARTUP_PROGRESS();

    create_modules_from_initrd(bootinfo, initrd_base, initrd_bytes);

    STARTUP_PROGRESS();
    create_phys_caps(&spawn_state.physaddrcn->cap, bootinfo);

    STARTUP_PROGRESS();

    bootinfo->mem_spawn_core  = ~0;     // Size of kernel if bringing up others

    // Map dispatcher
    spawn_init_map(init_l2, INIT_VBASE, INIT_DISPATCHER_VBASE,
                   mem_to_local_phys(init_dcb->disp), DISPATCHER_SIZE,
                   INIT_PERM_RW);

    STARTUP_PROGRESS();

    // NB libbarrelfish initialization sets up the stack.
    disp_arm->disabled_save_area.named.pc   = init_ep;
    disp_arm->disabled_save_area.named.cpsr = ARM_MODE_USR | CPSR_F_MASK;
    disp_arm->disabled_save_area.named.rtls = INIT_DISPATCHER_VBASE;
    disp_arm->disabled_save_area.named.r10  = got_base;

#ifdef __XSCALE__
    cp15_disable_cache();
#endif

    printf("Kernel ready.\n");

    pit_start();

    // On to userland...
    STARTUP_PROGRESS();
    dispatch(init_dcb);
    
    panic("Not reached.");
}

void arm_kernel_startup(phys_mmap_t* mmap,
                        lpaddr_t     initrd_base,
                        size_t       initrd_bytes)
{
    g_phys_mmap = mmap;

    STARTUP_PROGRESS();

#ifdef __XSCALE__
    //Hardcoded because bootloader alter image if we pass the correct location
    //Size of image is obtained by header file which is generated during compilation
    initrd_base = 0x20000000;
    initrd_bytes = romfs_cpio_archive_size;
#endif

    const uint8_t* initrd_cpio_base = (uint8_t*)local_phys_to_mem(initrd_base);

    if (!cpio_archive_valid(initrd_cpio_base, initrd_bytes))
    {
                panic("Invalid initrd filesystem\n");
    }

    spawn_init(BSP_INIT_MODULE_NAME, 0, initrd_cpio_base, initrd_bytes);
}
