/**
 * \file
 * \brief x86_64 kernel bootup code.
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

/// Quick way to find the base address of a cnode capability
#define CNODE(cte)     (cte)->cap.u.cnode.cnode

/**
 * init's needed boot pages.
 */
#define INIT_PDPT_SIZE          X86_64_PDPT_ENTRIES(X86_64_INIT_SPACE_LIMIT)
#define INIT_PDIR_SIZE          X86_64_PDIR_ENTRIES(X86_64_INIT_SPACE_LIMIT)
#define INIT_PTABLE_SIZE        X86_64_PTABLE_ENTRIES(X86_64_INIT_SPACE_LIMIT)
#define INIT_PAGE_BITMAP        X86_64_PTABLE_PRESENT

/// Pointer to bootinfo structure for init
static struct bootinfo *bootinfo = (struct bootinfo *)BOOTINFO_BASE;

static struct spawn_state spawn_state;

/**
 * Page map level 4 table for init user address space.
 */
static union x86_64_pdir_entry *init_pml4; //[PTABLE_SIZE]

/**
 * Page directory pointer table for init user address space.
 */
static union x86_64_pdir_entry *init_pdpt; //[INIT_PDPT_SIZE][PTABLE_SIZE]

/**
 * Page directory for init user address space.
 */
static union x86_64_pdir_entry *init_pdir; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][PTABLE_SIZE]

/**
 * Page tables for init user address space.
 */
static union x86_64_ptable_entry *init_ptable; //[INIT_PDPT_SIZE][INIT_PDIR_SIZE][INIT_PTABLE_SIZE][PTABLE_SIZE]

/**
 * \brief Convert elf flags to page flags
 *
 * \param flags ELF64 program segment flags.
 *
 * \return page flags.
 *
 * Not all combinations may be supported by an architecture
 */
static uint64_t paging_elf_to_page_flags(uint32_t flags)
{
    uint64_t pageflags = 0;

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

    assert(vbase + size - X86_64_INIT_VBASE < X86_64_INIT_SPACE_LIMIT);

    // Map pages
    for(vaddr = vbase; vaddr < vbase + size;
        vaddr += BASE_PAGE_SIZE, base += BASE_PAGE_SIZE) {
        lvaddr_t baddr = vaddr - X86_64_INIT_VBASE;
        union x86_64_ptable_entry *ptable_base = &init_ptable[
                    X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                    X86_64_PTABLE_SIZE * X86_64_PTABLE_SIZE +
                    X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE *
                    X86_64_PTABLE_SIZE + X86_64_PDIR_BASE(baddr) *
                    X86_64_PTABLE_SIZE + X86_64_PTABLE_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4K page: vaddr = 0x%lx, base = 0x%lx, "
              "PML4_BASE = %lu, PDPT_BASE = %lu, PDIR_BASE = %lu, "
              "PTABLE_BASE = %lu -- ", vaddr, base, X86_64_PML4_BASE(baddr),
              X86_64_PDPT_BASE(baddr), X86_64_PDIR_BASE(baddr),
              X86_64_PTABLE_BASE(baddr));

        if(!X86_64_IS_PRESENT(ptable_base)) {
            debug(SUBSYS_PAGING, "mapped!\n");
            paging_x86_64_map(ptable_base, base,
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

    // map first meg of RAM, which contains lots of crazy BIOS tables
    err = create_caps_to_cnode(0, X86_64_START_KERNEL_PHYS,
                               RegionType_PlatformData, &spawn_state, bootinfo);
    assert(err_is_ok(err));

    char *mmap_addr = MBADDR_ASSTRING(glbl_core_data->mmap_addr);
    lpaddr_t last_end_addr = 0;

#define PRINT_REGIONS(map, map_length) do {\
        for(char * printcur = map; printcur < map + map_length;) {\
            struct multiboot_mmap * printcurmmap = (struct multiboot_mmap * SAFE)TC(printcur);\
            printf("\t0x%08lx - 0x%08lx Type: %d Length: 0x%lx\n", printcurmmap->base_addr, printcurmmap->base_addr + printcurmmap->length, printcurmmap->type, printcurmmap->length);\
            printcur += printcurmmap->size + 4;\
        }\
    } while (0)

    printf("Raw MMAP from BIOS\n");
    PRINT_REGIONS(mmap_addr, glbl_core_data->mmap_length);

    // normalize memory regions
    char *clean_mmap_addr = MBADDR_ASSTRING(init_alloc_addr); // FIXME: Hack!!!! TODO: properly get memory form somewhere
    assert(glbl_core_data->mmap_length < BOOTINFO_SIZE);
    uint32_t clean_mmap_length = glbl_core_data->mmap_length;

    memcpy(clean_mmap_addr, mmap_addr, glbl_core_data->mmap_length);

    // first of all, sort regions by base address
    // yes, it's a bubble sort, but the dataset is small and usually in the right order
    bool swapped;
    do {
        swapped = false;

        for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
            struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
            if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
                break; // do not try to move this check into the forloop as entries do not have to be the same length

            struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

            if (nextmmap->base_addr < curmmap->base_addr ||
                (nextmmap->base_addr == curmmap->base_addr && nextmmap->length > curmmap->length)) {
                // swap
                assert(curmmap->size == 20); // FIXME: The multiboot specification does not require this size
                assert(nextmmap->size == 20);

                struct multiboot_mmap tmp;
                tmp = *curmmap;
                *curmmap = *nextmmap;
                *nextmmap = tmp;

                swapped = true;
            }

            cur += curmmap->size + 4;
        }
    } while(swapped);

    printf("Sorted MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

    // now merge consecutive memory regions of the same or lower type
    for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
        struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
        if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
            break; // do not try to move this check into the forloop as entries do not have to be the same length

        struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

        /* On some machines (brie1) the IOAPIC region is only 1kB.
         * Currently we're not able to map regions that are <4kB so we
         * make sure that every region (if there is no problematic overlap)
         * is at least BASE_PAGE_SIZEd (==4kB) here.
         */
        if ((curmmap->length < BASE_PAGE_SIZE) && (curmmap->base_addr + BASE_PAGE_SIZE <= nextmmap->base_addr)) {
            curmmap->length = BASE_PAGE_SIZE;
        }

#define DISCARD_NEXT_MMAP do {\
    uint32_t discardsize = nextmmap->size + 4;\
    memmove(cur + curmmap->size + 4, cur + curmmap->size + 4 + discardsize, clean_mmap_length - (cur - clean_mmap_addr) - curmmap->size - 4 - discardsize);\
    clean_mmap_length -= discardsize;\
    } while (0)

#define BUBBLE_NEXT_MMAP do {\
    for (char * bubblecur = cur + curmmap->size + 4; bubblecur < clean_mmap_addr + clean_mmap_length;){\
        struct multiboot_mmap * bubblecur_mmap = (struct multiboot_mmap * SAFE)TC(bubblecur);\
        if (bubblecur + bubblecur_mmap->size + 4 >= clean_mmap_addr + clean_mmap_length)\
            break;\
        struct multiboot_mmap * bubblenext_mmap = (struct multiboot_mmap * SAFE)TC(bubblecur + bubblecur_mmap->size + 4);\
        if (bubblenext_mmap->base_addr < bubblecur_mmap->base_addr || (bubblecur_mmap->base_addr == bubblenext_mmap->base_addr && bubblenext_mmap->length > bubblecur_mmap->length)) {\
            struct multiboot_mmap bubbletmp; bubbletmp = *bubblecur_mmap; *bubblecur_mmap = *bubblenext_mmap; *bubblenext_mmap = bubbletmp;\
        } else break;\
    }} while(0)


        bool reduced = false;
        do {
            reduced = false;

            if (curmmap->base_addr == nextmmap->base_addr) {
                // regions start at the same location
                if (curmmap->length == nextmmap->length) {
                    // trivial case. They are the same. Choose higher type and discard next
                    curmmap->type = max(curmmap->type, nextmmap->type);

                    DISCARD_NEXT_MMAP;

                    reduced = true;
                    continue;
                } else {
                    // next region is smaller (we sorted that way)
                    if (nextmmap->type <= curmmap->type) {
                        // next regions type is the same or smaller. discard
                        DISCARD_NEXT_MMAP;

                        reduced = true;
                        continue;
                    } else {
                        // next regions type is higher, so it gets priority
                        // change type of current region and shrink next
                        uint32_t tmptype = curmmap->type;
                        uint64_t newlength = curmmap->length - nextmmap->length;
                        curmmap->type = nextmmap->type;
                        curmmap->length = nextmmap->length;
                        nextmmap->type = tmptype;
                        nextmmap->base_addr += nextmmap->length;
                        nextmmap->length = newlength;

                        // now we need to bubble next to the right place to restore order
                        BUBBLE_NEXT_MMAP;

                        reduced = true;
                        continue;
                    }
                }
            }

            // regions overlap
            if (nextmmap->base_addr > curmmap->base_addr && nextmmap->base_addr < curmmap->base_addr + curmmap->length) {
                // same type
                if (curmmap->type == nextmmap->type) {
                    // simple. just extend if necessary and discard next
                    if (nextmmap->base_addr + nextmmap->length > curmmap->base_addr + curmmap->length)
                        curmmap->length = (nextmmap->base_addr + nextmmap->length) - curmmap->base_addr;

                    DISCARD_NEXT_MMAP;

                    reduced = true;
                    continue;
                } else {
                    // type is not the same
                    if (nextmmap->base_addr + nextmmap->length < curmmap->base_addr + curmmap->length) {
                        // there is a chunk at the end. create a new region
                        struct multiboot_mmap tmpmmap;
                        tmpmmap.size = 20;
                        tmpmmap.base_addr = nextmmap->base_addr + nextmmap->length;
                        tmpmmap.length = (curmmap->base_addr + curmmap->length) - (nextmmap->base_addr + nextmmap->length);
                        tmpmmap.type = curmmap->type;

                        // move everything to make room
                        assert(clean_mmap_length + tmpmmap.length + 4 < BOOTINFO_SIZE);
                        memmove(cur + curmmap->size + 4 + tmpmmap.size + 4, cur + curmmap->size + 4, clean_mmap_length - ((cur - clean_mmap_addr) + curmmap->size + 4));
                        clean_mmap_length += tmpmmap.size + 4;

                        // insert new
                        *nextmmap = tmpmmap;

                        // restore order
                        BUBBLE_NEXT_MMAP;

                        reduced = true;
                    }

                    // after the previous step, the next region either ends
                    // at the same location as the current or is longer
                    uint64_t overlap = (curmmap->base_addr + curmmap->length) - nextmmap->base_addr;

                    if (curmmap-> type > nextmmap->type) {
                        // current has priority, shrink next and extend current
                        nextmmap->length -= overlap;
                        nextmmap->base_addr += overlap;
                        curmmap->length += overlap;

                        if (nextmmap->length == 0)
                            DISCARD_NEXT_MMAP;

                        reduced = true;
                        continue;
                    } else {
                        // next has priority, shrink current and extend next
                        nextmmap->length += overlap;
                        nextmmap->base_addr -= overlap;
                        curmmap->length -= overlap;

                        reduced = true;
                        continue;
                    }
                }
            }
        } while (reduced);

        cur += curmmap->size + 4;

#undef DISCARD_NEXT_MMAP
#undef BUBBLE_NEXT_MMAP
    }

    printf("Preprocessed MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

    // we can only map pages. Therefore page align regions
    for(char * cur = clean_mmap_addr; cur < clean_mmap_addr + clean_mmap_length;) {
        struct multiboot_mmap * curmmap = (struct multiboot_mmap * SAFE)TC(cur);
        if (cur + curmmap->size + 4 >= clean_mmap_addr + clean_mmap_length)
            break; // do not try to move this check into the forloop as entries do not have to be the same length

        struct multiboot_mmap * nextmmap = (struct multiboot_mmap * SAFE)TC(cur + curmmap->size + 4);

        if (nextmmap->base_addr & BASE_PAGE_MASK) {
            uint64_t offset = nextmmap->base_addr - ((nextmmap->base_addr >> BASE_PAGE_BITS) << BASE_PAGE_BITS);

            // round in favour of higher type
            if (curmmap->type > nextmmap->type) {
                curmmap->length += BASE_PAGE_SIZE - offset;
                nextmmap->base_addr += BASE_PAGE_SIZE - offset;
                nextmmap->length -= BASE_PAGE_SIZE - offset;
            } else {
                curmmap->length -= offset;
                nextmmap->base_addr -= offset;
                nextmmap->length += offset;
            }
        }

        cur += curmmap->size + 4;
    }

    printf("Pagealigned MMAP\n");
    PRINT_REGIONS(clean_mmap_addr, clean_mmap_length);

#undef PRINT_REGIONS

    for(char *m = clean_mmap_addr; m < clean_mmap_addr + clean_mmap_length;) {
        struct multiboot_mmap *mmap = (struct multiboot_mmap * SAFE)TC(m);

        debug(SUBSYS_STARTUP, "MMAP %lx--%lx Type %u\n",
              mmap->base_addr, mmap->base_addr + mmap->length,
              mmap->type);

        if (last_end_addr >= init_alloc_addr
            && mmap->base_addr > last_end_addr) {
            /* we have a gap between regions. add this as a physaddr range */
            debug(SUBSYS_STARTUP, "physical address range %lx--%lx\n",
                  last_end_addr, mmap->base_addr);

            err = create_caps_to_cnode(last_end_addr,
                                       mmap->base_addr - last_end_addr,
                                       RegionType_PhyAddr, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        if (mmap->type == MULTIBOOT_MEM_TYPE_RAM) {
            genpaddr_t base_addr = mmap->base_addr;
            genpaddr_t end_addr = base_addr + mmap->length;

            // only map the rest of RAM which is greater than init_alloc_addr
            if (end_addr > local_phys_to_gen_phys(init_alloc_addr)) {
                if (base_addr < local_phys_to_gen_phys(init_alloc_addr)) {
                    base_addr = local_phys_to_gen_phys(init_alloc_addr);
                }
                debug(SUBSYS_STARTUP, "RAM %lx--%lx\n", base_addr, end_addr);
                err = create_caps_to_cnode(base_addr, end_addr - base_addr,
                                           RegionType_Empty, &spawn_state, bootinfo);
                if(err_is_fail(err)) {
		    printk(LOG_WARN, "Skipping RAM %lx--%lx...\n", base_addr, end_addr);
                }
                /* assert(err_is_ok(err)); */
            }
        } else if (mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr)) {
            /* XXX: The multiboot spec just says that mapping types other than
             * RAM are "reserved", but GRUB always maps the ACPI tables as type
             * 3, and things like the IOAPIC tend to show up as type 2 or 4,
             * so we map all these regions as platform data
             */
            debug(SUBSYS_STARTUP, "platform %lx--%lx\n", mmap->base_addr,
                  mmap->base_addr + mmap->length);
            assert(mmap->base_addr > local_phys_to_gen_phys(init_alloc_addr));
            err = create_caps_to_cnode(mmap->base_addr, mmap->length,
                                       RegionType_PlatformData, &spawn_state, bootinfo);
            assert(err_is_ok(err));
        }

        last_end_addr = mmap->base_addr + mmap->length;
        m += mmap->size + 4;
    }

    assert(last_end_addr != 0);

    if (last_end_addr < X86_64_PADDR_SPACE_SIZE) {
        /*
         * FIXME: adding the full range results in too many caps to add
         * to the cnode (and we can't handle such big caps in user-space
         * yet anyway) so instead we limit it to something much smaller
         */
        size_t size = X86_64_PADDR_SPACE_SIZE - last_end_addr;
        const lpaddr_t phys_region_limit = 1UL << 32; // PCI implementation limit
        if (last_end_addr > phys_region_limit) {
            size = 0; // end of RAM is already too high!
        } else if (last_end_addr + size > phys_region_limit) {
            size = phys_region_limit - last_end_addr;
        }
        debug(SUBSYS_STARTUP, "end physical address range %lx--%lx\n",
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

#define OBJSPERPAGE_CTE         (1UL << (BASE_PAGE_BITS - OBJBITS_CTE))

static void init_page_tables(struct spawn_state *st, alloc_phys_func alloc_phys)
{
    /* Allocate memory for init's page tables */
    init_pml4 = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * sizeof(union x86_64_pdir_entry)));
    init_pdpt = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE
                           * sizeof(union x86_64_pdir_entry)));
    init_pdir = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE * INIT_PDIR_SIZE
                           * sizeof(union x86_64_pdir_entry)));
    init_ptable = (void *)local_phys_to_mem(
                alloc_phys(X86_64_PTABLE_SIZE * INIT_PDPT_SIZE * INIT_PDIR_SIZE
                           * INIT_PTABLE_SIZE * sizeof(union x86_64_ptable_entry)));

    /* Page table setup */
    /* Initialize init page tables */
    for(size_t i = 0; i < INIT_PDPT_SIZE; i++) {
        paging_x86_64_clear_pdir(&init_pdpt[i]);
        for(size_t j = 0; j < INIT_PDIR_SIZE; j++) {
            paging_x86_64_clear_pdir(&init_pdir[i * PTABLE_SIZE + j]);
            for(size_t k = 0; k < INIT_PTABLE_SIZE; k++) {
                paging_x86_64_clear_ptable(
                &init_ptable[i * PTABLE_SIZE * PTABLE_SIZE + j * PTABLE_SIZE + k]);
            }
        }
    }
    /* Map pagetables into pageCN */
    int     pagecn_pagemap = 0;
    // Map PML4 (slot 0 in pagecn)
    caps_create_new(ObjType_VNode_x86_64_pml4, mem_to_local_phys((lvaddr_t)init_pml4),
                    BASE_PAGE_BITS, 0,
                    caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    // Map PDPT into successive slots in pagecn
    for(size_t i = 0; i < INIT_PDPT_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_pdpt,
                        mem_to_local_phys((lvaddr_t)init_pdpt) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_BITS, 0,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Map PDIR into successive slots in pagecn
    for(size_t i = 0; i < INIT_PDIR_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_pdir,
                        mem_to_local_phys((lvaddr_t)init_pdir) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_BITS, 0,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Map page tables into successive slots in pagecn
    for(size_t i = 0; i < INIT_PTABLE_SIZE; i++) {
        caps_create_new(ObjType_VNode_x86_64_ptable,
                        mem_to_local_phys((lvaddr_t)init_ptable) + i * BASE_PAGE_SIZE,
                        BASE_PAGE_BITS, 0,
                        caps_locate_slot(CNODE(st->pagecn), pagecn_pagemap++));
    }
    // Connect all page tables to page directories.
    // init's memory manager expects page tables within the pagecn to
    // already be connected to the corresponding directories. To avoid
    // unneccessary special cases, we connect them here.
    for(lvaddr_t vaddr = X86_64_INIT_VBASE; vaddr < X86_64_INIT_SPACE_LIMIT;
        vaddr += BASE_PAGE_SIZE) {
        lvaddr_t baddr = vaddr - X86_64_INIT_VBASE;
        union x86_64_pdir_entry *pml4_base, *pdpt_base, *pdir_base;
        union x86_64_ptable_entry *ptable_base;
        pml4_base = &init_pml4[X86_64_PML4_BASE(vaddr)];
        pdpt_base = &init_pdpt[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE +
                               X86_64_PDPT_BASE(vaddr)];
        pdir_base = &init_pdir[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                               X86_64_PTABLE_SIZE
                               + X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE
                               + X86_64_PDIR_BASE(vaddr)];
        ptable_base = &init_ptable[X86_64_PML4_BASE(baddr) * X86_64_PTABLE_SIZE *
                                   X86_64_PTABLE_SIZE * X86_64_PTABLE_SIZE +
                                   X86_64_PDPT_BASE(baddr) * X86_64_PTABLE_SIZE *
                                   X86_64_PTABLE_SIZE + X86_64_PDIR_BASE(baddr) *
                                   X86_64_PTABLE_SIZE + X86_64_PTABLE_BASE(vaddr)];

        paging_x86_64_map_table(pml4_base, mem_to_local_phys((lvaddr_t)pdpt_base));
        paging_x86_64_map_table(pdpt_base, mem_to_local_phys((lvaddr_t)pdir_base));
        paging_x86_64_map_table(pdir_base, mem_to_local_phys((lvaddr_t)ptable_base));
    }

    /* Initialize and switch to init's PML4 */
    paging_x86_64_make_good_pml4(mem_to_local_phys((lvaddr_t)init_pml4));
    paging_x86_64_context_switch(mem_to_local_phys((lvaddr_t)init_pml4));

    /***** VSpace available now *****/
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

    /* Map cmdline args R/W into VSpace at ARGS_BASE */
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(ARGS_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < ARGS_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(ARGS_BASE) + i],
                          st->args_page + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    /* Map dispatcher frame R/W into VSpace */
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(DISPATCHER_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < (1 << DISPATCHER_FRAME_BITS) / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(DISPATCHER_BASE) + i],
                          mem_to_local_phys(init_dcb->disp) + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    struct dispatcher_shared_generic *init_disp =
        get_dispatcher_shared_generic(init_dcb->disp);
    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);

    registers_set_param(&init_disp_x86_64->enabled_save_area, paramaddr);

    // Map IO cap in task cnode
    struct cte *iocap = caps_locate_slot(CNODE(st->taskcn), TASKCN_SLOT_IO);
    err = caps_create_new(ObjType_IO, 0, 0, 0, iocap);
    assert(err_is_ok(err));

    /* Set fields in DCB */
    // Set Vspace
    init_dcb->vspace = mem_to_local_phys((lvaddr_t)init_pml4);

    // init dispatcher
    init_disp->disabled = true;
    strncpy(init_disp->name, argv[0], DISP_NAME_LEN);

    /* tell init the vspace addr of its dispatcher */
    init_disp->udisp = DISPATCHER_BASE;

    init_disp_x86_64->disabled_save_area.rdi = DISPATCHER_BASE;
    init_disp_x86_64->disabled_save_area.fs = 0;
    init_disp_x86_64->disabled_save_area.gs = 0;
    init_disp_x86_64->disabled_save_area.eflags = USER_EFLAGS;

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
    snprintf(bootinfochar, sizeof(bootinfochar), "%lu", BOOTINFO_BASE);
    const char *argv[] = { "init", bootinfochar };

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name, 
                                             ARRAY_LENGTH(argv), argv,
                                             bootinfo_phys, alloc_phys);

    /* Map bootinfo R/W into VSpace at vaddr BOOTINFO_BASE */
    paging_x86_64_map_table(&init_pml4[0], mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[0], mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[1], mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < BOOTINFO_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[i], bootinfo_phys + i * BASE_PAGE_SIZE,
                   INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R|PF_W));
    }

    /* Load init ELF64 binary from multiboot */
    struct multiboot_modinfo *module = multiboot_find_module(name);
    if (module == NULL) {
        panic("Could not find init module!");
    }
    lvaddr_t init_ep;
    err = elf_load(EM_X86_64, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(module->mod_start),
                   MULTIBOOT_MODULE_SIZE(*module), &init_ep);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);
    init_disp_x86_64->disabled_save_area.rip = init_ep;

    /* Create caps for init to use */
    create_module_caps(&spawn_state);
    // XXX: temporary fix for trac ticket #253: make it more unlikely that
    // we run out of root cnode slots by aligning the memory we declare free
    // to 1MB.
    lpaddr_t init_alloc_end = alloc_phys(0);
    lpaddr_t align = 1UL << 20; // 1MB
    // XXX: No checks are in place to make sure that init_alloc_end_aligned
    // is actually a valid physical memory address (e.g. a location at which
    // RAM exists.
    lpaddr_t init_alloc_end_aligned = (init_alloc_end + align) & ~(align-1);
    printf("aligning free memory start to 0x%"PRIxLPADDR" (was 0x%"PRIxLPADDR
           "): wasting %lu kB\n",
           init_alloc_end_aligned, init_alloc_end,
           (init_alloc_end_aligned - init_alloc_end) / 1024);
    create_phys_caps(init_alloc_end_aligned);

    /* Fill bootinfo struct */
    bootinfo->mem_spawn_core = NEEDED_KERNEL_SPACE; // Size of kernel

    return init_dcb;
}

struct dcb *spawn_app_init(struct x86_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys)
{
    errval_t err;

    /* Construct cmdline args */
    // Core id of the core that booted this core
    char coreidchar[16];
    snprintf(coreidchar, sizeof(coreidchar), "%d", core_data->src_core_id);

    // IPI channel id of core that booted this core
    char chanidchar[30];
    snprintf(chanidchar, sizeof(chanidchar), "chanid=%d", core_data->chan_id);

    // Arch id of the core that booted this core
    char archidchar[30];
    snprintf(archidchar, sizeof(archidchar), "archid=%d",
             core_data->src_arch_id);

    const char *argv[] = { name, coreidchar, chanidchar, archidchar };

    struct dcb *init_dcb = spawn_init_common(&spawn_state, name, 
                                             ARRAY_LENGTH(argv), argv,
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
    paging_x86_64_map_table(&init_pml4[X86_64_PML4_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdpt));
    paging_x86_64_map_table(&init_pdpt[X86_64_PDPT_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_pdir));
    paging_x86_64_map_table(&init_pdir[X86_64_PDIR_BASE(MON_URPC_BASE)],
                            mem_to_local_phys((lvaddr_t)init_ptable));
    for (int i = 0; i < MON_URPC_SIZE / BASE_PAGE_SIZE; i++) {
        paging_x86_64_map(&init_ptable[X86_64_PTABLE_BASE(MON_URPC_BASE) + i],
                          urpc_ptr + i * BASE_PAGE_SIZE,
                          INIT_PAGE_BITMAP | paging_elf_to_page_flags(PF_R | PF_W));
    }

    // elf load the domain
    genvaddr_t entry_point;
    err = elf_load(EM_X86_64, startup_alloc_init, &spawn_state,
                   local_phys_to_mem(core_data->monitor_binary),
                   core_data->monitor_binary_size, &entry_point);
    if (err_is_fail(err)) {
        //err_print_calltrace(err);
        panic("ELF load of init module failed!");
    }

    struct dispatcher_shared_x86_64 *init_disp_x86_64 =
        get_dispatcher_shared_x86_64(init_dcb->disp);
    init_disp_x86_64->disabled_save_area.rip = entry_point;

    return init_dcb;
}
