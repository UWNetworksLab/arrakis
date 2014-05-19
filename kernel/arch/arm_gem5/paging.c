/*
 * Copyright (c) 2009 - 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <cp15.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <arm_hal.h>
#include <cap_predicates.h>
#include <dispatch.h>

/**
 * Kernel L1 page table
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 16K after relocation
static union arm_l1_entry kernel_l1_table[2*ARM_L1_MAX_ENTRIES]
__attribute__((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry *aligned_kernel_l1_table;
/**
 * Kernel L2 page table for first MB
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 1K after relocation
static union arm_l2_entry low_l2_table[2*ARM_L2_MAX_ENTRIES]
__attribute__((aligned(ARM_L2_ALIGN)));
static union arm_l2_entry *aligned_low_l2_table;

// ------------------------------------------------------------------------
// Utility declarations

inline static uintptr_t paging_round_down(uintptr_t address, uintptr_t size)
{
    return address & ~(size - 1);
}

inline static uintptr_t paging_round_up(uintptr_t address, uintptr_t size)
{
    return (address + size - 1) & ~(size - 1);
}

inline static int aligned(uintptr_t address, uintptr_t bytes)
{
    return (address & (bytes - 1)) == 0;
}

static void
paging_write_l1_entry(uintptr_t ttbase, lvaddr_t va, union arm_l1_entry l1)
{
    union arm_l1_entry *l1_table;
    if (ttbase == 0) {
        if(va < MEMORY_OFFSET)
            ttbase = cp15_read_ttbr0() + MEMORY_OFFSET;
        else
            ttbase = cp15_read_ttbr1() + MEMORY_OFFSET;
    }
    l1_table = (union arm_l1_entry *) ttbase;
    l1_table[ARM_L1_OFFSET(va)] = l1;
}
// ------------------------------------------------------------------------
// Exported functions


void paging_map_kernel_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{

    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 1;
    l1.section.cacheable    = 1;
    l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = pa >> 20u;

    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_up(paddr + bytes, BYTES_PER_SECTION);
    while (paddr < pend) {
        paging_map_kernel_section(ttbase, paddr + MEMORY_OFFSET, paddr);
        paddr += BYTES_PER_SECTION;
    }
}


static inline struct cte *cte_for_cap(struct capability *cap)
{
    return (struct cte *) (cap - offsetof(struct cte, cap));
}

static void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    l1.section.base_address = pa >> 20u;

    paging_write_l1_entry(ttbase, va, l1);
}

lvaddr_t paging_map_device(lpaddr_t device_base, size_t device_bytes)
{
    // HACK to put device in high memory.
    // Should likely track these allocations.
    static lvaddr_t dev_alloc = DEVICE_OFFSET;
    assert(device_bytes <= BYTES_PER_SECTION);
    dev_alloc -= BYTES_PER_SECTION;

    paging_map_device_section((uintptr_t)aligned_kernel_l1_table, dev_alloc, device_base);

    return dev_alloc;
}

/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears out
 * all other mappings. Use this only at system bootup!
 */
void paging_arm_reset(lpaddr_t paddr, size_t bytes)
{
    // make sure kernel pagetable is aligned to 16K after relocation
    aligned_kernel_l1_table = (union arm_l1_entry *)ROUND_UP((uintptr_t)kernel_l1_table, ARM_L1_ALIGN);

    // make sure low l2 pagetable is aligned to 1K after relocation
    aligned_low_l2_table = (union arm_l2_entry *)ROUND_UP((uintptr_t)low_l2_table, ARM_L2_ALIGN);

    // Re-map physical memory
    paging_map_memory((uintptr_t)aligned_kernel_l1_table, paddr, bytes);

    // map first MB at granularity of 4K pages
    uint32_t l2_flags = ARM_L2_SMALL_USR_NONE | ARM_L2_SMALL_CACHEABLE | ARM_L2_SMALL_BUFFERABLE;
    paging_map_user_pages_l1((uintptr_t)aligned_kernel_l1_table, MEMORY_OFFSET,
                             mem_to_local_phys((uintptr_t)aligned_low_l2_table));
    for(lpaddr_t pa=0; pa < ARM_L1_SECTION_BYTES; pa += BYTES_PER_PAGE)
    {
        lvaddr_t va = pa + MEMORY_OFFSET;
        paging_set_l2_entry((uintptr_t *)&aligned_low_l2_table[ARM_L2_OFFSET(va)], pa, l2_flags);
    }

    // map high-mem relocated exception vector to corresponding page in low MB
    // core 0: 0xffff0000 -> 0x80000
    // core 1: 0xffff0000 -> 0x81000
    // ...
    paging_map_user_pages_l1((uintptr_t)aligned_kernel_l1_table, ETABLE_ADDR,
            mem_to_local_phys((uintptr_t)aligned_low_l2_table));
    int core_id = hal_get_cpu_id();
    lpaddr_t addr = ETABLE_PHYS_BASE + core_id * BASE_PAGE_SIZE;
    paging_set_l2_entry((uintptr_t *)&aligned_low_l2_table[ARM_L2_OFFSET(ETABLE_ADDR)], addr, l2_flags);

    cp15_write_ttbr1(mem_to_local_phys((uintptr_t)aligned_kernel_l1_table));

    cp15_invalidate_tlb();
}


void paging_make_good(lvaddr_t new_table_base, size_t new_table_bytes)
{
    assert(new_table_base >= MEMORY_OFFSET);
    assert(new_table_bytes == ARM_L1_ALIGN);
    assert(aligned(new_table_base, ARM_L1_ALIGN));

    lvaddr_t ttbr = local_phys_to_mem(cp15_read_ttbr0());
    size_t st = (MEMORY_OFFSET / ARM_L1_SECTION_BYTES) * ARM_L1_BYTES_PER_ENTRY;

    // Copy kernel pages (everything from MEMORY_OFFSET upwards)
    memcpy((void*)new_table_base + st, (void*)ttbr + st,
           ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY - st);
}

void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, ARM_L1_ALIGN));
    assert(aligned(pa, BYTES_PER_SMALL_PAGE));

    union arm_l1_entry e;

    e.raw                     = 0;
    e.page_table.type         = L1_TYPE_PAGE_TABLE_ENTRY;
    e.page_table.domain       = 0;
    e.page_table.base_address = (pa >> 10);

    paging_write_l1_entry(table_base, va, e);
}

void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union arm_l2_entry e;
    e.raw = flags;

    e.small_page.type = L2_TYPE_SMALL_PAGE;
    e.small_page.base_address = (addr >> 12);

    *l2e = e.raw;
}

void paging_context_switch(lpaddr_t ttbr)
{
    assert(ttbr < MEMORY_OFFSET);
    //assert((ttbr & 0x3fff) == 0);

    lpaddr_t old_ttbr = cp15_read_ttbr0();
    if (ttbr != old_ttbr)
    {
        cp15_write_ttbr0(ttbr);
        cp15_invalidate_tlb();
        //this isn't necessary on gem5, since gem5 doesn't implement the cache
        //maintenance instructions, but ensures coherency by itself
        //cp15_invalidate_i_and_d_caches();
    }
}
