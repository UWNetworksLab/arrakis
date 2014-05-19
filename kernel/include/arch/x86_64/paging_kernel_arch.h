/**
 * \file
 * \brief Architecture specific kernel page table definitions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_X86_64_PAGING_H
#define KERNEL_ARCH_X86_64_PAGING_H

#include <target/x86_64/paging_kernel_target.h>
#include <paging_kernel_helper.h>

/** Physical memory page size is 2 MBytes */
#define X86_64_MEM_PAGE_SIZE            X86_64_LARGE_PAGE_SIZE

/** Mask for physical memory page */
#define X86_64_MEM_PAGE_MASK            0x1fffff

/**
 * Resolves to required number of entries in page directory pointer table to map
 * 'limit' number of bytes.
 */
#define X86_64_PDPT_ENTRIES(limit)     (X86_64_PML4_BASE((limit) - 1) + 1)

/**
 * Resolves to required number of entries in page directory to map 'limit'
 * number of bytes.
 */
#define X86_64_PDIR_ENTRIES(limit)     (X86_64_PDPT_BASE((limit) - 1) + 1)

/**
 * Resolves to required number of entries in page table to map 'limit' number
 * of bytes.
 */
#define X86_64_PTABLE_ENTRIES(limit)   (X86_64_PDIR_BASE((limit) - 1) + 1)

/**
 * \brief Switch context.
 *
 * Assigns given physical base address to the CR3 register,
 * effectively switching context to new address space. Be
 * cautious that you only switch to "good" page tables.
 *
 * \param addr  Physical base address of page table.
 */
static void inline paging_context_switch(lpaddr_t addr)
{
    paging_x86_64_context_switch(addr);
}

static lvaddr_t inline paging_map_device(lpaddr_t base, size_t size)
{
    return paging_x86_64_map_device(base, size);
}

static inline bool is_root_pt(enum objtype type) {
    return type == ObjType_VNode_x86_64_pml4;
}

static inline size_t get_pte_size(void) {
    return sizeof(union x86_64_ptable_entry);
}

static inline void do_one_tlb_flush(genvaddr_t vaddr)
{
    __asm__ __volatile__("invlpg %0" : : "m" (*(char *)vaddr));
}

static inline void do_selective_tlb_flush(genvaddr_t vaddr, genvaddr_t vend)
{
    for (genvaddr_t addr = vaddr; addr < vend; addr += X86_64_BASE_PAGE_SIZE) {
        __asm__ __volatile__("invlpg %0" : : "m" (*(char *)addr));
    }
}

static inline void do_full_tlb_flush(void) {
    // XXX: FIXME: Going to reload cr3 to flush the entire TLB.
    // This is inefficient.
    // The current implementation is also not multicore safe.
    // We should only invalidate the affected entry using invlpg
    // and figure out which remote tlbs to flush.
    uint64_t cr3;
    __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));
}


#endif // KERNEL_ARCH_X86_64_PAGING_H
