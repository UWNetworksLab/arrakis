/**
 * \file
 * \brief Architecture specific kernel page table definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_X86_32_PAGING_H
#define KERNEL_ARCH_X86_32_PAGING_H

#include <target/x86_32/paging_kernel_target.h>
#include <paging_kernel_helper.h>

#if defined(CONFIG_PAE) || defined(CONFIG_PSE)
/** Physical memory page size is 2 MBytes */
#       define X86_32_MEM_PAGE_SIZE           X86_32_LARGE_PAGE_SIZE
#else
/** Physical memory page size is 4 KBytes */
#       define X86_32_MEM_PAGE_SIZE           X86_32_BASE_PAGE_SIZE

#       define MEM_PTABLE_SIZE                X86_32_PTABLE_ENTRIES(X86_32_PADDR_SPACE_LIMIT)
#endif

/** Mask for physical memory page */
#define X86_32_MEM_PAGE_MASK           (X86_32_MEM_PAGE_SIZE - 1)

#ifdef CONFIG_PAE

/**
 * Resolves to required number of entries in page directory to map 'limit'
 * number of bytes.
 */
#       define X86_32_PDIR_ENTRIES(limit)    (X86_32_PDPTE_BASE((limit) - 1) + 1)

/**
 * Resolves to required number of entries in page table to map 'limit' number
 * of bytes.
 */
#       define X86_32_PTABLE_ENTRIES(limit)  (X86_32_PDIR_BASE((limit) - 1) + 1)

#else

/**
 * Resolves to required number of entries in page directory to map 'limit'
 * number of bytes.
 */
#       define X86_32_PDIR_ENTRIES(limit)      1

/**
 * Resolves to required number of entries in page table to map 'limit' number
 * of bytes.
 */
#       define X86_32_PTABLE_ENTRIES(limit)  (X86_32_PDIR_BASE((limit) - 1) + 1)

#endif

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
    paging_x86_32_context_switch(addr);
}

static lvaddr_t inline paging_map_device(lpaddr_t base, size_t size)
{
    return paging_x86_32_map_device(base, size);
}

static inline bool is_root_pt(enum objtype type) {
#ifdef CONFIG_PAE
    return type == ObjType_VNode_x86_32_pdpt;
#else
    return type == ObjType_VNode_x86_32_pdir;
#endif
}

static inline size_t get_pte_size(void) {
    // the definition of x86_32_ptable entry is wrapped in an #ifdef CONFIG_PAE
    // block and will thus have the correct size for both PAE and non-PAE x86_32.
    return sizeof(union x86_32_ptable_entry);
}

static inline void do_selective_tlb_flush(genvaddr_t vaddr, genvaddr_t vend)
{
    assert(vaddr < ((genvaddr_t)1)<<32);
    assert(vend < ((genvaddr_t)1)<<32);
    uint32_t vaddr32 = (uint32_t)vaddr;
    uint32_t vend32 = (uint32_t)vend;

    for (uint32_t addr = vaddr32; addr < vend32; addr += X86_32_BASE_PAGE_SIZE) {
        __asm__ __volatile__("invlpg %0" : : "m" (*(char *)addr));
    }
}

static inline void do_one_tlb_flush(genvaddr_t vaddr)
{
    assert(vaddr < ((genvaddr_t)1)<<32);
    uint32_t addr = (uint32_t)vaddr;

    __asm__ __volatile__("invlpg %0" : : "m" (*(char *)addr));
}

static inline void do_full_tlb_flush(void)
{
    // XXX: FIXME: Going to reload cr3 to flush the entire TLB.
    // This is inefficient.
    // The current implementation is also not multicore safe.
    // We should only invalidate the affected entry using invlpg
    // and figure out which remote tlbs to flush.
    uint32_t cr3;
    __asm__ __volatile__("mov %%cr3,%0" : "=a" (cr3) : );
    __asm__ __volatile__("mov %0,%%cr3" :  : "a" (cr3));
}

#endif // KERNEL_ARCH_X86_32_PAGING_H
