/**
 * \file
 * \brief ARM kernel page-table structures.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_ARCH_ARM_PAGING_H
#define KERNEL_ARCH_ARM_PAGING_H

// XXX: Not sure if these includes are required
#include <capabilities.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/paging_arch.h>
#include <cp15.h>

/**
 * Setup bootstrap page table with direct and relocated mappings for kernel.
 *
 * This function does not enable paging.
 *
 * @param initial_base
 * @param initial_size
 */
void paging_map_kernel(uintptr_t initial_base, size_t initial_size);

lvaddr_t paging_map_device(lpaddr_t base, size_t size);

/**
 * Add kernel mappings to newly constructed page table.
 *
 * @param new_table_addr  address of newly constructed page table.
 * @param new_table_bytes size of newly constructed page table.
 */
void paging_make_good(lvaddr_t new_table_addr, size_t new_table_bytes);

void paging_map_user_pages_l1(lvaddr_t table_addr, lvaddr_t vaddr, lpaddr_t paddr);

void paging_set_l2_entry(uintptr_t* l2entry, lpaddr_t paddr, uintptr_t flags);

void paging_context_switch(lpaddr_t table_addr);

// REVIEW: [2010-05-04 orion]
// these were deprecated in churn, enabling now to get system running again.

void paging_map_kernel_section(uintptr_t ttbase,lvaddr_t vbase, lpaddr_t pbase);
void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes);

// L1 Alignment determined by TTBR register (bits 13:0 ignored by hardware)
#define ARM_L1_ALIGN                    16384u

#define ARM_L1_MAX_ENTRIES              4096u
#define ARM_L1_BYTES_PER_ENTRY          4u
#define ARM_L1_SECTION_BYTES            (1024u * 1024u)

#define ARM_L2_ALIGN                    1024u
#define ARM_L2_MAX_ENTRIES              256u
#define ARM_L2_BYTES_PER_ENTRY          4u
#define ARM_L2_TABLE_BYTES              ARM_L2_ALIGN

#define ARM_L2_SMALL_CACHEABLE          0x008
#define ARM_L2_SMALL_BUFFERABLE         0x004
#define ARM_L2_SMALL_USR_RO             0xaa0
#define ARM_L2_SMALL_USR_RW             0xff0

static inline bool is_root_pt(enum objtype type) {
    return type == ObjType_VNode_ARM_l1;
}

static inline size_t get_pte_size(void) {
    // both l1_entry and l2_entry are 4 bytes
    return 4;
}
#define PTABLE_ENTRY_SIZE get_pte_size()

static inline void do_one_tlb_flush(genvaddr_t vaddr)
{
    // TODO: figure out selective flushing for ARM
    cp15_invalidate_tlb();
}

static inline void do_selective_tlb_flush(genvaddr_t vaddr, genvaddr_t vend)
{
    // TODO: figure out selective flushing for ARM
    cp15_invalidate_tlb();
}

static inline void do_full_tlb_flush(void)
{
    cp15_invalidate_tlb();
}
#endif // KERNEL_ARCH_ARM_PAGING_H
