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
#include <dev/omap/omap44xx_mmu_dev.h>

omap44xx_mmu_t mmu;//use mackerel device for manipulating the MMU


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
 * Maps a device to a l2 page.
 * Assumption: corresponding L1 entry already set
 *
 */

void paging_map_device_page(uintptr_t l1_table,
					   	    lvaddr_t device_vbase,
					   	    lpaddr_t device_pbase,
					   	    size_t device_bytes);

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

void paging_arm_reset(lpaddr_t paddr, size_t bytes);


// REVIEW: [2010-05-04 orion]
// these were deprecated in churn, enabling now to get system running again.

void paging_map_kernel_section(uintptr_t ttbase,lvaddr_t vbase, lpaddr_t pbase);
void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes);

static inline bool is_root_pt(enum objtype type) {
    return type == ObjType_VNode_ARM_l1;
}

static inline size_t get_pte_size(void) {
    // both l1_entry and l2_entry are 4 bytes
    return 4;
}
#define PTABLE_ENTRY_SIZE get_pte_size()

void do_one_tlb_flush(genvaddr_t vaddr);

/*
 * \brief flushes all non-preserved TLB entries
 * XXX: if the currently executing code is not cached or protected, flushing will lead to a crash
 */
void do_full_tlb_flush(void);

/*
 * Cortex-M3 on pandaboard specific stuff
 * since the M3 memory model on the pandaboard is a bit weird, we need a few additional functions
 * some of these would be provided by a cp15, but we don't have one
 */


/*
 * \brief add another (protected) entry into L2 TLB
 * size: 0 -> section (1MB)
 *       1 -> large page (64KB)
 *       2 -> small page (4KB)
 *       3 -> supersection (16MB)
 */
void add_tlb_mapping(lvaddr_t vaddr, lpaddr_t paddr, bool preserved, uint8_t size);

void set_tlb_lock_basevalue(uint8_t basevalue);

/*
 * \brief read the version number of the table (called ignored3), to see if the upper half 
 * of the table has been modified and needs to be replicated. 
 * These version numbers are currently necessary for paging_context_switch, because
 * the upper half of the table is expected to be preserved
 */
uint8_t read_table_version(union arm_l1_entry* ttb);
void write_table_version(union arm_l1_entry* ttb, uint8_t version);
void increase_table_version(union arm_l1_entry* ttb);

#endif // KERNEL_ARCH_ARM_PAGING_H
