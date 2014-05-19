/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <target/x86_32/paging_kernel_target.h>
#include <target/x86_32/offsets_target.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <cap_predicates.h>

static inline struct cte *cte_for_cap(struct capability *cap)
{
    return (struct cte *) (cap - offsetof(struct cte, cap));
}

#ifdef CONFIG_PAE
/// Map within a x86_32 pdpt
static errval_t x86_32_pdpt(struct capability *dest, cslot_t slot,
                            struct capability * src, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count > 1) { // disallow multiple pdpt mappings at a time
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_VNode_x86_32_pdir) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    if(slot >= X86_32_PDPTE_BASE(X86_32_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_pdpt.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_pdpte_entry *entry =
        (union x86_32_pdpte_entry *)dest_lv + slot;

    // Set metadata
    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte = dest_lp + slot * sizeof(union x86_32_pdpte_entry);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.offset = offset;

    // Source
    genpaddr_t src_gp   = src->u.vnode_x86_32_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map_pdpte(entry, src_lp);
    paging_x86_32_context_switch(dcb_current->vspace); // To flush TLB

    return SYS_ERR_OK;
}
#endif

/// Map within a x86_32 pdir
static errval_t x86_32_pdir(struct capability *dest, cslot_t slot,
                            struct capability * src, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count > 1) { // disallow more than one page at a time
        // XXX: this prevents mapping multiple superpages at a time
        return SYS_ERR_VM_MAP_SIZE;
    }

#ifndef CONFIG_PAE
    if(slot >= X86_32_PDIR_BASE(X86_32_MEMORY_OFFSET)) { // Kernel mapped here
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }
#endif

    if (src->type != ObjType_VNode_x86_32_ptable) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // Destination
    genpaddr_t dest_gp   = dest->u.vnode_x86_32_pdir.base;
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    union x86_32_pdir_entry *entry =
        (union x86_32_pdir_entry *)dest_lv + slot;

    // Set metadata
    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte = dest_lp + slot * sizeof(union x86_32_pdir_entry);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.offset = offset;


    // Source
    // XXX: offset is ignored
    genpaddr_t src_gp   = src->u.vnode_x86_32_pdir.base;
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    paging_x86_32_map_table(entry, src_lp);

    return SYS_ERR_OK;
}

/// Map within a x86_32 ptable
static errval_t x86_32_ptable(struct capability *dest, cslot_t slot,
                              struct capability * src, uintptr_t uflags,
                              uintptr_t offset, uintptr_t pte_count)
{
    if (slot >= X86_32_PTABLE_SIZE) { // Slot within page table
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    cslot_t last_slot = slot + pte_count;

    if (last_slot > X86_32_PTABLE_SIZE) {
        printf("slot = %"PRIuCSLOT", last_slot = %"PRIuCSLOT", PTABLE_SIZE = %d\n", slot, last_slot, X86_32_PTABLE_SIZE);
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_Frame &&
        src->type != ObjType_DevFrame) { // Right mapping
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if (offset + pte_count * X86_32_BASE_PAGE_SIZE > get_size(src)) {
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_32_flags_t flags =
        paging_x86_32_cap_to_page_flags(src->rights);
    // Mask with provided access rights mask
    flags = paging_x86_32_mask_attrs(flags, X86_32_PTABLE_ACCESS(uflags));
    // Add additional arch-specific flags
    flags |= X86_32_PTABLE_FLAGS(uflags);
    // Unconditionally mark the page present
    flags |= X86_32_PTABLE_PRESENT;

    // Convert destination base address
    genpaddr_t dest_gp   = get_address(dest);
    lpaddr_t dest_lp     = gen_phys_to_local_phys(dest_gp);
    lvaddr_t dest_lv     = local_phys_to_mem(dest_lp);
    // Convert source base address
    genpaddr_t src_gp   = get_address(src);
    lpaddr_t src_lp     = gen_phys_to_local_phys(src_gp);
    // Set metadata
    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte = dest_lp + slot * sizeof(union x86_32_ptable_entry);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.offset = offset;


    for (; slot < last_slot; slot++, offset += X86_32_BASE_PAGE_SIZE) {
        union x86_32_ptable_entry *entry =
            (union x86_32_ptable_entry *)dest_lv + slot;

        /* FIXME: Flush TLB if the page is already present
         * in the meantime, since we don't do this, we just assert that
         * we never reuse a VA mapping */
        if (X86_32_IS_PRESENT(entry)) {
            panic("Trying to map into an already present page NYI.");
        }

        // Carry out the page mapping
        paging_x86_32_map(entry, src_lp + offset, flags);
    }

    return SYS_ERR_OK;
}

typedef errval_t (*mapping_handler_t)(struct capability *dest_cap,
                                      cslot_t dest_slot,
                                      struct capability *src_cap,
                                      uintptr_t flags, uintptr_t offset,
                                      uintptr_t pte_count);

/// Dispatcher table for the type of mapping to create
static mapping_handler_t handler[ObjType_Num] = {
#ifdef CONFIG_PAE
    [ObjType_VNode_x86_32_pdpt]   = x86_32_pdpt,
#endif
    [ObjType_VNode_x86_32_pdir]   = x86_32_pdir,
    [ObjType_VNode_x86_32_ptable] = x86_32_ptable,
};

#define DIAGNOSTIC_ON_ERROR 1
#define RETURN_ON_ERROR 1

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    assert(type_is_vnode(dest_vnode_cte->cap.type));

    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;
    mapping_handler_t handler_func = handler[dest_cap->type];

    assert(handler_func != NULL);

    if (src_cte->mapping_info.pte) {
        // already mapped
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: this copy is already mapped @0x%lx\n", src_cte->mapping_info.pte);
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_ALREADY_MAPPED;
#endif
    }

    cslot_t last_slot = dest_slot + pte_count;

    // TODO: PAE
    if (last_slot > X86_32_PTABLE_SIZE) {
        // requested map overlaps leaf page table
#if DIAGNOSTIC_ON_ERROR
        printf("caps_copy_to_vnode: requested mapping spans multiple leaf page tables\n");
#endif
#if RETURN_ON_ERROR
        return SYS_ERR_VM_RETRY_SINGLE;
#endif
    }

#if 0
    genvaddr_t vaddr;
    compile_vaddr(dest_vnode_cte, dest_slot, &vaddr);
    printf("caps_copy_to_vnode: mapping %lu pages (slots %"PRIuCSLOT" to %"PRIuCSLOT") to 0x%"PRIxGENVADDR"\n",
            pte_count, dest_slot, last_slot, vaddr);
    genpaddr_t paddr = get_address(&src_cte->cap) + offset;
    printf("mapping 0x%"PRIxGENPADDR" to 0x%"PRIxGENVADDR"\n", paddr, vaddr);
#endif


    errval_t r = handler_func(dest_cap, dest_slot, src_cap, flags, offset, pte_count);
    if (err_is_fail(r)) {
        printf("caps_copy_to_vnode: handler func returned %ld\n", r);
    }
#if 0
    else {
        printf("mapping_info.pte       = 0x%lx\n", src_cte->mapping_info.pte);
        printf("mapping_info.offset    = 0x%"PRIx64"\n", src_cte->mapping_info.offset);
        printf("mapping_info.pte_count = %zu\n", src_cte->mapping_info.pte_count);
    }
#endif
    return r;
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    size_t unmapped_pages = 0;
    union x86_32_ptable_entry *ptentry = (union x86_32_ptable_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

static inline void read_pt_entry(struct capability *pgtable, size_t slot,
        genpaddr_t *mapped_addr, lpaddr_t *pte,
        void **entry)
{
    assert(type_is_vnode(pgtable->type));

    genpaddr_t paddr;
    lpaddr_t pte_;
    void *entry_;

    genpaddr_t gp = get_address(pgtable);
    lpaddr_t lp = gen_phys_to_local_phys(gp);
    lvaddr_t lv = local_phys_to_mem(lp);

    // get paddr
    switch (pgtable->type) {
        case ObjType_VNode_x86_32_pdpt:
        case ObjType_VNode_x86_32_pdir: {
            union x86_32_pdir_entry *e =
                (union x86_32_pdir_entry *)lv + slot;
            paddr = e->d.base_addr << BASE_PAGE_BITS;
            entry_ = e;
            pte_ = lp + slot * sizeof(union x86_32_pdir_entry);
            break;
        }
        case ObjType_VNode_x86_32_ptable: {
            union x86_32_ptable_entry *e =
                (union x86_32_ptable_entry *)lv + slot;
            paddr = e->base.base_addr << BASE_PAGE_BITS;
            entry_ = e;
            pte_ = lp + slot * sizeof(union x86_32_ptable_entry);
            break;
        }
        default:
            assert(!"Should not get here");
    }

    if (mapped_addr) {
        *mapped_addr = paddr;
    }
    if (pte) {
        *pte = pte_;
    }
    if (entry) {
        *entry = entry_;
    }
}

errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping, size_t slot, size_t num_pages)
{
    assert(type_is_vnode(pgtable->type));
    //printf("page_mappings_unmap(%zd pages, slot = %zd)\n", num_pages, slot);

    // get page table entry data
    genpaddr_t paddr;
    //lpaddr_t pte;
    read_pt_entry(pgtable, slot, &paddr, NULL, NULL);
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    // get virtual address of first page
    // TODO: error checking
    genvaddr_t vaddr;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    compile_vaddr(leaf_pt, slot, &vaddr);
    // printf("vaddr = 0x%"PRIxGENVADDR"\n", vaddr);
    // printf("num_pages = %zu\n", num_pages);

    // get cap for mapping
    /*
    struct cte *mem;
    errval_t err = lookup_cap_for_mapping(paddr, pte, &mem);
    if (err_is_fail(err)) {
        printf("page_mappings_unmap: %ld\n", err);
        return err;
    }
    */
    //printf("state before unmap: mapped_pages = %zd\n", mem->mapping_info.mapped_pages);
    //printf("state before unmap: num_pages    = %zd\n", num_pages);

    if (num_pages != mapping->mapping_info.pte_count) {
        // want to unmap a different amount of pages than was mapped
        return SYS_ERR_VM_MAP_SIZE;
    }

    do_unmap(pt, slot, num_pages);

    // flush TLB for unmapped pages
    // TODO: heuristic that decides if selective or full flush is more
    //       efficient?
    if (num_pages > 1) {
        do_full_tlb_flush();
    } else {
        do_one_tlb_flush(vaddr);
    }

    // update mapping info
    memset(&mapping->mapping_info, 0, sizeof(struct mapping_info));

    return SYS_ERR_OK;
}

errval_t page_mappings_modify_flags(struct capability *frame, size_t offset,
                                    size_t pages, size_t uflags)
{
    struct cte *mapping = cte_for_cap(frame);
    struct mapping_info *info = &mapping->mapping_info;

    /* Calculate page access protection flags */
    // Get frame cap rights
    paging_x86_32_flags_t flags =
        paging_x86_32_cap_to_page_flags(frame->rights);
    // Mask with provided access rights mask
    flags = paging_x86_32_mask_attrs(flags, X86_32_PTABLE_ACCESS(uflags));
    // Add additional arch-specific flags
    flags |= X86_32_PTABLE_FLAGS(uflags);
    // Unconditionally mark the page present
    flags |= X86_32_PTABLE_PRESENT;

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) + offset;

    for (int i = 0; i < pages; i++) {
        union x86_32_ptable_entry *entry =
            (union x86_32_ptable_entry *)base + i;
        paging_x86_32_modify_flags(entry, flags);
    }

    return paging_tlb_flush_range(mapping, pages);
}

void paging_dump_tables(struct dcb *dispatcher)
{
    printf("dump_hw_page_tables\n");
    lvaddr_t root_pt = local_phys_to_mem(dispatcher->vspace);

#ifdef CONFIG_PAE
    // loop over pdpt entries
    for (int pdir_index = 0; pdir_index < X86_64_PDPTE_SIZE; pdir_index++) {
        // get pdir
        union x86_32_pdpte_entry *pdir = (union x86_64_pdir_entry *)root_pt + pdir_index;
        if (!pdir->raw) { continue; }
        genpaddr_t pdir_gp = pdir->d.base_addr << BASE_PAGE_BITS;
        lvaddr_t pdir_lv = local_phys_to_mem(gen_phys_to_local_phys(pdir_gp));
#else
        int pdir_index = 0;
        lvaddr_t pdir_lv = root_pt;
#endif

        for (int ptable_index = 0; ptable_index < X86_32_PDIR_SIZE; ptable_index++) {
            // get ptable
            union x86_32_pdir_entry *ptable = (union x86_32_pdir_entry *)pdir_lv + ptable_index;
            if (!ptable->raw) { continue; }
            genpaddr_t ptable_gp = ptable->d.base_addr << BASE_PAGE_BITS;
            lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

            for (int entry = 0; entry < X86_32_PTABLE_SIZE; entry++) {
                union x86_32_ptable_entry *e =
                    (union x86_32_ptable_entry *)ptable_lv + entry;
                genpaddr_t paddr = (genpaddr_t)e->base.base_addr << BASE_PAGE_BITS;
                if (!paddr) {
                    continue;
                }
                printf("%d.%d.%d: 0x%"PRIxGENPADDR"\n", pdir_index, ptable_index, entry, paddr);
            }
        }
#ifdef CONFIG_PAE
    } // endfor PDPT entries
#endif
}
