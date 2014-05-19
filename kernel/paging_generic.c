/**
 * \file
 * \brief Kernel memory management.
 */

/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <paging_generic.h>
#include <barrelfish_kpi/paging_arch.h>
#include <kernel.h>
#include <paging_kernel_arch.h>
#include <capabilities.h>
#include <cap_predicates.h>
#include <mdb/mdb_tree.h>
#include <stdio.h>

static inline errval_t find_next_ptable(struct cte *old, struct cte **next)
{
    errval_t err;
    if (old->mapping_info.pte) {
        err = mdb_find_cap_for_address(local_phys_to_gen_phys((lpaddr_t)old->mapping_info.pte), next);
        if (err_no(err) == CAPS_ERR_CAP_NOT_FOUND) {
            debug(SUBSYS_PAGING, "could not find cap associated "
                    "with 0x%"PRIxLPADDR"\n", old->mapping_info.pte);
            return SYS_ERR_VNODE_LOOKUP_NEXT;
        }
        if (err_is_fail(err)) {
            debug(SUBSYS_PAGING, "error in compile_vaddr:"
                   " mdb_find_range: 0x%"PRIxERRV"\n", err);
            return err;
        }
        return SYS_ERR_OK;
    }
    else {
        *next = NULL;
        return SYS_ERR_VNODE_SLOT_INVALID;
    }
}

static inline size_t get_offset(struct cte *old, struct cte *next)
{
    return (old->mapping_info.pte - get_address(&next->cap)) / get_pte_size();
}

/*
 * compile_vaddr returns the lowest address that is addressed by entry 'entry'
 * in page table 'ptable'
 */
errval_t compile_vaddr(struct cte *ptable, size_t entry, genvaddr_t *retvaddr)
{
    if (!type_is_vnode(ptable->cap.type)) {
        return SYS_ERR_VNODE_TYPE;
    }

    genvaddr_t vaddr = 0;
    // shift at least by BASE_PAGE_BITS for first vaddr part
    size_t shift = BASE_PAGE_BITS;

    // figure out how much we need to shift (assuming that
    // compile_vaddr can be used on arbitrary page table types)
    // A couple of cases have fallthroughs in order to avoid having
    // multiple calls to vnode_objbits with the same type argument.
    switch (ptable->cap.type) {
        case ObjType_VNode_x86_64_pml4:
            shift += vnode_objbits(ObjType_VNode_x86_64_pdpt);
        case ObjType_VNode_x86_64_pdpt:
            shift += vnode_objbits(ObjType_VNode_x86_64_pdir);
        case ObjType_VNode_x86_64_pdir:
            shift += vnode_objbits(ObjType_VNode_x86_64_ptable);
        case ObjType_VNode_x86_64_ptable:
            break;

        case ObjType_VNode_x86_32_pdpt:
            shift += vnode_objbits(ObjType_VNode_x86_32_pdir);
        case ObjType_VNode_x86_32_pdir:
            shift += vnode_objbits(ObjType_VNode_x86_32_ptable);
        case ObjType_VNode_x86_32_ptable:
            break;

        case ObjType_VNode_ARM_l2:
            shift += vnode_objbits(ObjType_VNode_ARM_l1);
        case ObjType_VNode_ARM_l1:
            break;

        default:
            return SYS_ERR_VNODE_TYPE;
    }

    size_t mask = (1ULL<<vnode_objbits(ptable->cap.type))-1;
    vaddr = ((genvaddr_t)(entry & mask)) << shift;

    // add next piece of virtual address until we are at root page table
    struct cte *old = ptable;
    struct cte *next;
    errval_t err;
    while (!is_root_pt(old->cap.type))
    {
        err = find_next_ptable(old, &next);
        if (err == SYS_ERR_VNODE_NOT_INSTALLED) { // no next page table
            *retvaddr = 0;
            return err;
        }
        if (err_is_fail(err)) {
            return err;
        }
        // calculate offset into next level ptable
        size_t offset = get_offset(old, next);
        // shift new part of vaddr by old shiftwidth + #entries of old ptable
        shift += vnode_entry_bits(old->cap.type);

        mask = (1ULL<<vnode_objbits(next->cap.type))-1;
        vaddr |= ((offset & mask) << shift);
        old = next;
    }

    *retvaddr = vaddr;
    return SYS_ERR_OK;
}

errval_t unmap_capability(struct cte *mem)
{
    if (!mem->mapping_info.pte) {
        // mem is not mapped, so just return
        return SYS_ERR_OK;
    }

    errval_t err;

    // get leaf pt cap
    struct cte *pgtable;
    err = mdb_find_cap_for_address(mem->mapping_info.pte, &pgtable);
    if (err_is_fail(err)) {
        // no page table, should be ok.
        return SYS_ERR_OK;
    }
    lpaddr_t ptable_lp = gen_phys_to_local_phys(get_address(&pgtable->cap));
    lvaddr_t ptable_lv = local_phys_to_mem(ptable_lp);
    cslot_t slot = (mem->mapping_info.pte - ptable_lp) / PTABLE_ENTRY_SIZE;
    genvaddr_t vaddr;
    err = compile_vaddr(pgtable, slot, &vaddr);
    if (err_is_ok(err)) {
        // only perform unmap when we successfully reconstructed the virtual address
        do_unmap(ptable_lv, slot, mem->mapping_info.pte_count);
        if (mem->mapping_info.pte_count > 1) {
            do_full_tlb_flush();
        } else {
            do_one_tlb_flush(vaddr);
        }
    }

    return SYS_ERR_OK;
}

errval_t lookup_cap_for_mapping(genpaddr_t paddr, lvaddr_t pte, struct cte **retcte)
{
    // lookup matching cap
    struct cte *mem, *last, *orig;
    // find a cap for paddr
#if 0
    printf("lookup request = 0x%"PRIxGENPADDR"\n", paddr);
#endif
    errval_t err = mdb_find_cap_for_address(paddr, &mem);
    if (err_is_fail(err)) {
        printf("could not find a cap for 0x%"PRIxGENPADDR" (%ld)\n", paddr, err);
        return err;
    }
#if 0
    printf("lookup request = 0x%"PRIxGENPADDR"\n", paddr);
    printf("has_copies(mem) = %d\n", has_copies(mem));
    printf("pte = 0x%lx\n", pte);
    printf("0x%lx, %zd\n", get_address(&mem->cap), get_size(&mem->cap));
    printf("mem->mapping_info.pte          = 0x%lx\n", mem->mapping_info.pte);
    printf("mem->mapping_info.offset       = %zd\n", mem->mapping_info.offset);
    printf("mem->mapping_info.pte_count    = %zd\n", mem->mapping_info.pte_count);
    printf("mem = %p\n", mem);
#endif

    // look at all copies of mem
    last = mem;
    orig = mem;
    // search backwards in tree
    while (is_copy(&mem->cap, &last->cap)) {
        struct capability *cap = &mem->cap;
        struct mapping_info *map = &mem->mapping_info;
        genpaddr_t base = get_address(cap);
        // only match mappings that start where we want to unmap
        if (base + map->offset == paddr && map->pte == pte)
        {
            // found matching cap
            *retcte = mem;
            return SYS_ERR_OK;
        }
        last = mem;
        mem = mdb_predecessor(mem);
    }
    last = orig;
    // search forward in tree
    mem = mdb_successor(orig);
    while (is_copy(&mem->cap, &last->cap)) {
        struct capability *cap = &mem->cap;
        struct mapping_info *map = &mem->mapping_info;
        genpaddr_t base = get_address(cap);
        // only match mappings that start where we want to unmap
        if (base + map->offset == paddr && map->pte == pte)
        {
            // found matching cap
            *retcte = mem;
            return SYS_ERR_OK;
        }
        last = mem;
        mem = mdb_successor(mem);
    }

    // if we get here, we have not found a matching cap
    return SYS_ERR_CAP_NOT_FOUND;
}

errval_t paging_tlb_flush_range(struct cte *frame, size_t pages)
{
    // reconstruct first virtual address for TLB flushing
    struct cte *leaf_pt;
    errval_t err;
    err = mdb_find_cap_for_address(frame->mapping_info.pte, &leaf_pt);
    if (err_is_fail(err)) {
        return err;
    }
    genvaddr_t vaddr;
    size_t entry = (frame->mapping_info.pte - get_address(&leaf_pt->cap)) /
        PTABLE_ENTRY_SIZE;
    err = compile_vaddr(leaf_pt, entry, &vaddr);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_VNODE_NOT_INSTALLED) {
            debug(SUBSYS_PAGING, "couldn't reconstruct virtual address\n");
        }
        else {
            return err;
        }
    }
    debug(SUBSYS_PAGING, "flushing TLB entries for vaddrs 0x%"
            PRIxGENVADDR"--0x%"PRIxGENVADDR"\n",
            vaddr, vaddr+(pages * BASE_PAGE_SIZE));
    // flush TLB entries for all modified pages
    for (int i = 0; i < pages; i++) {
        do_one_tlb_flush(vaddr);
        vaddr += BASE_PAGE_SIZE;
    }

    return SYS_ERR_OK;
}
