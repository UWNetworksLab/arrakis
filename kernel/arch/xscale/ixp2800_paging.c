/*
 * Copyright (c) 2009, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <cp15.h>
#include <paging_kernel_arch.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <string.h>

// ------------------------------------------------------------------------
// Internal declarations

union l1_entry {
    uint32_t raw;

    /// Invalid L1 entry
    struct {
        uint32_t        type            :2;     // == 0
    } invalid;

    /// L1 entry for 256 4K L2 entries
    struct {
        uint32_t        base_address    :22;
        uint32_t        sbz1            :1;     // Should-be-zero
        uint32_t        domain          :4;
        uint32_t        mb1             :1;     // Must-be-one
        uint32_t        sbz0            :2;     // Should-be-zero
        uint32_t        type            :2;     // == 1
    } coarse;

    /// L1 entry for 1MB mapped section
    struct {
        uint32_t        base_address    :12;
        uint32_t        sbz1            :8;
        uint32_t        ap              :2;
        uint32_t        sbz0            :1;
        uint32_t        domain          :4;
        uint32_t        mb1             :1;     // Must-be-One
        uint32_t        cacheable       :1;
        uint32_t        bufferable      :1;
        uint32_t        type            :2;     // == 2

    } section;

    /// L1 entry for 1024 1K L2 descriptors
    struct {
        uint32_t        base_address    :20;
        uint32_t        sbz1            :3;
        uint32_t        domain          :4;
        uint32_t        mb1             :1;
        uint32_t        sbz0            :2;
        uint32_t        type            :2;     // == 3
    } fine;
};


STATIC_ASSERT_SIZEOF(union l1_entry, 4);

#define L1_TYPE_INVALID_ENTRY   0
#define L1_TYPE_COARSE_ENTRY    1
#define L1_TYPE_SECTION_ENTRY   2
#define L1_TYPE_FINE_ENTRY      3
#define L1_TYPE(x)              ((x) & 3)

union l2_entry {
    uint32_t raw;

    /// Invalid L2 entry
    struct {
        uint32_t        type            :2;     // == 0
    } invalid;

    /// Descriptior for a 64K page
    struct {
        uint32_t        base_address    :16;
        uint32_t        sbz             :4;
        uint32_t        ap3             :2;
        uint32_t        ap2             :2;
        uint32_t        ap1             :2;
        uint32_t        ap0             :2;
        uint32_t        cacheable       :1;
        uint32_t        bufferable      :1;
        uint32_t        type            :2;     // == 1
    } large_page;

    /// Descriptor for a 4K page
    struct {
        uint32_t        base_address    :20;
        uint32_t        ap3             :2;
        uint32_t        ap2             :2;
        uint32_t        ap1             :2;
        uint32_t        ap0             :2;
        uint32_t        cacheable       :1;
        uint32_t        bufferable      :1;
        uint32_t        type            :2;     // == 2
    } small_page;

    /// Descriptor for a 1K page
    struct {
        uint32_t        base_address    :22;
        uint32_t        sbz             :4;
        uint32_t        ap              :2;
        uint32_t        cacheable       :1;
        uint32_t        bufferable      :1;
        uint32_t        type            :2;     // == 3
    } tiny_page;
};

STATIC_ASSERT_SIZEOF(union l2_entry, 4);

#define L2_TYPE_INVALID_PAGE    0
#define L2_TYPE_LARGE_PAGE      1
#define L2_TYPE_SMALL_PAGE      2
#define L2_TYPE_TINY_PAGE       3
#define L2_TYPE(x)              ((x) & 3)

#define BYTES_PER_SECTION       0x100000
#define BYTES_PER_PAGE          0x1000
#define BYTES_PER_SMALL_PAGE    0x400

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

static inline struct cte *cte_for_cap(struct capability *cap)
{
    return (struct cte *) (cap - offsetof(struct cte, cap));
}

// ------------------------------------------------------------------------
// Exported functions

static void
paging_write_section_entry(uintptr_t ttbase, lvaddr_t va, union l1_entry l1)
{
    union l1_entry *l1_table;
    if (ttbase == 0) {
        ttbase = cp15_read_ttbr() + KERNEL_OFFSET;
    }
    l1_table = (union l1_entry *) ttbase;
    l1_table[va >> 20u] = l1;

    //Flush TLB?
}

void paging_map_kernel_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{

    union l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 1;
    l1.section.cacheable    = 1;
    l1.section.ap           = 1;
    l1.section.base_address = pa >> 20u;

    paging_write_section_entry(ttbase, va, l1);
}

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_down(paddr + bytes, BYTES_PER_SECTION);
    while (paddr < pend) {
        paging_map_kernel_section(0, paddr + MEMORY_OFFSET, paddr);
        paddr += BYTES_PER_SECTION;
    }
}

static void paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{
    union l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap           = 1;
    l1.section.base_address = pa >> 20u;
    l1.section.mb1          = 1;
    l1.section.sbz0         = 0;
    l1.section.sbz1         = 0;

    paging_write_section_entry(ttbase, va, l1);
}

lvaddr_t paging_map_device(lpaddr_t device_base, size_t device_bytes)
{
    // HACK to put device in high memory.
    // Should likely track these allocations.
    static lvaddr_t dev_alloc = KERNEL_OFFSET;
    assert(device_bytes <= BYTES_PER_SECTION);
    dev_alloc -= BYTES_PER_SECTION;

    paging_map_device_section(0, dev_alloc, device_base);
    
    return dev_alloc;
}

void paging_make_good(lvaddr_t new_table_base, size_t new_table_bytes)
{
    assert(new_table_base >= MEMORY_OFFSET);
    assert(new_table_bytes == ARM_L1_ALIGN);
    assert(aligned(new_table_base, ARM_L1_ALIGN));

    lvaddr_t ttbr = local_phys_to_mem(cp15_read_ttbr());
    size_t st = (MEMORY_OFFSET / ARM_L1_SECTION_BYTES) * ARM_L1_BYTES_PER_ENTRY;

    // Copy kernel pages (everything from MEMORY_OFFSET upwards)
    memcpy((void*)new_table_base + st, (void*)ttbr + st,
           ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY - st);
}

void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, ARM_L1_ALIGN));
    assert(aligned(va, BYTES_PER_SECTION));
    assert(aligned(pa, BYTES_PER_SMALL_PAGE));

    union l1_entry e;

    e.raw                 = 0;
    e.coarse.type         = L1_TYPE_COARSE_ENTRY;
    e.coarse.mb1          = 1;
    e.coarse.domain       = 0;
    e.coarse.base_address = (pa >> 10);

    uintptr_t* l1table = (uintptr_t*)table_base;
    l1table[va / BYTES_PER_SECTION] = e.raw;
}

void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
    assert(0 == (flags & 0xfffff000));
    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union l2_entry e;
    e.raw = flags;
    assert(e.small_page.ap0 == e.small_page.ap1 &&
           e.small_page.ap0 == e.small_page.ap2 &&
           e.small_page.ap0 == e.small_page.ap3);

    e.small_page.type = L2_TYPE_SMALL_PAGE;
    e.small_page.base_address = (addr >> 12);

    *l2e = e.raw;
}

void paging_context_switch(lpaddr_t ttbr)
{
    assert(ttbr < MEMORY_OFFSET);
    assert((ttbr & 0x3fff) == 0);

    lpaddr_t old_ttbr = cp15_read_ttbr();
    if (ttbr != old_ttbr)
    {
        cp15_write_ttbr(ttbr);
        cp15_invalidate_tlb();
        cp15_invalidate_i_and_d_caches();
    }
}

static void
paging_set_flags(union l2_entry *entry, uintptr_t kpi_paging_flags)
{
    entry->small_page.bufferable = 1;
    entry->small_page.cacheable =
        (kpi_paging_flags & KPI_PAGING_FLAGS_NOCACHE) ? 0 : 1;

    entry->small_page.ap0  =
        (kpi_paging_flags & KPI_PAGING_FLAGS_READ)  ? 2 : 0;
    entry->small_page.ap0 |=
        (kpi_paging_flags & KPI_PAGING_FLAGS_WRITE) ? 3 : 0;
    entry->small_page.ap1 = entry->small_page.ap0;
    entry->small_page.ap2 = entry->small_page.ap0;
    entry->small_page.ap3 = entry->small_page.ap0;
}

static errval_t
caps_map_l1(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count)
{
    //
    // Note:
    //
    // We have chicken-and-egg problem in initializing resources so
    // instead of treating an L2 table it's actual 1K size, we treat
    // it as being 4K. As a result when we map an "L2" table we actually
    // map a page of memory as if it is 4 consecutive L2 tables.
    //
    // See lib/barrelfish/arch/arm/pmap_arch.c for more discussion.
    //
    const int ARM_L1_SCALE = 4;

    if (slot >= 1024) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: slot id >= 1024");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (pte_count != 1) {
        printf("pte_count = %zu\n",(size_t)pte_count);
        panic("oops: pte_count");
        return SYS_ERR_VM_MAP_SIZE;
    }

    if (src->type != ObjType_VNode_ARM_l2) {
        panic("oops: wrong src type");
        return SYS_ERR_WRONG_MAPPING;
    }

    if (slot >= ARM_L1_OFFSET(MEMORY_OFFSET) / ARM_L1_SCALE) {
        printf("slot = %"PRIuCSLOT"\n",slot);
        panic("oops: slot id");
        return SYS_ERR_VNODE_SLOT_RESERVED;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union l1_entry* entry = (union l1_entry*)dest_lvaddr + (slot * ARM_L1_SCALE);

    // Source
    genpaddr_t src_gpaddr = get_address(src);
    lpaddr_t   src_lpaddr = gen_phys_to_local_phys(src_gpaddr);

    assert(offset == 0);
    assert(aligned(src_lpaddr, 1u << 10));
    assert((src_lpaddr < dest_lpaddr) || (src_lpaddr >= dest_lpaddr + 16384));

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr + (slot * ARM_L1_SCALE);
    src_cte->mapping_info.offset = 0;

    for (int i = 0; i < 4; i++, entry++)
    {
        entry->raw = 0;
        entry->coarse.type   = L1_TYPE_COARSE_ENTRY;
        entry->coarse.mb1    = 1;
        entry->coarse.domain = 0;
        entry->coarse.base_address =
            (src_lpaddr + i * BASE_PAGE_SIZE / ARM_L1_SCALE) >> 10;
        debug(SUBSYS_PAGING, "L1 mapping %ld. @%p = %08"PRIu32"\n",
              slot * ARM_L1_SCALE + i, entry, entry->raw);
    }

    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

static errval_t
caps_map_l2(struct capability* dest,
            cslot_t            slot,
            struct capability* src,
            uintptr_t          kpi_paging_flags,
            uintptr_t          offset,
            uintptr_t          pte_count)
{
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    // ARM L2 has 256 entries, but we treat a 4K page as a consecutive
    // region of L2 with a single index. 4K == 4 * 1K
    if (slot >= (256 * 4)) {
        panic("oops");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
        panic("oops");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if ((offset + BYTES_PER_PAGE > get_size(src)) ||
        ((offset % BYTES_PER_PAGE) != 0)) {
        panic("oops");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // check mapping does not overlap leaf page table
    if (slot + pte_count > (256 * 4)) {
        return SYS_ERR_VM_MAP_SIZE;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union l2_entry* entry = (union l2_entry*)dest_lvaddr + slot;
    if (entry->small_page.type != L2_TYPE_INVALID_PAGE) {
        panic("Remapping valid page.");
    }

    lpaddr_t src_lpaddr = gen_phys_to_local_phys(get_address(src));
    if ((src_lpaddr & (BASE_PAGE_SIZE - 1))) {
        panic("Invalid target");
    }

    struct cte *src_cte = cte_for_cap(src);
    src_cte->mapping_info.pte_count = pte_count;
    src_cte->mapping_info.pte = dest_lpaddr;
    src_cte->mapping_info.offset = offset;

    for (int i = 0; i < pte_count; i++) {
        entry->raw = 0;

        entry->small_page.type = L2_TYPE_SMALL_PAGE;
        paging_set_flags(entry, kpi_paging_flags);
        entry->small_page.base_address = src_lpaddr >> 12;

        debug(SUBSYS_PAGING, "L2 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
                dest_lvaddr, slot, entry, entry->raw);
    }

    // Flush TLB if remapping.
    cp15_invalidate_tlb();

    return SYS_ERR_OK;
}

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;

    if (ObjType_VNode_ARM_l1 == dest_cap->type) {
        return caps_map_l1(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else if (ObjType_VNode_ARM_l2 == dest_cap->type) {
        return caps_map_l2(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else {
        panic("ObjType not VNode");
    }
}

size_t do_unmap(lvaddr_t pt, cslot_t slot, size_t num_pages)
{
    size_t unmapped_pages = 0;
    union l2_entry *ptentry = (union l2_entry *)pt + slot;
    for (int i = 0; i < num_pages; i++) {
        ptentry++->raw = 0;
        unmapped_pages++;
    }
    return unmapped_pages;
}

static inline void read_pt_entry(struct capability *pgtable, size_t slot, genpaddr_t *paddr)
{
    assert(type_is_vnode(pgtable->type));
    assert(paddr);

    genpaddr_t gp = get_address(pgtable);
    lpaddr_t lp = gen_phys_to_local_phys(gp);
    lvaddr_t lv = local_phys_to_mem(lp);

    switch (pgtable->type) {
        case ObjType_VNode_ARM_l1:
        {
            union l1_entry *e = (union l1_entry*)lv;
            *paddr = (genpaddr_t)(e->coarse.base_address) << 10;
            return;
        }
        case ObjType_VNode_ARM_l2:
        {
            union l2_entry *e = (union l2_entry*)lv;
            *paddr = (genpaddr_t)(e->small_page.base_address) << 12;
            return;
        }
        default:
            assert(!"Should not get here");
    }
}

errval_t page_mappings_unmap(struct capability *pgtable, struct cte *mapping, size_t slot, size_t num_pages)
{
    assert(type_is_vnode(pgtable->type));
    //printf("page_mappings_unmap(%zd pages, slot = %zd)\n", num_pages, slot);

    // get page table entry data
    genpaddr_t paddr;
    //lpaddr_t pte;
    read_pt_entry(pgtable, slot, &paddr);
    lvaddr_t pt = local_phys_to_mem(gen_phys_to_local_phys(get_address(pgtable)));

    // get virtual address of first page
    // TODO: error checking
    genvaddr_t vaddr;
    struct cte *leaf_pt = cte_for_cap(pgtable);
    compile_vaddr(leaf_pt, slot, &vaddr);
    //genvaddr_t vend = vaddr + num_pages * BASE_PAGE_SIZE;
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
        printf("num_pages = %zu, mapping = %zu\n", num_pages, mapping->mapping_info.pte_count);
        // want to unmap a different amount of pages than was mapped
        return SYS_ERR_VM_MAP_SIZE;
    }

    do_unmap(pt, slot, num_pages);

    // flush TLB for unmapped pages
    // TODO: selective TLB flush
    cp15_invalidate_tlb();

    // update mapping info
    memset(&mapping->mapping_info, 0, sizeof(struct mapping_info));

    return SYS_ERR_OK;
}

errval_t paging_modify_flags(struct capability *frame, uintptr_t offset,
                             uintptr_t pages, uintptr_t kpi_paging_flags)
{
    // check flags
    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    struct cte *mapping = cte_for_cap(frame);
    struct mapping_info *info = &mapping->mapping_info;

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = info->pte + offset;

    for (int i = 0; i < pages; i++) {
        union l2_entry *entry =
            (union l2_entry *)base + i;
        paging_set_flags(entry, kpi_paging_flags);
    }

    return SYS_ERR_OK;
}

void paging_dump_tables(struct dcb *dispatcher)
{
    printf("dump_hw_page_tables\n");
    lvaddr_t l1 = local_phys_to_mem(dispatcher->vspace);

    for (int l1_index = 0; l1_index < ARM_L1_MAX_ENTRIES; l1_index++) {
        // get level2 table
        union l1_entry *l2 = (union l1_entry *)l1 + l1_index;
        if (!l2->raw) { continue; }
        genpaddr_t ptable_gp = (genpaddr_t)(l2->coarse.base_address) << 10;
        lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

        for (int entry = 0; entry < ARM_L2_MAX_ENTRIES; entry++) {
            union l2_entry *e =
                (union l2_entry *)ptable_lv + entry;
            genpaddr_t paddr = (genpaddr_t)(e->small_page.base_address) << BASE_PAGE_BITS;
            if (!paddr) {
                continue;
            }
            printf("%d.%d: 0x%"PRIxGENPADDR"\n", l1_index, entry, paddr);
        }
    }
}
