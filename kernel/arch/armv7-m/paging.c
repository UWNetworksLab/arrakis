/*
 * Copyright (c) 2009 - 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <paging_kernel_arch.h>
#include <string.h>
#include <exceptions.h>
#include <arm_hal.h>
#include <cap_predicates.h>
#include <dispatch.h>
#include <dev/omap/omap44xx_mmu_dev.h>
#include <omap44xx_map.h>

/**
 * Kernel L1 page table
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 16K after relocation
static union arm_l1_entry kernel_l1_table[2*ARM_L1_MAX_ENTRIES]
__attribute__((aligned(ARM_L1_ALIGN)));
static union arm_l1_entry *aligned_kernel_l1_table;

#if 0
/**
 * Kernel L2 page table for first MB
 */
//XXX: We reserve double the space needed to be able to align the pagetable
//     to 1K after relocation
static union arm_l2_entry low_l2_table[2*ARM_L2_MAX_ENTRIES]
__attribute__((aligned(ARM_L2_ALIGN)));
static union arm_l2_entry *aligned_low_l2_table;
#endif // 0

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

/*
 * \brief read the version number of the table (called ignored3), to see if the upper half 
 * of the table has been modified and needs to be replicated. 
 * These version numbers are necessary for paging_context_switch, because
 * the upper half of the table is expected to be preserved
 */
inline uint8_t read_table_version(union arm_l1_entry* ttb){
    return ttb->section.ignored3;
} 

inline void write_table_version(union arm_l1_entry* ttb, uint8_t version){
    ttb->section.ignored3 = version;
}

//call this whenever the upper half of a page table is modified
inline void increase_table_version(union arm_l1_entry* ttb){
    if (ttb == NULL){//read current TTB, so the caller does not have to
    //assumes the virtual and physical address are the same (should always hold for kernel)
        ttb = (union arm_l1_entry*) omap44xx_mmu_ttb_rd(&mmu);
    } 
    write_table_version(ttb, read_table_version(ttb)+1);
}

static void
paging_write_l1_entry(uintptr_t ttbase, lvaddr_t va, union arm_l1_entry l1)
{
    union arm_l1_entry *l1_table;
    if (ttbase == 0) {//read current TTB, so the caller does not have to
        ttbase = omap44xx_mmu_ttb_rd(&mmu);
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
    //XXX: we currently ignore all paging flags
    /*
    l1.section.bufferable   = 1;
    l1.section.cacheable    = 1;
    l1.section.ap10         = 1;    // RW/NA
    l1.section.ap2          = 0;
    */
    l1.section.base_address = pa >> 20u;

    paging_write_l1_entry(ttbase, va, l1);
}

void paging_map_memory(uintptr_t ttbase, lpaddr_t paddr, size_t bytes)
{
    lpaddr_t pend  = paging_round_up(paddr + bytes, BYTES_PER_SECTION);
    while (paddr < pend) {
        paging_map_kernel_section(ttbase, paddr, paddr);
        paddr += BYTES_PER_SECTION;
    }
}

void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa);

void
paging_map_device_section(uintptr_t ttbase, lvaddr_t va, lpaddr_t pa)
{
    union arm_l1_entry l1;

    l1.raw = 0;
    l1.section.type = L1_TYPE_SECTION_ENTRY;
    //XXX: we currently ignore all paging flags
    /*
    l1.section.bufferable   = 0;
    l1.section.cacheable    = 0;
    l1.section.ap10         = 3; // prev value: 3 // RW/NA RW/RW
    l1.section.ap2        = 0;
    */
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

    //printf("paging_map_device_section: 0x%"PRIxLVADDR", 0x%"PRIxLVADDR", "
    //        "0x%"PRIxLPADDR".\n",
    //        (uintptr_t)aligned_kernel_l1_table, dev_alloc, device_base);

    paging_map_device_section((uintptr_t)aligned_kernel_l1_table, dev_alloc,
            device_base);

    increase_table_version(NULL);
    
    //do_full_tlb_flush();//probably not needed: the newly mapped address should not have 
    //ever been accessed before

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
    omap44xx_mmu_initialize(&mmu, (mackerel_addr_t) OMAP44XX_MAP_M3_L2MMU);//base address for mmu
    printf("MMU revision: 0x%x\n", omap44xx_mmu_revision_rd(&mmu));
    // make sure kernel pagetable is aligned to 16K after relocation
    aligned_kernel_l1_table = (union arm_l1_entry *)ROUND_UP(
            (uintptr_t)kernel_l1_table, ARM_L1_ALIGN);
    // Re-map physical memory
    //
    paging_map_memory((uintptr_t)aligned_kernel_l1_table , paddr, bytes);

    //because the kernel is mapped 1:1 (virtual = physical address),
    //we can write the TTB easily
    omap44xx_mmu_ttb_wr(&mmu, (uint32_t) aligned_kernel_l1_table);

    omap44xx_mmu_cntl_tlw_wrf(&mmu, 1);
    printf("Table walking enabled.\n");
    
    increase_table_version(NULL);//mark current table as changed
    
    //make all MMU errors to bus faults
    omap44xx_mmu_gp_reg_bus_error_back_en_wrf(&mmu, 1);
    
    //now that we can use the page table, we can map devices into high memory
    //sadly, the MMU can not be remapped, as it is accessed by its virtual(!) address
    
    //XXX: DO NOT INVALIDATE TLB YET, because devices like serial are not remapped yet
   //do not even set the lock basevalue down yet
}


/*
 * Describe me
 */
//If we use table versioning, then this seems superfluous -> ignore for the moment
void paging_make_good(lvaddr_t new_table_base, size_t new_table_bytes)
{
printf("paging_make_good called, ignored.\n");
    assert(new_table_base >= MEMORY_OFFSET);
    assert(new_table_bytes == ARM_L1_ALIGN);
    assert(aligned(new_table_base, ARM_L1_ALIGN));
/*
    lvaddr_t ttbr = local_phys_to_mem(omap44xx_mmu_ttb_rd(&mmu));
    size_t st = (MEMORY_OFFSET / ARM_L1_SECTION_BYTES) * ARM_L1_BYTES_PER_ENTRY;

    // Copy kernel pages (everything from MEMORY_OFFSET upwards)
    memcpy((void*)new_table_base + st, (void*)ttbr + st,
           ARM_L1_MAX_ENTRIES * ARM_L1_BYTES_PER_ENTRY - st);
*/
}

void paging_map_user_pages_l1(lvaddr_t table_base, lvaddr_t va, lpaddr_t pa)
{
    assert(aligned(table_base, ARM_L1_ALIGN));
    assert(aligned(pa, BYTES_PER_SMALL_PAGE));

    union arm_l1_entry e;

    e.raw                 = 0;
    e.page_table.type         = L1_TYPE_PAGE_TABLE_ENTRY;
//    e.page_table.domain       = 0;//there is no such thing in armv7-m L2 MMU
    e.page_table.base_address = (pa >> 10);

    paging_write_l1_entry(table_base, va, e);
}

void paging_set_l2_entry(uintptr_t* l2e, lpaddr_t addr, uintptr_t flags)
{
//    assert(0 == (flags & 0xfffff000));
//    assert(0 == (flags & 0x3));
    assert(0 == (addr & 0xfff));

    union arm_l2_entry e;
//    e.raw = flags;

    e.small_page.type = L2_TYPE_SMALL_PAGE;
    e.small_page.base_address = (addr >> 12);

    *l2e = e.raw;
}

void paging_context_switch(lpaddr_t ttbr)
{
//    printf("paging context switch to %"PRIxLPADDR"\n", ttbr);
    lpaddr_t old_ttbr = omap44xx_mmu_ttb_rd(&mmu);
    if (ttbr != old_ttbr) {
        //we currently do table versioning, to make sure we always have the most recent
        //kernel region mapping
        uint8_t version_current = read_table_version((union arm_l1_entry*) old_ttbr);
        uint8_t version_replacement = read_table_version((union arm_l1_entry*) ttbr);

        if (version_current > version_replacement){
        //the kernel part of the new table is not up to date -> replicate
        printf("current TTB version: %hhd, replacement version %hhd \n", version_current, version_replacement);
            printf("replicating upper half of page table\n");
            //something in the upper half of the page table has changed -> copy changes
            //copy full upper half of page table. this is very coarse grained, but should be enough for the moment
            memcpy((void*)ttbr+(ARM_L1_BYTES_PER_ENTRY*ARM_L1_MAX_ENTRIES/2), 
                (void*)old_ttbr+(ARM_L1_BYTES_PER_ENTRY*ARM_L1_MAX_ENTRIES/2),
                (ARM_L1_BYTES_PER_ENTRY*ARM_L1_MAX_ENTRIES/2));
            write_table_version((union arm_l1_entry*) ttbr, version_current);
        }
        omap44xx_mmu_ttb_wr(&mmu, ttbr);
        //XXX: cachemarker: flush cache here (including cache MMU)
        do_full_tlb_flush();
    }
}

static void
paging_set_flags(union arm_l2_entry *entry, uintptr_t kpi_paging_flags)
{
//XXX: we currently ignore all flags - we would need the cache MMU to enforce them,
// and the L2 MMU completely ignores them
/*
        entry->small_page.bufferable = 1;
        entry->small_page.cacheable =
            (kpi_paging_flags & KPI_PAGING_FLAGS_NOCACHE) ? 0 : 1;
        entry->small_page.ap10  =
            (kpi_paging_flags & KPI_PAGING_FLAGS_READ)  ? 2 : 0;
        entry->small_page.ap10 |=
            (kpi_paging_flags & KPI_PAGING_FLAGS_WRITE) ? 3 : 0;
        entry->small_page.ap2 = 0;
*/
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

    union arm_l1_entry* entry = (union arm_l1_entry*)dest_lvaddr + (slot * ARM_L1_SCALE);

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
        entry->page_table.type   = L1_TYPE_PAGE_TABLE_ENTRY;
//        entry->page_table.domain = 0;//no such thing on ermv7-m
        entry->page_table.base_address =
            (src_lpaddr + i * BASE_PAGE_SIZE / ARM_L1_SCALE) >> 10;
        debug(SUBSYS_PAGING, "L1 mapping %"PRIuCSLOT". @%p = %08"PRIx32"\n",
              slot * ARM_L1_SCALE + i, entry, entry->raw);
    }

    do_full_tlb_flush();

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
        panic("oops: slot >= (256 * 4)");
        return SYS_ERR_VNODE_SLOT_INVALID;
    }

    if (src->type != ObjType_Frame && src->type != ObjType_DevFrame) {
        panic("oops: src->type != ObjType_Frame && src->type != ObjType_DevFrame");
        return SYS_ERR_WRONG_MAPPING;
    }

    // check offset within frame
    if ((offset + BYTES_PER_PAGE > get_size(src)) ||
        ((offset % BYTES_PER_PAGE) != 0)) {
        panic("oops: frame offset invalid");
        return SYS_ERR_FRAME_OFFSET_INVALID;
    }

    // check mapping does not overlap leaf page table
    if (slot + pte_count > (256 * 4)) {
        return SYS_ERR_VM_MAP_SIZE;
    }

    // Destination
    lpaddr_t dest_lpaddr = gen_phys_to_local_phys(get_address(dest));
    lvaddr_t dest_lvaddr = local_phys_to_mem(dest_lpaddr);

    union arm_l2_entry* entry = (union arm_l2_entry*)dest_lvaddr + slot;
    if (entry->small_page.type != L2_TYPE_INVALID_PAGE) {
        panic("Remapping valid page.");
    }

    lpaddr_t src_lpaddr = gen_phys_to_local_phys(get_address(src) + offset);
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
        entry->small_page.base_address = (src_lpaddr + i * BYTES_PER_PAGE) >> 12;

        entry++;

        debug(SUBSYS_PAGING, "L2 mapping %08"PRIxLVADDR"[%"PRIuCSLOT"] @%p = %08"PRIx32"\n",
               dest_lvaddr, slot, entry, entry->raw);
    }

    // Flush TLB if remapping.
    do_full_tlb_flush();

    return SYS_ERR_OK;
}

/// Create page mappings
errval_t caps_copy_to_vnode(struct cte *dest_vnode_cte, cslot_t dest_slot,
                            struct cte *src_cte, uintptr_t flags,
                            uintptr_t offset, uintptr_t pte_count)
{
    struct capability *src_cap  = &src_cte->cap;
    struct capability *dest_cap = &dest_vnode_cte->cap;

    if (src_cte->mapping_info.pte) {
        return SYS_ERR_VM_ALREADY_MAPPED;
    }

    if (ObjType_VNode_ARM_l1 == dest_cap->type) {
        //printf("caps_map_l1: %zu\n", (size_t)pte_count);
        return caps_map_l1(dest_cap, dest_slot, src_cap,
                           flags,
                           offset,
                           pte_count
                          );
    }
    else if (ObjType_VNode_ARM_l2 == dest_cap->type) {
        //printf("caps_map_l2: %zu\n", (size_t)pte_count);
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
    union arm_l2_entry *ptentry = (union arm_l2_entry *)pt + slot;
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
            union arm_l1_entry *e = (union arm_l1_entry*)lv;
            *paddr = (genpaddr_t)(e->page_table.base_address) << 10;
            return;
        }
        case ObjType_VNode_ARM_l2:
        {
            union arm_l2_entry *e = (union arm_l2_entry*)lv;
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
    do_full_tlb_flush();

    // update mapping info
    memset(&mapping->mapping_info, 0, sizeof(struct mapping_info));

    return SYS_ERR_OK;
}

errval_t paging_modify_flags(struct capability *frame, uintptr_t offset,
                             uintptr_t pages, uintptr_t kpi_paging_flags)
{
    // we currently ignore all permission flags
//    assert(0 == (kpi_paging_flags & ~KPI_PAGING_FLAGS_MASK));

    struct cte *mapping = cte_for_cap(frame);
    struct mapping_info *info = &mapping->mapping_info;

    /* Calculate location of page table entries we need to modify */
    lvaddr_t base = local_phys_to_mem(info->pte) + offset;

    for (int i = 0; i < pages; i++) {
        union arm_l2_entry *entry =
            (union arm_l2_entry *)base + i;
        paging_set_flags(entry, kpi_paging_flags);
    }

    return paging_tlb_flush_range(mapping, pages);
}

void paging_dump_tables(struct dcb *dispatcher)
{
    printf("dump_hw_page_tables\n");
    lvaddr_t l1 = local_phys_to_mem(dispatcher->vspace);

    for (int l1_index = 0; l1_index < ARM_L1_MAX_ENTRIES; l1_index++) {
        // get level2 table
        union arm_l1_entry *l1_e = (union arm_l1_entry *)l1 + l1_index;
        if (!l1_e->raw) { continue; }
        genpaddr_t ptable_gp = (genpaddr_t)(l1_e->page_table.base_address) << 10;
        lvaddr_t ptable_lv = local_phys_to_mem(gen_phys_to_local_phys(ptable_gp));

        for (int entry = 0; entry < ARM_L2_MAX_ENTRIES; entry++) {
            union arm_l2_entry *e =
                (union arm_l2_entry *)ptable_lv + entry;
            genpaddr_t paddr = (genpaddr_t)(e->small_page.base_address) << BASE_PAGE_BITS;
            if (!paddr) {
                continue;
            }
            printf("%d.%d: 0x%"PRIxGENPADDR"\n", l1_index, entry, paddr);
        }
    }
}

/*
 * Cortex-M3 on pandaboard specific stuff
 * since the M3 memory model on the pandaboard is a bit weird, we need a few additional functions
 * some of these would be provided by a cp15, but we don't have one
 */

void do_one_tlb_flush(genvaddr_t vaddr){
    omap44xx_mmu_cam_virtual_tag_wrf(&mmu, vaddr>>12);
    omap44xx_mmu_flush_entry_flush_entry_wrf(&mmu,1);//flush all entries corresponding to CAM
}

void do_full_tlb_flush(void){
    omap44xx_mmu_gflush_global_flush_wrf(&mmu,1);//flushes all non-protected entries
}


/*
 * \brief provided here so we can use it in init.c, after we mapped all devices
 */
inline void set_tlb_lock_basevalue(uint8_t basevalue){
    omap44xx_mmu_lock_basevalue_wrf(&mmu, basevalue);
    printf("set TLB lock base to %hhd\n", basevalue);
}

/*
 * \brief add another (protected) entry into L2 TLB
 * size: 0 -> section (1MB)
 *       1 -> large page (64KB)
 *       2 -> small page (4KB)
 *       3 -> supersection (16MB)
 *
 */
void add_tlb_mapping(lvaddr_t vaddr, lpaddr_t paddr, bool preserved, uint8_t size){
    uint8_t lockbase = omap44xx_mmu_lock_basevalue_rdf(&mmu);
//    printf("add_tlb_mapping: lockbase: %hhd\n", lockbase);
    
    omap44xx_mmu_cam_virtual_tag_wrf(&mmu, vaddr>>12);
    omap44xx_mmu_cam_preserved_wrf(&mmu, preserved);
    omap44xx_mmu_cam_page_size_wrf(&mmu, (omap44xx_mmu_page_size_t) size);
    omap44xx_mmu_cam_valid_wrf(&mmu, 1);
    
    omap44xx_mmu_ram_physical_address_wrf(&mmu, paddr>>12);
    omap44xx_mmu_ram_endianness_wrf(&mmu, (omap44xx_mmu_page_endianness_t) 0);
    omap44xx_mmu_ram_element_size_wrf(&mmu, (omap44xx_mmu_page_element_size_t) 3);
    
    printf("flushing all previous mappings for virtual address 0x%x\n", (uint32_t) vaddr);
    omap44xx_mmu_flush_entry_flush_entry_wrf(&mmu,1);//flush all entries corresponding to CAM
    
    uint32_t cam_temp = omap44xx_mmu_cam_rd(&mmu);//temporarily store the CAM we later want to write
    //flush all entries corresponding to larger regions containing ours
    omap44xx_mmu_cam_virtual_tag_wrf(&mmu, (vaddr & (~LARGE_PAGE_MASK))>>12);
    omap44xx_mmu_flush_entry_flush_entry_wrf(&mmu,1);//flush all entries corresponding to CAM
    omap44xx_mmu_cam_virtual_tag_wrf(&mmu, (vaddr & (~ARM_L1_SECTION_MASK))>>12);
    omap44xx_mmu_flush_entry_flush_entry_wrf(&mmu,1);//flush all entries corresponding to CAM
    omap44xx_mmu_cam_virtual_tag_wrf(&mmu, (vaddr & (~ARM_L1_SUPERSECTION_MASK))>>12);
    omap44xx_mmu_flush_entry_flush_entry_wrf(&mmu,1);//flush all entries corresponding to CAM
    

    omap44xx_mmu_cam_wr(&mmu, cam_temp);
    
    //make sure we overwrite the next contiguous entry
    omap44xx_mmu_lock_current_victim_wrf(&mmu, lockbase);

    omap44xx_mmu_ld_tlb_wr(&mmu, (omap44xx_mmu_ld_tlb_t) 1);//actually load entry into TLB
    printf("TLB entry overwritten\n");
    omap44xx_mmu_lock_basevalue_wrf(&mmu, lockbase + 1);//lock our new entry
}
