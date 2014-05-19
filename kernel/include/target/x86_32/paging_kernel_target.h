/**
 * \file
 * \brief x86-32 kernel page-table structures.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_TARGET_X86_32_PAGING_H
#define KERNEL_TARGET_X86_32_PAGING_H

#include <capabilities.h>
#include <barrelfish_kpi/paging_arch.h>

// Functions defined elsewhere. Move the declerations to appropriate includes
int paging_x86_32_map_memory(lpaddr_t base, size_t size);
lvaddr_t paging_x86_32_map_special(lpaddr_t base, size_t size, uint64_t bitmap);
lvaddr_t paging_x86_32_map_device(lpaddr_t base, size_t size);

#ifdef CONFIG_PAE
void paging_x86_32_make_good_pdpte(lpaddr_t base);
#else
void paging_x86_32_make_good_pdir(lpaddr_t base);
#endif

void paging_x86_32_reset(void);

/// All arch-specific flags valid to be set from user-space
#define X86_32_PTABLE_FLAGS_MASK \
    (X86_32_PTABLE_GLOBAL_PAGE | X86_32_PTABLE_ATTR_INDEX | \
     X86_32_PTABLE_DIRTY | PTABLE_ACCESSED |                \
     PTABLE_CACHE_DISABLED | PTABLE_WRITE_THROUGH)

/// All flags valid for page access protection from user-space
#define X86_32_PTABLE_ACCESS_MASK \
    (X86_32_PTABLE_EXECUTE_DISABLE | X86_32_PTABLE_USER_SUPERVISOR | \
     X86_32_PTABLE_READ_WRITE)

/// Mask out all arch-specific flags except those valid from user-space
#define X86_32_PTABLE_FLAGS(flags)     (flags & X86_32_PTABLE_FLAGS_MASK)

/// Mask out all flags except those for access protection
#define X86_32_PTABLE_ACCESS(flags)    (flags & X86_32_PTABLE_ACCESS_MASK)

#ifdef CONFIG_PAE

/** True if page entry is present in memory */
#define X86_32_IS_PRESENT(entry)                        \
    ((*(uint64_t *)(entry)) & X86_32_PTABLE_PRESENT)

#else

/** True if page entry is present in memory */
#define X86_32_IS_PRESENT(entry)                        \
    ((*(uint32_t *)(entry)) & X86_32_PTABLE_PRESENT)

#endif

#ifdef CONFIG_PAE

union x86_32_pdpte_entry {
    uint64_t raw;
    struct {
        uint64_t        present         :1;
        uint64_t        reserved0       :2;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        reserved        :4;
        uint64_t        ignored         :3;
        uint64_t        base_addr       :52;
    } d;
};

/**
 * A page directory entry.
 */
union x86_32_pdir_entry {
    uint64_t raw;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        reserved        :3;
        uint64_t        available       :3;
        uint64_t        base_addr       :28;
        uint64_t        reserved2       :12;    // XXX: Part of base_addr
        uint64_t        reserved3       :11;
        uint64_t        execute_disable :1;
    } d;
};

/**
 * A page table entry.
 */
union x86_32_ptable_entry {
    uint64_t raw;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        dirty           :1;
        uint64_t        always1         :1;
        uint64_t        global          :1;
        uint64_t        available       :3;
        uint64_t        attr_index      :1;
        uint64_t        reserved        :8;
        uint64_t        base_addr       :42;
        uint64_t        execute_disable :1;
    } large;
    struct {
        uint64_t        present         :1;
        uint64_t        read_write      :1;
        uint64_t        user_supervisor :1;
        uint64_t        write_through   :1;
        uint64_t        cache_disabled  :1;
        uint64_t        accessed        :1;
        uint64_t        dirty           :1;
        uint64_t        attr_index      :1;
        uint64_t        global          :1;
        uint64_t        reserved        :3;
        uint64_t        base_addr       :51;
        uint64_t        execute_disable :1;
    } base;
};

#else

/**
 * A page directory entry.
 */
union x86_32_pdir_entry {
    uint32_t raw;
    struct {
        uint32_t        present         :1;
        uint32_t        read_write      :1;
        uint32_t        user_supervisor :1;
        uint32_t        write_through   :1;
        uint32_t        cache_disabled  :1;
        uint32_t        accessed        :1;
        uint32_t        available       :1;
        uint32_t        always0         :1;
        uint32_t        reserved        :4;
        uint32_t        base_addr       :20;
    } d;
};

/**
 * A page table entry.
 */
union x86_32_ptable_entry {
    uint32_t raw;
    struct {
        uint32_t        present         :1;
        uint32_t        read_write      :1;
        uint32_t        user_supervisor :1;
        uint32_t        write_through   :1;
        uint32_t        cache_disabled  :1;
        uint32_t        accessed        :1;
        uint32_t        dirty           :1;
        uint32_t        always1         :1;
        uint32_t        global          :1;
        uint32_t        available       :3;
        uint32_t        attr_index      :1;
        uint32_t        reserved        :9;
        uint32_t        base_addr       :10;
    } large;
    struct {
        uint32_t        present         :1;
        uint32_t        read_write      :1;
        uint32_t        user_supervisor :1;
        uint32_t        write_through   :1;
        uint32_t        cache_disabled  :1;
        uint32_t        accessed        :1;
        uint32_t        dirty           :1;
        uint32_t        attr_index      :1;
        uint32_t        global          :1;
        uint32_t        available       :3;
        uint32_t        base_addr       :20;
    } base;
};

#endif

/**
 * \brief Clear page directory.
 *
 * Clears page directory pointed to by 'p'.
 *
 * \param p     Pointer to page directory to clear.
 */
static inline void paging_x86_32_clear_pdir(union x86_32_pdir_entry * COUNT(X86_32_PTABLE_SIZE)
                                            NONNULL p)
{
    for (int i = 0; i < X86_32_PTABLE_SIZE; i++) {
        p[i].raw = X86_32_PTABLE_CLEAR;
    }
}

/**
 * \brief Clear page table.
 *
 * Clears page table pointed to by 'p'.
 *
 * \param p     Pointer to page table to clear.
 */
static inline void paging_x86_32_clear_ptable(union x86_32_ptable_entry * COUNT(X86_32_PTABLE_SIZE)
                                              NONNULL p)
{
    for (int i = 0; i < X86_32_PTABLE_SIZE; i++) {
        p[i].raw = X86_32_PTABLE_CLEAR;
    }
}

/**
 * \brief Maps from page directory entry to page directory/table.
 *
 * Maps page directory or table, based at 'base', from page directory entry
 * pointed to by 'entry'.
 *
 * \param entry Pointer to page directory entry to point from.
 * \param base  Base virtual address of page directory/table to point to.
 */
static inline void paging_x86_32_map_table(union x86_32_pdir_entry * NONNULL entry,
                                           lpaddr_t base)
{
    union x86_32_pdir_entry tmp;
    tmp.raw = PTABLE_CLEAR;

    tmp.d.present = 1;
    tmp.d.read_write = 1;
    tmp.d.user_supervisor = 1;
    tmp.d.base_addr = base >> 12;

    *entry = tmp;
}

#ifdef CONFIG_PAE
static inline void paging_x86_32_map_pdpte(union x86_32_pdpte_entry *entry,
                                           lpaddr_t base)
{
    union x86_32_pdpte_entry tmp;
    tmp.raw = X86_32_PTABLE_CLEAR;

    tmp.d.present = 1;
    tmp.d.base_addr = base >> 12;

    *entry = tmp;
}
#endif

/**
 * \brief Maps a large page.
 *
 * From large page table entry, pointed to by 'entry', maps physical address
 * 'base' with page attribute bitmap 'bitmap'.
 *
 * \param entry         Pointer to page table entry to map from.
 * \param base          Physical address to map to (will be page-aligned).
 * \param bitmap        Bitmap to apply to page attributes.
 */
static inline void paging_x86_32_map_large(union x86_32_ptable_entry * NONNULL entry,
                                           lpaddr_t base,
                                           paging_x86_32_flags_t bitmap)
{
    union x86_32_ptable_entry tmp;
    tmp.raw = X86_32_PTABLE_CLEAR;

    tmp.large.present = bitmap & X86_32_PTABLE_PRESENT ? 1 : 0;
    tmp.large.read_write = bitmap & X86_32_PTABLE_READ_WRITE ? 1 : 0;
    tmp.large.user_supervisor = bitmap & X86_32_PTABLE_USER_SUPERVISOR ? 1 : 0;
    tmp.large.write_through = bitmap & X86_32_PTABLE_WRITE_THROUGH ? 1 : 0;
    tmp.large.cache_disabled = bitmap & X86_32_PTABLE_CACHE_DISABLED ? 1 : 0;
    tmp.large.global = bitmap & X86_32_PTABLE_GLOBAL_PAGE ? 1 : 0;
    tmp.large.attr_index = bitmap & X86_32_PTABLE_ATTR_INDEX ? 1 : 0;
#ifdef CONFIG_NXE
    tmp.large.execute_disable = bitmap & X86_32_PTABLE_EXECUTE_DISABLE ? 1 : 0;
#endif
    tmp.large.always1 = 1;
#ifdef CONFIG_PAE
    tmp.large.base_addr = base >> 21;
#else
    tmp.large.base_addr = base >> 22;
#endif

    *entry = tmp;
}

/**
 * \brief Maps a normal (small) page.
 *
 * From small page table entry, pointed to by 'entry', maps physical address
 * 'base' with page attribute bitmap 'bitmap'.
 *
 * \param entry         Pointer to page table entry to map from.
 * \param base          Physical address to map to (will be page-aligned).
 * \param bitmap        Bitmap to apply to page attributes.
 */
static inline void paging_x86_32_map(union x86_32_ptable_entry * NONNULL entry,
                                     lpaddr_t base,
                                     paging_x86_32_flags_t bitmap)
{
    union x86_32_ptable_entry tmp;
    tmp.raw = X86_32_PTABLE_CLEAR;

    tmp.base.present = bitmap & X86_32_PTABLE_PRESENT ? 1 : 0;
    tmp.base.read_write = bitmap & X86_32_PTABLE_READ_WRITE ? 1 : 0;
    tmp.base.user_supervisor = bitmap & X86_32_PTABLE_USER_SUPERVISOR ? 1 : 0;
    tmp.base.write_through = bitmap & X86_32_PTABLE_WRITE_THROUGH ? 1 : 0;
    tmp.base.cache_disabled = bitmap & X86_32_PTABLE_CACHE_DISABLED ? 1 : 0;
    tmp.base.attr_index = bitmap & X86_32_PTABLE_ATTR_INDEX ? 1 : 0;
    tmp.base.global = bitmap & X86_32_PTABLE_GLOBAL_PAGE ? 1 : 0;
#ifdef CONFIG_NXE
    tmp.base.execute_disable = bitmap & X86_32_PTABLE_EXECUTE_DISABLE ? 1 : 0;
#endif
    tmp.base.base_addr = base >> 12;

    *entry = tmp;
}

/**
 * \brief Modify flags of a normal (small) page.
 *
 * From small page table entry, pointed to by 'entry', maps physical address
 * 'base' with page attribute bitmap 'bitmap'.
 *
 * \param entry         Pointer to page table entry to map from.
 * \param bitmap        Bitmap to apply to page attributes.
 */
static inline void paging_x86_32_modify_flags(union x86_32_ptable_entry * NONNULL entry,
                                              paging_x86_32_flags_t bitmap)
{
    union x86_32_ptable_entry tmp;

    tmp.base.present = bitmap & X86_32_PTABLE_PRESENT ? 1 : 0;
    tmp.base.read_write = bitmap & X86_32_PTABLE_READ_WRITE ? 1 : 0;
    tmp.base.user_supervisor = bitmap & X86_32_PTABLE_USER_SUPERVISOR ? 1 : 0;
    tmp.base.write_through = bitmap & X86_32_PTABLE_WRITE_THROUGH ? 1 : 0;
    tmp.base.cache_disabled = bitmap & X86_32_PTABLE_CACHE_DISABLED ? 1 : 0;
    tmp.base.attr_index = bitmap & X86_32_PTABLE_ATTR_INDEX ? 1 : 0;
    tmp.base.global = bitmap & X86_32_PTABLE_GLOBAL_PAGE ? 1 : 0;
#ifdef CONFIG_NXE
    tmp.base.execute_disable = bitmap & X86_32_PTABLE_EXECUTE_DISABLE ? 1 : 0;
#endif

    *entry = tmp;
}

static inline void paging_x86_32_unmap(union x86_32_ptable_entry * NONNULL entry)
{
    entry->raw = X86_32_PTABLE_CLEAR;
}

/**
 * \brief Convert Capability access rights to X86-64 page flags.
 *
 * Returns corresponding X86-64 page flags to given capability access rights
 * mask 'rights'.
 *
 * \param rights        Capability rightsmask.
 *
 * \return X86-64 page flags.
 */
static inline uint64_t paging_x86_32_cap_to_page_flags(CapRights rights)
{
    uint64_t pageflags = 0;

    // Sanity-check given flags
    if(!(rights & CAPRIGHTS_READ) &&
       (rights & CAPRIGHTS_WRITE || rights & CAPRIGHTS_EXECUTE)) {
        printk(LOG_ERR, "Page mapped writable and/or executable, but not "
               "readable. Impossible on X86! Will map non-everything "
               "instead.\n");
    }

    // Convert flags
    pageflags |= rights & CAPRIGHTS_READ ? X86_32_PTABLE_USER_SUPERVISOR : 0;
    pageflags |= rights & CAPRIGHTS_WRITE ? X86_32_PTABLE_READ_WRITE : 0;
    pageflags |= rights & CAPRIGHTS_EXECUTE ? 0 : X86_32_PTABLE_EXECUTE_DISABLE;

    return pageflags;
}

/**
 * \brief Switch context.
 *
 * Assigns given physical base address of PDPTE/PDE 'cr3' to the CR3
 * register, effectively switching context to new address space. Be
 * cautious that you only switch to "good" (as explained in
 * paging_make_good_pdpte()/paging_make_good_pdir()) PDPTEs/PDEs!
 *
 * \param pdpte  Physical base address of PDPTE/PDE table.
 */
static void inline paging_x86_32_context_switch(lpaddr_t cr3)
{
    __asm volatile("mov %[cr3], %%cr3"
                   : /* No output */
                   :
                   [cr3] "r" (cr3)
                   );
}

/**
 * \brief Mask out page attributes.
 *
 * Masks out all attributes and access rights from 'attr' according to
 * 'mask'. This is architecture-specific. On x86-64, except for the
 * execute disable attribute, rights are given by setting a
 * corresponding bit. Thus, setting that bit within 'mask' to zero,
 * masks out the right. For the execute disable bit, the right is
 * masked out when the bit is set, so the mask works the other way
 * around in this case: When the bit is set in 'mask', but not set in
 * 'attr', it will be set in the return value, so mask-out behavior is
 * preserved.
 *
 * \param attr  The page attributes to mask.
 * \param mask  Mask for the page attributes.
 *
 * \return Masked version of 'attr'.
 */
static inline uint64_t paging_x86_32_mask_attrs(uint64_t attr, uint64_t mask)
{
    // First, mask out all "bit-sets-enabled" attributes
    attr &= mask | X86_32_PTABLE_EXECUTE_DISABLE;

    // Now, mask out all "bit-sets-disabled" attributes
    attr |= mask & X86_32_PTABLE_EXECUTE_DISABLE;

    return attr;
}

#endif // KERNEL_TARGET_X86_32_PAGING_H
