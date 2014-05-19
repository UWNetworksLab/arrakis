/**
 * \file
 * \brief x86-32 kernel page-table setup
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>
#include <dispatch.h>

/*
 * Page attribute bitmaps for various address spaces.
 */
#define MEM_PAGE_BITMAP                                 \
    (X86_32_PTABLE_PRESENT | X86_32_PTABLE_READ_WRITE | \
     X86_32_PTABLE_GLOBAL_PAGE)
#define DEVICE_PAGE_BITMAP                              \
    (X86_32_PTABLE_PRESENT | X86_32_PTABLE_READ_WRITE | \
     X86_32_PTABLE_CACHE_DISABLED | PTABLE_GLOBAL_PAGE)

#ifdef CONFIG_PAE

/*
 * Table requirements for various address spaces.
 */
#define MEM_PDIR_SIZE           X86_32_PDIR_ENTRIES(X86_32_PADDR_SPACE_LIMIT)

/**
 * Kernel PDPTE table.
 */
static union x86_32_pdpte_entry pdpte[X86_32_PDPTE_SIZE]
__attribute__((aligned(X86_32_BASE_PAGE_SIZE)));

/**
 * Page directory for physical memory address space.
 */
static union x86_32_ptable_entry mem_pdir[MEM_PDIR_SIZE][X86_32_PTABLE_SIZE]
__attribute__((aligned(X86_32_BASE_PAGE_SIZE)));

static inline void mapit(union x86_32_pdpte_entry *pdpte_base,
                         union x86_32_ptable_entry *pdir_base, lpaddr_t addr,
                         uint64_t bitmap)
{
    if(!X86_32_IS_PRESENT(pdpte_base)) {
        paging_x86_32_map_pdpte(pdpte_base,
                                mem_to_local_phys((lvaddr_t)pdir_base));
    }

    if(!X86_32_IS_PRESENT(pdir_base)) {
        debug(SUBSYS_PAGING, "mapped!\n");
    } else {
        //remap the page anyway, this is important for the memory latency benchmark
        debug(SUBSYS_PAGING, "already existing! remapping it\n");
    }

    paging_x86_32_map_large(pdir_base, addr, bitmap);
}

#else

#       ifdef CONFIG_PSE

/**
 * Page directory for physical memory address space.
 */
static union x86_32_ptable_entry pdir[X86_32_PTABLE_SIZE]
__attribute__((aligned(X86_32_BASE_PAGE_SIZE)));

static inline void mapit(union x86_32_ptable_entry *pdir_base, lpaddr_t addr,
                         uint64_t bitmap)
{
    if(!X86_32_IS_PRESENT(pdir_base)) {
        debug(SUBSYS_PAGING, "mapped!\n");
    } else {
        //remap the page anyway, this is important for the memory latency benchmark
        debug(SUBSYS_PAGING, "already existing! remapping it\n");
    }

    paging_x86_32_map_large(pdir_base, addr, bitmap);
}

#       else

/**
 * Page directory for physical memory address space.
 */
static union x86_32_pdir_entry pdir[X86_32_PTABLE_SIZE]
__attribute__((aligned(X86_32_BASE_PAGE_SIZE)));

/**
 * Page table for physical memory address space.
 */
static union x86_32_ptable_entry mem_ptable[MEM_PTABLE_SIZE][X86_32_PTABLE_SIZE]
__attribute__((aligned(X86_32_BASE_PAGE_SIZE)));

static inline void mapit(union x86_32_pdir_entry *pdir_base,
                         union x86_32_ptable_entry *ptable_base,
                         lpaddr_t addr, uint64_t bitmap)
{
    if(!X86_32_IS_PRESENT(pdir_base)) {
        paging_x86_32_map_table(pdir_base,
                                mem_to_local_phys((lvaddr_t)ptable_base));
    }

    if(!X86_32_IS_PRESENT(ptable_base)) {
        debug(SUBSYS_PAGING, "mapped!\n");
    } else {
        //remap the page anyway, this is important for the memory latency benchmark
        debug(SUBSYS_PAGING, "already existing! remapping it\n");
    }

    paging_x86_32_map(ptable_base, addr, bitmap);
}

#       endif

#endif

/**
 * \brief Map a region of physical memory into physical memory address space.
 *
 * Maps the region of physical memory, based at base and sized size bytes
 * to the same-sized virtual memory region. All pages are flagged according to
 * bitmap. This function automatically fills the needed page directory entries
 * in the page hierarchy rooted at pml4. base and size will be made
 * page-aligned by this function.
 *
 * \param base          Base address of memory region
 * \param size          Size in bytes of memory region
 * \param bitmap        Bitmap of flags for page tables/directories
 *
 * \return 0 on success, -1 on error (out of range)
 */
static int paging_x86_32_map_mem(lpaddr_t base, size_t size, uint64_t bitmap)
{
    lvaddr_t    vaddr, vbase = local_phys_to_mem(base);
    lpaddr_t    addr;

    paging_align(&vbase, &base, &size, X86_32_MEM_PAGE_SIZE);

    // Is mapped region out of range?
    assert(local_phys_to_gen_phys(base + size) <= X86_32_PADDR_SPACE_LIMIT);
    if(local_phys_to_gen_phys(base + size) > X86_32_PADDR_SPACE_LIMIT) {
        printk(LOG_ERR, "Mapped region [%"PRIxLPADDR",%"PRIxLPADDR"]"
                        "out of physical address range!",
               base, base + size);
        return -1;
    }

    assert(local_phys_to_gen_phys(vbase + size) <= X86_32_VADDR_SPACE_SIZE);

    // Map pages, tables and directories
    for(vaddr = vbase, addr = base;;
        vaddr += X86_32_MEM_PAGE_SIZE, addr += X86_32_MEM_PAGE_SIZE) {
#ifdef CONFIG_PAE
        union x86_32_pdpte_entry *pdpte_base = &pdpte[X86_32_PDPTE_BASE(vaddr)];
        union x86_32_ptable_entry *pdir_base =
            &mem_pdir[X86_32_PDPTE_BASE(addr)][X86_32_PDIR_BASE(vaddr)];
#else
        union x86_32_pdir_entry *pdir_base = &pdir[X86_32_PDIR_BASE(vaddr)];
#       ifndef CONFIG_PSE
        union x86_32_ptable_entry *ptable_base =
            &mem_ptable[X86_32_PDIR_BASE(addr)][X86_32_PTABLE_BASE(vaddr)];
#       endif
#endif

        if(vbase + size != 0) {
            if(vaddr >= vbase + size) {
                break;
            }
        }

#ifdef CONFIG_PAE
        debug(SUBSYS_PAGING, "Mapping 2M page: vaddr = 0x%x, addr = 0x%x, "
              "PDPTE_BASE = %u, PDIR_BASE = %u -- ", vaddr,
              addr, X86_32_PDPTE_BASE(vaddr), X86_32_PDIR_BASE(vaddr));
        mapit(pdpte_base, pdir_base, addr, bitmap);
#else
#       ifdef CONFIG_PSE
        debug(SUBSYS_PAGING, "Mapping 4M page: vaddr = 0x%x, addr = 0x%x, "
              "PDIR_BASE = %u -- ", vaddr,
              addr, X86_32_PDIR_BASE(vaddr));
        mapit(pdir_base, addr, bitmap);
#       else
        debug(SUBSYS_PAGING, "Mapping 4K page: vaddr = 0x%"PRIxLVADDR", "
              "addr = 0x%"PRIxLVADDR", "
              "PDIR_BASE = %"PRIuLPADDR", PTABLE_BASE = %"PRIuLPADDR" -- ", vaddr,
              addr, X86_32_PDIR_BASE(vaddr), X86_32_PTABLE_BASE(vaddr));
        mapit(pdir_base, ptable_base, addr, bitmap);
#       endif
#endif

        if(vbase + size == 0) {
            // Bail out if mapped last page of address space to prevent overflow
            if(vaddr == 0xffe00000) {
                break;
            }
        }
    }

    return 0;
}

lvaddr_t paging_x86_32_map_special(lpaddr_t base, size_t size, uint64_t bitmap)
{
    // Allocate backwards from a page below end of address space
    static lvaddr_t vbase = (lvaddr_t)X86_32_VADDR_SPACE_SIZE;
    lpaddr_t addr;
    lvaddr_t vaddr;

    paging_align(&vbase, &base, &size, X86_32_MEM_PAGE_SIZE);

    // Align physical base address
    lpaddr_t offset = base & (X86_32_MEM_PAGE_SIZE - 1);
    base -= offset;

    if(vbase - size < X86_32_VADDR_SPACE_SIZE - X86_32_DEVICE_SPACE_LIMIT) {
        return 0;
    }

    // Map pages, tables and directories (reverse order)
    for(vaddr = vbase - X86_32_MEM_PAGE_SIZE,
            addr = base + size - X86_32_MEM_PAGE_SIZE;
        vaddr >= vbase - size;
        vaddr -= X86_32_MEM_PAGE_SIZE, addr -= X86_32_MEM_PAGE_SIZE) {
#ifdef CONFIG_PAE
        union x86_32_pdpte_entry *pdpte_base = &pdpte[X86_32_PDPTE_BASE(vaddr)];
        union x86_32_ptable_entry *pdir_base =
            &mem_pdir[X86_32_PDPTE_BASE(mem_to_local_phys(vaddr))][X86_32_PDIR_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 2M device page: vaddr = 0x%x, addr = 0x%x, "
              "PDPTE_BASE = %u, PDIR_BASE = %u -- ", vaddr,
              addr, X86_32_PDPTE_BASE(vaddr), X86_32_PDIR_BASE(vaddr));
        mapit(pdpte_base, pdir_base, addr, bitmap);
#else
#       ifdef CONFIG_PSE
        union x86_32_ptable_entry *pdir_base = &pdir[X86_32_PDIR_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4M device page: vaddr = 0x%x, addr = 0x%x, "
              "PDIR_BASE = %u -- ", vaddr, addr, X86_32_PDIR_BASE(vaddr));
        mapit(pdir_base, addr, bitmap);
#       else
        union x86_32_pdir_entry *pdir_base = &pdir[X86_32_PDIR_BASE(vaddr)];
        union x86_32_ptable_entry *ptable_base =
            &mem_ptable[X86_32_PDIR_BASE(vaddr) - (X86_32_PTABLE_SIZE - MEM_PTABLE_SIZE)][X86_32_PTABLE_BASE(vaddr)];

        debug(SUBSYS_PAGING, "Mapping 4K device page: vaddr = 0x%"PRIxLVADDR", "
              "addr = 0x%"PRIxLPADDR", "
              "PDIR_BASE = %"PRIxLPADDR", PTABLE_BASE = %"PRIxLPADDR", pdir = %p, ptable = %p -- ",
              vaddr, addr, X86_32_PDIR_BASE(vaddr), X86_32_PTABLE_BASE(vaddr), pdir,
              mem_ptable[X86_32_PDIR_BASE(vaddr) - (X86_32_PTABLE_SIZE - MEM_PTABLE_SIZE)]);
        mapit(pdir_base, ptable_base, addr, bitmap);
#       endif
#endif
    }

    vbase -= size;
    return vbase + offset;
}

lvaddr_t paging_x86_32_map_device(lpaddr_t base, size_t size)
{
    return paging_x86_32_map_special(base, size, DEVICE_PAGE_BITMAP);
}

int paging_x86_32_map_memory(lpaddr_t base, size_t size)
{
    return paging_x86_32_map_mem(base, size, MEM_PAGE_BITMAP);
}

/**
 * \brief Reset kernel paging.
 *
 * This function resets the page maps for kernel and memory-space. It clears out
 * all other mappings. Use this only at system bootup!
 */
void paging_x86_32_reset(void)
{
    // Re-map physical memory
    // XXX: Map in what we get from Multiboot. We should actually map
    // stuff dynamically, whenever raw mem gets retyped into a kernel
    // object
/*     if(paging_map_memory(0, multiboot_info->mem_upper * 1024 + 0x100000) */
    lpaddr_t lpaddr = gen_phys_to_local_phys(X86_32_PADDR_SPACE_LIMIT -
                                             X86_32_DEVICE_SPACE_LIMIT);
    if(paging_x86_32_map_memory(0, lpaddr) != 0) {
        panic("error while mapping physical memory!");
    }

    // Switch to new page layout
#ifdef CONFIG_PAE
    paging_x86_32_context_switch(mem_to_local_phys((lvaddr_t)pdpte));
#else
    paging_x86_32_context_switch(mem_to_local_phys((lvaddr_t)pdir));
#endif
}

#ifdef CONFIG_PAE
/**
 * \brief Make a "good" PDPTE table out of a page table.
 *
 * A "good" PDPTE table is one that has all physical address space and
 * the kernel mapped in. This function modifies the passed PDPTE, based
 * at physical address 'base' accordingly. It does this by taking out
 * the corresponding entries of the kernel's pristine PDPTE table.
 *
 * \param base  Physical base address of PDPTE table to make "good".
 */
void paging_x86_32_make_good_pdpte(lpaddr_t base)
{
    union x86_32_pdpte_entry   *newpdpte =
        (union x86_32_pdpte_entry *)local_phys_to_mem(base);
    int                 i;

    debug(SUBSYS_PAGING, "Is now a PDPTE: table = 0x%x\n", base);
    // Map memory
    for(i = X86_32_PDPTE_BASE(X86_32_MEMORY_OFFSET); i < X86_32_PDPTE_SIZE; i++) {
        newpdpte[i] = pdpte[i];
    }
}
#else
/**
 * \brief Make a "good" PDE table out of a page table.
 *
 * A "good" PDE table is one that has all physical address space and
 * the kernel mapped in. This function modifies the passed PDE, based
 * at physical address 'base' accordingly. It does this by taking out
 * the corresponding entries of the kernel's pristine PDE table.
 *
 * \param base  Physical base address of PDE table to make "good".
 */
void paging_x86_32_make_good_pdir(lpaddr_t base)
{
#ifdef CONFIG_PSE
    union x86_32_ptable_entry  *newpdir =
        (union x86_32_ptable_entry *)local_phys_to_mem(base);
#else
    union x86_32_pdir_entry  *newpdir =
        (union x86_32_pdir_entry *)local_phys_to_mem(base);
#endif
    int                 i;

    debug(SUBSYS_PAGING, "Is now a PDE: table = 0x%" PRIxLPADDR "\n", base);

    // Map memory
    for(i = X86_32_PDIR_BASE(X86_32_MEMORY_OFFSET); i < X86_32_PDIR_SIZE; i++) {
        newpdir[i] = pdir[i];
    }
}
#endif
