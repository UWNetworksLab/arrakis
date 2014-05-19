/**
 * \file
 * \brief X86-32 address space sizes and offsets
 *
 * The layout of the x86-32 virtual address space can be summarized as
 * follows:
 *
 *<pre>
 * +--------------------------------+------------------------------------+
 * | User-space                     | Physical memory                    |
 * | PDPTE entries: 0 1             | PDPTE entries: 2 3                 |
 * +--------------------------------+------------------------------------+</pre>
 *
 * User-space maps user-space programs. Physical memory maps all
 * available physical memory (up to PADDR_SPACE_LIMIT).
 *
 * This partition is static and can only be changed at compile-time.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_TARGET_X86_32_OFFSETS_H
#define KERNEL_TARGET_X86_32_OFFSETS_H

/**
 * Absolute size of virtual address space. This is 32-bit on x86-32,
 * which equals 4 GBytes and allows for 1024 PDE slots, each of which
 * can map 4 MBytes.
 */
#define X86_32_VADDR_SPACE_SIZE        ((genpaddr_t)1 << 32)

/**
 * Absolute size of physical address space. This is 48-bit.
 */
#ifdef CONFIG_PAE
#       define X86_32_PADDR_SPACE_SIZE         ((genpaddr_t)1 << 48)
#else
#       define X86_32_PADDR_SPACE_SIZE         ((genpaddr_t)1 << 32)
#endif

/**
 * Start address of kernel image in physical memory. This is passed to
 * the linker also. The bootloader will load us there.
 */
#define X86_32_START_KERNEL_PHYS       0x100000

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  We set this to 32-bit, i.e. 4
 * GBytes.
 */
#define X86_32_PADDR_SPACE_LIMIT       ((genpaddr_t)1 << 31)

/**
 * Maximum device address space mappable by the kernel.  Adjust this
 * for a bigger device address space.  We set this to one page, so the APIC
 * driver can do its job.
 */
#ifdef __scc__
//#       define X86_32_DEVICE_SPACE_LIMIT      (148 * X86_32_MEM_PAGE_SIZE)
#       define X86_32_DEVICE_SPACE_LIMIT      (196 * X86_32_MEM_PAGE_SIZE)
#else
#       define X86_32_DEVICE_SPACE_LIMIT      (1 * X86_32_MEM_PAGE_SIZE)
#endif

/**
 * Static virtual address space limit for the init user-space
 * domain. init's virtual address space always starts at address
 * zero. The static space is used to map in code and static data of
 * the init module, as well as all loaded multiboot modules, the
 * dispatcher frame and bootinfo structure. init can freely allocate
 * dynamic memory as soon as it is running. This is 32 MBytes right
 * now.
 *
 * You should make this constant a multiple of #BASE_PAGE_SIZE *
 * #PTABLE_SIZE or you'll restrict init's static address space
 * unneccessarily. init's lowest segment should also be based at these
 * multiples or it restricts itself.
 */
#define X86_32_INIT_SPACE_LIMIT        (32 * 1024 * 1024)

#ifdef CONFIG_PAE
/**
 * Aligns an address to the nearest PDPTE entry by masking out lower 29
 * bits.
 */
#       define X86_32_PDPTE_ALIGN(addr)         \
    ((addr) & ~(((genpaddr_t)1 << 30) - 1))

/**
 * Absolute offset of mapped physical memory within virtual address
 * space.  This occupies one or more PDE slots directly before the
 * kernel. This needs to be aligned to PADDR_SPACE_LIMIT.
 *
 * Change VSPACE_END in lib/barrelfish if you change this.
 */
#       define X86_32_MEMORY_OFFSET                             \
    X86_32_PDPTE_ALIGN((genpaddr_t)2 * 1024 * 1024 * 1024)

#else

/**
 * Aligns an address to the nearest PDE entry by masking out lower 21
 * bits.
 */
#       define X86_32_PDE_ALIGN(addr)           \
    ((addr) & ~(((genpaddr_t)1 << 22) - 1))

/**
 * Absolute offset of mapped physical memory within virtual address
 * space.  This occupies one or more PDE slots directly before the
 * kernel. This needs to be aligned to PADDR_SPACE_LIMIT.
 */
#       define X86_32_MEMORY_OFFSET                             \
    X86_32_PDE_ALIGN((genpaddr_t)2 * 1024 * 1024 * 1024)
#endif

/**
 * Kernel stack size -- 16KB
 */
#define X86_32_KERNEL_STACK_SIZE       0x4000

/**
 * The real-mode addresses
 */

#define X86_32_REAL_MODE_SEGMENT 0x0600 /**< The real-mode segment */
#define X86_32_REAL_MODE_OFFSET  0x0000 /**< The real-mode offset _has to be_ 0000!! */

#define X86_32_REAL_MODE_LINEAR_OFFSET \
    (X86_32_REAL_MODE_SEGMENT << 4) /**< The linear offset
                                       of the real-mode
                                       segment */
#define X86_32_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(seg) ((uint8_t)(seg >> 8))
#define X86_32_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(seg,off) ((uint32_t)(seg << 16) | off)

#ifndef __ASSEMBLER__

/**
 * \brief The kernel stack.
 */
extern uintptr_t x86_32_kernel_stack[X86_32_KERNEL_STACK_SIZE/sizeof(uintptr_t)];

#endif

#endif // KERNEL_TARGET_X86_32_OFFSETS_H
