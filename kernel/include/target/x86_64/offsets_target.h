/**
 * \file
 * \brief X86-64 address space sizes and offsets
 *
 * The layout of the x86-64 virtual address space can be summarized as
 * follows:
 *
 *<pre>
 * +----------------------------------------------------+-----------------+
 * | User-space                                         | Physical memory |
 * | PML4 entries: 0 1 2 3 4 ... 510                    | 511             |
 * +----------------------------------------------------+-----------------+</pre>
 *
 * User-space maps user-space programs. Physical memory maps all
 * available physical memory (up to PADDR_SPACE_LIMIT).
 *
 * This partition is static and can only be changed at compile-time.
 *
 * Physical memory can grow downwards, towards user-space, although it
 * is expected to stay within PML4 entry 511 for quite some time (one
 * PML4 entry can map 512 GBytes). The rest of the address space can
 * be freely mapped by (possibly multiple) user-space programs.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_TARGET_X86_64_OFFSETS_H
#define KERNEL_TARGET_X86_64_OFFSETS_H

/**
 * Absolute size of virtual address space. This is 48-bit on x86-64
 * currently, which equals 256 TBytes and allows for 512 PML4 slots,
 * each of which can map 512 GBytes.
 */
#define X86_64_VADDR_SPACE_SIZE        ((genpaddr_t)1 << 48)

/**
 * Absolute size of physical address space. This is also 48-bit.
 */
#define X86_64_PADDR_SPACE_SIZE        ((genpaddr_t)1 << 48)

/**
 * Start address of kernel image in physical memory. This is passed to
 * the linker also. The bootloader will load us there.
 */
#define X86_64_START_KERNEL_PHYS       0x100000

/**
 * Kernel stack size -- 16KB
 */
#define X86_64_KERNEL_STACK_SIZE       0x4000

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  We set this to 37-bit,
 * i.e. 128 GBytes.
 */
#define X86_64_PADDR_SPACE_LIMIT       ((genpaddr_t)1 << 37)

/**
 * Static address space limit for the init user-space domain. The
 * static space is used to map in code and static data of the init
 * module, as well as all loaded multiboot modules. init can freely
 * allocate dynamic memory as soon as it is running. This is 32 MBytes
 * right now.
 *
 * You should make this constant a multiple of #BASE_PAGE_SIZE *
 * #PTABLE_SIZE or you'll restrict init's static address space
 * unneccessarily. init's lowest segment should also be based at these
 * multiples or it restricts itself.
 */
#define X86_64_INIT_SPACE_LIMIT        (32 * 1024 * 1024)

/**
 * Base address of init address space in virtual memory. init should
 * start at 4 MByte. The kernel maps in important structures at 2
 * MByte. This address should be page-table size aligned (i.e. with 4
 * KByte pages, a page table maps 2 MBytes. Thus, align it to
 * multiples of 2 MBytes).
 */
#define X86_64_INIT_VBASE              0x200000

/**
 * Initial amount of physical memory to map during bootup. The low
 * 1MByte of memory is always expected to be there and has to be
 * specified here at minimum. If you need more during bootup, increase
 * this value. This value is also the amount of memory you _expect_ to
 * be in the system during bootup, or the kernel will crash!
 */
#define X86_64_KERNEL_INIT_MEMORY      (1 * 1024 * 1024)

/**
 * Aligns an address to the nearest PML4 entry by masking out lower 39
 * bits.
 */
#define X86_64_PML4_ALIGN(addr)        ((addr) & ((genpaddr_t)0x1ffffff << 39))

/**
 * Absolute offset of mapped physical memory within virtual address
 * space.  This occupies one or more (usually one) PML4 slots directly
 * before the kernel. This needs to be aligned to PADDR_SPACE_LIMIT.
 *
 * Change VSPACE_END in lib/barrelfish if you change this.
 */
#define X86_64_MEMORY_OFFSET        X86_64_PML4_ALIGN(-X86_64_PADDR_SPACE_LIMIT)

/**
 * The real-mode addresses
 */

#define X86_64_REAL_MODE_SEGMENT 0x0600 /**< The real-mode segment */
#define X86_64_REAL_MODE_OFFSET  0x0000 /**< The real-mode offset _has to be_ 0000!! */

#define X86_64_REAL_MODE_LINEAR_OFFSET \
    (X86_64_REAL_MODE_SEGMENT << 4) /**< The linear offset
                                       of the real-mode
                                       segment */

#define X86_64_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(seg) ((uint8_t)(seg >> 8))
#define X86_64_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(seg,off) ((uint32_t)(seg << 16) | off)

#ifndef __ASSEMBLER__

/**
 * \brief The kernel stack.
 */
extern uintptr_t x86_64_kernel_stack[X86_64_KERNEL_STACK_SIZE/sizeof(uintptr_t)];

#endif

#endif // KERNEL_TARGET_X86_64_OFFSETS_H
