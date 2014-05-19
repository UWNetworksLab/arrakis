/**
 * \file
 * \brief ARM address space sizes and offsets
 *
 * The layout of the ARM virtual address space can be summarized as
 * follows:
 *
 *
 * User-space maps user-space programs. Physical memory maps all
 * available physical memory (up to PADDR_SPACE_LIMIT). Kernel-space
 * maps only the kernel text and data.
 *
 * This partition is static and can only be changed at compile-time.
 *
 */

/* [2009-07-30 ohodson]TODO: This is a first-cut, layout likely
 * does not make sense.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OFFSETS_H
#define OFFSETS_H

#define GEN_ADDR(bits)          (((genpaddr_t)1) << bits)

/**
 * Absolute size of virtual address space. This is 32-bit on ARM.
 */
#define VADDR_SPACE_SIZE        GEN_ADDR(32);

/**
 * Absolute size of physical address space.
 */
#define PADDR_SPACE_SIZE        GEN_ADDR(32)

/**
 * Start address of kernel image in physical memory. This is passed to
 * the linker also. This address is chosen to be the same as Linux on ARM
 * for QEMU and/or bootloader compatibility.
 *
 * Entry point is 0x10000.
 *
 * Our kernel blob is the ELF file with the first word overwritten to
 * be BL instruction to the entry point in the .text section. The link
 * register is then a pointer to the ELF header for ELF loading.
 * ELF header consumes a 4K page.
 *
 * The .text section ELF loads the kernel using relative addressing
 * initially.
 */
#define START_KERNEL_PHYS       (0x10000 + 0x1000)

/**
 * Physical address of the kernel stack at boot time.
 */
#define BOOT_STACK_PHYS         0x10000

/**
 * Kernel offset - virtual base of kernel.
 */
#define KERNEL_OFFSET           0xfff00000

/**
 * Maximum physical address space mappable by the kernel.  Adjust this
 * for a bigger physical address space.  We set this to 36-bit, i.e. 64
 * GBytes.
 */
#define PADDR_SPACE_LIMIT       GEN_ADDR(31)

/**
 * Kernel address space limit is 1 MB currently.
 */
#define KERNEL_SPACE_LIMIT      (1L << 20)

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
 *
 *
 * NB 32MB is size of the fast context switch extension
 * per-process address space.
 */
#define INIT_SPACE_LIMIT        (32 * 1024 * 1024)

/**
 * Base address of init address space in virtual memory. init should
 * start at 4 MByte. The kernel maps in important structures at 2
 * MByte. This address should be page-table size aligned (i.e. with 4
 * KByte pages, a page table maps 2 MBytes. Thus, align it to
 * multiples of 2 MBytes).
 */
#define INIT_VBASE              (2 * 1024 * 1024)

/**
 * Initial amount of physical memory to map during bootup. The low
 * 1MByte of memory is always expected to be there and has to be
 * specified here at minimum. If you need more during bootup, increase
 * this value. This value is also the amount of memory you _expect_ to
 * be in the system during bootup, or the kernel will crash!
 */
#define KERNEL_INIT_MEMORY      (1 * 1024 * 1024)

/**
 * Absolute offset of mapped physical memory within virtual address
 * space.  This occupies one or more (usually one) PML4 slots directly
 * before the kernel. This needs to be aligned to PADDR_SPACE_LIMIT.
 *
 * 2GB.
 */
#define MEMORY_OFFSET           GEN_ADDR(31)

/**
 * Kernel stack size -- 16KB
 */
#define KERNEL_STACK_SIZE       0x4000

/**
 * The size of the whole kernel image.
 */
#define KERNEL_IMAGE_SIZE       (size_t)(&kernel_final_byte - &kernel_first_byte)

#ifndef __ASSEMBLER__

static inline lvaddr_t local_phys_to_mem(lpaddr_t addr)
{
    assert(addr < PADDR_SPACE_LIMIT);
    return (lvaddr_t)(addr + (lpaddr_t)MEMORY_OFFSET);
}

static inline lpaddr_t mem_to_local_phys(lvaddr_t addr)
{
    assert(addr >= MEMORY_OFFSET);
    return (lpaddr_t)(addr - (lvaddr_t)MEMORY_OFFSET);
}

static inline lpaddr_t gen_phys_to_local_phys(genpaddr_t addr)
{
    assert(addr < PADDR_SPACE_SIZE);
    return (lpaddr_t)addr;
}

static inline genpaddr_t local_phys_to_gen_phys(lpaddr_t addr)
{
    return (genpaddr_t)addr;
}

/**
 * Symbol: Start of kernel image. This symbol points to the start
 * address of the kernel image.
 */
extern uint8_t kernel_first_byte;

/**
 * Symbol: End of kernel image. This symbol points to the end address
 * of the kernel image.
 */
extern uint8_t kernel_text_final_byte;

/**
 * Symbol: End of kernel image. This symbol points to the end address
 * of the kernel image.
 */
extern uint8_t kernel_final_byte;

/**
 * \brief The kernel stack.
 *
 * Declared in boot.S.
 */
extern uintptr_t kernel_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)];

#endif  // __ASSEMBLER__

#endif  // OFFSETS_H
