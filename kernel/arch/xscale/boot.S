/**
 * \file
 * \brief Bootstrap the kernel.
 */
/*
 * Copyright (c) 2009 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ASSEMBLER__
#define __ASSEMBLER__   1
#endif

#include <barrelfish_kpi/flags_arch.h> // ARM_MODE_MASK
#include <offsets.h> // BOOT_STACK_PHYS
        
        .text
        .arm

        .globl start, halt, got_base

        // Used to track phys memory allocator limit globally.
        alloc_top .req r11

start:
        // Entry constraints same as ARM Linux so QEMU can boot
	// this file directly and to make it easy to use existing
        // loaders on real h/w.
        //
        // The build process has built an ELF kernel image. The first
        // word is overwritten to branch to entry point, `start', in
        // the text section of the unpacked ELF file. The start routine
        // then loads the ELF image, initializes the page table, and
        // enables paging.
        //
        // NB Writing the branch instruction into the first word of the
        // ELF header causes QEMU to treat the file as a Linux kernel
        // image and it provides the ATAG_HEADER info.
        //
        // On entry:
        //
        // MMU disabled
        // Caches in unknown state, but no lockdown
        // No TLB lockdown.
        // CPU is in a priveledged mode.
        //
        // r0 contains zero
        // r1 contains board id
        // r2 contains pointer to kernel args structure
        // lr contains pointer to elf header + 4

	sub     lr, lr, #4                      // lr = address of elf header
        mov     r4, lr                          // r4 = p_elf_header [KEEP]
        
        mrs     r3, cpsr                        // Ensure in SYS mode
        bic     r3, r3, #ARM_MODE_MASK
        orr     r3, r3, #ARM_MODE_SVC
        msr     cpsr_c, r3

        mov     sp, #BOOT_STACK_PHYS
        stmfd   sp!, {r1, r2}

        /* Validate ELF header */
        mov     r0, r4
        bl      elf_header_is_valid
        cmp     r0, #0
        beq     .

        /* Compute size of elf file */
        mov     r0, r4                          // r0 = p_elf_header
        bl      elf_get_file_size               // r0 = elf_file_size
        mov     r5, r0                          // r5 = elf_file_size [KEEP]

        mov     r0, r4
        bl      elf_get_expanded_limits

        mov     r6, r0                          // r6 = kernel v_addr [KEEP]
        mov     r7, r1                          // r7 = kernel v_limit [KEEP]

        /* Want to map low 1MB section with 1:1 to kernel address space */
        mov     r2, #(512 * 1024 * 1024)
        cmp     pc, r2
        bhs     $start_rom_image

        /*
        The ELF image is in RAM, copy it just above the
	physical pages of the unpacked kernel (1:1 mapping
	between kernel v_addr and p_addr).
         */

        ldr     r3, =4095                       // Page align kernel v_limit
        add     r0, r7, r3
        orr     r3, r3, r3, LSL #20
        bic     r0, r0, r3                      // r0 = new ELF file physical address
        mov     r1, r4                          // r1 = old ELF file physical address
        mov     r2, r5                          // r2 = ELF size
        sub     r8, r0, r1                      // r8 = delta (new, old) ELF physical address
        bl      memcpy

        add     r4, r4, r8                      // Update r4, p_elf_header
        sub     r8, r8, #4
        add     pc, pc, r8                      // Jump to next line, but in relocated ELF image.

$start_rom_image:
        // NOP

$start_expand_elf:
        // Zero physical region
        sub     r1, r7, r6                      // r1 = expanded ELF size
        ldr     r2, =0xfff00000
        bic     r0, r6, r2                      // r0 = physical address of expanded image
        bl      memzero

        // Expand file
        ldr     r2, =0xfff00000
        bic     r2, r6, r2                      // r2 = kernel p_addr
        mov     r1, r6                          // r1 = kernel v_addr
        mov     r0, r4                          // r0 = elf_header
        bl      elf_expand

        add     alloc_top, r4, r5               // alloc_top = end of ELF file.

        mov     r0, #16384                      // allocate L1 page table (16K aligned to 16K).
        mov     r1, r0
        bl      alloc_phys
        mov     r8, r0                          // r8 = L1 page table [KEEP]

$start_map_kernel:
        ldr     r3, =0xfff00000                 // Map kernel section to physical section
        and     r1, r6, r3
        and     r2, sp, r3
        bl      section_map                     // section_map (l1_addr, v_addr, p_addr)

        ldr     r3, =0xfff00000                 // Map section containing program counter 1:1
        and     r2, pc, r3
        mov     r1, r1
        mov     r0, r8
        bl      section_map

        ldr     r3, =0xfff00000                 // Map ATAG headers 1:1
	ldr	r0, =0x2c000100
        and     r2, r0, r3
        mov     r1, r2
        mov     r0, r8
	bl      section_map


$start_mmu_config:
        mcr     p15, 0, r8, c2, c0, 0           // Load TTBR with L1
        ldr     r0, =0x55555555                 // Initial domain permissions - all client [checked]
        mcr     p15, 0, r0, c3, c0, 0

        ldr     lr, =$start_with_mmu_enabled    // Address to continue at when paging is enabled
        ldr     r0, =KERNEL_OFFSET
        add     sp, sp, r0                      // Prepare stack for relocation

//	ldr     r1, =0x1007                     // Enable: D-Cache, I-Cache, Alignment, MMU
	ldr     r1, =0x1005                     // Enable: D-Cache, I-Cache, MMU
        mrc     p15, 0, r0, c1, c0, 0
        orr     r0, r0, r1
        mcr     p15, 0, r0, c1, c0, 0           // MMU is enabled
        mov     pc, lr                          // Flat fetched.
        mov     r0, r0                          // Flat fetched.

        // Up until this point PC is in ELF file.
$start_with_mmu_enabled:
        // MMU is enabled and PC is in the loaded ELF image.

        mov     r1, #0                          // Unmap section with VA = 0
        mov     r0, r8                          // r0 = page table address
        bl      section_unmap

        mov     r1, #0
        mcr     p15, 0, r1, c8, c7, 0           // Invalidate ID-TLB entry for section with VA = 0.

$start_set_got_register:
        ldr     PIC_REGISTER, =got_base


$start_set_init_arguments:
        ldmfd   sp!, {r0, r1}                   // r0 = board id
                                                // r1 = paddr of kern args, already mapped 1:1 to vaddr
        ldr     r2, =KERNEL_OFFSET              // Convert paddr's to vaddr's
        add     r3, alloc_top, r2               // r3 = alloc_top
        add     r2, r4, r2                      // r2 = addr kernel ELF file
        mov     lr, #0
        b       arch_init

/**
 * bool elf_header_is_valid(struct Elf32_EHdr*)
 *
 * A cursory check of ELF header. nb first word is known to be invalid
 */
elf_header_is_valid:
        ldr     r1, [r0, #4]!                   // e_ident[4..7]
        ldr     r2, =0x00010101
        eors    r3, r1, r2
        bne     $done
        ldr     r1, [r0, #4]!                   // e_ident[8..11]
        mov     r2, #0
        eors    r3, r1, r2
        bne     $done
        ldr     r1, [r0, #4]!                   // e_ident[12..15]
        eors    r3, r1, r2
        bne     $done
        ldr     r1, [r0, #4]!                   // (e_type, e_machine)
        ldr     r2, =0x00280002
        eors    r3, r1, r2
        bne     $done
        ldr     r1, [r0, #4]!                   // e_version
        mov     r2, #1
        eors    r3, r1, r2
$done:
        mvn     r3, r3
        bx      lr

/**
  * uint32_t elf_get_file_size(struct Elf32_EHdr*)
  */
elf_get_file_size:
        ldr     r1, [r0, #32]                   // r1 = offset of sections
        ldrh    r2, [r0, #46]                   // r2 = e_shentsize
        ldrh    r3, [r0, #48]                   // r3 = e_shnum
        mul     r0, r2, r3
        add     r0, r0, r1
        bx      lr

/**
  * (vaddr_t, size_t) elf_get_expanded_limits(struct Elf32_EHdr*)
  */
elf_get_expanded_limits:
        stmfd   sp!, {r4-r6, lr}
        mov     r5, #0                          // r5 = max vaddr
        sub     r4, r5, #1                      // r4 = min vaddr
        ldr     r1, [r0, #28]                   // r1 = e_phoff
        ldrh    r2, [r0, #42]                   // r2 = e_phentsize
        ldrh    r3, [r0, #44]                   // r3 = e_phnum
        add     r1, r1, r0                      // r1 = start of prog headers
        mul     r0, r2, r3                      // r0 = size of prog headers
        add     r3, r0, r1                      // r3 = end of prog headers
        b       $looptest
$loopstart:
        ldr     r6, [r1, #20]                   // r6 = memsz
        cmp     r6, #0
        beq     $loopinc                        // SKIP If memsz = 0
        ldr     r0, [r1, #8]                    // r0 = vaddr
        cmp     r0, r4
        movlo   r4, r0                          // r4 = min(r4, vaddr)
        add     r6, r0, r6                      // r0 = vaddr + memsz
        cmp     r6, r5
        movhs   r5, r6                          // r5 = max(r5, vaddr + memsz)
        ldr     r0, [r1, #28]                   // r0 = alignment
$loopinc:
        add     r1, r1, r2
$looptest:
        cmp     r1, r3
        bne     $loopstart
        mov     r0, r4
        mov     r1, r5
        ldmfd   sp!, {r4-r6, pc}

/**
 * void elf_expand(Struct Elf32_EHdr*, vaddr_t kernel_v, paddr_t kernel_p)
 */
elf_expand:
        stmfd   sp!, {r4-r7, lr}
        ldr     r3, [r0, #28]                   // r3 = e_phoff
        ldrh    r4, [r0, #44]                   // r4 = e_phnum
        add     r3, r0, r3                      // r3 = addr phdr[0]
$elf_expand_start:
        subs    r4, r4, #1
        beq     $elf_expand_done
        ldr     r5, [r3, #4]                    // r5 = p_offset
        cmp     r5, #0
        beq     $elf_expand_next
        ldr     r6, [r3, #8]                    // r6 = p_vaddr
        cmp     r6, #0
        beq     $elf_expand_next
        stmfd   sp!, {r0-r3}                    // Save scratch registers
        mov     r7, r1                          // r7 = kernel_v
        add     r1, r0, r5                      // r1 = ptr to segment in file
        sub     r0, r6, r7                      // r0 = kernel_v_offset
        add     r0, r0, r2                      // r0 = phys addr of segment
        ldr     r2, [r3, #16]                   // r2 = p_filesz
        bl      memcpy
        ldmfd   sp!, {r0-r3}                    // Restore scratch registers
$elf_expand_next:
        ldrh    r5, [r0, #42]                   // r5 = e_phentsize
        add     r3, r3, r5                      // r3 = addr phdr[next]
        b       $elf_expand_start
$elf_expand_done:
        ldmfd   sp!, {r4-r7, pc}

/**
 * void memzero(uintptr_t addr, size_t bytes)
 * Assumes addr is 4-byte aligned and bytes is a multiple of 4.
 */
memzero:
        subs    r1, #4
        blt     $zero0
        mov     r2, #0
        mov     r3, #0
        subs    r1, #12
        blt     $zero8
$zero16:
        stmia   r0!, {r2-r3}
        subs    r1, r1, #16
        stmia   r0!, {r2-r3}
        bge     $zero16
$zero8:
        adds    r1, r1, #8
        stmgeia r0!, {r2, r3}
        subge   r1, #8
$zero4:
        adds    r1, r1, #4
        stmgeia r0!, {r2}
$zero0:
        bx      lr

/**
 * void memcpy(uintptr_t dest, uintptr_t src, size_t bytes)
 *
 * Assumes addr is 4-byte aligned and bytes is a multiple of 4.
 */
memcpy:
        stmfd   sp!, {r4, r5, lr}
        subs    r2, #16
        blt     $cpy8
$cpy16:
        ldmia   r1!, {r4-r5}
        stmia   r0!, {r4-r5}
        subs    r2, r2, #16
        ldmia   r1!, {r4-r5}
        stmia   r0!, {r4-r5}
        bge     $cpy16
$cpy8:
        adds    r2, r2, #8
        ldmgeia r1!, {r4, r5}
        subge   r2, #8
        stmgeia r0!, {r4, r5}
$cpy4:
        adds    r2, r2, #4
        ldrge   r4, [r0], #4
        strge   r4, [r1], #4
$cpy0:
        ldmfd   sp!, {r4, r5, pc}

/**
 * Allocate Physical Memory.
 *
 * uintptr_t alloc_phys(size_t bytes, size_t align)
 */
alloc_phys:
        sub     r1, r1, #1
        mov     r3, alloc_top
        add     r3, r3, r1                      // Align start address
        bic     r3, r3, r1                      // r3 = alloc address
        add     r0, r0, r3                      // r0 = new alloc_top value
        mov     alloc_top, r0
        stmfd   sp!, {r3, lr}
        sub     r1, r0, r3
        mov     r0, r3
        bl      memzero
        ldmfd   sp!, {r0, pc}

/**
 * void section_map(L1PageTable *p, vaddr_t v, paddr_t p)
 */
section_map:
        lsr     r1, r1, #20                     // r1 is table offset
        lsr     r2, r2, #20
        ldr     r3, =0x41e                      // AP = 01, Domain = 0, CB, Section
        orr     r2, r3, r2, LSL #20             // r2 = Section Entry
        str     r2, [r0, r1, LSL #2]            // table[v >> 20] = r2
        bx      lr

/**
 * void section_unmap(L1PageTable *p, vaddr_t v)
 */
section_unmap:
        mov     r2, #0                          // Invalid L1 PTE
        str     r2, [r0, r1, LSL #18]
        bx      lr

/**
 * extern "C" void halt(void) __attribute__((noreturn))
 */
halt:
        b       .

.ltorg

got_base:
        .word                                   // Initialized by linker

        .end
