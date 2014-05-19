/**
 * \file
 * \brief x86-32 architecture initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <string.h>
#include <stdio.h>
#include <paging_kernel_arch.h>
#include <elf/elf.h>
#include <init.h>
#include <irq.h>
#include <x86.h>
#include <serial.h>
#include <kernel_multiboot.h>
#include <syscall.h>
#include <getopt/getopt.h>
#include <exec.h>
#include <kputchar.h>
#include <arch/x86/conio.h>
#include <arch/x86/pic.h>
#include <arch/x86/apic.h>
#include <arch/x86/perfmon_intel.h>
#include <arch/x86/perfmon_amd.h>
#include <arch/x86/rtc.h>
#ifdef __scc__
#       include <rck.h>
#       include <diteinfo.h>
#else
#       include <arch/x86/ipi_notify.h>
#endif
#include <target/x86/barrelfish_kpi/coredata_target.h>
#include <arch/x86/timing.h>
#include <arch/x86/startup_x86.h>

#include "xapic_dev.h" // XXX

/**
 * Used to store the address of global struct passed during boot across kernel
 * relocations.
 */
// XXX: This won't work if this kernel is not relocated from a pristine image!
static uint32_t addr_global;

/**
 * EFLAGS mask for fast system calls. Put values to mask out here.
 * We mask out everything (including interrupts).
 */
#define SYSCALL_FMASK   (~(EFLAGS_ALWAYS1) & 0xffffffff)

/**
 * Segment selector bases for both kernel- and user-space for fast
 * system calls
 */
#define SYSCALL_STAR \
    ((((uint64_t)GSEL(KSTACK_SEL, SEL_UPL)) << 48) | \
     ((uint64_t)GSEL(KCODE_SEL, SEL_KPL) << 32))

/**
 * \brief Kernel stack.
 *
 * This is the one and only kernel stack for a kernel instance.
 */
uintptr_t x86_32_kernel_stack[X86_32_KERNEL_STACK_SIZE/sizeof(uintptr_t)];

/**
 * \brief Global Task State Segment (TSS).
 *
 * This is the global, static and only Task State Segment (TSS). It is used
 * for interrupt and exception handling (stack setup) while in user-space.
 */
static struct task_state_segment tss __attribute__ ((aligned (4)));

/**
 * \brief Global Descriptor Table (GDT) for processor this kernel is running on.
 *
 * This descriptor table is completely static, as segments are basically
 * turned off in 64-bit mode. They map flat-mode code and stack segments for
 * both kernel- and user-space and the only Task State Segment (TSS).
 */
static union segment_descriptor gdt[] __attribute__ ((aligned (4))) = {
    {   // Null segment
        .raw = 0
    },
    {   // Kernel code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 0,
            .operation_size = 1,
            .granularity = 1,
            .hi_base = 0
        }
    },
    {   // Kernel stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 0,
            .operation_size = 1,
            .granularity = 1,
            .hi_base = 0
        }
    },
    {   // User stack segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 0,
            .operation_size = 1,
            .granularity = 1,
            .hi_base = 0
        }
    },
    {   // User code segment
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 0xa,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 0,
            .operation_size = 1,
            .granularity = 1,
            .hi_base = 0
        }
    },
    {   // Global Task State Segment (TSS)
        .tss = {
            .lo_limit = sizeof(tss) & 0xffff,
            .type = SDT_SYSTSS,
            .privilege_level = SEL_KPL,
            .present = 1,
            .hi_limit = (sizeof(tss) >> 16) & 0xf,
            .available = 0,
            .granularity = 0,
        }
    },
    {
        // Dispatcher "segment"
        .d = {
            .lo_limit = 0xffff,
            .lo_base = 0,
            .type = 2,
            .system_desc = 1,
            .privilege_level = SEL_UPL,
            .present = 1,
            .hi_limit = 0xf,
            .available = 0,
            .long_mode = 0,
            .operation_size = 1,
            .granularity = 1,
            .hi_base = 0
        }
    }
};

volatile union segment_descriptor *curdisp = &gdt[6];

#ifdef CONFIG_PAE
/**
 * Bootup PDPTE.
 */
static union x86_32_pdpte_entry boot_pdpte[X86_32_PDPTE_SIZE]
__attribute__ ((aligned(X86_32_BASE_PAGE_SIZE)));

/**
 * Bootup low-map PDIR and hi-map PDIR.
 */
static union x86_32_ptable_entry boot_pdir[X86_32_PTABLE_SIZE]
__attribute__ ((aligned(X86_32_BASE_PAGE_SIZE))),
    boot_pdir_hi[X86_32_PTABLE_SIZE] __attribute__ ((aligned(X86_32_BASE_PAGE_SIZE)));
#else
#       ifdef CONFIG_PSE
/**
 * Bootup PDIR.
 */
static union x86_32_ptable_entry boot_pdir[X86_32_PTABLE_SIZE]
__attribute__ ((aligned(X86_32_BASE_PAGE_SIZE)));
#       else
/**
 * Bootup PDIR.
 */
static union x86_32_pdir_entry boot_pdir[X86_32_PTABLE_SIZE]
__attribute__ ((aligned(X86_32_BASE_PAGE_SIZE)));

/**
 * Bootup low-map PTABLE and hi-map PTABLE.
 */
static union x86_32_ptable_entry
boot_ptable[MEM_PTABLE_SIZE][X86_32_PTABLE_SIZE]
__attribute__ ((aligned(X86_32_BASE_PAGE_SIZE)));
#       endif
#endif

/**
 * This flag is set to true once the IDT is initialized and exceptions can be
 * caught.
 */
bool idt_initialized = false;

/**
 * \brief Setup bootup page table.
 *
 * This function sets up the page table needed to boot the kernel proper.
 * The table identity maps the first 2 MBytes (page size is 2 MBytes) of
 * physical memory in order to have access to the first MByte containing
 * bootloader-passed data structures. It also identity maps the local copy
 * of the kernel in low memory and aliases it in kernel address space.
 */
static void paging_init(void)
{
    lvaddr_t vbase = X86_32_MEMORY_OFFSET, base = 0;

    // Align vbase to kernel page size
    if(vbase & X86_32_MEM_PAGE_MASK) {
        vbase -= vbase & X86_32_MEM_PAGE_MASK;
    }

#ifdef CONFIG_PAE
    for(size_t i = 0; i < X86_32_PTABLE_SIZE; i++,
            base += X86_32_MEM_PAGE_SIZE, vbase += X86_32_MEM_PAGE_SIZE) {
        // Identity-map the kernel's physical region, so we don't lose ground
        paging_x86_32_map_pdpte(&boot_pdpte[X86_32_PDPTE_BASE(base)],
                                (lpaddr_t)boot_pdir);
        paging_x86_32_map_large(&boot_pdir[X86_32_PDIR_BASE(base)], base,
                                X86_32_PTABLE_PRESENT
                                | X86_32_PTABLE_READ_WRITE
                                | X86_32_PTABLE_USER_SUPERVISOR);

        // Alias the same region at MEMORY_OFFSET
        paging_x86_32_map_pdpte(&boot_pdpte[X86_32_PDPTE_BASE(vbase)],
                                (lpaddr_t)boot_pdir_hi);
        paging_x86_32_map_large(&boot_pdir_hi[X86_32_PDIR_BASE(vbase)], base,
                                X86_32_PTABLE_PRESENT
                                | X86_32_PTABLE_READ_WRITE
                                | X86_32_PTABLE_USER_SUPERVISOR);
    }

    // Activate new page tables
    paging_x86_32_context_switch((lpaddr_t)boot_pdpte);
#else
    for(size_t i = 0; i < X86_32_PADDR_SPACE_LIMIT; i += X86_32_MEM_PAGE_SIZE,
            base += X86_32_MEM_PAGE_SIZE, vbase += X86_32_MEM_PAGE_SIZE) {
#       ifdef CONFIG_PSE
        // Identity-map the kernel's physical region, so we don't lose ground
        paging_x86_32_map_large(&boot_pdir[X86_32_PDIR_BASE(base)], base,
                                X86_32_PTABLE_PRESENT
                                | X86_32_PTABLE_READ_WRITE
                                | X86_32_PTABLE_USER_SUPERVISOR);

        // Alias the same region at MEMORY_OFFSET
        paging_x86_32_map_large(&boot_pdir[X86_32_PDIR_BASE(vbase)], base,
                                X86_32_PTABLE_PRESENT
                                | X86_32_PTABLE_READ_WRITE
                                | X86_32_PTABLE_USER_SUPERVISOR);
#       else
        // Identity-map the kernel's physical region, so we don't lose ground
        paging_x86_32_map_table(&boot_pdir[X86_32_PDIR_BASE(base)],
                                (lpaddr_t)boot_ptable[X86_32_PDIR_BASE(base)]);
        paging_x86_32_map(&boot_ptable[X86_32_PDIR_BASE(base)][X86_32_PTABLE_BASE(base)],
                          base,
                          X86_32_PTABLE_PRESENT
                          | X86_32_PTABLE_READ_WRITE
                          | X86_32_PTABLE_USER_SUPERVISOR);

        // Alias the same region at MEMORY_OFFSET
        paging_x86_32_map_table(&boot_pdir[X86_32_PDIR_BASE(vbase)],
                                (lpaddr_t)boot_ptable[X86_32_PDIR_BASE(base)]);
#       endif
    }

    // Activate new page tables
    paging_x86_32_context_switch((lpaddr_t)boot_pdir);
#endif
}

/**
 * \brief Setup default GDT.
 *
 * Loads the GDT register with the default GDT and reloads CS and SS
 * to point to the new entries. Resets all other segment registers to null.
 * Finally, completes setup of GDT to include TSS base address mapping and
 * loads TSS into task register.
 */
static void gdt_reset(void)
{
    lvaddr_t                     ptss = (lvaddr_t)&tss;
    struct region_descriptor    region = {
        .rd_limit = sizeof(gdt),
        .rd_base = (uint32_t)&gdt
    };

    // Load default GDT
    __asm volatile("lgdt %[region]" :: [region] "m" (region));

    // Reload segments
    __asm volatile("mov %[ds], %%ds        \n\t"
                   "mov %[ds], %%es        \n\t"
                   "mov %[ss], %%ss        \n\t"
                   "mov %[null], %%gs      \n\t"
                   "mov %[null], %%fs      \n\t"
                   "pushl %[cs]            \n\t"          // new CS
                   "lea 1f, %%eax          \n\t"          // jumps to after lret
                   "pushl %%eax            \n\t"          // new IP
                   "lretl                  \n\t"          // fake return
                   "1:                     \n\t"          // we'll continue here
                   : /* No Output */
                   :
                   [null] "r" (0),
                   [ss] "r" (GSEL(KSTACK_SEL, SEL_KPL)),
                   [cs] "i" (GSEL(KCODE_SEL, SEL_KPL)),
                   [ds] "r" (GSEL(USTACK_SEL, SEL_UPL))
                   : "eax"
                   );

    // Complete setup of TSS descriptor (by inserting base address of TSS)
    gdt[TSS_SEL].tss.lo_base = ptss & 0xffffff;
    gdt[TSS_SEL].tss.hi_base = (ptss >> 24) & 0xff;

    // Complete setup of TSS
    tss.esp0 = (lvaddr_t)&x86_32_kernel_stack[X86_32_KERNEL_STACK_SIZE / sizeof(uintptr_t)];
    tss.ss0 = GSEL(KSTACK_SEL, SEL_KPL);

    // Load task state register
    __asm volatile("ltr %%ax" :: "a" (GSEL(TSS_SEL, SEL_KPL)));
}

/**
 * \brief Relocates the active stack.
 *
 * This function relocates the stack, by adding 'offset' to the stack
 * pointer.
 *
 * \param offset        Offset to add to the stack pointer.
 */
static inline void __attribute__ ((always_inline))
relocate_stack(lvaddr_t offset)
{
    __asm volatile("add %[stack], %%esp\n\t"
                   : /* No output */
                   : [stack] "g" (offset)
                   : "esp"
                   );
}

/**
 * \brief Enable SYSCALL/SYSRET fast system calls.
 *
 * This function enables the SYSCALL/SYSRET pair of fast system calls in
 * long mode. Also sets the IA32_STAR and IA32_FMASK MSRs to point to the
 * user-space base selector and RFLAGS mask for SYSCALL/SYSRET fast system
 * calls.
 */
static inline void enable_fast_syscalls(void)
{
    // Set IA32_STAR MSR to point to user-space base selector
    wrmsr(MSR_IA32_STAR, SYSCALL_STAR);

    // Set IA32_LSTAR MSR to point to kernel-space system call multiplexer
    wrmsr(MSR_IA32_LSTAR, (lvaddr_t)syscall_entry);

    // Set IA32_FMASK MSR for our OSes EFLAGS mask
    wrmsr(MSR_IA32_FMASK, SYSCALL_FMASK);

    // Enable fast system calls
    addmsr(MSR_IA32_EFER, IA32_EFER_SCE);
}

#define CR0_CD  (1 << 30)
#define CR0_NW  (1 << 29)
#define CR0_PG  (1 << 31)
#define CR4_MPE (1 << 11)
#define CR4_PCE (1 << 8)
#define CR4_PGE (1 << 7)
#define CR4_PAE (1 << 5)
#define CR4_PSE (1 << 4)

static inline void enable_user_rdpmc(void)
{
    uint32_t cr4;

    __asm volatile("mov %%cr4, %[cr4]" : [cr4] "=r" (cr4));
    cr4 |= CR4_PCE;
    __asm volatile("mov %[cr4], %%cr4" :: [cr4] "r" (cr4));
}

static inline void enable_tlb_flush_filter(void)
{
    uint32_t eax, ebx, ecx, edx;

    // Must read "AuthenticAMD"
    cpuid(0, &eax, &ebx, &ecx, &edx);
    if(ebx != 0x68747541 || ecx != 0x444d4163 || edx != 0x69746e65) {
        return;
    }

    // Is at least family 0fh?
    cpuid(1, &eax, &ebx, &ecx, &edx);
    if(((eax >> 8) & 0xf) != 0xf) {
        return;
    }

    debug(SUBSYS_STARTUP, "Enabling TLB flush filter\n");
    uint64_t hwcr = rdmsr(MSR_AMD_HWCR);
    hwcr &= ~AMD_HWCR_FFDIS;
    wrmsr(MSR_AMD_HWCR, hwcr);
}

static inline void enable_pge(void)
{
    uint32_t cr4;

    __asm volatile("mov %%cr4, %[cr4]" : [cr4] "=r" (cr4));
    cr4 |= CR4_PGE;
    __asm volatile("mov %[cr4], %%cr4" :: [cr4] "r" (cr4));
}

static inline void enable_pae(void)
{
    uint32_t cr4;

    __asm volatile("mov %%cr4, %[cr4]" : [cr4] "=r" (cr4));
    cr4 |= CR4_PAE;
    __asm volatile("mov %[cr4], %%cr4" :: [cr4] "r" (cr4));
}

static inline void enable_pse(void)
{
    uint32_t cr4;

    __asm volatile("mov %%cr4, %[cr4]" : [cr4] "=r" (cr4));
    cr4 |= CR4_PSE;
    __asm volatile("mov %[cr4], %%cr4" :: [cr4] "r" (cr4));
}

static inline void enable_pg(void)
{
    uint32_t cr0;

    __asm volatile("mov %%cr0, %[cr0]" : [cr0] "=r" (cr0));
    cr0 |= CR0_PG;
    __asm volatile("mov %[cr0], %%cr0" :: [cr0] "r" (cr0));
}

#ifdef __scc__
static inline void enable_caches(void)
{
    uint32_t cr0;

    __asm volatile("mov %%cr0, %[cr0]" : [cr0] "=r" (cr0));
    cr0 &= ~CR0_CD;
    cr0 &= ~CR0_NW;
    __asm volatile("mov %[cr0], %%cr0" :: [cr0] "r" (cr0));
}

static inline void enable_message_passing(void)
{
    uint32_t cr4;

    __asm volatile("mov %%cr4, %[cr4]" : [cr4] "=r" (cr4));
    cr4 |= CR4_MPE;
    __asm volatile("mov %[cr4], %%cr4" :: [cr4] "r" (cr4));
}
#endif

static inline void enable_monitor_mwait(void)
{
    uint32_t eax, ebx, ecx, edx;

    cpuid(1, &eax, &ebx, &ecx, &edx);

    if (ecx & (1 << 3)) {
        cpuid(5, &eax, &ebx, &ecx, &edx);
        debug(SUBSYS_STARTUP, "MONITOR/MWAIT supported: "
              "min size %"PRIu32" bytes, max %"PRIu32" bytes. %s %s\n",
              eax, ebx, (ecx & 2) ? "IBE" : "", (ecx & 1) ? "EMX" : "");
    }
}

/**
 * \brief Continue kernel initialization in kernel address space.
 *
 * This function resets paging to map out low memory and map in physical
 * address space, relocating all remaining data structures. It resets the
 * Global Descriptor Table for flat mode and to exclude legacy segments from
 * boot initialization code. It sets up the IDT for exception and interrupt
 * handling, initializes the local APIC and enables interrupts. After that it
 * calls kernel_startup(), which should not return (if it does, this function
 * halts the kernel).
 */
static void  __attribute__ ((noreturn, noinline)) text_init(void)
{
    // Relocate global to "memory"
    global = (struct global*)local_phys_to_mem((lpaddr_t)addr_global);

    // Relocate glbl_core_data to "memory"
    glbl_core_data = (struct x86_core_data *)
        local_phys_to_mem((lpaddr_t)glbl_core_data);

    // Map-out low memory
    paging_x86_32_reset();

    /*
     * Use new physical address space for video memory -- no calls to functions
     * that end up calling a conio.c function may be called between
     * paging_reset() and conio_relocate_vidmem()!
     */
#ifndef __scc__
    conio_relocate_vidmem(local_phys_to_mem(VIDEO_MEM));
#endif

    /*
     * Also reset the global descriptor table (GDT), so we get
     * segmentation again and can catch interrupts/exceptions (the IDT
     * needs the GDT).
     */
    gdt_reset();

    // Arch-independent early startup
    kernel_startup_early();

    // XXX: re-init the serial driver, in case the port changed after parsing args
    serial_console_init();

    // Setup IDT
    setup_default_idt();
    idt_initialized = true;

    // Initialize local APIC
    apic_init();

#ifdef __scc__
    enable_message_passing();

    // Initialize Rockcreek driver
    rck_init();

    // XXX: Set core ID and fake APIC ID to be the tile's core ID
    my_core_id = apic_id = rck_get_coreid();
    printf("My APIC ID: %d\n", apic_id);
#endif

    // do not remove/change this printf: needed by regression harness
#ifndef __scc__
    printf("Barrelfish CPU driver starting on x86_32 core %u\n", apic_id);

    if(apic_is_bsp()) {
        // Initialize classic (8259A) PIC
        pic_init();
    }

    // Initialize real-time clock
    rtc_init();
#else
    printf("Barrelfish CPU driver starting on scc core %u\n", apic_id);
#endif

    // Initialize local APIC timer
    if (kernel_ticks_enabled) {
        timing_calibrate();
        apic_timer_init(false, true);
        timing_apic_timer_set_ms(kernel_timeslice);
    } else {
        printk(LOG_WARN, "APIC timer disabled: NO timeslicing\n");
        apic_mask_timer();
    }

#ifndef __scc__
    // Initialize IPI notification mechanism
    ipi_notify_init();
#endif

    // Enable SYSCALL/SYSRET fast system calls
    /* enable_fast_syscalls(); */

#ifdef CONFIG_NXE
    // Enable "no execute" page-level protection bit
    addmsr(MSR_IA32_EFER, IA32_EFER_NXE);
#endif

    // Enable FPU and MMX
    enable_fpu();

#ifndef __scc__
    // Enable user-mode RDPMC opcode
    enable_user_rdpmc();

    // AMD64: Check if TLB flush filter is enabled
    enable_tlb_flush_filter();

    // Enable global pages
    enable_pge();

    // Check/Enable MONITOR/MWAIT opcodes
    enable_monitor_mwait();
#endif

    // Call main kernel startup function -- this should never return
    kernel_startup();

    halt();
    // Returning here will crash! -- low pages not mapped anymore!
}

/**
 * \brief Architecture-specific initialization function.
 *
 * This function is called by the bootup code in boot.S to initialize
 * architecture-specific stuff. It is expected to call the kernel main
 * loop. This function never returns.
 *
 * The kernel expects one of two magic values in 'magic' that determine how it
 * has been booted. If 'magic' is #MULTIBOOT_INFO_MAGIC the kernel has been
 * booted by a (Multiboot-compliant) bootloader and this is the first image on
 * the boot CPU. It will relocate itself to a default position. If 'magic' is
 * #KERNEL_BOOT_MAGIC it has been booted by another image of itself and is
 * running on an (so-called) application CPU. It expects 'dest' to be a physical
 * address pointing to the base of a memory area to relocate itself to.
 *
 * For x86-64, after performing some sanity checks to the kernel image, this
 * function first copies the whole kernel to a CPU-local version and then calls
 * local_init(), at the offset of the local copy, to initialize that local
 * copy. local_init() should never return.
 *
 * For bsp kernels, the void pointer is of type multiboot_info, for application
 * CPUs, it is of type global. Global carries a pointer to multiboot_info.
 * Global also contains pointers to memory that is shared between kernels.
 *
 * \param magic         Boot magic value
 * \param pointer       Pointer to Multiboot Info or to Global structure
 */
void arch_init(uint32_t magic, void *pointer)
{
#ifdef __scc__
    // Enable caches
    enable_caches();
#endif

    // Sanitize the screen
#ifndef __scc__
    conio_cls();
#endif
    serial_console_init();

    /* determine page-aligned physical address past end of multiboot */
    lvaddr_t dest = (lvaddr_t)&_start_kernel;
    if (dest & (BASE_PAGE_SIZE - 1)) {
        dest &= ~(BASE_PAGE_SIZE - 1);
        dest += BASE_PAGE_SIZE;
    }

    // XXX: print kernel address for debugging with gdb
    printf("Kernel starting at address 0x%"PRIxLVADDR"\n", local_phys_to_mem(dest));

    void __attribute__ ((noreturn)) (*reloc_text_init)(void) =
        (void *)local_phys_to_mem((lpaddr_t)text_init);
    struct Elf32_Shdr *rela, *symtab;
    struct x86_coredata_elf *elf;

    /*
     * If this is the boot image, make Multiboot information structure globally
     * known. Otherwise the passed value should equal the original structure.
     * If magic value does not match what we expect, we cannot proceed safely.
     */
    switch(magic) {
    case MULTIBOOT_INFO_MAGIC:
        {
            struct multiboot_info *mb = (struct multiboot_info *)pointer;

            elf = (struct x86_coredata_elf *)&mb->syms.elf;
            // We need the ELF section header table for relocation
            if (!(mb->flags & MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS)) {
                panic("Multiboot information structure does not include ELF section"
                      "header information -- Relocation impossible!");
            }
            assert(mb->flags & MULTIBOOT_INFO_FLAG_HAS_MMAP);

            // Determine where free RAM starts
            memset(glbl_core_data, 0, sizeof(struct x86_core_data));
            glbl_core_data->start_free_ram =
                ROUND_UP(max(multiboot_end_addr(mb), (uintptr_t)&_end_kernel),
                         BASE_PAGE_SIZE);

            glbl_core_data->mods_addr = mb->mods_addr;
            glbl_core_data->mods_count = mb->mods_count;
            glbl_core_data->cmdline = mb->cmdline;
            glbl_core_data->mmap_length = mb->mmap_length;
            glbl_core_data->mmap_addr = mb->mmap_addr;
        }
        break;

#ifdef __scc__
    case DITE_BOOT_MAGIC:
        {
            struct diteinfo *di = (struct diteinfo *)(dest - BASE_PAGE_SIZE);
            elf = (struct x86_coredata_elf *)&di->elf;

            memset(glbl_core_data, 0, sizeof(struct x86_core_data));
            glbl_core_data->start_free_ram = di->start_free_ram;
            glbl_core_data->mods_addr = di->mods_addr;
            glbl_core_data->mods_count = di->mods_count;
            glbl_core_data->cmdline = di->cmdline;
            glbl_core_data->mmap_length = di->mmap_length;
            glbl_core_data->mmap_addr = di->mmap_addr;
            glbl_core_data->urpc_frame_base = di->urpc_frame_base;
            glbl_core_data->urpc_frame_bits = di->urpc_frame_bits;
            glbl_core_data->src_core_id = di->src_core_id;
            glbl_core_data->chan_id = di->chan_id;
        }
        break;
#endif

    case KERNEL_BOOT_MAGIC:
        global = (struct global*)pointer;
        // Store the address of global to retrive it across relocation
        addr_global = (uint32_t)global;
        memset(&global->locks, 0, sizeof(global->locks));
        struct x86_core_data *core_data =
            (struct x86_core_data*)(dest - BASE_PAGE_SIZE);
        glbl_core_data = core_data;
        glbl_core_data->cmdline = (lpaddr_t)&core_data->kernel_cmdline;
        my_core_id = core_data->dst_core_id;
        elf = &core_data->elf;
        break;

    default:
        panic("Magic value does not match! (0x%x != 0x%"PRIu32" != 0x%x)",
              KERNEL_BOOT_MAGIC, magic, MULTIBOOT_INFO_MAGIC);
        break;
    }

    if(magic != KERNEL_BOOT_MAGIC) {
        // Construct the global structure and store its address to retrive it
        // across relocation
        memset(&global->locks, 0, sizeof(global->locks));
        addr_global            = (uint32_t)global;
    }

    // We're only able to process Elf32_Rela entries
    if (elf->size != sizeof(struct Elf32_Shdr)) {
        panic("ELF section header entry size mismatch!");
    }

    // Find relocation section
    rela = elf32_find_section_header_type((struct Elf32_Shdr *)
                                          (lpaddr_t)elf->addr,
                                          elf->num, SHT_REL);

    if (rela == NULL) {
        panic("Kernel image does not include relocation section!");
    }

    // Find symbol table section
    symtab = elf32_find_section_header_type((struct Elf32_Shdr *)
                                            (lpaddr_t)elf->addr,
                                            elf->num, SHT_DYNSYM);

    if (symtab == NULL) {
        panic("Kernel image does not include symbol table!");
    }

    // Kernel has to fit in mappable area
    assert((lvaddr_t)&_end_kernel < X86_32_PADDR_SPACE_LIMIT);

    // Map alias at MEMORY_OFFSET
    paging_init();

#ifdef CONFIG_PAE
    // Put CPU in PAE mode
    enable_pae();
#elif defined(CONFIG_PSE)
    // Enable page-size extensions
    enable_pse();
#endif

    // Enable paging
    enable_pg();

    // Relocate kernel image for top of memory
    elf32_relocate(X86_32_MEMORY_OFFSET + (lvaddr_t)&_start_kernel,
                   (lvaddr_t)&_start_kernel,
                   (struct Elf32_Rel *)(rela->sh_addr - X86_32_START_KERNEL_PHYS + &_start_kernel),
                   rela->sh_size,
                   (struct Elf32_Sym *)(symtab->sh_addr - X86_32_START_KERNEL_PHYS + &_start_kernel),
                   symtab->sh_size,
                   X86_32_START_KERNEL_PHYS, &_start_kernel);

    /*** Aliased kernel available now -- low memory still mapped ***/

    // Relocate stack to aliased location
    relocate_stack(X86_32_MEMORY_OFFSET);

    // Call aliased text_init() function and continue initialization
    reloc_text_init();
}
