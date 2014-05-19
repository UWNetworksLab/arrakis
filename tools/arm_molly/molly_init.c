// Simple boot-loader.
//
// This code is only intended for use on M5 where it is started via
// molly_boot.S which runs on Core 0.

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>

#include "../../kernel/include/multiboot.h"



#define BASE_PAGE_SIZE                  0x1000
#define ALIGNMENT                       0x10000

#if defined(HETEROPANDA) && defined(__ARM_ARCH_7A__)
//this is code for a cortex-A9 image that will boot up a cortex-m3 core

//symbol declared by linker script: start of heteropanda_slave image
extern void *_start_slave;
#endif

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))


static lpaddr_t phys_alloc_start;

static errval_t linear_alloc(void *s, genvaddr_t base, size_t size, uint32_t flags,
                             void **ret)
{
    // round to base page size
    uint32_t npages = (size + BASE_PAGE_SIZE - 1) / BASE_PAGE_SIZE;

    /* *ret = (void *)(uintptr_t)base; */
    *ret = (void *)phys_alloc_start;

    phys_alloc_start += npages * BASE_PAGE_SIZE;
    return SYS_ERR_OK;
}

genvaddr_t kernel_entry;

// Prototypes for functions from molly_boot.S:
void molly_to_kernel_transition(genvaddr_t entry_addr, void* mbi_ptr);


// Prototypes for functions from arm_gem5_kernel.c:
extern struct multiboot_info *molly_get_mbi(void);

// Prototypes for symbols declared via linker script:
extern void *_start_img;
extern void *_end_img;

void molly_init(void);

void molly_init(void)
{
    errval_t err;

    //uint32_t kernel_blob_size = (uint32_t)(&kernel_blob_end - &kernel_blob_start);


    struct multiboot_info *mbi = molly_get_mbi();

    // align kernel start to 16KB
    phys_alloc_start = ROUND_UP((uint32_t) &_end_img, ALIGNMENT); //+
                // BASE_PAGE_SIZE);
    lpaddr_t kernel_start = phys_alloc_start;

    // Load the kernel out from the boot image:
    struct multiboot_modinfo *mbi_mods;
    mbi_mods = (struct multiboot_modinfo*)(uint32_t)(mbi->mods_addr);
    void *kernel = (void*)(uint32_t)(mbi_mods[0].mod_start);
    uint32_t kernel_bytes = mbi_mods[0].mod_end - mbi_mods[0].mod_start;

    err = elf32_load(EM_ARM, linear_alloc, NULL, (uint32_t)kernel,
                    kernel_bytes, &kernel_entry, NULL, NULL, NULL);
    if (err_is_fail(err)) {
        return;
    }

    // Relocate kernel image
    struct Elf32_Ehdr *cpu_head = (struct Elf32_Ehdr *)kernel;
    struct Elf32_Shdr *rela, *symtab, *symhead =
        (struct Elf32_Shdr *)(kernel + (uintptr_t)cpu_head->e_shoff);
    genvaddr_t elfbase = elf_virtual_base32(cpu_head);
    rela = elf32_find_section_header_type(symhead, cpu_head->e_shnum, SHT_REL);
    symtab = elf32_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);
    elf32_relocate(kernel_start, elfbase,
                   (struct Elf32_Rel *)(uintptr_t)(kernel + rela->sh_offset),
                   rela->sh_size,
                   (struct Elf32_Sym *)(uintptr_t)(kernel + symtab->sh_offset),
                   symtab->sh_size,
                   elfbase, (void *)kernel_start);
    kernel_entry = kernel_entry - elfbase + kernel_start;

    //initialize arm_core_data elf info for relocated header
    mbi->syms.elf.num = cpu_head->e_shnum;
    mbi->syms.elf.size = cpu_head->e_shentsize;
    mbi->syms.elf.addr = (uint32_t)kernel+ cpu_head->e_shoff;
    mbi->syms.elf.shndx = cpu_head->e_shstrndx;

#if defined(HETEROPANDA) && defined(__ARM_ARCH_7A__)
//this is code for a cortex-A9 image that will boot up a cortex-m3 core

//XXX: HACK: put the address of the slave image into mbi->mem_lower (which is otherwise unused)
//so the kernel will know where to find it
    mbi->mem_lower= (uint32_t) &_start_slave;
#endif  

    molly_to_kernel_transition((uintptr_t)kernel_entry,
                               mbi
                               );


}
