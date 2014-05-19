/**
 * \file
 * \brief Elver - Intermediary stage bootloader
 *
 * Elver is used to switch the system into 64-bit long-mode and load
 * the kernel, which is a relocatable ELF64 image. Unfortunately, GRUB
 * is not able to this without a patch. This is purely for
 * backwards-compatibility. As soon as bootloaders support loading
 * relocatable ELF64 images into 64-bit mode, this can be dropped.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
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

#define PTABLE_EXECUTE_DISABLE  (1LL << 63)
#define PTABLE_GLOBAL_PAGE      (1L << 8)
#define PTABLE_ATTR_INDEX       (1L << 7)
#define PTABLE_DIRTY            (1L << 6)
#define PTABLE_ACCESSED         (1L << 5)
#define PTABLE_CACHE_DISABLED   (1L << 4)
#define PTABLE_WRITE_THROUGH    (1L << 3)
#define PTABLE_USER_SUPERVISOR  (1L << 2)
#define PTABLE_READ_WRITE       (1L << 1)
#define PTABLE_PRESENT          (1L << 0)

#define BASE_PAGE_SIZE                  0x1000

#define PTABLE_SIZE             512     /**< Page directory/table size */
#define PTABLE_MASK             0x1ff   /**< Page dir/table address mask */
#define PTABLE_CLEAR            0       /**< Bitmap of a clear table entry */

#define PML4_BASE(base)         (((uint64_t)(base) >> 39) & PTABLE_MASK)
#define PDPT_BASE(base)         (((uint64_t)(base) >> 30) & PTABLE_MASK)
#define PDIR_BASE(base)         (((uint64_t)(base) >> 21) & PTABLE_MASK)
#define PTABLE_BASE(base)       (((uint64_t)(base) >> 12) & PTABLE_MASK)

/// Round up n to the next multiple of size
#define ROUND_UP(n, size)           ((((n) + (size) - 1)) & (~((size) - 1)))

union pdir_entry {
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
        uint64_t        reserved2       :12;
        uint64_t        available2      :11;
        uint64_t        execute_disable :1;
    } d;
};

/**
 * A page table entry.
 */
union ptable_entry {
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
        uint64_t        base_addr       :19;
        uint64_t        reserved2       :12;
        uint64_t        available2      :11;
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
        uint64_t        available       :3;
        uint64_t        base_addr       :28;
        uint64_t        reserved2       :12;
        uint64_t        available2      :11;
        uint64_t        execute_disable :1;
    } base;
};

struct multiboot_info *multiboot_info;
uint32_t eax;

union pdir_entry boot_pml4[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static union pdir_entry pdpt[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

static union ptable_entry pdir[PTABLE_SIZE]
__attribute__ ((aligned(BASE_PAGE_SIZE)));

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

static struct multiboot_modinfo *multiboot_find_module(const char *basename)
{
    struct multiboot_modinfo *mod = (struct multiboot_modinfo *)
        multiboot_info->mods_addr;

    for(size_t i = 0; i < multiboot_info->mods_count; i++) {
        const char *modname = strrchr((char *)mod[i].string, '/');

        if(modname == NULL) {
            modname = (char *)mod[i].string;
        } else {
            modname++;
        }

        if(!strncmp(modname, basename, strlen(basename))) {
            return &mod[i];
        }
    }

    return NULL;
}

static uintptr_t multiboot_end_addr(void)
{
    lpaddr_t end = ((lpaddr_t)multiboot_info) + sizeof(struct multiboot_info);
    struct multiboot_info *mi = multiboot_info;

#define CHECK(pa)           { lpaddr_t tmp = pa; if (tmp > end) { end = tmp; } }
#define CHECK_STR(pstr)     CHECK(pstr + strlen((char *)pstr) + 1)

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_CMDLINE) {
        CHECK_STR(mi->cmdline)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_MODS) {
        struct multiboot_modinfo *mod = (void *)mi->mods_addr;

        for(int i = 0; i < mi->mods_count; i++) {
            CHECK(mod[i].mod_end)
            CHECK_STR(mod[i].string)
        }
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS) {
        CHECK(mi->syms.elf.addr + mi->syms.elf.num * mi->syms.elf.size)
        /* FIXME: does this include mi_elfshdr_shndx?? */
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_MMAP) {
        CHECK(mi->mmap_addr + mi->mmap_length)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_DRIVES) {
        CHECK(mi->drives_addr + mi->drives_length)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_LOADERNAME) {
        CHECK_STR(mi->boot_loader_name)
    }

    /* TODO: config table, APM table, VBE */

#undef CHECK
#undef CHECK_STR

    return end;
}

static inline void paging_map_table(union pdir_entry *entry, uint64_t base)
{
    entry->raw = PTABLE_CLEAR;

    entry->d.present = 1;
    entry->d.read_write = 1;
    entry->d.user_supervisor = 1;
    entry->d.base_addr = base >> 12;
}

static inline void paging_map_large(union ptable_entry *entry, uint64_t base,
                                    uint64_t bitmap)
{
    entry->raw = PTABLE_CLEAR;

    entry->large.present = bitmap & PTABLE_PRESENT ? 1 : 0;
    entry->large.read_write = bitmap & PTABLE_READ_WRITE ? 1 : 0;
    entry->large.user_supervisor = bitmap & PTABLE_USER_SUPERVISOR ? 1 : 0;
    entry->large.write_through = bitmap & PTABLE_WRITE_THROUGH ? 1 : 0;
    entry->large.cache_disabled = bitmap & PTABLE_CACHE_DISABLED ? 1 : 0;
    entry->large.global = bitmap & PTABLE_GLOBAL_PAGE ? 1 : 0;
    entry->large.attr_index = bitmap & PTABLE_ATTR_INDEX ? 1 : 0;
    entry->large.execute_disable = bitmap & PTABLE_EXECUTE_DISABLE ? 1 : 0;
    entry->large.always1 = 1;
    entry->large.base_addr = base >> 21;
}

genvaddr_t kernel_entry;

static void set_elf_headers(uint32_t base)
{
    struct Elf64_Ehdr *head = (struct Elf64_Ehdr *)base;

    multiboot_info->syms.elf.num = head->e_shnum;
    multiboot_info->syms.elf.size = head->e_shentsize;
    multiboot_info->syms.elf.addr = base + head->e_shoff;
    multiboot_info->syms.elf.shndx = head->e_shstrndx;
    multiboot_info->flags |= MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS;
}

int startup(uint32_t magic, struct multiboot_info *mb);

int startup(uint32_t magic, struct multiboot_info *mb)
{
    errval_t err;

    // Store important registers
    multiboot_info = mb;
    eax = magic;

    // Look for the kernel to boot, which may have several names
    struct multiboot_modinfo *kernel;
    kernel = multiboot_find_module("cpu");
    if (kernel == NULL) {
        kernel = multiboot_find_module("kernel");
    }

    // Reserve a page before kernel start
    phys_alloc_start = ROUND_UP(multiboot_end_addr(), BASE_PAGE_SIZE) +
        BASE_PAGE_SIZE;
    lpaddr_t kernel_start = phys_alloc_start;

    err = elf64_load(EM_X86_64, linear_alloc, NULL, kernel->mod_start,
                     MULTIBOOT_MODULE_SIZE(*kernel), &kernel_entry, NULL, NULL, NULL);
    if (err_is_fail(err)) {
        printf("Elver ELF loading failed!\n");
        return -1;
    }

    // Relocate kernel image
    struct Elf64_Ehdr *cpu_head = (struct Elf64_Ehdr *)kernel->mod_start;
    struct Elf64_Shdr *rela, *symtab, *symhead =
        (struct Elf64_Shdr *)(kernel->mod_start + (uintptr_t)cpu_head->e_shoff);
    genvaddr_t elfbase = elf_virtual_base64(cpu_head);
    rela = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_RELA);
    symtab = elf64_find_section_header_type(symhead, cpu_head->e_shnum, SHT_DYNSYM);
    elf64_relocate(kernel_start, elfbase,
                   (struct Elf64_Rela *)(uintptr_t)(kernel->mod_start + rela->sh_offset),
                   rela->sh_size,
                   (struct Elf64_Sym *)(uintptr_t)(kernel->mod_start + symtab->sh_offset),
                   symtab->sh_size,
                   elfbase, (void *)kernel_start);
    kernel_entry = kernel_entry - elfbase + kernel_start;

    // Identity map the first 1 GByte of physical memory in long mode
    paging_map_table(&boot_pml4[PML4_BASE(0)], (uint64_t)(uint32_t)pdpt);
    paging_map_table(&pdpt[PDPT_BASE(0)], (uint64_t)(uint32_t)pdir);
    for(uint32_t i = 0; i < 0xf000000; i += 0x200000) {
        paging_map_large(&pdir[PDIR_BASE(i)], i, PTABLE_PRESENT
                         | PTABLE_READ_WRITE | PTABLE_USER_SUPERVISOR);
    }

    // Put real kernel's ELF symbols into multiboot
    set_elf_headers(kernel->mod_start);

    return 0;
}
