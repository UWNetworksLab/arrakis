/**
 * \file
 * \brief Rudimentary ELF64 loader and handling routines.
 *
 * Note that on 32-bit platforms, this loader is only able to load
 * ELF64 files that it can address (ie. those that are not bigger than
 * what fits into a 32-bit address space).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <stddef.h>
#include <string.h>
#include "elf.h"

/**
 * \brief Return pointer to relocation section ELF header.
 *
 * This function finds and returns a pointer to the first ELF section
 * header of type 'type'.
 *
 * \param shdr          Pointer to head of ELF section header table.
 * \param entries       Number of entries in the ELF section header table.
 * \param type          ELF section header type to look for.
 *
 * \return Pointer to first ELF section header of type 'type', or NULL.
 */
static struct Elf64_Shdr *
elf64_find_section_header_type(struct Elf64_Shdr * shdr,
                               uint32_t entries, uint32_t type)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf64_Shdr *s = &shdr[i];

        if(s->sh_type == type) {
            return s;
        }
    }

    return NULL;
}

/**
 * \brief Return pointer to relocation section ELF header.
 *
 * This function finds and returns a pointer to the first ELF section
 * header at virtual address 'addr'.
 *
 * \param shdr          Pointer to head of ELF section header table.
 * \param entries       Number of entries in the ELF section header table.
 * \param addr          Virtual address to look for
 *
 * \return Pointer to first ELF section header loaded at 'addr', or NULL.
 */
static struct Elf64_Shdr *
elf64_find_section_header_vaddr(struct Elf64_Shdr * shdr,
                                uint32_t entries, genvaddr_t addr)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf64_Shdr *s = &shdr[i];

        if(s->sh_addr == addr) {
            return s;
        }
    }

    return NULL;
}

/**
 * \brief Relocates the ELF image from src to dst.
 *
 * This function processes the ELF relocation section 'rela' of size 'size' of
 * the ELF image, formerly located at 'src', to the new location 'dst'.
 * Relocation is necessary for certain variables that cannot be coded as
 * position-independent code.
 *
 * \param dst           Address to relocate to.
 * \param src           Former base address of the ELF image.
 * \param rela          Pointer to relocation section of the ELF image.
 * \param size          Size in bytes of the ELF relocation section.
 * \param symtab        Pointer to ELF symbol table.
 * \param symsize       Size in bytes of the ELF symbol table.
 * \param start         Original base address of the ELF image (needed for
 * symbol-table-based relocations -- we don't touch the symbol table).
 * \param vbase         Pointer to ELF image in virtual memory.
 */
void elf64_relocate(genvaddr_t dst, genvaddr_t src,
                    struct Elf64_Rela *rela, size_t size,
                    struct Elf64_Sym *symtab, size_t symsize,
                    genvaddr_t start, void *vbase)
{
    genvaddr_t base = dst - src, abase = dst - start;

    for(int i = 0; i < size / sizeof(struct Elf64_Rela); i++) {
        struct Elf64_Rela *r = &rela[i];
        uint32_t type = ELF64_R_TYPE(r->r_info);
        uint64_t *addr = (uint64_t *)((char *)vbase + r->r_offset - start);

        switch(type) {
        case R_X86_64_NONE:
            // Do nothing
            break;

        case R_X86_64_64:{
            uint32_t sym = ELF64_R_SYM(r->r_info);
            assert(sym < symsize / sizeof(struct Elf64_Sym));
#if 0 // XXX: symbols should be 0 but this fires sometimes
            assert(symtab[sym].st_value != 0);
#endif
            *addr = abase + symtab[sym].st_value + r->r_addend;
            break;}

        case R_X86_64_RELATIVE:
            // FIXME: why doesn't the following work? -AB
            // I don't think this makes sense. It is a relative
            // relocation from the old position. Thus, base and not
            // abase should be used. Further, since r->r_addend is not
            // updated between relocations, this will fail as soon as
            // a binary is relocated more than once. -SP
            //*addr = abase + r->r_addend;
            *addr += base;
            break;

        default:
            printf("elf_relocate: relocation %d type %d\n", i, type);
            assert(!"Unimplemented: Cannot handle relocation type");
            break;
        }
    }
}

/**
 * \brief Load ELF64 binary image into memory
 *
 * This function loads an ELF64 binary image, based at 'base' and of size
 * 'size' into the memory provided by 'allocate'
 *
 * \param allocate      Memory allocation function.
 * \param state         Pointer to state for allocation function.
 * \param base          Base address of ELF64 binary image in memory.
 * \param size          Size of ELF64 binary image in bytes.
 * \param retentry      Used to return entry point address
 */
errval_t elf64_load(elf_allocator_fn allocate_func,
                    void *state, lvaddr_t base, size_t size,
                    genvaddr_t *retentry)
{
    struct Elf64_Ehdr   *head = (struct Elf64_Ehdr *)base;
    errval_t err;
    int i;

    // Check for valid file size
    if (size < sizeof(struct Elf64_Ehdr)) {
        return -1;
    }

    // Check for compatible ELF64 header
    if (!IS_ELF(*head)
        || head->e_ident[EI_CLASS] != ELFCLASS64
        || head->e_ident[EI_DATA] != ELFDATA2LSB
        || head->e_ident[EI_VERSION] != EV_CURRENT
        || head->e_ident[EI_OSABI] != ELFOSABI_SYSV
        || head->e_ident[EI_ABIVERSION] != 0
        || (head->e_type != ET_EXEC && head->e_type != ET_DYN)
        || head->e_version != EV_CURRENT) {
        return -2;
    }

    // More sanity checks
    if (head->e_phoff + head->e_phentsize * head->e_phnum > size
        || head->e_phentsize != sizeof(struct Elf64_Phdr)) {
        return -3;
    }

    struct Elf64_Shdr *shead =
        (struct Elf64_Shdr *)(base + (uintptr_t)head->e_shoff);
    struct Elf64_Shdr *rela =
        elf64_find_section_header_type(shead, head->e_shnum, SHT_RELA);
    struct Elf64_Shdr *symtab =
        elf64_find_section_header_type(shead, head->e_shnum, SHT_SYMTAB);

    size_t rela_size = rela ? rela->sh_size : 0, new_rela_size = 0;
    struct Elf64_Shdr *new_rela = NULL;

    // Find dynamic program header, if any
    struct Elf64_Phdr *phead =
        (struct Elf64_Phdr *)(base + (uintptr_t)head->e_phoff);
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];

        if (p->p_type == PT_DYNAMIC) {
            struct Elf64_Dyn *dynamic = (void *)(base + (uintptr_t)p->p_offset);
            int n_dynamic = p->p_filesz / sizeof(struct Elf64_Dyn);
            for (int j = 0; j < n_dynamic; j++) {
                switch (dynamic[j].d_tag) {
                case DT_RELA:
                    // virtual address of relocations, look for matching section
                    new_rela =
                        elf64_find_section_header_vaddr(shead, head->e_shnum,
                                                        dynamic[j].d_un.d_val);
                    break;

                case DT_RELASZ:
                    // store size of relocations, as they may cover more than
                    // one section
                    new_rela_size = dynamic[j].d_un.d_val;
                    break;

                case DT_SYMTAB:
                    // virtual address of symtab, look for matching section
                    symtab =
                        elf64_find_section_header_vaddr(shead, head->e_shnum,
                                                        dynamic[j].d_un.d_val);
                    break;

                case DT_SYMENT:
                    assert(dynamic[j].d_un.d_val == sizeof(struct Elf64_Sym));
                    break;
                }
            }

            if (new_rela != NULL) {
                assert(new_rela_size != 0);
                rela = new_rela;
                rela_size = new_rela_size;
            }
            break;
        }
    }

    // Process program headers to load file
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf64_Phdr *p = &phead[i];

        if (p->p_type == PT_LOAD) {
            //printf("Loading segment: start=0x%lx, size=%lu, flags=%d\n",
            //p->p_vaddr, p->p_memsz, p->p_flags);

            // Map segment in user-space memory
            void *dest = NULL;
            err = allocate_func(state, (genvaddr_t)p->p_vaddr, p->p_memsz, p->p_flags, &dest);
            if (err != 0) {
                return -4;
            }
            assert(dest != NULL);

            // Copy file segment into memory
            memcpy(dest, (void *)(base + (uintptr_t)p->p_offset), p->p_filesz);

            // Initialize rest of memory segment (ie. BSS) with all zeroes
            memset((char *)dest + p->p_filesz, 0, p->p_memsz - p->p_filesz);

            // Apply relocations
            if (rela != NULL && symtab != NULL) {
                elf64_relocate(p->p_vaddr, p->p_vaddr,
                               (struct Elf64_Rela *)
                               (base + (uintptr_t)rela->sh_offset),
                               rela_size,
                               (struct Elf64_Sym *)
                               (base + (uintptr_t)symtab->sh_offset),
                               symtab->sh_size, p->p_vaddr, dest);
            }
        }
    }

    if (retentry != NULL) {
        *retentry = head->e_entry;
    }

    return 0;
}
