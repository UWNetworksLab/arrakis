/**
 * \file
 * \brief Rudimentary ELF32 loader and handling routines.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
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
#include <barrelfish_kpi/paging_arch.h>
#include <barrelfish_kpi/types.h>
#include <errors/errno.h>
#include <elf/elf.h>

/**
 * \brief Calculates the base of the loadable portion of the elf image in
 * virtual memory.
 */
genvaddr_t elf_virtual_base32(struct Elf32_Ehdr *ehead)
{
    struct Elf32_Phdr *phead =
        (struct Elf32_Phdr *)((uintptr_t)ehead + (uintptr_t)ehead->e_phoff);

    genvaddr_t retval = 0;
    int i;

    for (i = 0; i < ehead->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];
        if (p->p_type == PT_LOAD) {
            if(retval == 0) {
                retval = p->p_vaddr;
            }
            retval = p->p_vaddr < retval ? p->p_vaddr : retval;
        }
    }

    return retval;
}

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
struct Elf32_Shdr *
elf32_find_section_header_type(struct Elf32_Shdr *shdr,
                               uint32_t entries, uint32_t type)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf32_Shdr *s = &shdr[i];

        if(s->sh_type == type) {
            return s;
        }
    }
    return NULL;
}

/**
 * \brief Return pointer to section header with given name.
 *
 * @param elf_base      Address of ELF header
 * @param elf_bytes     Size of ELF file.
 * @param section_name  Named section to look for.
 *
 * @return Pointer to ELF section header with name, or NULL.
 */
struct Elf32_Shdr *
elf32_find_section_header_name(genvaddr_t  elf_base,
                               size_t      elf_bytes,
                               const char* section_name)
{
    lvaddr_t elf_lbase = (lvaddr_t)elf_base;
    struct Elf32_Ehdr *head = (struct Elf32_Ehdr *)elf_lbase;

    if (elf_bytes < sizeof(struct Elf32_Ehdr) || !IS_ELF(*head) ||
        head->e_ident[EI_CLASS] != ELFCLASS32) {
        return NULL;
    }

    struct Elf32_Shdr *shead =
        (struct Elf32_Shdr *)(elf_lbase + (uintptr_t)head->e_shoff);

    struct Elf32_Shdr *strtab =
        elf32_find_section_header_type(shead, head->e_shnum, SHT_STRTAB);

    if (strtab == NULL)
    {
        return NULL;
    }

    for (uint32_t i = 0; i < head->e_shnum; i++)
    {
        const char* strings = (const char*)(elf_lbase +
                                            (size_t)strtab->sh_offset);
        if (!strcmp(section_name, strings + shead[i].sh_name)) {
            return &shead[i];
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
static struct Elf32_Shdr *
elf32_find_section_header_vaddr(struct Elf32_Shdr *shdr,
                                uint32_t entries, genvaddr_t addr)
{
    int i;

    for(i = 0; i < entries; i++) {
        struct Elf32_Shdr *s = &shdr[i];

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
 * \param start         Original base address of the ELF image.
 * \param vbase         Pointer to ELF image in virtual memory.
 */
void elf32_relocate(genvaddr_t dst, genvaddr_t src,
                    struct Elf32_Rel * SAFE NONNULL rel, size_t size,
                    struct Elf32_Sym * SAFE NONNULL symtab, size_t symsize,
                    genvaddr_t start, void *vbase)
{
    genvaddr_t base = dst - src, abase = dst - start;

    for(int i = 0; i < size / sizeof(struct Elf32_Rel); i++) {
        struct Elf32_Rel *r = &rel[i];
        uint32_t type = ELF32_R_TYPE(r->r_info);
        uint32_t *addr = (uint32_t *)((char *)vbase + r->r_offset - start);

        switch(type) {
        case R_ARM_ABS32: //fall through
        case R_386_32:
            {
                uint32_t sym = ELF32_R_SYM(r->r_info);
                assert(sym < symsize / sizeof(struct Elf32_Sym));
                /* assert(symtab[sym].st_value != 0); */
                *addr = abase + symtab[sym].st_value;
            }
            break;
        case R_ARM_RELATIVE: //fall through
        case R_386_RELATIVE:
            *addr += base;
            break;

        default:
            printf("elf_relocate: relocation %d type %"PRIu32"\n", i, type);
            assert(!"Unimplemented: Cannot handle relocation type");
            break;
        }
    }
}

// Return if machine is big endian.
// TODO: move to appropriate place
static bool is_big_endian(void)
{
    int i = 1;
    char c[sizeof(int)];
    memcpy(c, &i, sizeof(int));
    return !(c[0] & 1);
}

/**
 * \brief Load ELF32 binary image into memory
 *
 * This function loads an ELF32 binary image, based at 'base' and of size
 * 'size' into the memory provided by 'allocate'
 *
 * \param em_machine    ELF machine type.
 * \param allocate      Memory allocation function.
 * \param state         Pointer to state for allocation function.
 * \param base          Base address of ELF32 binary image in memory.
 * \param size          Size of ELF32 binary image in bytes.
 * \param retentry      Used to return entry point address
 * \param ret_tlsbase   Used to return TLS block base address
 * \param ret_tlsinitlen Used to return length of initialised TLS data block
 * \param ret_tlstotallen Used to return total length of TLS data
 */
errval_t elf32_load(uint16_t em_machine, elf_allocator_fn allocate_func,
                    void *state, lvaddr_t base, size_t size,
                    genvaddr_t *retentry,
                    genvaddr_t *ret_tlsbase, size_t *ret_tlsinitlen,
                    size_t *ret_tlstotallen)
{
    struct Elf32_Ehdr *head = (struct Elf32_Ehdr *)base;
    errval_t err;
    int i;

    // Check for valid file size
    if (size < sizeof(struct Elf32_Ehdr)) {
        return ELF_ERR_FILESZ;
    }

    // Stage 1: Check for compatible ELF32 header: check endianess
    if(is_big_endian() && head->e_ident[EI_DATA] != ELFDATA2MSB){
        return ELF_ERR_HEADER;
    } else if(!is_big_endian() && head->e_ident[EI_DATA] != ELFDATA2LSB){
        return ELF_ERR_HEADER;
    }

    // Stage 2: Check for compatible ELF32 header
    if (!IS_ELF(*head)
        || head->e_ident[EI_CLASS] != ELFCLASS32
//        || head->e_ident[EI_DATA] != ELFDATA2MSB   //Enhanced with a function to check machine endianess
        || head->e_ident[EI_VERSION] != EV_CURRENT
        || head->e_ident[EI_OSABI] != ELFOSABI_SYSV
        || head->e_ident[EI_ABIVERSION] != 0
        || (head->e_type != ET_EXEC && head->e_type != ET_DYN)
        || head->e_machine != em_machine
        || head->e_version != EV_CURRENT) {
        return ELF_ERR_HEADER;
    }

    // More sanity checks
    if (head->e_phoff + head->e_phentsize * head->e_phnum > size
        || head->e_phentsize != sizeof(struct Elf32_Phdr)) {
        return ELF_ERR_PROGHDR;
    }

    struct Elf32_Shdr *shead =
        (struct Elf32_Shdr *)(base + (uintptr_t)head->e_shoff);
    struct Elf32_Shdr *rela =
        elf32_find_section_header_type(shead, head->e_shnum, SHT_REL);
    struct Elf32_Shdr *symtab =
        elf32_find_section_header_type(shead, head->e_shnum, SHT_SYMTAB);

    size_t rela_size = rela ? rela->sh_size : 0, new_rela_size = 0;
    struct Elf32_Shdr *new_rela = NULL;

    // Find dynamic program header, if any
    struct Elf32_Phdr *phead =
        (struct Elf32_Phdr *)(base + (uintptr_t)head->e_phoff);
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];

        if (p->p_type == PT_DYNAMIC) {
            struct Elf32_Dyn *dynamic = (void *)(base + (uintptr_t)p->p_offset);
            int n_dynamic = p->p_filesz / sizeof(struct Elf32_Dyn);
            for (int j = 0; j < n_dynamic; j++) {
                switch (dynamic[j].d_tag) {
                case DT_RELA:
                    // virtual address of relocations, look for matching section
                    new_rela =
                        elf32_find_section_header_vaddr(shead, head->e_shnum,
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
                        elf32_find_section_header_vaddr(shead, head->e_shnum,
                                                        dynamic[j].d_un.d_val);
                    break;

                case DT_SYMENT:
                    assert(dynamic[j].d_un.d_val == sizeof(struct Elf32_Sym));
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

    genvaddr_t tls_base = 0;
    size_t tls_init_len = 0, tls_total_len = 0;

    // Process program headers to load file
    for (i = 0; i < head->e_phnum; i++) {
        struct Elf32_Phdr *p = &phead[i];

        if (p->p_type == PT_LOAD) {
            // Map segment in user-space memory
            void *dest = NULL;
            err = allocate_func(state, p->p_vaddr, p->p_memsz, p->p_flags, &dest);
            if (err_is_fail(err)) {
                return err_push(err, ELF_ERR_ALLOCATE);
            }
            assert(dest != NULL);

            // Copy file segment into memory
            memcpy(dest, (void *)(base + (uintptr_t)p->p_offset), p->p_filesz);

            // Initialize rest of memory segment (ie. BSS) with all zeroes
            memset((char *)dest + p->p_filesz, 0, p->p_memsz - p->p_filesz);

            // Apply relocations
            if (rela != NULL && symtab != NULL) {
                elf32_relocate(p->p_vaddr, p->p_vaddr,
                               (struct Elf32_Rel *)
                               (base + (uintptr_t)rela->sh_offset),
                               rela_size,
                               (struct Elf32_Sym *)
                               (base + (uintptr_t)symtab->sh_offset),
                               symtab->sh_size, p->p_vaddr, dest);
            }
        } else if (p->p_type == PT_TLS) {
            assert(p->p_vaddr != 0);
            assert(tls_base == 0); // if not we have multiple TLS sections!
            tls_base = p->p_vaddr;
            tls_init_len = p->p_filesz;
            tls_total_len = p->p_memsz;
        }
    }

    if (retentry != NULL) {
        *retentry = head->e_entry;
    }

    if (ret_tlsbase != NULL) {
        *ret_tlsbase = tls_base;
    }

    if (ret_tlsinitlen != NULL) {
        *ret_tlsinitlen = tls_init_len;
    }

    if (ret_tlstotallen != NULL) {
        *ret_tlstotallen = tls_total_len;
    }

    return SYS_ERR_OK;
}
