/**
 * \file
 * \brief Multiboot utility functions.
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
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <paging_kernel_arch.h>
#include <elf/elf.h>
#include <kernel_multiboot.h>
#include <target/x86/barrelfish_kpi/coredata_target.h>
#ifdef __scc__
#       include <rck.h>
#       include <init.h>
#endif

struct x86_core_data *glbl_core_data = NULL;

/**
 * \brief Return multiboot module by trailing pathname.
 *
 * Returns a pointer to the multiboot module which has a trailing
 * pathname of 'pathname' or NULL if there is none.
 *
 * \param pathname      Trailing pathname of module to look for.
 *
 * \return Pointer to multiboot module or NULL if none found.
 */
struct multiboot_modinfo *multiboot_find_module(const char *pathname)
{
    struct multiboot_modinfo *mod = (struct multiboot_modinfo *)
        local_phys_to_mem(glbl_core_data->mods_addr);

    for(size_t i = 0; i < glbl_core_data->mods_count; i++) {
        const char *modname = MBADDR_ASSTRING(mod[i].string), *endstr;

        // Strip off trailing whitespace
        if(strchr(modname, ' ')) {
            endstr = strchr(modname, ' ');
        } else {
            endstr = modname + strlen(modname);
        }

        if(!strncmp(endstr - strlen(pathname), pathname, strlen(pathname))) {
            return &mod[i];
        }
    }

    return NULL;
}

/**
 * \brief Return end address of multiboot image
 *
 * This function is used to compute a safe location to place the boot kernel.
 */
uintptr_t multiboot_end_addr(struct multiboot_info *mi)
{
    lpaddr_t end = (lpaddr_t)mi + sizeof(struct multiboot_info);

#define CHECK(pa)           { lpaddr_t tmp = pa; if (tmp > end) { end = tmp; } }
#define CHECK_STR(pstr)     CHECK(pstr + strlen((char *)(uintptr_t)pstr) + 1)

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_CMDLINE) {
        CHECK_STR(mi->cmdline)
    }

    if (mi->flags & MULTIBOOT_INFO_FLAG_HAS_MODS) {
        struct multiboot_modinfo *mod = (void *)(uintptr_t)mi->mods_addr;

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
