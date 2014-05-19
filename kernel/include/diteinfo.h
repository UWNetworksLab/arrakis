/**
 * \file
 * \brief Data sent to a newly booted SCC kernel by the bootloader
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DITEINFO_H
#define DITEINFO_H

#ifndef __ASSEMBLER__

struct diteinfo_modinfo {
    uint32_t    mod_start;
    uint32_t    mod_end;
    uint32_t    string;
    uint32_t    reserved;
};

struct diteinfo_mmap {
    uint32_t    size;
    uint64_t    base_addr;
    uint64_t    length;
    uint32_t    type;
} __attribute__ ((packed));

struct diteinfo_elf {
    uint32_t    num;
    uint32_t    size;
    uint32_t    addr;
    uint32_t    shndx;
};

struct diteinfo {
    struct diteinfo_elf elf;
    uint32_t    cmdline;
    uint32_t    mods_count;
    uint32_t    mods_addr;
    uint32_t    mmap_length;
    uint32_t    mmap_addr;
    uint32_t    start_free_ram;

    // XXX: Remove this once app core boot protocol stabilizes
    genpaddr_t urpc_frame_base;
    uint8_t urpc_frame_bits;
    coreid_t src_core_id;
    uint32_t chan_id;

    struct diteinfo_modinfo    modinfo[20];
    struct diteinfo_mmap       mmap[20];

    char        sh[2048];
    char        strings[1024];
} __attribute__ ((packed));

#endif

#define DITEINFO_SIZE  4096

#define DITE_BOOT_MAGIC 0xd11eb001

#endif
