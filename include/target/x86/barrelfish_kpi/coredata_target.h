/**
 * \file
 * \brief Data sent to a newly booted x86 kernel
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef COREDATA_H
#define COREDATA_H

struct x86_coredata_modinfo {
    uint32_t    mod_start;
    uint32_t    mod_end;
    uint32_t    string;
    uint32_t    reserved;
};

struct x86_coredata_mmap {
    uint32_t    size;
    uint64_t    base_addr;
    uint64_t    length;
    uint32_t    type;
} __attribute__ ((packed));

struct x86_coredata_elf {
    uint32_t    num;
    uint32_t    size;
    uint32_t    addr;
    uint32_t    shndx;
};

/**
 * \brief Data sent to a newly booted kernel
 *
 * \bug Should use mackerel to define this struct instead of packing
 * it so that it matches up between heterogeneous cores.
 */
struct x86_core_data {
    uint32_t multiboot_flags; ///< The multiboot flags of the cpu module
    struct x86_coredata_elf elf; ///< elf structure for the cpu module
    genpaddr_t module_start;  ///< The start of the cpu module
    genpaddr_t module_end;    ///< The end of the cpu module
    genpaddr_t urpc_frame_base;
    uint8_t urpc_frame_bits;
    genpaddr_t monitor_binary;
    genpaddr_t monitor_binary_size;
    genpaddr_t memory_base_start;
    uint8_t memory_bits;
    coreid_t src_core_id;
    uint8_t src_arch_id;
    coreid_t dst_core_id;
    char kernel_cmdline[128];

    uint32_t    cmdline;
    uint32_t    mods_count;
    uint32_t    mods_addr;

    uint32_t    mmap_length;
    uint32_t    mmap_addr;

    uint32_t    start_free_ram;

    uint32_t    chan_id;

#ifdef __scc__
    struct x86_coredata_modinfo    modinfo[10];
    struct x86_coredata_mmap       mmap[20];

    char        sh[2048];
    char        strings[1024];
#endif
} __attribute__ ((packed));

#define X86_CORE_DATA_PAGES 1100

#endif
