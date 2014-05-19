/**
 * \file
 * \brief Definition of multiboot header formats.
 * Spec: http://www.gnu.org/software/grub/manual/multiboot/multiboot.html
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _MULTIBOOT_H
#define _MULTIBOOT_H

#define MULTIBOOT_HEADER_MAGIC                  0x1BADB002
#define MULTIBOOT_HEADER_FLAG_MODS_PGALIGNED    1
#define MULTIBOOT_HEADER_FLAG_NEED_MEMINFO      2
#define MULTIBOOT_HEADER_FLAG_NEED_VIDMODE      4

#define MULTIBOOT_INFO_MAGIC                    0x2BADB002
#define MULTIBOOT_INFO_FLAG_HAS_MEMINFO         0x001
#define MULTIBOOT_INFO_FLAG_HAS_BOOTDEV         0x002
#define MULTIBOOT_INFO_FLAG_HAS_CMDLINE         0x004
#define MULTIBOOT_INFO_FLAG_HAS_MODS            0x008
#define MULTIBOOT_INFO_FLAG_HAS_AOUT_SYMS       0x010
#define MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS        0x020
#define MULTIBOOT_INFO_FLAG_HAS_MMAP            0x040
#define MULTIBOOT_INFO_FLAG_HAS_DRIVES          0x080
#define MULTIBOOT_INFO_FLAG_HAS_CONFIG          0x100
#define MULTIBOOT_INFO_FLAG_HAS_LOADERNAME      0x200
#define MULTIBOOT_INFO_FLAG_HAS_APM             0x400
#define MULTIBOOT_INFO_FLAG_HAS_VBE             0x800

#if !defined(__ASSEMBLER__)

struct multiboot_elf {
    uint32_t    num;
    uint32_t    size;
    uint32_t    addr;
    uint32_t    shndx;
};

/// Multiboot information structure passed from bootloader to OS
struct multiboot_info {
    uint32_t    flags;
    
    // if MULTIBOOT_INFO_FLAG_HAS_MEMINFO is set
    uint32_t    mem_lower;
    uint32_t    mem_upper;
    
    // if MULTIBOOT_INFO_FLAG_HAS_BOOTDEV
    uint32_t    boot_device;
    
    // if MULTIBOOT_INFO_FLAG_HAS_CMDLINE
    uint32_t    cmdline;
    
    // if MULTIBOOT_INFO_FLAG_HAS_MODS
    uint32_t    mods_count;
    uint32_t    mods_addr;
    
    union {
        // if MULTIBOOT_INFO_FLAG_HAS_AOUT_SYMS
        struct {
            uint32_t    tabsize;
            uint32_t    strsize;
            uint32_t    addr;
            uint32_t    reserved;
        } aout;
        
        // if MULTIBOOT_INFO_FLAG_HAS_ELF_SYMS
        struct multiboot_elf elf;
    } syms;
    
    // if MULTIBOOT_INFO_FLAG_HAS_MMAP
    uint32_t    mmap_length;
    uint32_t    mmap_addr;
    
    // if MULTIBOOT_INFO_FLAG_HAS_DRIVES
    uint32_t    drives_length;
    uint32_t    drives_addr;
    
    // if MULTIBOOT_INFO_FLAG_HAS_CONFIG
    uint32_t    config_table;
    
    // if MULTIBOOT_INFO_FLAG_HAS_LOADERNAME
    uint32_t    boot_loader_name;
    
    // if MULTIBOOT_INFO_FLAG_HAS_APM
    uint32_t    apm_table;
    
    // if MULTIBOOT_INFO_FLAG_HAS_VBE
    uint32_t    vbe_control_info;
    uint32_t    vbe_mode_info;
    uint16_t    vbe_mode;
    uint16_t    vbe_interface_seg;
    uint16_t    vbe_interface_off;
    uint16_t    vbe_interface_len;
};

#define MULTIBOOT_MODULE_SIZE(mod)      ((mod).mod_end - (mod).mod_start)

struct multiboot_modinfo {
    uint32_t    mod_start;
    uint32_t    mod_end;
    uint32_t    string;
    uint32_t    reserved;
};

#define MULTIBOOT_MEM_TYPE_RAM          1

struct multiboot_mmap {
    uint32_t    size;
    uint64_t    base_addr;
    uint64_t    length;
    uint32_t    type;
} __attribute__ ((packed));

#endif /* !defined(__ASSEMBLER__) */

#endif /* _MULTIBOOT_H */
