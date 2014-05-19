/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VMKITMON_X86_H
#define VMKITMON_X86_H

#include <stdint.h>

/**
 * \brief Structure to represent a ModR/M byte used by certain instructions
 */
union x86_modrm {
    struct {
        uint32_t     rm : 3;
        uint32_t     regop : 3;
        uint32_t     mod : 2;
    } __attribute__((packed)) u;
    uint8_t raw;
} __attribute__((packed));

/**
 * \brief This enumeration represents the state of an IO-port access.
 */
enum x86_io_access {
    X86_IO_ACCESS_TYPE  = (1 << 0),     ///< 0 = output (write), 1 = input (read)
    X86_IO_ACCESS_STR   = (1 << 1),     ///< String based port access (INS, OUTS)
    X86_IO_ACCESS_REP   = (1 << 2),     ///< Repeated port access
    X86_IO_ACCESS_SZ8   = (1 << 3),     ///< 8-bit operand size
    X86_IO_ACCESS_SZ16  = (1 << 4),     ///< 16-bit operand size
    X86_IO_ACCESS_SZ32  = (1 << 5),     ///< 32-bit operand size
    X86_IO_ACCESS_A16   = (1 << 6),     ///< 16-bit address size
    X86_IO_ACCESS_A32   = (1 << 7),     ///< 32-bit address size
    X86_IO_ACCESS_A64   = (1 << 8)      ///< 64-bit address size
};

// MSRs
#define X86_MSR_SYSENTER_CS     0x00000174
#define X86_MSR_SYSENTER_ESP    0x00000175
#define X86_MSR_SYSENTER_EIP    0x00000176
#define X86_MSR_EFER            0xc0000080
#define X86_MSR_STAR            0xc0000081
#define X86_MSR_LSTAR           0xc0000082
#define X86_MSR_CSTAR           0xc0000083
#define X86_MSR_SFMASK          0xc0000084
#define X86_MSR_FS_BASE         0xc0000100
#define X86_MSR_GS_BASE         0xc0000101
#define X86_MSR_KERNEL_GS_BASE  0xc0000102


// Long Mode Paging

/**
 * \brief Long mode virtual address
 */
union x86_lm_va {
    struct {
        uint32_t    pa_offset           : 12;
        uint32_t    pt_idx              : 9;
        uint32_t    pd_idx              : 9;
        uint32_t    pdp_idx             : 9;
        uint32_t    pml4_idx            : 9;
        uint32_t    sign_extend         : 16;
    } __attribute__((packed)) u;
    struct {
        uint32_t    pa_offset           : 21;
        uint32_t    pd_idx              : 9;
        uint32_t    pdp_idx             : 9;
        uint32_t    pml4_idx            : 9;
        uint32_t    sign_extend         : 16;
    } __attribute__((packed)) u2mb;
    struct {
        uint32_t    pa_offset           : 30;
        uint32_t    pdp_idx             : 9;
        uint32_t    pml4_idx            : 9;
        uint32_t    sign_extend         : 16;
    } __attribute__((packed)) u1gb;
    uint64_t raw;
} __attribute__((packed));

/**
 * \brief PML4 Record
 */
union x86_lm_pml4_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    ign                 : 1;
        uint32_t    rsvd                : 2;
        uint32_t    avl                 : 3;
        uint64_t    pdp_base_pa         : 40;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u;
    uint64_t raw;
} __attribute__((packed));

/**
 * \brief Page Directory Pointer Entry
 */
union x86_lm_pdp_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    ign                 : 1;
        uint32_t    ps                  : 1;
        uint32_t    rsvd                : 1;
        uint32_t    avl                 : 3;
        uint64_t    pd_base_pa          : 40;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u;
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    d                   : 1;
        uint32_t    ps                  : 1;
        uint32_t    g                   : 1;
        uint32_t    avl                 : 3;
        uint32_t    pat                 : 1;
        uint32_t    rsvd                : 17;
        uint64_t    base_pa             : 22;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u1gb;
    uint64_t raw;
} __attribute__((packed));

/**
 * \brief Long mode page directory entry
 */
union x86_lm_pd_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    ign1                : 1;
        uint32_t    ps                  : 1;
        uint32_t    ign2                : 1;
        uint32_t    avl                 : 3;
        uint64_t    pt_base_pa          : 40;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u;
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    d                   : 1;
        uint32_t    ps                  : 1;
        uint32_t    g                   : 1;
        uint32_t    avl                 : 3;
        uint32_t    pat                 : 1;
        uint32_t    rsvd                : 8;
        uint64_t    base_pa             : 31;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u2mb;
    uint64_t raw;
} __attribute__((packed));

/**
 * \brief Long mode page table entry
 */
union x86_lm_pt_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    d                   : 1;
        uint32_t    pat                 : 1;
        uint32_t    g                   : 1;
        uint32_t    avl                 : 3;
        uint64_t    base_pa             : 40;
        uint32_t    avail               : 11;
        uint32_t    nx                  : 1;
    } __attribute__((packed)) u;
    uint64_t raw;
} __attribute__((packed));


// Legacy Mode Paging

/**
 * \brief Legacy mode virtual address
 */
union x86_legm_va {
    struct {
        uint32_t    pa_offset           : 12;
        uint32_t    pt_idx              : 10;
        uint32_t    pd_idx              : 10;
    } __attribute__((packed)) u;
    struct {
        uint32_t    pa_offset           : 22;
        uint32_t    pd_idx              : 10;
    } __attribute__((packed)) u4mb;
    uint32_t raw;
} __attribute__((packed));

/**
 * \brief Legacy mode page directory entry
 */
union x86_legm_pd_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    ign1                : 1;
        uint32_t    ps                  : 1;
        uint32_t    ign2                : 1;
        uint32_t    avl                 : 3;
        uint32_t    pt_base_pa          : 20;
    } __attribute__((packed)) u;
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    d                   : 1;
        uint32_t    ps                  : 1;
        uint32_t    g                   : 1;
        uint32_t    avl                 : 3;
        uint32_t    pat                 : 1;
        uint32_t    rsvd                : 9;
        uint32_t    base_pa             : 10;
    } __attribute__((packed)) u4mb;
    uint32_t raw;
} __attribute__((packed));

/**
 * \brief Legacy mode page table entry
 */
union x86_legm_pt_entry {
    struct {
        uint32_t    p                   : 1;
        uint32_t    rw                  : 1;
        uint32_t    us                  : 1;
        uint32_t    pwt                 : 1;
        uint32_t    pcd                 : 1;
        uint32_t    a                   : 1;
        uint32_t    d                   : 1;
        uint32_t    pat                 : 1;
        uint32_t    g                   : 1;
        uint32_t    avl                 : 3;
        uint32_t    base_pa             : 20;
    } __attribute__((packed)) u;
    uint32_t raw;
} __attribute__((packed));

#endif // VMKITMON_X86_H
