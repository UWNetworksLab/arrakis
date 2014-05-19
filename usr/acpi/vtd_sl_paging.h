/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef VTD_SL_PAGING_H
#define VTD_SL_PAGING_H

#include <barrelfish_kpi/types.h>
#include <target/x86_64/barrelfish_kpi/paging_target.h>

typedef uint64_t paging_sl_flags_t;

#define SL_PML4_BASE(addr)         X86_64_PML4_BASE(addr) 
#define SL_PDPT_BASE(addr)         X86_64_PDPT_BASE(addr)
#define SL_PDIR_BASE(addr)         X86_64_PDIR_BASE(addr)
#define SL_PTABLE_BASE(addr)       X86_64_PTABLE_BASE(addr)

#define SL_BASE_PAGE_SIZE          X86_64_BASE_PAGE_SIZE
#define SL_BASE_PAGE_MASK          X86_64_BASE_PAGE_MASK

#define SL_PTABLE_SIZE             X86_64_PTABLE_SIZE

#define SL_PTABLE_MASK_BITS        9
#define SL_PTABLE_CLEAR            0

#define SL_PTABLE_TRANS_MAP        (((paging_sl_flags_t)1) << 62)
#define SL_PTABLE_SNOOP            (((paging_sl_flags_t)1) << 11)
#define SL_PTABLE_IGNORE_PAT       (((paging_sl_flags_t)1) << 6)
#define SL_PTABLE_EXECUTE          (((paging_sl_flags_t)1) << 2)
#define SL_PTABLE_WRITE            (((paging_sl_flags_t)1) << 1)
#define SL_PTABLE_READ             (((paging_sl_flags_t)1) << 0)

/**
 * A second-level page directory entry.
 */
union sl_pdir_entry {
    uint64_t raw;
    struct {
        uint64_t        read            :1;
        uint64_t        write           :1;
        uint64_t        execute         :1;
        uint64_t        available       :4;
        // Must be 0 for sl-pdpe to sl-pdt and sl-pde to sl-ptable
        uint64_t        reserved        :1;
        uint64_t        available2      :3;
        uint64_t        reserved2       :1;
        uint64_t        base_addr       :28;
        uint64_t        reserved3       :12;
        uint64_t        available3      :10;
        uint64_t        reserved4       :1;
        uint64_t        available4      :1;
    } d;
};

/**
 * A second-level translation page table entry.
 */
union sl_ptable_entry {
    uint64_t raw;
    struct {
        uint64_t        read            :1;
        uint64_t        write           :1;
        uint64_t        execute         :1;
        uint64_t        extend_mem_type :3;
        uint64_t        ignore_pat      :1;
        uint64_t        always1         :1;
        uint64_t        available       :3;
        uint64_t        snoop           :1;
        uint64_t        reserved        :18;
        uint64_t        base_addr       :10;
        uint64_t        reserved2       :12;
        uint64_t        available2      :10;
        uint64_t        trans_map       :1;
        uint64_t        available3      :1;
    } large30;
    struct {
        uint64_t        read            :1;
        uint64_t        write           :1;
        uint64_t        execute         :1;
        uint64_t        extend_mem_type :3;
        uint64_t        ignore_pat      :1;
        uint64_t        always1         :1;
        uint64_t        available       :3;
        uint64_t        snoop           :1;
        uint64_t        reserved        :9;
        uint64_t        base_addr       :19;
        uint64_t        reserved2       :12;
        uint64_t        available2      :10;
        uint64_t        trans_map       :1;
        uint64_t        available3      :1;
    } large21;
    struct {
        uint64_t        read            :1;
        uint64_t        write           :1;
        uint64_t        execute         :1;
        uint64_t        extend_mem_type :3;
        uint64_t        ignore_pat      :1;
        uint64_t        available       :4;
        uint64_t        snoop           :1;
        uint64_t        base_addr       :28;
        uint64_t        reserved        :12;
        uint64_t        available2      :10;
        uint64_t        trans_map       :1;
        uint64_t        available3      :1;
    } base;
};

static inline void sl_map_table(union sl_pdir_entry *entry, genpaddr_t base)
{
    union sl_pdir_entry tmp;
    tmp.raw = SL_PTABLE_CLEAR;

    tmp.d.read = 1;
    tmp.d.write = 1;
    tmp.d.base_addr = base >> 12;

    *entry = tmp;
}

static inline void sl_map(union sl_ptable_entry *entry, genpaddr_t base, uint64_t bitmap)
{
    union sl_ptable_entry tmp;
    tmp.raw = SL_PTABLE_CLEAR;

    tmp.base.read = bitmap & SL_PTABLE_READ ? 1 : 0;
    tmp.base.write = bitmap & SL_PTABLE_WRITE ? 1 : 0;
    tmp.base.execute = bitmap & SL_PTABLE_EXECUTE ? 1 : 0;
    tmp.base.ignore_pat = bitmap & SL_PTABLE_IGNORE_PAT ? 1 : 0;
    tmp.base.snoop = bitmap & SL_PTABLE_SNOOP ? 1 : 0;
    tmp.base.trans_map = bitmap & SL_PTABLE_TRANS_MAP ? 1 : 0;
    tmp.base.base_addr = base >> 12;

    *entry = tmp;
}

static inline void sl_map_large21(union sl_ptable_entry *entry, genpaddr_t base, uint64_t bitmap)
{
    union sl_ptable_entry tmp;
    tmp.raw = SL_PTABLE_CLEAR;

    tmp.large21.read = bitmap & SL_PTABLE_READ ? 1 : 0;
    tmp.large21.write = bitmap & SL_PTABLE_WRITE ? 1 : 0;
    tmp.large21.execute = bitmap & SL_PTABLE_EXECUTE ? 1 : 0;
    tmp.large21.ignore_pat = bitmap & SL_PTABLE_IGNORE_PAT ? 1 : 0;
    tmp.large21.snoop = bitmap & SL_PTABLE_SNOOP ? 1 : 0;
    tmp.large21.trans_map = bitmap & SL_PTABLE_TRANS_MAP ? 1 : 0;
    tmp.large21.always1 = 1;
    tmp.large21.base_addr = base >> 21;
    
    *entry = tmp;
}

static inline void sl_map_large30(union sl_ptable_entry *entry, genpaddr_t base, uint64_t bitmap)
{
    union sl_ptable_entry tmp;
    tmp.raw = SL_PTABLE_CLEAR;

    tmp.large30.read = bitmap & SL_PTABLE_READ ? 1 : 0;
    tmp.large30.write = bitmap & SL_PTABLE_WRITE ? 1 : 0;
    tmp.large30.execute = bitmap & SL_PTABLE_EXECUTE ? 1 : 0;
    tmp.large30.ignore_pat = bitmap & SL_PTABLE_IGNORE_PAT ? 1 : 0;
    tmp.large30.snoop = bitmap & SL_PTABLE_SNOOP ? 1 : 0;
    tmp.large30.trans_map = bitmap & SL_PTABLE_TRANS_MAP ? 1 : 0;
    tmp.large30.always1 = 1;
    tmp.large30.base_addr = base >> 30;

    *entry = tmp;
}

#endif //VTD_SL_PAGING_H
