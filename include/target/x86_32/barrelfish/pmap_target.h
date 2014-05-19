/**
 * \file
 * \brief Arch specific declerations that can be included by others
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_32_BARRELFISH_PMAP_H
#define TARGET_X86_32_BARRELFISH_PMAP_H

#include <target/x86/barrelfish/pmap_target.h>

struct pmap_dump_info {
#ifdef CONFIG_PAE
    size_t pdpt_index;
#endif
    size_t pdir_index, pt_index;
    vregion_flags_t flags;
    struct capref cap;
    genvaddr_t offset;
};
#ifdef CONFIG_PAE
#define PRIfmtPTIDX "%zd.%zd.%zd"
#define GET_PTIDX(dump_info) (dump_info)->pdpt_index, (dump_info)->pdir_index, (dump_info)->pt_index
#else
#define PRIfmtPTIDX "%zd.%zd"
#define GET_PTIDX(dump_info) (dump_info)->pdir_index, (dump_info)->pt_index
#endif

errval_t pmap_x86_32_init(struct pmap *pmap, struct vspace *vspace,
                          struct capref vnode,
                          struct slot_allocator *opt_slot_alloc);
errval_t pmap_x86_32_current_init(bool);

#endif // TARGET_X86_32_BARRELFISH_PMAP_H
