/**
 * \file
 * \brief pmap management wrappers
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_PMAP_H
#define ARCH_X86_64_BARRELFISH_PMAP_H

#include <target/x86_64/barrelfish/pmap_target.h>

#define ARCH_DEFAULT_PMAP_SIZE sizeof(struct pmap_x86)

static inline errval_t pmap_init(struct pmap *pmap, struct vspace *vspace,
                                 struct capref vnode,
                                 struct slot_allocator *opt_slot_alloc)
{
    return pmap_x86_64_init(pmap, vspace, vnode, opt_slot_alloc);
}

static inline errval_t pmap_current_init(bool init_domain)
{
    return pmap_x86_64_current_init(init_domain);
}

#endif // ARCH_X86_64_BARRELFISH_PMAP_H
