/**
 * \file
 * \brief Vspace definitions
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_VSPACE_H
#define LIBBARRELFISH_VSPACE_H

#include <sys/cdefs.h>

__BEGIN_DECLS

struct vspace {
    struct pmap *pmap;           ///< Pmap associated with the vspace
    struct vspace_layout layout; ///< The layout of the address space
    struct vregion *head;        ///< List of vregions in the vspace
};

/**
 * \brief Get the pmap for the vspace
 *
 * \param vspace The vspace to get the pmap for
 */
static inline struct pmap* vspace_get_pmap(struct vspace *vspace)
{
    return vspace->pmap;
}

genvaddr_t vspace_lvaddr_to_genvaddr(lvaddr_t lvaddr);
lvaddr_t vspace_genvaddr_to_lvaddr(genvaddr_t genvaddr);

errval_t vspace_current_init(bool init_domain);
errval_t vspace_init(struct vspace* vspace, struct pmap *pmap);
errval_t vspace_destroy(struct vspace* vspace);
struct vregion* vspace_get_region(struct vspace* vspace, const void *addr);
errval_t vspace_pagefault_handler(struct vspace* vspace, lvaddr_t addr,
                                  vm_fault_type_t type);

__END_DECLS

#endif // LIBBARRELFISH_VSPACE_H
