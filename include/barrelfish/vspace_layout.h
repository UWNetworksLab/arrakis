/**
 * \file
 * \brief Vspace_Layout definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_VSPACE_LAYOUT_H
#define LIBBARRELFISH_VSPACE_LAYOUT_H

#include <sys/cdefs.h>

__BEGIN_DECLS

struct vspace_layout;
struct vspace_layout_funcs {
    errval_t (*alloc)(struct vspace_layout *layout, genvaddr_t *addr);
};

struct vspace_layout {
    struct vspace_layout_funcs f;
    lvaddr_t granularity;
    genvaddr_t size;
    genvaddr_t offset;
};


/**
 * \brief Get the granularity of minimum allocation
 */
static inline lvaddr_t vspace_layout_get_granularity(struct vspace_layout *l)
{
    return l->granularity;
}

/**
 * \brief Get the size of the address space
 */
static inline size_t vspace_layout_get_size(struct vspace_layout *l)
{
    return l->size;
}

/**
 * \brief Translate a lvaddr_t to genvaddr_t
 */
static inline genvaddr_t vspace_layout_lvaddr_to_genvaddr(struct vspace_layout *l,
                                                          lvaddr_t lvaddr)
{
    return ((genvaddr_t)lvaddr + l->offset);
}

/**
 * \brief Translate a genvaddr_t to lvaddr_t
 */
static inline lvaddr_t vspace_layout_genvaddr_to_lvaddr(struct vspace_layout *l,
                                                        genvaddr_t genvaddr)
{
    assert(genvaddr >= l->offset &&
           genvaddr <= ((genvaddr_t)l->offset + l->size));
    return (lvaddr_t)(genvaddr - l->offset);
}

errval_t vspace_layout_init(struct vspace_layout *l);

__END_DECLS

#endif // LIBBARRELFISH_VSPACE_LAYOUT_H
