/**
 * \file
 * \brief Vregion definitions
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_VREGION_H
#define LIBBARRELFISH_VREGION_H

#include <sys/cdefs.h>

__BEGIN_DECLS

#define VREGION_FLAGS_READ       0x01  // Reading allowed
#define VREGION_FLAGS_WRITE      0x02  // Writing allowed
#define VREGION_FLAGS_EXECUTE    0x04  // Execute allowed
#define VREGION_FLAGS_NOCACHE    0x08  // Caching disabled
#define VREGION_FLAGS_MPB        0x10  // Message passing buffer
#define VREGION_FLAGS_GUARD      0x20  // Guard page
#define VREGION_FLAGS_MASK       0x2f  // Mask of all individual VREGION_FLAGS
#define VREGION_FLAGS_VTD_SNOOP  0x800 // Snooping (for pages) allowed by VT-d

#define VREGION_FLAGS_READ_WRITE \
    (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_VTD_SNOOP)
#define VREGION_FLAGS_READ_EXECUTE \
    (VREGION_FLAGS_READ | VREGION_FLAGS_EXECUTE | VREGION_FLAGS_VTD_SNOOP)
#define VREGION_FLAGS_READ_WRITE_NOCACHE \
    (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_NOCACHE | VREGION_FLAGS_VTD_SNOOP)
#define VREGION_FLAGS_READ_WRITE_MPB \
    (VREGION_FLAGS_READ | VREGION_FLAGS_WRITE | VREGION_FLAGS_MPB | VREGION_FLAGS_VTD_SNOOP)

struct vregion {
    struct vspace *vspace;   ///< A vregion is always associated with one vspace
    struct memobj *memobj;   ///< A vregion is always associated with one memobj
    genvaddr_t offset;       ///< Offset into the memobj
    genvaddr_t size;         ///< Size of the vregion
    genvaddr_t base;         ///< Base address of the vregion
    vregion_flags_t flags;   ///< Flags
    struct vregion *next;    ///< Pointer for the list in vspace
};

/**
 * \brief Get the vspace associated with the vregion
 *
 * \param vregion  The vregion
 */
static inline struct vspace *vregion_get_vspace(struct vregion *vregion)
{
    return vregion->vspace;
}

/**
 * \brief Get the memory object associated with the region
 *
 * \param vregion  The region
 */
static inline struct memobj* vregion_get_memobj(struct vregion *vregion)
{
    return vregion->memobj;
}

/**
 * \brief Get the base address of the region
 *
 * \param piont  The region
 */
static inline genvaddr_t vregion_get_base_addr(struct vregion *vregion)
{
    return vregion->base;
}

/**
 * \brief Get the offset into the memory object the vregion has
 *
 * \param vregion  The region
 */
static inline size_t vregion_get_offset(struct vregion *vregion)
{
    return vregion->offset;
}

/**
 * \brief Get the size of the region
 *
 * \param vregion  The region
 */
static inline size_t vregion_get_size(struct vregion *vregion)
{
    return vregion->size;
}

/**
 * \brief Get the flags/attributes of the region
 *
 * \param vregion  The region
 */
static inline vregion_flags_t vregion_get_flags(struct vregion *vregion)
{
    return vregion->flags;
}

errval_t vregion_map(struct vregion* point, struct vspace* vspace, struct memobj* memobj,
                     size_t offset, size_t size, vregion_flags_t flags);
errval_t vregion_map_aligned(struct vregion* point, struct vspace* vspace,
                             struct memobj* memobj, size_t offset, size_t size,
                             vregion_flags_t flags, size_t alignment);
errval_t vregion_map_fixed(struct vregion* point, struct vspace* vspace, struct memobj* memobj,
                           size_t offset, size_t size, genvaddr_t addr,
                           vregion_flags_t flags);
errval_t vregion_destroy(struct vregion* region);
errval_t vregion_pagefault_handler(struct vregion* region, genvaddr_t addr,
                                   vm_fault_type_t type);

__END_DECLS

#endif // LIBBARRELFISH_VREGION_H
