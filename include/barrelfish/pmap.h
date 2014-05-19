/**
 * \file
 * \brief Generic pmap definitions
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_PMAP_H
#define LIBBARRELFISH_PMAP_H

struct pmap_dump_info;
struct pmap;
struct pmap_funcs {
    errval_t (*determine_addr)(struct pmap *pmap, struct memobj *memobj,
                               size_t alignment, genvaddr_t *vaddr);
    errval_t (*map)(struct pmap* pmap, genvaddr_t vaddr, struct capref frame,
                    size_t offset, size_t size, vregion_flags_t flags,
                    size_t *retoffset, size_t *retsize);
    errval_t (*unmap)(struct pmap* pmap, genvaddr_t vaddr, size_t size,
                      size_t *retsize);
    errval_t (*modify_flags)(struct pmap* pmap, genvaddr_t vaddr, size_t size,
                             vregion_flags_t flags, size_t *retsize);
    errval_t (*lookup)(struct pmap *pmap, genvaddr_t vaddr,
                       genvaddr_t *retvaddr, size_t *retsize,
                       struct capref *retcap, genvaddr_t *retoffset,
                       vregion_flags_t *retflags);
    errval_t (*serialise)(struct pmap *pmap, void *buf, size_t buflen);
    errval_t (*deserialise)(struct pmap *pmap, void *buf, size_t buflen);

    errval_t (*dump)(struct pmap *pmap, struct pmap_dump_info *buf, size_t buflen,
		     size_t *items_written);
};

struct pmap {
    struct pmap_funcs f;
    struct vspace *vspace;      ///< The vspace this pmap is associated with
    struct slot_allocator *slot_alloc; ///< (Optional) slot allocator for vnodes
};

#endif // LIBBARRELFISH_PMAP_H
