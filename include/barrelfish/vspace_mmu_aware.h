/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_VSPACE_MMU_AWARE_H
#define LIBBARRELFISH_VSPACE_MMU_AWARE_H

#include <sys/cdefs.h>

__BEGIN_DECLS

struct vspace_mmu_vregion_list {
    struct vregion vregion;
    struct vspace_mmu_vregion_list *next;
};

/// Struct to support mmu_aware memory management
struct vspace_mmu_aware {
    size_t size;
    size_t consumed;
    struct vregion vregion;           ///< Needs just one vregion
    struct memobj_anon memobj;        ///< Needs just one memobj
    lvaddr_t offset;    ///< Offset of free space in anon
    lvaddr_t mapoffset; ///< Offset into the anon that has been mapped in
};

errval_t vspace_mmu_aware_init(struct vspace_mmu_aware *state, size_t size);
errval_t vspace_mmu_aware_map(struct vspace_mmu_aware *state,
                              struct capref frame, size_t req_size,
                              void **retbuf, size_t *retsize);
errval_t vspace_mmu_aware_unmap(struct vspace_mmu_aware *state,
                                lvaddr_t base, size_t bytes);

__END_DECLS

#endif // LIBBARRELFISH_VSPACE_MMU_AWARE_H
