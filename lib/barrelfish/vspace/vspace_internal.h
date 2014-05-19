/**
 * \file
 * \brief internal functions for the vspace library
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VSPACE_INTERNAL_H_
#define VSPACE_INTERNAL_H_

#define VSPACE_PINNED_UNIT 5
#define VSPACE_PINNED_SIZE (BASE_PAGE_SIZE * 1024)

enum slab_type {
    VREGION_LIST,
    FRAME_LIST,
    TRACK_LIST,
};

errval_t vspace_add_vregion(struct vspace* vspace, struct vregion* region);
errval_t vspace_remove_vregion(struct vspace*qvspace, struct vregion* region);

errval_t vspace_pinned_init(void);
errval_t vspace_pinned_alloc(void **retbuf, enum slab_type slab_type);

#endif //VSPACE_INTERNAL_H_
