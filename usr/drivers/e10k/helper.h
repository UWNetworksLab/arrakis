/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef HELPER_H_
#define HELPER_H_

#include <errors/errno.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/vregion.h>

void debug_dumpmem(void* buf, size_t len);
void* alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap);
errval_t get_apicid_from_core(coreid_t cid, uint8_t *apicid);

/* Simple bitmap-based allocator */
#define BMALLOCATOR_BITS 8
#define BMALLOCATOR_TYPE uint8_t
struct bmallocator {
    BMALLOCATOR_TYPE *bitmap;
    size_t count;
};

/** Init allocator for n objects. */
bool bmallocator_init(struct bmallocator *alloc, size_t n);
/** Release memory associated with allocator. */
void bmallocator_destroy(struct bmallocator *alloc);
/** Allocate object, return index in *n if successful (return true). */
bool bmallocator_alloc(struct bmallocator *alloc, size_t *n);
/** Free object n, return value indicates if it was allocated before. */
bool bmallocator_free(struct bmallocator *alloc, size_t n);

#endif // ndef HELPER_H_
