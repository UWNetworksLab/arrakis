/**
 * \file
 * \brief Common vspace library includes definitions
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_VSPACE_COMMON_H
#define LIBBARRELFISH_VSPACE_COMMON_H

typedef uint32_t vm_fault_type_t;
#define PRIuVMFAULT PRIu32
#define PRIxVMFAULT PRIx32

typedef uint32_t vregion_flags_t;
#define PRIuVREGIONFLAGS PRIu32
#define PRIxVREGIONFLAGS PRIx32

#include <sys/cdefs.h>

#include <barrelfish/memobj.h>
#include <barrelfish/vregion.h>
#include <barrelfish/pmap_arch.h>
#include <barrelfish/vspace_layout.h>
#include <barrelfish/vspace.h>
#include <barrelfish/vspace_mmu_aware.h>

__BEGIN_DECLS

errval_t vspace_unmap(const void *buf);
errval_t vspace_map_anon_attr(void **retaddr, struct memobj **ret_memobj,
                              struct vregion **ret_vregion, size_t size,
                              size_t *retsize, vregion_flags_t flags);
errval_t vspace_map_anon_nomalloc(void **retaddr, struct memobj_anon *memobj,
                                  struct vregion *vregion, size_t size,
                                  size_t *retsize, vregion_flags_t flags,
                                  size_t alignment);
errval_t vspace_map_anon_aligned(void **retaddr, struct memobj **ret_memobj,
                                 struct vregion **ret_vregion, size_t size,
                                 size_t *retsize, vregion_flags_t flags,
                                 size_t alignment);
errval_t vspace_map_anon_fixed(genvaddr_t base, size_t size,
                               vregion_flags_t flags,
                               struct vregion **ret_vregion,
                               struct memobj **ret_memobj);
errval_t vspace_map_one_frame_attr(void **retaddr, size_t size,
                                   struct capref frame, vregion_flags_t flags,
                                   struct memobj **retmemobj,
                                   struct vregion **retvregion);
errval_t vspace_map_one_frame(void **retaddr, size_t size, struct capref frame,
                              struct memobj **retmemobj,
                              struct vregion **retvregion);
errval_t vspace_map_one_frame_one_map(struct memobj_one_frame_one_map *memobj,
                                      struct vregion *vregion, size_t size,
                                      struct capref frame);

errval_t vspace_map_one_frame_fixed(lvaddr_t addr, size_t size,
                                    struct capref frame,
                                    struct memobj **retmemobj,
                                    struct vregion **retvregion);
errval_t vspace_map_one_frame_fixed_attr(lvaddr_t addr, size_t size,
                                    struct capref frame, vregion_flags_t flags,
                                    struct memobj **retmemobj,
                                         struct vregion **retvregion);

__END_DECLS

#endif // LIBBARRELFISH_VSPACE_COMMON_H
