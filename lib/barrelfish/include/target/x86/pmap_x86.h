/**
 * \file
 * \brief Pmap definition common for the x86 archs, but private to libbarrelfish
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TARGET_X86_BARRELFISH_PMAP_X86_H
#define TARGET_X86_BARRELFISH_PMAP_X86_H

struct pmap;

errval_t pmap_x86_serialise(struct pmap *pmap, void *buf, size_t buflen);
errval_t pmap_x86_deserialise(struct pmap *pmap, void *buf, size_t buflen);
errval_t pmap_x86_determine_addr(struct pmap *pmap, struct memobj *memobj,
                                 size_t alignment, genvaddr_t *vaddr);

#endif // TARGET_X86_BARRELFISH_PMAP_X86_H
