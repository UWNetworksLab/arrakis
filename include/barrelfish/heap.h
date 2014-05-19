/**
 * \file
 * \brief Simple heap allocator
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_HEAP_H
#define LIBBARRELFISH_HEAP_H

#include <sys/cdefs.h>

__BEGIN_DECLS

union heap_header {                 /* block header */
    struct {
        union heap_header *ptr;     /* next block if on free list */
        unsigned size;              /* size of this block */
    } s;
    uintptr_t x;                    /* force alignment of blocks */
};

struct heap;

typedef union heap_header *(*Morecore_func_t)(struct heap *h, unsigned nu);

struct heap {
    union heap_header base;                         /* allocated list head */
    union heap_header *freep;                       /* start of free list */
    Morecore_func_t morecore_func;                  /* morecore function */
};

void heap_init(struct heap *heap, void *buf, size_t buflen,
               Morecore_func_t morecore_func);
void *heap_alloc(struct heap *heap, size_t nbytes);
void heap_free(struct heap *heap, void *ap);
union heap_header *heap_default_morecore(struct heap *h, unsigned nu);

__END_DECLS

#endif // LIBBARRELFISH_HEAP_H
