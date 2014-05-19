/**
 * \file
 * \brief Simple heap allocator.
 *
 * This file implements a very simple heap allocator, based on K&R malloc.
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/heap.h>

/**
 * \brief Initialise a new heap
 *
 * \param heap      Heap structure to be filled in
 * \param buf       Memory buffer out of which to allocate
 * \param buflen    Size of buffer
 * \param morecore_func Function to call to increase heap, or NULL
 */
void heap_init(struct heap *heap, void *buf, size_t buflen,
               Morecore_func_t morecore_func)
{
    assert(heap != NULL);
    assert(buf != NULL);
    assert(buflen > sizeof(union heap_header));

    // Initialise base header (nothing allocated)
    heap->base.s.ptr = heap->freep = &heap->base;
    heap->base.s.size = 0;
    heap->morecore_func = morecore_func;

    // Insert freelist header into new memory buffer
    union heap_header *h = buf;
    h->s.size = buflen / sizeof(union heap_header);

    // Add header to freelist
    heap_free(heap, (void *)(h + 1));
}

/**
 * \brief Equivalent of malloc; allocates memory out of given heap.
 *
 * \returns NULL on failure
 */
void *heap_alloc(struct heap *heap, size_t nbytes)
{
    union heap_header *p, *prevp;
    unsigned nunits;
    nunits = (nbytes + sizeof(union heap_header) - 1)
             / sizeof(union heap_header) + 1;

    prevp = heap->freep;
    assert(prevp != NULL);

    for (p = prevp->s.ptr;; prevp = p, p = p->s.ptr) {
        if (p->s.size >= nunits) {    /* big enough */
            if (p->s.size == nunits) {    /* exactly */
                prevp->s.ptr = p->s.ptr;
            } else {    /* allocate tail end */
                p->s.size -= nunits;
                p += p->s.size;
                p->s.size = nunits;
            }
            heap->freep = prevp;

            return (void *) (p + 1);
        }
        if (p == heap->freep) {    /* wrapped around free list */
            /* try morecore, if we have one */
            if (heap->morecore_func == NULL
                || (p = (union heap_header *) 
                    heap->morecore_func(heap, nunits)) == NULL) {
                return NULL;    /* none left */
            }
        }
    }
}

/**
 * \brief Equivalent of free: put block back in free list.
 */
void heap_free(struct heap *heap, void *ap)
{
    union heap_header *bp, *p;

    assert(heap != NULL);
    assert(heap->freep != NULL);
    if (ap == NULL) {
        return;
    }

    bp = (union heap_header *) ap - 1;    /* point to block header */
    for (p = heap->freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr) {
        if (p >= p->s.ptr && (bp > p || bp < p->s.ptr)) {
            break;    /* freed block at start or end of arena */
        }
    }

    if (bp + bp->s.size == p->s.ptr) {    /* join to upper nbr */
        bp->s.size += p->s.ptr->s.size;
        bp->s.ptr = p->s.ptr->s.ptr;
    } else {
        bp->s.ptr = p->s.ptr;
    }

    if (p + p->s.size == bp) {    /* join to lower nbr */
        p->s.size += bp->s.size;
        p->s.ptr = bp->s.ptr;
    } else {
        p->s.ptr = bp;
    }

    heap->freep = p;
}


/**
 * \brief Allocate and map in one or more pages of memory
 */
static void *pages_alloc(size_t pages)
{
    assert(!"not implemented");
    return NULL;
}

/**
 * \brief sbrk() equivalent.
 *
 * This function allocates at least the amount given by 'nu' in sizeof(::Header)
 * byte units. It returns a pointer to the freelist header of the memory region.
 * NULL is returned when out of memory.
 *
 * \param nu    Number of memory units (1 unit == sizeof(::Header) bytes)
 *
 * \return Pointer to freelist header of new memory region or NULL on out of
 * memory.
 */
union heap_header *heap_default_morecore(struct heap *heap, unsigned nu)
{
    union heap_header *up;
    size_t            nb = ROUND_UP(nu * sizeof(union heap_header),
                                    BASE_PAGE_SIZE);

    // Allocate requested number of pages and insert freelist header
    up = (union heap_header*)pages_alloc(nb / BASE_PAGE_SIZE);
    up->s.size = nb / sizeof(union heap_header);

    // Add header to freelist
    heap_free(heap, (void *)(up + 1));
    return up;
}
