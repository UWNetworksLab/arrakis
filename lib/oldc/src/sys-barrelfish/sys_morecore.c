/**
 * \file
 * \brief morecore() is a sbrk() equivalent.
 */

/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <k_r_malloc.h>
#include <stddef.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>

Header *get_malloc_freep(void);

typedef void *(*morecore_alloc_func_t)(size_t bytes, size_t *retbytes);
typedef void (*morecore_free_func_t)(void *base, size_t bytes);

morecore_alloc_func_t sys_morecore_alloc;
morecore_free_func_t sys_morecore_free;

/**
 * \brief sbrk() equivalent.
 *
 * This function tries to allocate at least the amount given by 'nu'
 * in sizeof(::Header) byte units. In some cases, it will allocate
 * less, if no more memory was available. In any case, It returns a
 * pointer to the freelist header of the memory region.  NULL is
 * returned when out of memory.
 *
 * \param nu    Number of memory units (1 unit == sizeof(::Header) bytes)
 *
 * \return Pointer to freelist header of new memory region or NULL on out of
 * memory.
 */
Header *morecore(unsigned nu)
{
    Header *up;
    size_t nb = nu * sizeof(Header);

    // Allocate requested number of pages and insert freelist header
    assert(sys_morecore_alloc);
    up = (Header *)sys_morecore_alloc(nb, &nb);
    if (up == NULL) {
        return NULL;
    }
    assert(nb % sizeof(Header) == 0);
    up->s.size = nb / sizeof(Header);

    // Add header to freelist
    __free_locked((void *)(up + 1));
    return get_malloc_freep();
}

/**
 * \brief sbrk() garbage collector.
 *
 * Tries to free up pages at the end of the segment, so to shorten the
 * segment and return memory to the operating system.
 */
void lesscore(void)
{
#if defined(__arm__)
    // Not implemented

#else
    struct morecore_state *state = get_morecore_state();
    genvaddr_t gvaddr =
        vregion_get_base_addr(&state->mmu_state.vregion)
        + state->mmu_state.offset;
    void *eaddr = (void*)vspace_genvaddr_to_lvaddr(gvaddr);

    assert(sys_morecore_free);

    // Deallocate from end of segment
    Header *prevp = state->header_freep, *p;
    for(p = prevp->s.ptr;; prevp = p, p = p->s.ptr) {
        if(p + p->s.size == eaddr) {
            prevp->s.ptr = p->s.ptr;
            state->header_freep = prevp;

            // Give back the memory
            sys_morecore_free(p, p->s.size * sizeof(Header));
            break;
        }

        if (p == state->header_freep) {	/* wrapped around free list */
            break;
        }
    }
#endif
}
