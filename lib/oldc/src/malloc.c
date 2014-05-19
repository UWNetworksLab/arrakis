
/*
 * K&R Malloc
 *
 * System specifc code should implement `more_core'
 */
#include "k_r_malloc.h"
#include <stddef.h> /* For NULL */
#include <stdlib.h>
#include <string.h> /* For memcpy */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h> /* XXX */

typedef void *(*alt_malloc_t)(size_t bytes);
alt_malloc_t alt_malloc = NULL;

typedef void (*alt_free_t)(void *p);
alt_free_t alt_free = NULL;

#define MALLOC_LOCK thread_mutex_lock(&state->mutex)
#define MALLOC_UNLOCK thread_mutex_unlock(&state->mutex)

#ifdef CONFIG_MALLOC_INSTRUMENT
size_t __malloc_instrumented_allocated;
#endif

#ifdef CONFIG_MALLOC_DEBUG_INTERNAL
#include <stdio.h>
#include <assert.h>
int __malloc_check(void);
void __malloc_dump(void);
#endif

/*
 * malloc: general-purpose storage allocator
 */
void *
malloc(size_t nbytes)
{
    if (alt_malloc != NULL) {
        return alt_malloc(nbytes);
    }

    struct morecore_state *state = get_morecore_state();
	Header *p, *prevp;
	unsigned nunits;
	nunits = (nbytes + sizeof(Header) - 1) / sizeof(Header) + 1;

	MALLOC_LOCK;
	if ((prevp = state->header_freep) == NULL) {	/* no free list yet */
		state->header_base.s.ptr = state->header_freep = prevp = &state->header_base;
		state->header_base.s.size = 0;
	}
	for (p = prevp->s.ptr;; prevp = p, p = p->s.ptr) {
		if (p->s.size >= nunits) {	/* big enough */
			if (p->s.size == nunits)	/* exactly */
				prevp->s.ptr = p->s.ptr;
			else {	/* allocate tail end */
				p->s.size -= nunits;
				p += p->s.size;
				p->s.size = nunits;
			}
			state->header_freep = prevp;
#ifdef CONFIG_MALLOC_DEBUG
			{
				/* Write bit pattern over data */
				char *x = (char *) (p + 1);
				int i;
				for (i = 0; i < nbytes; i++)
					x[i] = 0xd0;
			}
#endif

#ifdef CONFIG_MALLOC_INSTRUMENT
			__malloc_instrumented_allocated += nunits;
#endif
#ifdef CONFIG_MALLOC_DEBUG_INTERNAL
			if (__malloc_check() != 0) {
				printf("malloc %lu %p\n", nbytes, (void *) (p + 1));
				__malloc_dump();
				assert(__malloc_check() == 0);
			}
#endif
			MALLOC_UNLOCK;
			return (void *) (p + 1);
		}
		if (p == state->header_freep) {	/* wrapped around free list */
			if ((p = (Header *) morecore(nunits)) == NULL) {
				MALLOC_UNLOCK;
				return NULL;	/* none left */
			} else {

			}
		}
	}
	MALLOC_UNLOCK;
}

/*
 * free: put block ap in free list
 */
void
__free_locked(void *ap)
{
    struct morecore_state *state = get_morecore_state();
	Header *bp, *p;

	if (ap == NULL)
		return;

	bp = (Header *) ap - 1;	/* point to block header */
	for (p = state->header_freep; !(bp > p && bp < p->s.ptr); p = p->s.ptr)
		if (p >= p->s.ptr && (bp > p || bp < p->s.ptr))
			break;	/* freed block at start or end of arena */

#ifdef CONFIG_MALLOC_INSTRUMENT
	__malloc_instrumented_allocated -= bp->s.size;
#endif

	if (bp + bp->s.size == p->s.ptr) {	/* join to upper nbr */
		bp->s.size += p->s.ptr->s.size;
		bp->s.ptr = p->s.ptr->s.ptr;
	} else {
		bp->s.ptr = p->s.ptr;
	}

	if (p + p->s.size == bp) {	/* join to lower nbr */
		p->s.size += bp->s.size;
		p->s.ptr = bp->s.ptr;
	} else {
		p->s.ptr = bp;
	}

	state->header_freep = p;

#ifdef CONFIG_MALLOC_DEBUG_INTERNAL
	if (__malloc_check() != 0) {
		printf("free %p\n", ap);
		__malloc_dump();
		assert(__malloc_check() == 0);
	}
#endif
}

void free(void *ap)
{
    if (ap == NULL) {
        return;
    }


    if (alt_free != NULL) {
        return alt_free(ap);
    }

    struct morecore_state *state = get_morecore_state();

#ifdef __x86_64__
    /* XXX: Since dispatchers on different cores maintain different malloc arena,
     * we detect instances when one dispatcher tries to free memory not in it's
     * arena and leak it
     */
    lvaddr_t base = vregion_get_base_addr(&state->mmu_state.vregion);
    lvaddr_t limit = base + vregion_get_size(&state->mmu_state.vregion);

    if ((lvaddr_t)ap < base || (lvaddr_t)ap >= limit) {
        if (X86_64_PML4_BASE(ap) != X86_64_PML4_BASE(base)) {
            return;
        }
    }

    assert((lvaddr_t)ap >= base && (lvaddr_t)ap < limit);
#endif

    MALLOC_LOCK;
    __free_locked(ap);
    lesscore();
    MALLOC_UNLOCK;
}

#ifdef CONFIG_MALLOC_DEBUG_INTERNAL

int
__malloc_check(void)
{
    struct morecore_state *state = get_morecore_state();
	Header *p, *prevp;
	if ((prevp = state->header_freep) == NULL) {	/* no free list yet */
		return 0;
	}
	for (p = prevp->s.ptr;; prevp = p, p = p->s.ptr) {
		if ((void*) p == NULL) {
			return 1;
		}
		/* Free bits should be in order */
		if (p > p->s.ptr && p->s.ptr != &state->header_base) {
			return 1;
		}
		if ((uintptr_t) p + (p->s.size * sizeof(Header)) > (uintptr_t) p->s.ptr && p->s.ptr != &state->header_base) {
			return 1;
		}
		/* shouldn't have zero sized free bits */
		if (p->s.size == 0 && p != &state->header_base) {
			return 1;
		}
		if (p == state->header_freep) {	/* wrapped around free list */
			break;
		}
	}
	return 0;
}

void
__malloc_dump(void)
{
    struct morecore_state *state = get_morecore_state();
	Header *p, *prevp;
	if ((prevp = state->header_freep) == NULL) {	/* no free list yet */
		return;
	}
        printf("Malloc dump\n"
               "We expect the free list to be sorted from low to high addresses\n"
               "with no item overlapping another item and no empty items.\n"
               "Legend:\n"
               "* Successor in list is at lower address than current item\n"
               "# Item has size 0\n"
               "$ This item overlaps (base + size) the next item's base\n");
        printf("List base at %p, freep at %p\n", &state->header_base,
               state->header_freep);
	for (p = prevp->s.ptr;; prevp = p, p = p->s.ptr) {
		if (p > p->s.ptr && p->s.ptr != &state->header_base) {
			printf("* ");
		}
		if (p->s.size == 0 && p != &state->header_base) {
			printf("# ");
		}
		if ((uintptr_t) p + (p->s.size * sizeof(Header)) > (uintptr_t) p->s.ptr && p->s.ptr != &state->header_base) {
			printf("$ ");
		}
		if (p == &state->header_base) {
			printf(" p: <base>\n");
		} else {
			printf(" p: %p (%d) -> %p\n", p, p->s.size, p->s.ptr);
		}
		assert(p != NULL);
		if (p == state->header_freep) {	/* wrapped around free list */
			return;
		}
	}
}
#endif
