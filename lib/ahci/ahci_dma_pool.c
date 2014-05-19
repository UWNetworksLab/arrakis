/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <ahci/ahci_dma_pool.h>
#include <errors/errno.h>
#include <string.h>
#include <sys/types.h> /* for ssize_t */
#include "ahci_debug.h"

#define EIGHT_MEG 8388608

// meta-data structure for the dma pool
struct dma_pool {
    // size of the pool in bytes
    size_t size;

    // number of allocated backing memory regions
    size_t addr_count;

    // number of allocated slots for backing memory regions
    size_t addr_entries;

    // virtual addresses of backing memory regions
    // (valid for indices 0 .. addr_count - 1)
    void **virt_addrs;

    // physical addresses of backing memory regions
    // (valid for indices 0 .. addr_count - 1)
    genpaddr_t *phys_addrs;

    // frame caprefs for backing memory regions
    // (valid for indices 0 .. addr_count - 1)
    struct capref *frame_caps;

    // void* pointing to the first free chunk in backing 
    // memory regions (only valid for indices 0 .. addr_count - 1)
    struct free **first_free;
};

// free list element (contained in the first bytes of a free chunk itself)
struct free {
    // pointers to previous and next free chunks
    struct free *next, *prev;

    // size of this chunk
    size_t size;

    // the index of the backing region of memory
    size_t backing_region;
};


static struct dma_pool pool;
static struct free *first_free = NULL, *last_free = NULL;

/**
 * This function grows the address arrays in the pool metadata
 * \return LIB_ERR_MALLOC_FAIL when a realloc fails, SYS_ERR_OK else.
 */
static errval_t
grow_vp_arrays(void)
{
    AHCI_TRACE_ENTER0();
    if (pool.addr_entries == 0) { // initial alloc
        pool.virt_addrs = calloc(8, sizeof(void *));
        if (pool.virt_addrs == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
        pool.phys_addrs = calloc(8, sizeof(genpaddr_t));
        if (pool.phys_addrs == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
        pool.frame_caps = calloc(8, sizeof(struct capref));
        if (pool.frame_caps == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
        pool.first_free = calloc(8, sizeof(struct free *));
        if (pool.first_free == NULL) {
            return LIB_ERR_MALLOC_FAIL;
        }
        pool.addr_entries = 8;
    }
    else { // double size
        size_t new_size = pool.addr_entries * 2;
        void **newv;
        struct capref *newc;
        genpaddr_t *newp;
        struct free **newf;
        newv = realloc(pool.virt_addrs, new_size*sizeof(void *));
        if (newv == NULL) {
            AHCI_DEBUG("realloc failed for pool.virt_addrs");
            return LIB_ERR_MALLOC_FAIL;
        }
        memset(newv+pool.addr_entries, 0, pool.addr_entries);
        newp = realloc(pool.phys_addrs, new_size*sizeof(genpaddr_t));
        if (newp == NULL) {
            AHCI_DEBUG("realloc failed for pool.phys_addrs");
            return LIB_ERR_MALLOC_FAIL;
        }
        memset(newp+pool.addr_entries, 0, pool.addr_entries);
        newc = realloc(pool.frame_caps, new_size*sizeof(struct capref));
        if (newc == NULL) {
            AHCI_DEBUG("realloc failed for pool.frame_caps\n");
            return LIB_ERR_MALLOC_FAIL;
        }
        memset(newc+pool.addr_entries, 0, pool.addr_entries);
        newf = realloc(pool.first_free, new_size*sizeof(struct free *));
        if (newf == NULL) {
            AHCI_DEBUG("realloc failed for pool.first_free");
            return LIB_ERR_MALLOC_FAIL;
        }
        memset(newf+pool.addr_entries, 0, pool.addr_entries);
        pool.virt_addrs = newv;
        pool.phys_addrs = newp;
        pool.frame_caps = newc;
        pool.first_free = newf;
        pool.addr_entries = new_size;
    }
    return SYS_ERR_OK;
}

/**
 * Increase dma pool size to new_pool_size, growing
 * address arrays if necessary.
 */
static errval_t
grow_dma_pool(size_t new_pool_size)
{
    AHCI_TRACE_ENTER("new pool size = %zd", new_pool_size);
    // determine additional pool size (using ssize_t temp variable in order to
    // detect requests that would shrink the pool).
    ssize_t temp_pool_size = new_pool_size;
    if (pool.size != 0) {
        temp_pool_size -= pool.size;
    }

    // return if pool already big enough
    if (temp_pool_size <= 0) {
        return SYS_ERR_OK;
    }

    // from here: temp_pool_size > 0
    if (temp_pool_size <= BASE_PAGE_SIZE) {
        temp_pool_size = BASE_PAGE_SIZE;
    }

    // allocate and map memory
    struct capref frame;
    struct frame_identity frameid;
    errval_t err = SYS_ERR_OK;
    void *va;
    size_t retsize;
    // allocate, map and store frame(s)
    do {
        // allocate frame
        err = frame_alloc(&frame, temp_pool_size, &retsize);
        if (err_is_fail(err)) {
            break;
        }
        // map frame
        err = vspace_map_one_frame_attr(&va, retsize, frame,
                VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
        if (err_is_fail(err)) {
            break;
        }
        // get frame phys addr
        err = invoke_frame_identify(frame, &frameid);
        if (err_is_fail(err)) {
            break;
        }

        // grow arrays if needed
        if (pool.addr_count == pool.addr_entries) {
            err = grow_vp_arrays();
            if (err_is_fail(err)) {
                break;
            }
        }

        // store new frame in pool
        pool.size += retsize;
        pool.virt_addrs[pool.addr_count] = va;
        pool.phys_addrs[pool.addr_count] = frameid.base;
        pool.frame_caps[pool.addr_count] = frame;
        // update free list
        struct free *f = (struct free *)va;
        pool.first_free[pool.addr_count] = f;
        f->size = retsize;
        f->backing_region = pool.addr_count;
        f->prev = last_free;
        if (first_free == NULL) { // first region
            first_free = last_free = f;
        } else {
            last_free->next = f;
        }
        f->next = NULL;
        pool.addr_count += 1;
        temp_pool_size -= retsize;
    } while(temp_pool_size > 0);

    return err;
}

/**
 * Init DMA pool
 * \param pool_size Initial pool size in bytes
 */
errval_t
ahci_dma_pool_init(size_t pool_size)
{
    AHCI_TRACE_ENTER("pool size = %zd", pool_size);
    // round pool_size up to page size
    pool_size = ROUND_UP(pool_size, BASE_PAGE_SIZE);
    return grow_dma_pool(pool_size);
}

static void
remove_from_free_list(struct free *f)
{
    if (f == first_free && f == last_free) {
        // only remaining free chunk, clear free list
        first_free = last_free = NULL;
        pool.first_free[f->backing_region] = NULL;
    } else if (f == first_free) {
        // we were first free, f->next != NULL, make f->next first_free
        first_free = f->next;
        first_free->prev = NULL;
    } else if (f == last_free) {
        // we were last free, f->prev != NULL, make f->prev last free
        last_free = f->prev;
        last_free->next = NULL;
    } else {
        // we are somewhere in the middle, remove
        f->prev->next = f->next;
        f->next->prev = f->prev;
    }

    if (f == pool.first_free[f->backing_region]) {
        if (f->next && f->next->backing_region == f->backing_region) {
            // we were the first free in our backing region, and there is
            // another free chunk in the same region: make that chunk first_free
            // in our backing region
            pool.first_free[f->backing_region] = f->next;
        }
        else {
            // no other free chunks in our backing region, clear first_free
            pool.first_free[f->backing_region] = NULL;
        }
    }
}

/**
 * Build a struct ahci_dma_region using the end of the free chunk pointed to
 * by f ensuring that the resulting address for the return region is aligned to
 * alignment_requirement.
 * Note: All free chunk addresses should be aligned to at least 512 bytes.
 */
static struct ahci_dma_region*
get_region(struct free *f, size_t size, size_t alignment_requirement)
{
    AHCI_TRACE_ENTER("f = %p; size = %zd; alignment = 0x%zx",
            f, size, alignment_requirement);

    struct ahci_dma_region *r = calloc(1, sizeof(struct ahci_dma_region));
    if (r == NULL) {
        return NULL;
    }

    // set the backing region in the new ahci_dma_region
    r->backing_region = f->backing_region;
    AHCI_DEBUG("backing region = %zd\n", f->backing_region);

    // check that remaining free chunk is at least 512 bytes large.
    // If that is not the case just use the whole chunk.
    ssize_t rem = 0;
    if (f->size > size) {
        rem = f->size - size;
        if (rem < 512) {
            rem = 0;
            size = f->size;
        }
    }
    AHCI_DEBUG("rem: %zd; size: %zd; f->size: %zd\n", rem, size, f->size);

    // calculate the aligned virtual address of the new ahci_dma_region
    uintptr_t vaddr_unaligned = ((uintptr_t) f) + rem;
    AHCI_DEBUG("vaddr_unaligned = 0x%zx\n", vaddr_unaligned);
    uintptr_t vaddr = vaddr_unaligned & ~(alignment_requirement-1);

    // sanity check the aligned virtual address, return error if it is 
    // smaller than the address of the free chunk.
    //
    // This can happen when you have a merged free chunk which was created from
    // the following two (constructed) chunks: f1=0x600, f1->size=0x200 and
    // f2=0x800, f2->size=0x200 --(merge)--> f=0x600, f->size=0x400
    // Now suppose we have a request for size=0x400 and alignment_requirement=0x400:
    // on first glance `f' seems to match that request, but after aligning the address
    // vaddr will be smaller than f and thus f cannot fulfil this request.
    if (vaddr < (uintptr_t)f) {
        AHCI_DEBUG("too small\n");
        free(r);
        return (void*)-1;
    }
    AHCI_DEBUG("vaddr = 0x%zx\n", vaddr);

    // recalculate the size of the ahci_dma_region and the remaining chunk size
    ptrdiff_t addr_diff = vaddr_unaligned - vaddr;
    size += addr_diff;
    // here rem should either remain >= 512 or become 0
    rem -= addr_diff;

    AHCI_DEBUG("rem: %zd; size: %zd; f->size: %zd\n", rem, size, f->size);

    // calculate the offset into the backing region
    ptrdiff_t offset = vaddr - ((uintptr_t) pool.virt_addrs[f->backing_region]);
    AHCI_DEBUG("offset = 0x%zx\n", offset);
    // set the remaining fields in the ahci_dma_region
    r->vaddr = (void *) vaddr;
    r->paddr = pool.phys_addrs[f->backing_region] + offset;
    AHCI_DEBUG("paddr = 0x%zx\n", r->paddr);
    r->size = size;

    if (rem == 0) {
        // no remaining space in this free chunk, need to delete from free list
        remove_from_free_list(f);
    }
    else {
        // adjust size of free chunk
        f->size = rem;
    }

    return r;
}

/**
 * Allocate a dma region aligned to a multiple of alignment_requirement.
 * \param size the minimum size of the requested region
 * \param alignment_requirement align address and size of the chunk to a
 *        multiple of this number
 * \param retregion the newly allocated ahci_dma_region
 */
errval_t
ahci_dma_region_alloc_aligned(size_t size, size_t alignment_requirement,
        struct ahci_dma_region **retregion)
{
    AHCI_DEBUG("size = %zd; alignment_requirement = 0x%zx; retregion = %p\n",
            size, alignment_requirement, retregion);

    if (retregion == NULL) {
        return AHCI_ERR_ILLEGAL_ARGUMENT;
    }

    // set alignment_requirement (and therefore size) of request
    // to at least 512
    if (alignment_requirement < 512) {
	alignment_requirement = 512;
    }

    // round alloc request to alignment_requirement bytes
    size = ROUND_UP(size, alignment_requirement);

    // iterate over free list
    struct free *f;
    f = first_free;
restart:
    for (; f; f = f->next) {
        AHCI_DEBUG("f = %p; f->size = %zd\n", f, f->size);
        if (f->size >= size) { // found sufficiently big free chunk
            AHCI_DEBUG("found free chunk\n");
            *retregion = get_region(f, size, alignment_requirement);
            if (retregion == (void*)-1) {
                // when aligning the address the address became smaller than
                // the address of the free region. Search more.
                continue;
            }
            AHCI_DEBUG("retregion = %p\n", *retregion);
            // if *retregion is NULL here, get_region() failed.
            if (*retregion == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
            return SYS_ERR_OK;
        }
    }
    AHCI_DEBUG("did not find sufficiently large free region, trying to grow pool.\n");
    // save index of first free slot in pool
    size_t first_new = pool.addr_count;

    // grow pool by at least the size of the request.
    // This prevents that we reach this code again, because in the next search
    // we will find the newly allocated memory with size >= request size
    errval_t err = grow_dma_pool(pool.size + size);
    if (err_is_fail(err)) {
        return err;
    }

    // continue searching for sufficiently large regions starting where we
    // allocated new memory just now
    f = pool.first_free[first_new];
    goto restart;
}

/**
 * Allocate a new dma region with alignment requirement 512
 * \param size the minimum size of the requested region
 * \param alignment_requirement align address and size of the chunk to a
 *        multiple of this number
 * \param retregion the newly allocated ahci_dma_region
 */
errval_t
ahci_dma_region_alloc(size_t size, struct ahci_dma_region **retregion)
{
    return ahci_dma_region_alloc_aligned(size, 512, retregion);
}

/**
 * Insert dma region region into free list.
 * \param region region to insert into free list
 */
static errval_t
return_region(struct ahci_dma_region *region)
{
    AHCI_TRACE_ENTER("region = %p; region->vaddr = %p", region, region->vaddr);
    if (first_free == NULL) {
        // no free chunks. init free list with current region
        AHCI_DEBUG("no free chunks. init free list with current region\n");
        first_free = last_free = region->vaddr;
        first_free->size = region->size;
        first_free->backing_region = region->backing_region;
        pool.first_free[first_free->backing_region] = first_free;
    }
    else {
        // insert into free list
        AHCI_DEBUG("insert into free list\n");
        struct free *n = pool.first_free[region->backing_region];
        struct free *f;
        f = region->vaddr;
        f->size = region->size;
        f->backing_region = region->backing_region;
        AHCI_DEBUG("free: vaddr = %p; size = %zd; backing_region = %zd\n",
                f, f->size, f->backing_region);

        if (n == NULL) {
            // current region only free in backing block
            AHCI_DEBUG("current region only free in backing block\n");
            pool.first_free[region->backing_region] = f;
            if (region->backing_region >= pool.addr_count) {
                // we are in the last backing block && only free region in block
                //  --> we are the new last_free
                AHCI_DEBUG("new last_free\n");
                f->next = NULL;
                f->prev = last_free;
                last_free->next = f;
                last_free = f;
            }
            else {
                // find next backing region with free chunks
                AHCI_DEBUG("find next backing_region w/ free chunks\n");
                size_t br = region->backing_region + 1;
                while (br < pool.addr_count) {
                    if ((n = pool.first_free[br]) != NULL) {
                        // found next free, insert before
                        AHCI_DEBUG("found next br w/ free: %zd\n", br);
                        n->prev->next = f;
                        f->prev = n->prev;
                        n->prev = f;
                        f->next = n;
                        break;
                    }
                    br++;
                }

                // we didn't find any backing region after ours with free
                // chunks --> we are last free
                if (br >= pool.addr_count) {
                    AHCI_DEBUG("last_free");
                    f->next = NULL;
                    f->prev = last_free;
                    last_free->next = f;
                    last_free = f;
                }

                // we've inserted ourselves before the first_free,
                // becoming first_free
                if (f->prev == NULL) {
                    AHCI_DEBUG("first_free");
                    first_free = f;
                }
            }
        } // end current region only free in backing block
        else {
            // insert into backing block's free list
            AHCI_DEBUG("insert into backing block's free list\n");

            // find our next
            while (n && (void *)n < region->vaddr) {
                n = n->next;
            }

            // insert before n
            AHCI_DEBUG("insert before: %p\n", n);
            if (n == NULL) {
                // we are new last_free
                AHCI_DEBUG("last free\n");
                f->next = NULL;
                f->prev = last_free;
                last_free->next = f;
                last_free = f;
            }
            else if (n == first_free) {
                // we are new first_free
                AHCI_DEBUG("first free\n");
                f->prev = NULL;
                f->next = first_free;
                first_free->prev = f;
                first_free = f;
                // we're first_free; this implies that we are
                // first_free of our backing region
                pool.first_free[f->backing_region] = f;
            }
            else {
                if (n == pool.first_free[f->backing_region]) {
                    // we are first free in our region
                    AHCI_DEBUG("first free in region\n");
                    pool.first_free[f->backing_region] = f;
                }

                // insert before n
                n->prev->next = f;
                f->prev = n->prev;
                n->prev = f;
                f->next = n;
            }
        } // end insert into backing block's free list

        // merge with next, if possible
        if (f->next && f->next->backing_region == f->backing_region) {
            n = f->next;
            // check if f and f->next can be merged
            uintptr_t fp = (uintptr_t) f;
            uintptr_t np = (uintptr_t) n;
            if (fp + f->size == np) {
                AHCI_DEBUG("merging with next free (%p)\n", f->next);
                // remove f->next from free list and add its size to f
                struct free *new_next = n->next;
                f->size += n->size;
                f->next = new_next;

                if (last_free == n) {
                    // we are new last free
                    last_free = f;
                } else {
                    new_next->prev = f;
                }
            }
        } // end merge with next

        // merge with previous, if possible
        if (f->prev && f->prev->backing_region == f->backing_region) {
            struct free *p = f->prev;
            // check if f->prev and f can be merged
            uintptr_t fp = (uintptr_t) f;
            uintptr_t pp = (uintptr_t) p;
            if (pp + p->size == fp) {
                AHCI_DEBUG("merging with prev free (%p)\n", f->prev);
                // add f's size to f->prev's size and remove f from free list
                p->size += f->size;
                p->next = f->next;

                if (last_free == f) {
                    // we are new last free
                    last_free = p;
                }
                else {
                    f->next->prev = p;
                }
            }
        } // end merge with previous
    } // end insert into free list

    return SYS_ERR_OK;
}

/**
 * Free ahci_dma_region region
 * \param region the dma region to free
 */
errval_t
ahci_dma_region_free(struct ahci_dma_region *region)
{
    AHCI_TRACE_ENTER("region = %p", region);
    // insert memory into free list
    errval_t err = return_region(region);
    if (err_is_fail(err)) {
        return err;
    }
    // free struct ahci_dma_region
    free(region);

    return SYS_ERR_OK;
}
