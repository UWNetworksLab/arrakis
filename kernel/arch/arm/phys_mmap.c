/**
 * \file
 * \brief Rudimentary physical memory map implementation.
 */

/*
 * Copyright (c) 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stddef.h>
#include <barrelfish_kpi/types.h>
#include <phys_mmap.h>

#include <stdio.h>

static void phys_mmap_push(phys_mmap_t*  mmap,
                           int           index,
                           phys_region_t *region)
{
    assert(index <= mmap->region_count &&
           mmap->region_count < PHYS_MAX_REGIONS - 1);
    for (int i = mmap->region_count; i > index; i--)
    {
        mmap->regions[i] = mmap->regions[i - 1];
    }
    mmap->regions[index] = *region;
    mmap->region_count++;
}

static void phys_mmap_pop(phys_mmap_t* mmap,
                          int          index)
{
    assert(index < mmap->region_count &&
           mmap->region_count < PHYS_MAX_REGIONS);
    for (int i = index; i < mmap->region_count - 1; i++)
    {
        mmap->regions[i] = mmap->regions[i + 1];
    }
    mmap->region_count--;
}

int phys_mmap_add(phys_mmap_t* mmap,
                  lpaddr_t     start,
                  lpaddr_t     limit)
{
    phys_region_t n = { start, limit };

    assert(limit > start);

    for (int i = 0; i < mmap->region_count; i++)
    {
        if (limit <= mmap->regions[i].start)
        {
            assert(i == 0 || start >= mmap->regions[i - 1].limit);
            phys_mmap_push(mmap, i, &n);
            return 1;
        }
        assert(start >= mmap->regions[i].limit);
    }

    assert(mmap->region_count == 0 ||
           start >= mmap->regions[mmap->region_count - 1].limit);
    phys_mmap_push(mmap, mmap->region_count, &n);

    return 1;
}

lpaddr_t phys_mmap_alloc(phys_mmap_t*       mmap,
                         size_t             bytes,
                         size_t             align)
{
    int i = 0;

    assert(0 == (align & (align - 1)));
    align -= 1;

    for (i = 0; i < mmap->region_count; i++)
    {
        phys_region_t* r = &mmap->regions[i];
        lpaddr_t alloc = (r->start + align) & ~align;
        size_t   space = (r->limit > alloc) ? (r->limit - alloc) : 0;
        if (space < bytes)
        {
            continue;
        }
        else if ((alloc + bytes) == r->limit)
        {
            // end of allocation matches end of region
            if (alloc == r->start)
            {
                // completely consuming region
                phys_mmap_pop(mmap, i);
                return alloc;
            }
            else if (alloc > r->start)
            {
                // consuming end of region
                r->limit -= bytes;
                return alloc;
            }
        }
        else if (alloc == r->start)
        {
            // consuming start of region
            r->start = alloc + bytes;
            return alloc;
        }
        else if ((alloc + bytes) < mmap->regions[i].limit)
        {
            // consuming middle of region
            phys_mmap_push(mmap, i, r);
            r->limit     = alloc;
            mmap->regions[i + 1].start = alloc + bytes;
            return alloc;
        }
    }
    return 0;
}

void phys_mmap_remove(phys_mmap_t* mmap,
                      lpaddr_t     start,
                      lpaddr_t     limit)
{
    assert(limit > start);

    for (int i = mmap->region_count - 1; i >= 0; i--)
    {
        if (start >= mmap->regions[i].limit)
        {
            break;
        }
        else if (limit <= mmap->regions[i].start)
        {
            continue;
        }
        else if (start <= mmap->regions[i].start)
        {
            if (limit >= mmap->regions[i].limit)
            {
                // region i is subsumed
                phys_mmap_pop(mmap, i);
            }
            else
            {
                // region i is truncated
                mmap->regions[i].start = limit;
                assert(mmap->regions[i].start < mmap->regions[i].limit);
            }
        }
        else if (limit >= mmap->regions[i].limit)
        {
            // region i limit is truncated
            mmap->regions[i].limit = start;
            break;
        }
        else
        {
            // region i is sliced in two
            phys_region_t n = { limit, mmap->regions[i].limit };

            assert(start > mmap->regions[i].start &&
                   limit < mmap->regions[i].limit);
            mmap->regions[i].limit = start;
            phys_mmap_push(mmap, i + 1, &n);
            break;
        }
    }
}

