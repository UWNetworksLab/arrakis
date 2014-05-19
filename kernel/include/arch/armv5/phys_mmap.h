/**
 * \file
 * \brief Rudimentary physical memory map.
 */

/*
 * Copyright (c) 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARM_PHYS_MAP_H
#define ARM_PHYS_MAP_H

typedef struct
{
    lpaddr_t    start;
    lpaddr_t    limit;
} phys_region_t;

#define PHYS_MAX_REGIONS (32)

typedef struct phys_mmap
{
    int           region_count;
    phys_region_t regions[PHYS_MAX_REGIONS];
} phys_mmap_t;

/**
 * Insert range into physical memory map. Range must not overlap
 * with existing map entries. This function does not coalesce
 * abutting regions.
 *
 * @param mmap  memory map.
 * @param start start of physical region.
 * @param limit limit of physical region.
 *
 * @return non-zero upon success.
 */
int phys_mmap_add(phys_mmap_t* mmap,
                  lpaddr_t     start,
                  lpaddr_t     limit);

/**
 * Allocate and remove from physical memory map.
 *
 * @param mmap      memory map
 * @param bytes     number of bytes to remove.
 * @param alignment alignment required.
 *
 * @return pointer to allocated region or null.
 */
lpaddr_t phys_mmap_alloc(phys_mmap_t* mmap,
                         size_t       bytes,
                         size_t       alignment);

/**
 * Remove region from physical address map.
 *
 * @param mmap
 * @param start
 * @param limit
 */
void phys_mmap_remove(phys_mmap_t* mmap,
                      lpaddr_t     start,
                      lpaddr_t     limit);

#endif // ARM_PHYS_MAP_H
