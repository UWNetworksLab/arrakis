/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_DMA_POOL_H
#define _AHCI_DMA_POOL_H
#include <string.h>

struct ahci_dma_region {
    void *vaddr;
    genpaddr_t paddr;
    size_t size;
    size_t backing_region;
};

errval_t ahci_dma_pool_init(size_t pool_size);
errval_t ahci_dma_region_alloc(size_t size, struct ahci_dma_region **retregion);
errval_t ahci_dma_region_alloc_aligned(size_t size, size_t alignment_requirement, struct ahci_dma_region **retregion);
errval_t ahci_dma_region_free(struct ahci_dma_region *region);

static inline void *ahci_dma_region_copy_in(struct ahci_dma_region *region, const void *buf, genvaddr_t offset, size_t size) {
    void *dest = (char *)region->vaddr + offset;
    return memcpy(dest, buf, size);
}
static inline void *ahci_dma_region_copy_out(struct ahci_dma_region *region, void *buf, genvaddr_t offset, size_t size) {
    void *src_ = (char *)region->vaddr + offset;
    return memcpy(buf, src_, size);
}

#endif // _AHCI_DMA_POOL_H
