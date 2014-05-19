/**
 * \file
 * \brief A paging helper file
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_PAGING_HELPER_H
#define KERNEL_PAGING_HELPER_H

/**
 * \brief Align a virtual address
 *
 * \param vbase         Virtual base address of mapping.
 * \param base          Physical base address of mapping.
 * \param size          Size of mapping in bytes.
 * \param pagesize      Page size in bytes (must be power of 2).
 */
static inline void paging_align(lvaddr_t *vbase, lpaddr_t *base, size_t *size,
                                size_t pagesize)
{
    size_t      pagemask = pagesize - 1;

    // XXX: Page size must be power of 2 (we only check divisibility by 2).
    assert(pagesize % 2 == 0);

    // Align vbase to page size
    if(*vbase & pagemask) {
        *size += *vbase & pagemask;
        *vbase -= *vbase & pagemask;
    }

    // Check whether base is aligned
/////    assert(((*base) & pagemask) == 0);
    // Align base to page size
    if(base != NULL && (*base & pagemask)) {
        printk(LOG_WARN, "Given paddr %" PRIxLPADDR " unaligned to system "
               "page size %zu\n", *base, pagesize);
        /* *base -= *base & pagemask; */
    }

    // Align size to page size
    if(*size & pagemask) {
        *size += pagesize - (*size & pagemask);
    }
}

#endif
