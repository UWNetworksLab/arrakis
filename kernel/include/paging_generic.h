/**
 * \file
 * \brief Kernel memory management.
 */

/*
 * Copyright (c) 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PAGING_H
#define PAGING_H

#include <barrelfish/types.h>
#include <errors/errno.h>

struct mapping_info {
    lpaddr_t pte;       ///< where the capability is mapped
    size_t pte_count;   ///< the amount of PTEs mapped in this mapping
    uint64_t offset;    ///< the offset into the physical region identified by the capability where the mapping begins.
};

struct cte;
errval_t compile_vaddr(struct cte *ptable, size_t entry, genvaddr_t *retvaddr);
errval_t unmap_capability(struct cte *mem);
errval_t lookup_cap_for_mapping(genpaddr_t paddr, lvaddr_t pte, struct cte **retcte);
errval_t paging_tlb_flush_range(struct cte *frame, size_t pages);

#endif // PAGING_H
