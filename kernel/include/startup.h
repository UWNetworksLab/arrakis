/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __KERNEL_STARTUP_H
#define __KERNEL_STARTUP_H

struct capability;
struct mem_region;
struct bootinfo;
enum region_type;

struct spawn_state {
    /// Init's cnodes
    struct cte *taskcn, *segcn, *supercn, *physaddrcn, *modulecn,
               *pagecn, *basepagecn,
               *slot_alloc_cn0, *slot_alloc_cn1, *slot_alloc_cn2;

    /// Next slot in each cnode
    cslot_t segcn_slot, supercn_slot, physaddrcn_slot, modulecn_slot;

    /// Address of arguments page
    lpaddr_t args_page;
};

errval_t create_caps_to_cnode(lpaddr_t base_addr, size_t size,
                              enum region_type type,
                              struct spawn_state *st, struct bootinfo *bootinfo);

/**
 * \brief Linear physical memory allocator callback function.
 *
 * This function allocates a linear region of addresses of size 'size' from
 * physical memory.
 *
 * \param size  Number of bytes to allocate.
 *
 * \return Base physical address of memory region.
 */
typedef lpaddr_t (*alloc_phys_func)(size_t size);

struct dcb *spawn_module(struct spawn_state *st,
                         const char *name, int argc, const char** argv,
                         lpaddr_t bootinfo, lvaddr_t args_base,
                         alloc_phys_func alloc_phys, lvaddr_t *retparamaddr);

#endif // __KERNEL_STARTUP_H
