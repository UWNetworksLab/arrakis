/**
 * \file
 * \brief Local memory allocator for init till mem_serv is ready to use
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "init.h"
#include <mm/mm.h>

/* parameters for local memory allocator used until we spawn mem_serv */
#define MM_REQUIREDBITS    24          ///< Required size of memory to boot (16MB)
#define MM_MAXSIZEBITS     (MM_REQUIREDBITS + 2) ///< Max size of memory in allocator
#define MM_MINSIZEBITS     BASE_PAGE_BITS ///< Min size of allocation
#define MM_MAXCHILDBITS    1           ///< Max branching factor of BTree nodes
#define MM_MAXDEPTH (MM_MAXSIZEBITS - MM_MINSIZEBITS + 1)   ///< BTree depth
#define MM_NNODES   ((1UL << MM_MAXDEPTH) + MM_MINSIZEBITS - OBJBITS_DISPATCHER) ///< Max BTree nodes
#define MM_NCNODES  DIVIDE_ROUND_UP(MM_NNODES, 1UL << DEFAULT_CNODE_BITS) //CNodes

// Number of slots placed in smallcn of mem_serv
#define MEM_SERV_SMALLCN_SLOTS 10

/// MM allocator instance data
static struct mm mymm;

static errval_t mymm_alloc(struct capref *ret, uint8_t bits, uint64_t minbase,
                           uint64_t maxlimit)
{
    /* XXX: although we have calculated the space requirements for
     * MM_MINSIZEBITS, we only ever allocate a single dispatcher from this
     * allocator, so this should be safe */
    assert(bits >= OBJBITS_DISPATCHER);
    errval_t err = mm_alloc(&mymm, bits, ret, NULL);
    return err;
}

/**
 * \brief Setups a local memory allocator for init to use till the memory server
 * is ready to be used.
 */
errval_t initialize_ram_alloc(void)
{
    errval_t err;

    /* walk bootinfo looking for suitable RAM cap to use
     * we pick the first cap equal to MM_REQUIREDBITS,
     * or else the next closest less than MM_MAXSIZEBITS */
    int mem_region = -1, mem_slot = 0;
    struct capref mem_cap = {
        .cnode = cnode_super,
        .slot = 0,
    };

    assert(bi != NULL);
    for (int i = 0; i < bi->regions_length; i++) {
        assert(!bi->regions[i].mr_consumed);
        if (bi->regions[i].mr_type == RegionType_Empty) {
            if (bi->regions[i].mr_bits >= MM_REQUIREDBITS 
                && bi->regions[i].mr_bits <= MM_MAXSIZEBITS && (mem_region == -1
                 || bi->regions[i].mr_bits < bi->regions[mem_region].mr_bits)) {
                mem_region = i;
                mem_cap.slot = mem_slot;
                if (bi->regions[i].mr_bits == MM_REQUIREDBITS) {
                    break;
                }
            }
            mem_slot++;
        }
    }
    if (mem_region < 0) {
        printf("Error: no RAM capability found in the size range "
               "2^%d to 2^%d bytes\n", MM_REQUIREDBITS, MM_MAXSIZEBITS);
        return INIT_ERR_NO_MATCHING_RAM_CAP;
    }
    bi->regions[mem_region].mr_consumed = true;

    /* init slot allocator */
    static struct slot_alloc_basecn init_slot_alloc;
    err = slot_alloc_basecn_init(&init_slot_alloc);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_SLOT_ALLOC_INIT);
    }

    /*  init MM allocator */
    assert(bi->regions[mem_region].mr_type != RegionType_Module);
    err = mm_init(&mymm, ObjType_RAM, bi->regions[mem_region].mr_base,
                  bi->regions[mem_region].mr_bits, MM_MAXCHILDBITS, NULL,
                  slot_alloc_basecn, &init_slot_alloc, true);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_INIT);
    }

    /* give MM allocator enough static storage for its node allocator */
    static char nodebuf[SLAB_STATIC_SIZE(MM_NNODES, MM_NODE_SIZE(MM_MAXCHILDBITS))];
    slab_grow(&mymm.slabs, nodebuf, sizeof(nodebuf));

    /* add single RAM cap to allocator */
    err = mm_add(&mymm, mem_cap, bi->regions[mem_region].mr_bits,
               bi->regions[mem_region].mr_base);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_MM_ADD);
    }

    // initialise generic RAM allocator to use local allocator
    err = ram_alloc_set(mymm_alloc);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    return SYS_ERR_OK;
}
