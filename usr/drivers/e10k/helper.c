/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "helper.h"

#include <stdio.h>
#include <stdint.h>

#include <skb/skb.h>


/* Dump bytes of memory region to stdout */
void debug_dumpmem(void* buf, size_t len)
{
    uint8_t* b = buf;

    while (len--)
        printf("0x%02x ", *b++);
    printf("\n");
}

/* allocate a single frame, mapping it into our vspace with given attributes */
void* alloc_map_frame(vregion_flags_t attr, size_t size, struct capref *retcap)
{
    struct capref frame;
    errval_t r;

    r = frame_alloc(&frame, size, NULL);
    assert(err_is_ok(r));
    void *va;
    r = vspace_map_one_frame_attr(&va, size, frame, attr,
                                  NULL, NULL);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "vspace_map_one_frame failed");
        return NULL;
    }

    if (retcap != NULL) {
        *retcap = frame;
    }

    return va;
}

/* Get APIC id for specified core */
errval_t get_apicid_from_core(coreid_t cid, uint8_t *apicid)
{
    static bool connected = false;
    errval_t err;
    unsigned int i;

    if (!connected) {
        err = skb_client_connect();
        if (err_is_fail(err)) {
            return err;
        }
        connected = true;
    }

    err = skb_execute_query("corename(%d,_,apic(ApicID)),write(ApicID).",
                            cid);
    if (err_is_fail(err)) {
        return err;
    }
    err = skb_read_output("%u.", &i);
    *apicid = i;
    return err;
}


/*****************************************************************************/
/* Bitmap based allocator */


/** Init allocator for n objects. */
bool bmallocator_init(struct bmallocator *alloc, size_t n)
{
    alloc->count = n;
    alloc->bitmap = calloc((n + BMALLOCATOR_BITS - 1) / BMALLOCATOR_BITS,
                           BMALLOCATOR_BITS / 8);
    return alloc->bitmap != NULL;
}

/** Release memory associated with allocator. */
void bmallocator_destroy(struct bmallocator *alloc)
{
    free(alloc->bitmap);
    alloc->bitmap = NULL;
}

/** Allocate object, return index in *n if successful (return true). */
bool bmallocator_alloc(struct bmallocator *alloc, size_t *n)
{
    size_t i;
    BMALLOCATOR_TYPE bit;
    size_t idx;

    // This could be improved
    for (i = 0; i < alloc->count; i++) {
        bit = 1 << (i % BMALLOCATOR_BITS);
        idx = i / BMALLOCATOR_BITS;

        if (!(alloc->bitmap[idx] & bit)) {
            alloc->bitmap[idx] |= bit;
            *n = i;
            return true;
        }
    }
    return false;
}

/** Free object n, return value indicates if it was allocated before. */
bool bmallocator_free(struct bmallocator *alloc, size_t n)
{
    bool result;
    BMALLOCATOR_TYPE bit;
    size_t idx;

    if (n >= alloc->count) {
        return false;
    }

    bit = (1 << (n % BMALLOCATOR_BITS));
    idx = n / BMALLOCATOR_BITS;

    result = alloc->bitmap[idx] & bit;
    alloc->bitmap[idx] &= ~bit;

    return result;
}

