/**
 * \file
 * \brief memory free test.
 *
 * This program allocates a RAM capability, splits it in half, throws
 * away the parent and returns only one half. The memory server has to
 * cope with such a situation.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <assert.h>
#include <stdio.h>

int main(int argc, char **argv)
{
    struct capref theram;

    // Get an 8K RAM cap
    errval_t err = ram_alloc(&theram, 13);
    assert(err_is_ok(err));

    for(int i = 0; i < 100; i++) {
        thread_yield();
    }

    struct capability cap;
    err = debug_cap_identify(theram, &cap);
    assert(err_is_ok(err));
    assert(cap.type == ObjType_RAM);
    printf("got RAM at 0x%" PRIxGENPADDR ", size %u\n",
           cap.u.ram.base, cap.u.ram.bits);

    struct capref leftcap, rightcap;

    // XXX: Hopefully allocates two consecutive slots
    err = slot_alloc(&leftcap);
    assert(err_is_ok(err));
    err = slot_alloc(&rightcap);
    assert(err_is_ok(err));

    // Split in half
    err = cap_retype(leftcap, theram, ObjType_RAM, 12);
    assert(err_is_ok(err));

    err = debug_cap_identify(leftcap, &cap);
    assert(err_is_ok(err));
    assert(cap.type == ObjType_RAM);
    printf("left half at 0x%" PRIxGENPADDR ", size %u\n",
           cap.u.ram.base, cap.u.ram.bits);

    err = debug_cap_identify(rightcap, &cap);
    assert(err_is_ok(err));
    assert(cap.type == ObjType_RAM);
    printf("right half at 0x%" PRIxGENPADDR ", size %u\n",
           cap.u.ram.base, cap.u.ram.bits);

    // Delete the parent (8K range)
    printf("deleting parent\n");
    err = cap_delete(theram);
    assert(err_is_ok(err));

    // Delete the left half (4K)
    printf("deleting left half\n");
    err = cap_delete(leftcap);
    assert(err_is_ok(err));

    // Delete the right half (4K)
    printf("deleting right half\n");
    err = cap_delete(rightcap);
    assert(err_is_ok(err));

    return 0;
}
