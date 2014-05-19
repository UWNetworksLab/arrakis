/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

#define FRAME_SIZE 16384 // 16kb

#define RUN_COUNT 1000

#define THRESH 200000

int main(int argc, char *argv[])
{
    errval_t err;

    bench_init();

    size_t pt_bits, ret_bytes;
    // XXX: X86_64 only
    pt_bits = vnode_objbits(ObjType_VNode_x86_64_ptable);

    struct capref ram;
    struct capref pagetable;
    struct capref frame;
    err = ram_alloc(&ram, pt_bits);
    assert(err_is_ok(err));
    err = slot_alloc(&pagetable);
    assert(err_is_ok(err));
    err = cap_retype(pagetable, ram, ObjType_VNode_x86_64_ptable, pt_bits);
    assert(err_is_ok(err));
    err = frame_alloc(&frame, FRAME_SIZE, &ret_bytes);
    assert(err_is_ok(err));

    // map and unmap one frame 100 times in a row
    cycles_t runs[RUN_COUNT];
    cycles_t start, end;
    paging_x86_64_flags_t flags =
        PTABLE_USER_SUPERVISOR | PTABLE_EXECUTE_DISABLE |
        PTABLE_READ_WRITE;

    for (int i = 0; i < RUN_COUNT; i++) {
        start = bench_tsc();
        err = vnode_map(pagetable,frame,(cslot_t)(i%512),flags,0,1);
        assert(err_is_ok(err));
        end = bench_tsc();
        runs[i] = end - start;
        if (runs[i] > THRESH) {
            runs[i] = BENCH_IGNORE_WATERMARK;
        }
        err = vnode_unmap(pagetable,frame,(cslot_t)(i%512),1);
        assert(err_is_ok(err));
    }

    printf("mapping 1 page:\n");
    printf("  avg. cycles %"PRIuCYCLES", variance %"PRIuCYCLES"\n",
            bench_avg(runs, RUN_COUNT),
            bench_variance(runs, RUN_COUNT));

    printf("\n\n");
    for (int i = 0; i < RUN_COUNT; i+=4) {
        printf("%d: %"PRIuCYCLES" %"PRIuCYCLES" %"PRIuCYCLES" %"PRIuCYCLES"\n",
                i, runs[i], i+1 < RUN_COUNT ? runs[i+1] : 0,
                i+2 < RUN_COUNT ? runs[i+2] : 0,
                i+3 < RUN_COUNT ? runs[i+3] : 0);
    }

    return 0;
}
