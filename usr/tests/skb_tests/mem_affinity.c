/**
 * \file
 * \brief Queries the SKB to construct a multicast tree
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <stdlib.h>
#include <skb/skb.h>
#include <barrelfish/dispatch.h>

int main(int argc, char**argv)
{
    struct mem_aff {
        uint64_t base, limit;
    };
    struct mem_aff memory_affinities[20];
    uintptr_t idx = 0;
    errval_t err;

    char query[128];
    coreid_t core_id = disp_get_core_id();

    skb_client_connect();

    sprintf(query,"local_memory_affinity(%u, List),length(List,Len),"
                  "write(output,len(Len)),write(output,List).", core_id);

    printf("\nexecute query...\n");

    char *output, *errstr;
    int errint;
    err = skb_evaluate(query, &output, &errstr, &errint);
    assert(err_is_ok(err));
    assert(errint == 0);

    printf("\nquery executed.\n");
    printf("\nresult = %s\n", output);

    while(!((output[0] == 'l') && (output[1] == 'e') && (output[2] == 'n')))
        output++;
    output += 4;
    int len = atoi(output);
    printf("\nprocess %u elements.\n", len);
    for (int i = 0; i < len; i++) {
        while(!(output[0] == 'r')) output++;
        while (!((output[0] >= '0') && (output[0] <= '9'))) output++;
        uint64_t base = atol(output);
        while(!(output[0] == ',')) output++;
        while (!((output[0] >= '0') && (output[0] <= '9'))) output++;
        uint64_t limit = atol(output);
        memory_affinities[idx].base = base;
        memory_affinities[idx].limit = limit;
        idx++;
    }

    for (int i = 0; i < idx; i++) {
        printf("found affinity %d: [%lx, %lx]\n", i, memory_affinities[i].base,
                memory_affinities[i].limit);
    }
    return 0;
}
