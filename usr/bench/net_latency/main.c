/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/spawn_client.h>

#include "sleep.h"

#include <skb/skb.h>

// From lib/dist/skb.c
errval_t get_cores_skb(coreid_t **cores, int *n_cores);


static void memory_affinity(int core, uint64_t* min, uint64_t* max)
{
    errval_t r;
    r = skb_execute_query("local_memory_affinity(%d, B, L),"
                          "write(base(B)),write(limit(L)).", core);
    assert(err_is_ok(r));

    r = skb_read_output("base(%lu)limit(%lu).", min, max);
    assert(err_is_ok(r));
}


static void start_run(uint8_t core, uint8_t memory, int payload, int nocache,
                      int read_incoming, int head_idx_wb)
{
    errval_t r;
    domainid_t new_domain;
    uint8_t code;
    char plsz[strlen("payload_size=0000") + 1];
    char noca[strlen("elb_nocache=0") + 1];
    char rdic[strlen("read_incoming=0") + 1];
    char hiwb[strlen("head_idx_wb=0") + 1];
    char affmin[64];
    char affmax[64];
    char prefix[128];
    uint64_t aff_min, aff_max;

    memory_affinity(memory, &aff_min, &aff_max);

    char* const argv[] = { "e10k_queue_elb", "queue=0", "runs=1000",
                           "dump_each=1", plsz, noca, rdic, hiwb, affmin,
                           affmax, prefix, NULL };

    sprintf(plsz, "payload_size=%d", payload);
    sprintf(plsz, "elb_nocache=%d", nocache);
    sprintf(rdic, "read_incoming=%d", read_incoming);
    sprintf(hiwb, "head_idx_wb=%d", head_idx_wb);
    sprintf(affmin, "affinitymin=%"PRIu64, aff_min);
    sprintf(affmax, "affinitymax=%"PRIu64, aff_max);
    sprintf(prefix, "elp_outprefix=%%%d,%d,%d,%d,%d,%d,", core, memory, payload,
            nocache, read_incoming, head_idx_wb);

    printf("##### spawning programm %s\n", argv[0]);
    r = spawn_program(core, argv[0], argv, NULL, SPAWN_NEW_DOMAIN, &new_domain);
    assert(err_is_ok(r));

    r = spawn_wait_core(core, new_domain, &code, false);
    assert(err_is_ok(r));
}


int main(int argc, char **argv)
{
    coreid_t *cores;
    int core_count;

    int c_idx;
    uint8_t memory;
    int payloadsz = 64;
    int nocache = 0;
    int read_incoming = 0;
    int head_idx_wb = 1;
    errval_t err;

    sleep_init();

    // Connect to SKB to get info
    err = skb_client_connect();
    assert(err_is_ok(err));

    // Get information about available cores
    get_cores_skb(&cores, &core_count);

    printf("Net latency benchmark start\n");
    printf("%%\"core\",\"memory\",\"payload\",\"nocache\",\"touch\",\"hiwb\","
           "\"rtt\",\"time\"\n");
    for (c_idx = 0; c_idx < core_count; c_idx++) {
        for (memory = 0; memory < 16; memory += 4) {
        for (payloadsz = 64; payloadsz < 1500; payloadsz *= 4) {
            for (nocache = 0; nocache <= 1; nocache++) {
                for (read_incoming = 0; read_incoming <= 1; read_incoming++) {
                for (head_idx_wb = 0; head_idx_wb <= 1; head_idx_wb++) {
                    start_run(cores[c_idx], memory, payloadsz, nocache,
                             read_incoming, head_idx_wb);
                }
                }
            }
        }
        }
    }
    printf("Net latency benchmark done\n");
    return 0;
}

