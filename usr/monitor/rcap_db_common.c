/** \file
 *  \brief Inter-core capability database, common code
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <string.h>
#include <barrelfish_kpi/capbits.h>

#define HASH_M ((uint64_t)0xc6a4a7935bd1e995ULL)
#define HASH_R 47


uint64_t hash_cap(struct capability * cap)
{
    // Based upon MurmurHash
    uint64_t hash = 3 ^ (sizeof(struct capability) * HASH_M);

    uint64_t * data = (uint64_t *)cap;
    uint64_t * end = data + (sizeof(struct capability)/8);

    while(data != end) {
        uint64_t k = *data++;

        k *= HASH_M; 
        k ^= k >> HASH_R; 
        k *= HASH_M; 
		
        hash ^= k;
        hash *= HASH_M; 
    }

    uint8_t * data_end = (uint8_t *)data;

    switch(sizeof(struct cap_ident) & 7) {
    case 7: hash ^= (uint64_t)(data_end[6]) << 48;
    case 6: hash ^= (uint64_t)(data_end[5]) << 40;
    case 5: hash ^= (uint64_t)(data_end[4]) << 32;
    case 4: hash ^= (uint64_t)(data_end[3]) << 24;
    case 3: hash ^= (uint64_t)(data_end[2]) << 16;
    case 2: hash ^= (uint64_t)(data_end[1]) << 8;
    case 1: hash ^= (uint64_t)(data_end[0]);
    case 0: break;
    };
    hash *= HASH_M;
    
    hash ^= hash >> HASH_R;
    hash *= HASH_M;
    hash ^= hash >> HASH_R;

    return hash;
}
