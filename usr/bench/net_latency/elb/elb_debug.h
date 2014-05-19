/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ELB1_DEBUG_H_
#define ELB1_DEBUG_H_

// defined in usr/bench/net_latency/elb/benchmark.c
//extern char *app_type;
extern bool is_server;

// *****************************************************************
// * Debug printer:
// *****************************************************************
//#define ELB1_SERVICE_DEBUG 1
#if defined(ELB1_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define ELB1_DEBUG(x...) do{                                    \
            if (is_server){                                     \
                printf(" ELB1[Server]: " x);                    \
            } else {                                            \
                printf(" ELB1[Client]: " x);                    \
            }                                                   \
        }while(0);
#else
#define ELB1_DEBUG(x...) ((void)0)
#endif

#endif // ELB1_DEBUG_H_

