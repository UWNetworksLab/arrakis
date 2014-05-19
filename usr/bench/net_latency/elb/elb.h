/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ELB_H_
#define ELB_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <net_interfaces/net_interfaces.h>
#include "elb_debug.h"

//extern void *buffer_base;
//extern size_t buffer_size;

char *get_cardname(void);
uint64_t get_cmdline_queueid(void);

static inline void *buffer_address(size_t idx) {
    return (void*) ((uintptr_t) buffer_base + idx * buffer_size);
}

void terminate_benchmark(void);


#endif // ndef ELB_H_


