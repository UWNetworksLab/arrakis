/**
 * \file
 * \brief RCCE library
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERNAL_H
#define INTERNAL_H

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/bulk_transfer.h>
#include <if/rcce_defs.h>
#include <if/monitor_defs.h>

//#define DEBUG_ENABLED

#ifdef DEBUG_ENABLED
#       define dprintf  printf
#else
#       define dprintf(...)
#endif

#ifdef BULK_TRANSFER_ENABLED
/* #       define BULK_SIZE       (1 << 19) */
/* #       define BLOCK_SIZE      (1 << 18) */
#       define BULK_SIZE       (1 << 13)
#       define BLOCK_SIZE      (1 << 13)
#else
#       define BULK_SIZE       (1 << 12)
#       define BLOCK_SIZE      (1 << 12)
#endif

#define BULK_PAGE_MAP VREGION_FLAGS_READ_WRITE_MPB
//#define BULK_PAGE_MAP VREGION_FLAGS_READ_WRITE_NOCACHE

struct msg_buf {
    char *msg;
    size_t length, current;
    bool pending;
    uintptr_t id, bulk;
#ifdef BULK_TRANSFER_ENABLED
    bool bulk_ready;
#endif
};

struct rcce_state {
    coreid_t index;
    bool request_done, waitmsg, recv_ready, bulk_waitmsg;
    struct bulk_transfer bt;
    struct bulk_transfer_slave btr;
};

extern struct msg_buf msgbuf[MAX_CPUS];
extern struct rcce_binding *barray[MAX_CPUS];
extern coreid_t my_core_id, num_cores, bsp_id;

void barrier_wait(void);
errval_t send_message(char *msg, size_t size, coreid_t dest);
void barriers_init(coreid_t max_cpus);
void barrier_binding_init(struct rcce_binding * binding);
void setup_routes(int argc, char **argv);

#endif
