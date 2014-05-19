/**
 * \file
 * \brief Ask the monitor for all core IDs which are running
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
#include <if/monitor_defs.h>
#include <skb/skb.h>
#include "datagatherer.h"

#define MAXAPICIDS 256

bool nr_cores_done;
int nr_of_running_cores = 0;

static void nr_running_cores(struct monitor_binding *st,
                             coreid_t nr_cores)
{
    skb_add_fact("nr_running_cores(%u).", nr_cores);
    nr_of_running_cores = nr_cores;
    nr_cores_done = true;
}


void gather_nr_running_cores(struct monitor_binding *st)
{
    /* Set handlers */
    st->rx_vtbl.num_cores_reply = nr_running_cores;
    st->tx_vtbl.num_cores_request(st, NOP_CONT);
}
