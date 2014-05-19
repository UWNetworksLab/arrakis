/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PS_H
#define PS_H

#include <stdbool.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/init.h>

#define MAX_DOMAINS     256

enum ps_status {
    PS_STATUS_RUNNING,
    PS_STATUS_ZOMBIE
};

struct ps_waiter {
    struct ps_waiter *next;
    struct spawn_binding *binding;
};

struct ps_entry {
    char *argv[MAX_CMDLINE_ARGS];
    char *argbuf;
    size_t argbytes;
    struct capref rootcn_cap, dcb;
    struct cnoderef rootcn;
    uint8_t exitcode;
    enum ps_status status;
    struct ps_waiter *waiters;
};

errval_t ps_allocate(struct ps_entry *entry, domainid_t *domainid);
void ps_remove(domainid_t domain_id);
bool ps_exists(domainid_t domain_id);
struct ps_entry *ps_get(domainid_t domain_id);

#endif
