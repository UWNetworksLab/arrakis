/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <barrelfish/barrelfish.h>
#include <vfs/vfs.h>

#include "ps.h"

static struct ps_entry *entries[MAX_DOMAINS];

errval_t ps_allocate(struct ps_entry *entry, domainid_t *domainid)
{
    for(domainid_t i = 1; i < MAX_DOMAINS; i++) {
        if(entries[i] == NULL) {
            entries[i] = entry;
            *domainid = i;
            return SYS_ERR_OK;
        }
    }

    return SPAWN_ERR_DOMAIN_ALLOCATE;
}

void ps_remove(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    entries[domain_id] = NULL;
}

bool ps_exists(domainid_t domain_id)
{
    assert(domain_id < MAX_DOMAINS);
    return entries[domain_id] != NULL ? true : false;
}

struct ps_entry *ps_get(domainid_t domain_id)
{
    if(domain_id >= MAX_DOMAINS) {
        return NULL;
    }

    return entries[domain_id];
}
