/**
 * \file
 * \brief Management of inter-monitor bindings
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"

// Array of monitor bindings indexed by core ID
static struct intermon_state *bindings[MAX_COREID + 1];

// This is called when a new binding comes up
errval_t intermon_binding_set(struct intermon_state *st)
{
    assert(st != NULL);
    if (st->core_id > MAX_COREID) {
        return MON_ERR_INVALID_CORE_ID;
    }
    assert(bindings[st->core_id] == NULL);
    bindings[st->core_id] = st;

    return SYS_ERR_OK;
}

errval_t intermon_binding_get(coreid_t coreid, struct intermon_binding **ret)
{
    assert(ret != NULL);

    if (coreid > MAX_COREID) {
        *ret = NULL;
        return MON_ERR_INVALID_CORE_ID;
    }

    if (bindings[coreid] == NULL) {
        *ret = NULL;
        return MON_ERR_NO_MONITOR_FOR_CORE;
    }

    *ret = bindings[coreid]->binding;
    return SYS_ERR_OK;
}
