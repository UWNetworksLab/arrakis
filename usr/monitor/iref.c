/**
 * \file
 * \brief IREF allocation/management
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

struct iref_service {
    struct monitor_binding *binding;
    uintptr_t service_id;
};

#define MAX_IREF_PERCORE 256
static struct iref_service iref_table[MAX_IREF_PERCORE];

/**
 * \brief Allocate a new iref
 *
 * Associate it with the server's connection and service id.
 */
errval_t iref_alloc(struct monitor_binding *binding, uintptr_t service_id,
                    iref_t *iref)
{
    assert(binding != NULL);

    // find a free slot in the local table
    for (iref_t i = 0; i < MAX_IREF_PERCORE; i++) {
        if (iref_table[i].binding == NULL) {
            iref_table[i].binding = binding;
            iref_table[i].service_id = service_id;
            // XXX: avoid zero being a valid iref
            *iref = MAX_IREF_PERCORE * my_core_id + i + 1;
            return SYS_ERR_OK;
        }
    }

    return MON_ERR_IREF_ALLOC;
}

/**
 * \brief Return core_id
 *
 * The core_id is stored in the iref itself.
 */
errval_t iref_get_core_id(iref_t iref, coreid_t *core_id)
{
    *core_id = (iref - 1) / MAX_IREF_PERCORE;
    return SYS_ERR_OK;
}

/**
 * \brief Return conn
 */
errval_t iref_get_binding(iref_t iref, struct monitor_binding **binding)
{
    if ((iref - 1) / MAX_IREF_PERCORE != my_core_id) {
        return MON_ERR_INVALID_CORE_ID;
    }

    *binding = iref_table[(iref - 1) % MAX_IREF_PERCORE].binding;
    if (*binding == NULL) {
        return MON_ERR_INVALID_IREF;
    } else {
        return SYS_ERR_OK;
    }
}

/**
 * \brief Return service_id
 */
errval_t iref_get_service_id(iref_t iref, uintptr_t *service_id)
{
    if ((iref - 1) / MAX_IREF_PERCORE != my_core_id) {
        return MON_ERR_INVALID_CORE_ID;
    }

    *service_id = iref_table[(iref - 1) % MAX_IREF_PERCORE].service_id;
    return SYS_ERR_OK;
}
