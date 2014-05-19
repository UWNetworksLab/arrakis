/**
 * \file
 * \brief Functions for capability storage. This was
 * moved here from barrelfish/nameservice_client.c
 *
 */
/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

#include <barrelfish/barrelfish.h>
#include <octopus/init.h>
#include <octopus/capability_storage.h>

/**
 * \brief Get a capability from the capability store.
 *
 * \param key           String that identifies the capability
 * \param retcap        Pointer to structure holding capability
 */
errval_t oct_get_capability(const char *key, struct capref *retcap)
{
    errval_t reterr;
    struct octopus_thc_client_binding_t *cl = oct_get_thc_client();

    errval_t err = cl->call_seq.get_cap(cl, key, retcap, &reterr);
    if(err_is_fail(err)) {
        return err;
    }

    return reterr;
}

/**
 * \brief Put a capability to the capability store.
 *
 * \param key           String that identifies the capability
 * \param cap           The capability to store
 */
errval_t oct_put_capability(const char *key, struct capref cap)
{
    errval_t reterr;
    struct octopus_thc_client_binding_t *cl = oct_get_thc_client();

    errval_t err = cl->call_seq.put_cap(cl, key, cap, &reterr);
    if(err_is_fail(err)) {
        return err;
    }

    return reterr;
}

/**
 * \brief Remove a capability from the capability store.
 *
 * \param key           String that identifies the capability
 */
errval_t oct_remove_capability(const char *key)
{
    errval_t reterr;
    struct octopus_thc_client_binding_t *cl = oct_get_thc_client();

    errval_t err = cl->call_seq.remove_cap(cl, key, &reterr);
    if(err_is_fail(err)) {
        return err;
    }

    return reterr;
}
