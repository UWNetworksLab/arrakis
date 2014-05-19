/** \file
 *  \brief service helper functions
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <dist/service.h>

#define EXTRA_NAME_LEN 6 // enough extra space for an int or "done"

errval_t register_service_local(coreid_t core, char *name, iref_t iref)
{
    assert(name != NULL);
    
    errval_t err;
    int new_len = strlen(name) + EXTRA_NAME_LEN;
    char new_name[new_len];
    snprintf(new_name, new_len, "%s.%d", name, core);

    err = nameservice_register(new_name, iref);

    // TODO: comparing to core == 0 is kind of ugly.  We should somehow 
    // identify the 'master' core independent of its core id
    if (err_is_ok(err) && core == 0) {
        err = nameservice_register(name, iref);
    }
    
    return err;
}


errval_t lookup_service(coreid_t core, char *name, iref_t *iref)
{
    assert(name != NULL);

    errval_t err;

    int new_len = strlen(name) + EXTRA_NAME_LEN;
    char new_name[new_len];
    snprintf(new_name, new_len, "%s.%d", name, core);

    err = nameservice_lookup(new_name, iref);
    if (err_is_ok(err)) {
        return SYS_ERR_OK;
    } 

    err = nameservice_lookup(name, iref);

    return err;
}
