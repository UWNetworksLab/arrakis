/**
 * \file
 * \brief Calling and processing results from SKB
 */

/*
 * Copyright (c) 2007-2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>

#include <string.h>

#include <barrelfish/barrelfish.h>

#include <skb/skb.h>

#include "skb.h"

static errval_t parse_core_id_list(char *s, coreid_t *cores, int n)
{
    assert(cores != NULL);

    // a list looks like this: [0, 1, 2, 3, 4, 5]

    int i;
    char *p = s;
    char *tok = p;
    for (i = 0; (i < n) && (tok != NULL); i++, p = NULL) {
        tok = strtok(p, "[,]");
        if (tok != NULL) {
            cores[i] = strtol(tok, NULL, 10);
        } else {
            break;
        }
    }
    
    if (i != n) {
        return MS_ERR_SKB;
    }

    return SYS_ERR_OK;
}

errval_t get_cores_skb(coreid_t **cores, int *n_cores)
{
    assert(cores != NULL);
    assert(n_cores != NULL);

    errval_t err;

    char *result, *str_err;
    int32_t int_err;

    err = skb_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_client_connect failed");
        return err;
    }
    
    err = skb_evaluate("available_nr_cores(Nr), write(Nr)."
                       , &result, &str_err, &int_err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_evaluate failed");
        return err_push(err, SKB_ERR_EVALUATE);
    }

    // parse #cores
    *n_cores = strtol(result, NULL, 10);

    // request all coreids
    err = skb_evaluate("get_core_id_list(L), write(L)."
                       , &result, &str_err, &int_err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_evaluate failed");
        return err_push(err, SKB_ERR_EVALUATE);
    }

    // allocate result array
    // parse result into result array

    *cores = (coreid_t*)malloc(sizeof(coreid_t) * *n_cores);
    if (*cores == NULL) {
        return err_push(err, LIB_ERR_MALLOC_FAIL);
    }

    err = parse_core_id_list(result, *cores, *n_cores);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "parse_list failed");
        free(*cores);
        return err;
    }

    return SYS_ERR_OK;
}

