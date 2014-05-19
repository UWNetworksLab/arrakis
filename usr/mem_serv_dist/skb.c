/**
 * \file
 * \brief Calling and processing results from SKB
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
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

#if 0
static void get_range(char *result, genpaddr_t *base, genpaddr_t *limit)
{
    //    debug_printf("got affinity range results %s\n", result);
    
    // example expected format: 
    // [range(0, 655360), range(1048576, 3355443200), 
    //  range(4294967296, 5234491392)]
    // this functions creates a range using the first (lowest - e.g. 0) value 
    // and the last (highest - e.g. 5234491392) values.
    // FIXME: yes it is a fragile hack.

    *base = 0xf;
    *limit = 0;
    char *p = result;
    while(true) {
        p = strchr(p, '(');
        if (!p) {
            break;
        }
        p++;

        if (*base == 0xf) {
            *base = strtol(p, &p, 10);
        } else {
            strtol(p, &p, 10);
        }
        p++;p++;
        *limit = strtol(p, &p, 10);
    }
    // debug_printf("got range %"PRIuGENPADDR", %"PRIuGENPADDR"\n", 
    //*base, *limit);
}
#endif

// NOTE: the skb query currently returns the wrong output on qemu. However, in 
// that case this function returns base 0, limit 0, which is a good result.
errval_t get_percore_affinity(coreid_t core, genpaddr_t *base, genpaddr_t *limit)
{
    assert(base != NULL);
    assert(limit != NULL);

    errval_t err;
    
    *base = -1;
    *limit = 0;

    /* get the local memory affinity from SKB */

    err = skb_client_connect();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_client_connect failed");
        return err;
    }

    struct list_parser_status status;
    int myerr = skb_execute_query("local_memory_affinity(%d, Limit), "
             "write(Limit).", core);
    if (myerr != 0) {
        return SKB_ERR_EXECUTION;
    }

    //    debug_printf("output: %s\n", res);
    //    debug_printf("error_output: %s\n", skb_get_error_output());
    
    genpaddr_t b, l;
    skb_read_list_init(&status);
    while (skb_read_list(&status, "range(%"PRIuGENPADDR", %"PRIuGENPADDR")", 
                         &b, &l)) {
        // debug_printf("b: %lu l: %lu\n", b, l);
        if (*base == -1 || b < *base) {
            *base = b;
        } 
        if (l > *limit) {
            *limit = l;
        }
    }
    if (*base == -1) {
        *base = 0;
    }
    
    //    debug_printf("base: %lu limit %lu\n", *base, *limit);

#if 0
    // Adrian says not to use skb_evaluate anymore
    char *result, *str_err;
    int int_err;
    char input[128];
    snprintf(input, sizeof(input), "local_memory_affinity(%d, Limit), "
             "write(Limit).", core);
    err = skb_evaluate(input, &result, &str_err, &int_err);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "skb_evaluate failed");
        return err_push(err, SKB_ERR_EVALUATE);
    }

    get_range(result, base, limit);

    free(result);
    free(str_err);
#endif

    return SYS_ERR_OK;
}



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

