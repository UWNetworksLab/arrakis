/**
 * \file
 * \brief SKB helper functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <skb/skb.h>

/**
 * \brief This will set the affinity in ram_alloc to something
 * "sensible". It runs the ram_findall query, takes a union of the
 * ranges returned and sets that as ram_set_affinity.
 *
 * \bug This should eventually go away when we have a mem_serv that
 * can do more sensible memory allocation.
 */
errval_t skb_set_memory_affinity(void)
{
    errval_t err;

    char *result, *str_err;
    int32_t int_err;
    char input[128];
    snprintf(input, sizeof(input), "ram_findall(%d, Limit), "
             "write(Limit).", disp_get_core_id());
    err = skb_evaluate(input, &result, &str_err, &int_err);
    if (err_is_fail(err)) {
        return err_push(err, SKB_ERR_EVALUATE);
    }
    free(str_err);

    genpaddr_t base = 0xf, limit = 0;
    char *p = result;
    while(true) {
        p = strchr(p, '(');
        if (!p) {
            break;
        }
        p++;

        if (base == 0xf) {
            base = strtol(p, &p, 10);
        } else {
            strtol(p, &p, 10);
        }
        p++;p++;
        limit = strtol(p, &p, 10);
    }
    ram_set_affinity(base, limit);

    free(result);
    return SYS_ERR_OK;
}
