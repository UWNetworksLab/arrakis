/**
 * \file
 * \brief Argument processing for distributed services
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich. 
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>

#include <getopt.h>

#include <barrelfish/barrelfish.h>

#include "skb.h"

#include <dist/args.h>

static bool is_excluded(coreid_t core, coreid_t *exclude, int exclude_len)
{
    // assume exclude list isn't sorted
    for (int i = 0; i < exclude_len; i++) {
        if (core == exclude[i]) {
            return true;
        }
    }
    return false;
}

// mallocs *res_cores
static void make_core_list(coreid_t *cores, int cores_len,
                           coreid_t *exclude, int exclude_len,
                           int num_cores,
                           coreid_t **res_cores, int *res_len)
{
    assert(cores != NULL);
    assert(res_cores != NULL);
    assert(res_len != NULL);

    if ((exclude == NULL) || (exclude_len <= 0)) {
        // nothing to exclude, simply copy cores to res_cores
        *res_len = cores_len;
        *res_cores = malloc(*res_len * sizeof(coreid_t));
        if (*res_cores != NULL) {
            if (memcpy(*res_cores, cores, cores_len*sizeof(coreid_t)) == NULL) {
                *res_cores = NULL;
            }
        } else {
            DEBUG_ERR(LIB_ERR_MALLOC_FAIL,"when processing core list argument");
        }
        return;
    }

    // otherwise deal with exclusions

    *res_len = cores_len - exclude_len;
    *res_cores = malloc(*res_len * sizeof(coreid_t));
    if (*res_cores == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL,"when processing core list argument");
        return;
    }

    int r_i = 0;
    for (int i = 0; i < cores_len; i++) {
        if (!is_excluded(cores[i], exclude, exclude_len)) {
            (*res_cores)[r_i] = cores[i];
            r_i++;
        }
        if ((num_cores > 0) && (r_i >= num_cores)) {
            break;
        }
    }

    *res_len = r_i;
}

// mallocs *list
static void parse_list(char *optargs, coreid_t **list, int *len)
{
    assert(optargs != NULL);
    assert(list != NULL);
    assert(len != NULL);

    *list = NULL;

    // a list looks like this: 0,1,2,3,4,5

    // make extra copy bcause strtok clobbers it
    char *p = malloc(strlen(optargs) + 1);
    if (p == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "when parsing core list");
        return;
    }
    strcpy(p, optargs);

    // first get the number of elements
    int i;
    char *tok = p;
    for (i = 0; tok != NULL; i++, p = NULL) {
        tok = strtok(p, ",");
    }
    free(p);
    *len = --i;
    *list = malloc(sizeof(coreid_t)*i);
    if (*list == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "when parsing core list");
        return;
    }

    // now get the actual elements
    p = optargs;
    tok = p;
    for (i = 0; tok != NULL; i++, p = NULL) {
        tok = strtok(p, ",");
        if (tok != NULL) {
            (*list)[i] = strtol(tok, NULL, 10);
        } else {
            break;
        }
    }
} 


#if 0
static void print_list(char *s, coreid_t *list, int len)
{
    debug_printf("%s", s);
    if ((s == NULL) || (len <= 0)) {
        debug_printf("\tList is empty\n");
    } else {
        for (int i = 0; i < len; i++) {
            debug_printf("\t%d: %d\n", i, list[i]);
        }
    }
}
#endif


// mallocs return string
char *list_to_string(coreid_t *list, size_t l_len)
{
    /* Guess that we need up to 4 bytes per entry. */
    int n, size = 4 * l_len;
    char *s, *ns;

    if ((s = malloc(size)) == NULL) {
        DEBUG_ERR(LIB_ERR_MALLOC_FAIL, "when converting core list to string");
        return NULL;
    }

    int out_len = 0;
    for (int i = 0; i < l_len;  i++) {
        n = snprintf(&s[out_len], size - out_len, "%d,", list[i]);
        if (out_len + n < size) {
            out_len += n;
        } else {
            // ran out of space;
            size *= 2;
            ns = realloc(s, size);
            if (ns == NULL) {
                free(s);
                DEBUG_ERR(LIB_ERR_MALLOC_FAIL, 
                          "when converting core list to string");
                return NULL;
            } else {
                s = ns;
                i--; // try again
                continue;
            }
        }
    }
    return s;
}


// mallocs list elements of struct. but not path
struct dist_args process_dist_args(int *argc, char **argv[])
{
    errval_t err;

    debug_printf("process_dist_args: start\n");

    struct dist_args res = (struct dist_args) {
        .path = (*argv)[0],
        .cores = NULL, 
        .cores_len = 0, 
        //        .exclude = NULL,
        //        .exclude_len = 0,
        .num_cores = -1,
        //        .all_cores = false,
        .master = false,
    };

    coreid_t *exclude = NULL;
    int exclude_len = 0;
    bool all_cores = false;
    
    int opt;

    while ((opt = getopt(*argc, *argv, ":wmax:c:n:")) != -1) {
 
        switch (opt) {
        case 'w':
            // be a worker
            res.master = false;
            break;
        case 'm':
            // be a master
            res.master = true;
            break;
        case 'a':
            // use all available cores
            all_cores = true;
            break;
        case 'c':
            // use the cores given in the list argument
            parse_list(optarg, &res.cores, &res.cores_len);
            if (res.cores == NULL) {
                goto fail;
            }
            break;
        case 'x':
            // exclude the cores given in the list argument
            parse_list(optarg, &exclude, &exclude_len);
            if (exclude == NULL) {
                goto fail;
            }
            break;
        case 'n':
            // total number of cores to use
            res.num_cores = strtol(optarg, NULL, 10);
            break;
        default:
            // ignore
            break;
        }
    }


    debug_printf("pda: after getopt loop\n");

    if (all_cores) {
        err = get_cores_skb(&res.cores, &res.cores_len);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in get_cores_skb");
            goto fail;
        }
        /*
        print_list("res.cores with all cores: \n", res.cores, res.cores_len);
        */
    }

    debug_printf("pda: after all cores\n");

    // by this point we must have a list of cores, either because one was 
    // provided explicitly or because all_cores were requested.
    if ((res.cores == NULL) || (res.cores_len <= 0)) {
        goto fail;
    }

    if (res.cores != NULL) {
        coreid_t *cores;
        int cores_len;

        make_core_list(res.cores, res.cores_len, exclude, exclude_len, 
                       res.num_cores, &cores, &cores_len); 

        debug_printf("pda: after make_core_list\n");

        free(res.cores);
        if (exclude != NULL) {
            free(exclude);
        }

        if (cores == NULL) {
            goto fail;
        }

        res.cores = cores;
        res.cores_len = cores_len;
    }

    *argc = *argc - optind;
    *argv = &((*argv)[optind]);

    debug_printf("pda: after modifying argv\n");    

    return res;

 fail:
    if (exclude != NULL) {
        free(exclude);
    }
    printf("Usage: %s [-mw] [-a] [-c list] [-x list] [-n num_cores]\n", 
           (*argv)[0]);
    exit(EXIT_FAILURE);
    return res;
}

