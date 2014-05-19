/** \file
 *  \brief Simple name-server based barriers
 */

/*
 * Copyright (c) 2010, ETH Zurich.
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

#include <dist/barrier.h>

#define EXTRA_NAME_LEN 6 // enough extra space for an int or "done"
#define READY_SUFFIX "ready"
#define READY_LEN 5  // strlen(READY_SUFFIX)


errval_t nsb_wait_n(int n, char *name)
{
    errval_t err;
    int name_len = strlen(name) + EXTRA_NAME_LEN + READY_LEN;  
    char serv_name[name_len];
    snprintf(serv_name, name_len, "%s.%d.%s", name, n, READY_SUFFIX);
    err = nameservice_blocking_lookup(serv_name, NULL);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_LOOKUP);
    }
    return SYS_ERR_OK;
}

errval_t nsb_wait_s(char *s1, char *s2)
{
    errval_t err;
    int name_len = strlen(s1) + strlen(s2) + 2;
    char serv_name[name_len];

    snprintf(serv_name, name_len, "%s.%s", s1, s2);
    err = nameservice_blocking_lookup(serv_name, NULL);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_LOOKUP);
    }
    return SYS_ERR_OK;
}

errval_t nsb_wait_ready(char *name)
{
    return nsb_wait_s(name, READY_SUFFIX);
}

errval_t nsb_wait(char *name)
{
    errval_t err;
    err = nameservice_blocking_lookup(name, NULL);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_LOOKUP);
    }
    return SYS_ERR_OK;
}

errval_t nsb_register(char *name)
{
    errval_t err;
    err = nameservice_register(name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }
    return SYS_ERR_OK;
}

errval_t nsb_register_n(int n, char *name)
{
    errval_t err;
    int name_len = strlen(name) + EXTRA_NAME_LEN + READY_LEN;
    char serv_name[name_len];
    snprintf(serv_name, name_len, "%s.%d.%s", name, n, READY_SUFFIX);
    err = nameservice_register(serv_name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }
    return SYS_ERR_OK;
}

errval_t nsb_register_s(char *s1, char *s2)
{
    errval_t err;
    int name_len = strlen(s1) + strlen(s2) + 2;
    char serv_name[name_len];
    snprintf(serv_name, name_len, "%s.%s", s1, s2);
    err = nameservice_register(serv_name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }
    return SYS_ERR_OK;
}

errval_t nsb_register_ready(char *name)
{
    return nsb_register_s(name, READY_SUFFIX);
}

errval_t nsb_master(int min, int max, char *name)
{
    errval_t err;
    int name_len = strlen(name) + EXTRA_NAME_LEN + READY_LEN;  
    char serv_name[name_len];
    for (int i = min; i <= max; i++) {
        snprintf(serv_name, name_len, "%s.%d.%s", name, i, READY_SUFFIX);
        err = nameservice_blocking_lookup(serv_name, NULL);
        if (err_is_fail(err)) {
            return err_push(err, DIST_ERR_NS_LOOKUP);        }
    }

    snprintf(serv_name, name_len, "%s.%s", name, READY_SUFFIX);
    err = nameservice_register(serv_name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }

    return SYS_ERR_OK;
}

errval_t nsb_master_l(coreid_t *cores, int n_cores, char *name)
{
    assert(cores != NULL);
    assert(n_cores > 0);

    errval_t err;
    int name_len = strlen(name) + EXTRA_NAME_LEN + READY_LEN;  
    char serv_name[name_len];
    for (int i = 0; i < n_cores; i++) {
        snprintf(serv_name, name_len, "%s.%d.%s", name, cores[i], READY_SUFFIX);
        err = nameservice_blocking_lookup(serv_name, NULL);
        if (err_is_fail(err)) {
            return err_push(err, DIST_ERR_NS_LOOKUP);
        }
    }

    snprintf(serv_name, name_len, "%s.%s", name, READY_SUFFIX);
    err = nameservice_register(serv_name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }

    return SYS_ERR_OK;
}

errval_t nsb_worker(int num, char *name)
{
    errval_t err;
    int name_len = strlen(name) + EXTRA_NAME_LEN + READY_LEN;
    char serv_name[name_len];
    snprintf(serv_name, name_len, "%s.%d.%s", name, num, READY_SUFFIX);
    err = nameservice_register(serv_name, 0);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_REG);
    }

    snprintf(serv_name, name_len, "%s.%s", name, READY_SUFFIX);
    err = nameservice_blocking_lookup(serv_name, NULL);
    if (err_is_fail(err)) {
        return err_push(err, DIST_ERR_NS_LOOKUP);
    }

    return SYS_ERR_OK;
}


