/**
 * \file
 * \brief Filter API for terminal client library.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <assert.h>
#include <stdlib.h>
#include <stdint.h>

#include <barrelfish/barrelfish.h>
#include <collections/list.h>
#include <term/client/client.h>
#include <term/client/filter.h>

#include "filter_priv.h"

/* internal functions */
static int32_t filter_list_id_cmp(void *data, void *arg);
static int32_t filter_list_all_match_cmp(void *data, void *arg);

/**
 * \brief Add an input filter.
 *
 * \param client Terminal client state.
 * \param filter Input filter.
 *
 * \return Filter ID.
 */
term_filter_id_t term_client_add_input_filter(struct term_client *client,
                                              term_filter_fn *filter)
{
    struct term_filter *new_filter = NULL;

    assert(client != NULL);
    assert(client->input_filters != NULL);
    assert(filter != NULL);

    /* create new filter */
    new_filter = malloc(sizeof(struct term_filter));
    assert(new_filter != NULL);
    new_filter->filter = filter;
    new_filter->id = ++client->max_input_filter_id;

    /* add filter to list of input filters */
    collections_list_insert_tail(client->input_filters, new_filter);

    return new_filter->id;
}

/**
 * \brief Remove an input filter.
 *
 * \param client Terminal client state.
 * \param id     Filter ID.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_FILTER_NOT_FOUND if there was no filter with this ID.
 */
errval_t term_client_remove_input_filter(struct term_client *client,
                                         term_filter_id_t id)
{
    errval_t err = SYS_ERR_OK;
    struct term_filter *removed = NULL;

    assert(client != NULL);

    removed = collections_list_remove_if(client->input_filters,
                                         filter_list_id_cmp, &id);

    if (removed == NULL) {
        err = TERM_ERR_FILTER_NOT_FOUND;
    }
    return err;
}

void term_client_remove_all_input_filter(struct term_client *client)
{
    assert(client != NULL);
    assert(client->input_filters != NULL);

    /* remove all input filters */
    collections_list_remove_if_all(client->input_filters,
                                   filter_list_all_match_cmp,
                                   NULL);
}

term_filter_id_t term_client_add_output_filter(struct term_client *client,
                                               term_filter_fn *filter)
{
    struct term_filter *new_filter = NULL;

    assert(client != NULL);
    assert(client->output_filters != NULL);
    assert(filter != NULL);

    /* create new filter */
    new_filter = malloc(sizeof(struct term_filter));
    assert(new_filter != NULL);
    new_filter->filter = filter;
    new_filter->id = ++client->max_output_filter_id;

    /* add filter to list of output filters */
    collections_list_insert_tail(client->output_filters, new_filter);

    return new_filter->id;
}

errval_t term_client_remove_output_filter(struct term_client *client,
                                          term_filter_id_t id)
{
    errval_t err = SYS_ERR_OK;
    struct term_filter *removed = NULL;

    assert(client != NULL);

    removed = collections_list_remove_if(client->output_filters,
                                         filter_list_id_cmp, &id);

    if (removed == NULL) {
        err = TERM_ERR_FILTER_NOT_FOUND;
    }
    return err;
}

void term_remove_all_output_filter(struct term_client *client)
{
    assert(client != NULL);
    assert(client->input_filters != NULL);

    /* remove all output filters */
    collections_list_remove_if_all(client->output_filters,
                                   filter_list_all_match_cmp,
                                   NULL);

}

term_filter_id_t term_client_add_echo_filter(struct term_client *client,
                                             term_filter_fn *filter)
{
    struct term_filter *new_filter = NULL;

    assert(client != NULL);
    assert(client->echo_filters != NULL);
    assert(filter != NULL);

    /* create new filter */
    new_filter = malloc(sizeof(struct term_filter));
    assert(new_filter != NULL);
    new_filter->filter = filter;
    new_filter->id = ++client->max_echo_filter_id;

    /* add filter to list of echo filters */
    collections_list_insert_tail(client->echo_filters, new_filter);

    return new_filter->id;
}

errval_t term_client_remove_echo_filter(struct term_client *client,
                                         term_filter_id_t id)
{
    errval_t err = SYS_ERR_OK;
    struct term_filter *removed = NULL;

    assert(client != NULL);

    removed = collections_list_remove_if(client->echo_filters,
                                         filter_list_id_cmp, &id);

    if (removed == NULL) {
        err = TERM_ERR_FILTER_NOT_FOUND;
    }
    return err;
}

void term_client_remove_all_echo_filter(struct term_client *client)
{
    assert(client != NULL);
    assert(client->echo_filters != NULL);

    /* remove all echo filters */
    collections_list_remove_if_all(client->echo_filters,
                                   filter_list_all_match_cmp,
                                   NULL);
}

void term_filter_apply(collections_listnode *filter_list, char **data,
                       size_t *length)
{
    struct term_filter *filter = NULL;

    collections_list_traverse_start(filter_list);

    while ((filter = collections_list_traverse_next(filter_list)) != NULL) {
        filter->filter(data, length);
    }

    collections_list_traverse_end(filter_list);
}

/**
 * \privatesection
 * Internal functions follow.
 */

static int32_t filter_list_id_cmp(void *data, void *arg)
{
    struct term_filter *filter = data;
    term_filter_id_t id = *(term_filter_id_t *) arg;

    if (filter->id == id) {
        return 1;
    } else {
        return 0;
    }
}

static int32_t filter_list_all_match_cmp(void *data, void *arg)
{
    return 1;
}

void term_filter_free(void *data)
{
    free(data);
}
