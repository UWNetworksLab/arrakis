/**
 * \file
 * \brief Trigger API for terminal client library.
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
#include <stdint.h>

#include <barrelfish/barrelfish.h>
#include <term/client/client.h>
#include <term/client/trigger.h>

#include "trigger_priv.h"

/* internal functions */
static int32_t trigger_list_id_cmp(void *data, void *arg);
static int32_t trigger_list_user_cmp(void *data, void *arg);

/**
 * \brief Add a character trigger.
 *
 * \param client  Terminal client state.
 * \param trigger Trigger to add.
 *
 * \return Trigger ID.
 */
term_trigger_id_t term_client_add_trigger(struct term_client *client,
                                          struct term_trigger trigger)
{
    return term_client_add_trigger_type(client, trigger,
                                        TERM_TRIGGER_TYPE_USER);
}

errval_t term_client_remove_trigger(struct term_client *client,
                                    term_trigger_id_t id)
{
    errval_t err = SYS_ERR_OK;
    struct term_trigger_priv *removed = NULL;

    assert(client != NULL);
    assert(client->triggers != NULL);

    removed = collections_list_remove_if(client->triggers, trigger_list_id_cmp,
                                         &id);

    if (removed == NULL) {
        err = TERM_ERR_TRIGGER_NOT_FOUND;
    }
    return err;
}

void term_client_remove_all_triggers(struct term_client *client)
{
    assert(client != NULL);
    assert(client->triggers != NULL);

    /* remove all user triggers, i.e. all but built-in triggers */
    collections_list_remove_if_all(client->triggers, trigger_list_user_cmp,
                                   NULL);
}

/**
 * \privatesection
 * Internal functions follow.
 */
term_trigger_id_t term_client_add_trigger_type(struct term_client *client,
                                               struct term_trigger trigger,
                                               enum term_trigger_type type)
{
    struct term_trigger_priv *trigger_priv = NULL;

    assert(client != NULL);
    assert(client->triggers != NULL);

    /* create new trigger */
    trigger_priv = malloc(sizeof(struct term_trigger_priv));
    assert(trigger_priv != NULL);
    trigger_priv->trigger = trigger;
    trigger_priv->id = ++client->max_trigger_id;
    trigger_priv->type = type;

    /* add trigger to list of triggers */
    collections_list_insert_tail(client->triggers, trigger_priv);

    return trigger_priv->id;
}

static int32_t trigger_list_id_cmp(void *data, void *arg)
{
    struct term_trigger_priv *trigger_priv = data;
    term_trigger_id_t id = *(term_trigger_id_t *) arg;

    if ((trigger_priv->type == TERM_TRIGGER_TYPE_USER) &&
        (trigger_priv->id == id)) {
        return 1;
    } else {
        return 0;
    }
}

static int32_t trigger_list_user_cmp(void *data, void *arg)
{
    struct term_trigger_priv *trigger_priv = data;

    if (trigger_priv->type == TERM_TRIGGER_TYPE_USER) {
        return 1;
    } else {
        return 0;
    }
}

void term_trigger_free(void *data)
{
    free(data);
}
