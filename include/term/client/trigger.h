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

#ifndef LIBTERM_CLIENT_TRIGGER_H
#define LIBTERM_CLIENT_TRIGGER_H

#include <barrelfish/waitset.h>
#include <errors/errno.h>
#include <term/client/client.h>
#include <term/client/defs.h>

/* ASCII C0 control characters */
#define CTRL(x) ((x) - 'A' + 1)

struct term_trigger {
    struct event_closure closure;
    char trigger_character;
};

term_trigger_id_t term_client_add_trigger(struct term_client *client,
                                          struct term_trigger trigger);

errval_t term_client_remove_trigger(struct term_client *client,
                                    term_trigger_id_t id);

void term_client_remove_all_triggers(struct term_client *client);

#endif // LIBTERM_CLIENT_TRIGGER_H
