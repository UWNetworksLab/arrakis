/**
 * \file
 * \brief Private trigger header.
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

#ifndef LIBTERM_CLIENT_TRIGGER_PRIV_H
#define LIBTERM_CLIENT_TRIGGER_PRIV_H

#include <term/client/trigger.h>

enum term_trigger_type {
    /**
     * Built-in trigger that can not be removed by user.
     */
    TERM_TRIGGER_TYPE_BUILT_IN,

    /**
     * User trigger.
     */
    TERM_TRIGGER_TYPE_USER
};

struct term_trigger_priv {
    struct term_trigger trigger;
    term_trigger_id_t id;
    enum term_trigger_type type;
};

term_trigger_id_t term_client_add_trigger_type(struct term_client *client,
                                               struct term_trigger trigger,
                                               enum term_trigger_type type);

void term_trigger_free(void *data);

#endif // LIBTERM_CLIENT_TRIGGER_PRIV_H
