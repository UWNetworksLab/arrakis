/**
 * \file
 * \brief Set of default triggers for the terminal client library.
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

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <term/client/default_triggers.h>

static void term_trigger_kill_handler(void *arg)
{
    fprintf(stderr, "User hit Ctrl+\\.\n");
    exit(EXIT_FAILURE);
}

static void term_trigger_int_handler(void *arg)
{
    fprintf(stderr, "User hit Ctrl+C.\n");
    exit(EXIT_FAILURE);
}

struct term_trigger term_trigger_kill = {
    .closure = {
        .handler = term_trigger_kill_handler,
        .arg = NULL,
    },
    .trigger_character = CTRL('\\'),
};

struct term_trigger term_trigger_int = {
    .closure = {
        .handler = term_trigger_int_handler,
        .arg = NULL,
    },
    .trigger_character = CTRL('C'),
};
