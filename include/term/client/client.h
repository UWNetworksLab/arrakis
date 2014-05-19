/**
 * \file
 * \brief Non-blocking I/O API for terminal client library.
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

#ifndef LIBTERM_CLIENT_CLIENT_H
#define LIBTERM_CLIENT_CLIENT_H

#include <barrelfish/types.h>
#include <barrelfish/waitset.h>
#include <term/client/defs.h>

errval_t term_client_change_read_waitset(struct term_client *client,
                                         struct waitset *read_ws);

errval_t term_client_change_write_waitset(struct term_client *client,
                                          struct waitset *write_ws);

errval_t term_client_change_config_waitset(struct term_client *client,
                                           struct waitset *conf_ws);

errval_t term_client_write(struct term_client *client, const char *data,
                           size_t length, struct event_closure cont);

errval_t term_client_config(struct term_client *client,
                            terminal_config_option_t opt, char *arg,
                            struct event_closure cont);

void term_client_set_err_handler(struct term_client *client,
                                 term_async_err_handler_fn err_cb);

void term_client_set_chars_handler(struct term_client *client,
                                   term_characters_handler_fn *chars_cb);

#endif // LIBTERM_CLIENT_CLIENT_H
