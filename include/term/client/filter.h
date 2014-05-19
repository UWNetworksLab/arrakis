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

#ifndef LIBTERM_CLIENT_FILTER_H
#define LIBTERM_CLIENT_FILTER_H

#include <errors/errno.h>
#include <term/client/client.h>
#include <term/client/defs.h>

/* input filter */
term_filter_id_t term_client_add_input_filter(struct term_client *client,
                                              term_filter_fn *filter);

errval_t term_client_remove_input_filter(struct term_client *client,
                                         term_filter_id_t id);

void term_client_remove_all_input_filter(struct term_client *client);

/* output filter */
term_filter_id_t term_client_add_output_filter(struct term_client *client,
                                               term_filter_fn *filter);

errval_t term_client_remove_output_filter(struct term_client *client,
                                          term_filter_id_t id);

void term_remove_all_output_filter(struct term_client *client);

/* echo filter */
term_filter_id_t term_client_add_echo_filter(struct term_client *client,
                                             term_filter_fn *filter);

errval_t term_client_remove_echo_filter(struct term_client *client,
                                        term_filter_id_t id);

void term_client_remove_all_echo_filter(struct term_client *client);

#endif // LIBTERM_CLIENT_FILTER_H
