/**
 * \file
 * \brief Blocking I/O API for terminal client library.
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

#ifndef LIBTERM_CLIENT_CLIENT_BLOCKING_H
#define LIBTERM_CLIENT_CLIENT_BLOCKING_H

#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <term/client/defs.h>

/**
 * Character that is used by libterm_client to determine end of line.
 */
#define TERM_CLIENT_EOL_CHAR '\n'

errval_t term_client_blocking_init(struct term_client *client,
                                   struct capref sessionid);
void term_client_blocking_exit(struct term_client *client);

errval_t term_client_blocking_read(struct term_client *client, char *data,
                                   size_t length, size_t *read);
errval_t term_client_blocking_write(struct term_client *client,
                                    const char *data, size_t length,
                                    size_t *written);
errval_t term_client_blocking_config(struct term_client *client,
                                     terminal_config_option_t opt, char *arg);

#endif // LIBTERM_CLIENT_CLIENT_BLOCKING_H
