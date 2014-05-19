/**
 * \file
 * \brief Definitions shared between libterm_client and libterm_server.
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

#ifndef LIBTERM_DEFS_H
#define LIBTERM_DEFS_H

/**
 * Suffix used when a driver registers its terminal session interface at the
 * nameservice.
 *
 * E.g. the driver 'serial0' registers its terminal session interface with
 * the name 'serial0.terminal'.
 */
#define TERM_SESSION_IF_SUFFIX ".terminal"

typedef void term_characters_handler_fn(void *st, char *buffer, size_t length);
typedef void term_async_err_handler_fn(void *st, errval_t err);

#endif // LIBTERM_DEFS_H
