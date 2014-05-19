/**
 * \file
 * \brief General definitions for terminal client library.
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

#ifndef LIBTERM_CLIENT_DEFS_H
#define LIBTERM_CLIENT_DEFS_H

#include <barrelfish/waitset.h>
#include <collections/list.h>
#include <errors/errno.h>
#include <if/terminal_defs.h>
#include <if/terminal_config_defs.h>
#include <term/defs.h>

#include <stdbool.h>
#include <stdint.h>

typedef uint32_t term_filter_id_t;
typedef void term_filter_fn(char **data, size_t *length);

typedef uint32_t term_trigger_id_t;

/**
 * Terminal client state.
 */
struct term_client {
    /**
     * Arbitraty user state pointer. Passed to callback functions.
     */
    void *st;

    /**
     * \note read-only
     * Waitset used for incoming characters.
     */
    struct waitset *read_ws;

    /**
     * \note read-only
     * Waitset used for outgoing characters.
     */
    struct waitset *write_ws;

    /**
     * \note read-only
     * Waitset used for configuration messages.
     */
    struct waitset *conf_ws;

    /**
     * Are received characters echoed? Default: true.
     */
    bool echo;

    /**
     * Does a term_client_read return at most one line? Default: true.
     */
    bool line_mode;

    /**
     * Whether or not read is non-blocking.
     */
    bool non_blocking_read;

    /**
     * \internal
     * Callback when characters arrive and we are in non_blocking_read.
     */
    term_characters_handler_fn *chars_cb;

    /**
     * \internal
     * Callback when a asynchronous error happens.
     */
    term_async_err_handler_fn *err_cb;

    /**
     * \internal
     * Is the connection to the terminal server established.
     */
    bool connected;

    /**
     * \internal
     * Binding used for incoming characters.
     */
    struct terminal_binding *in_binding;

    /**
     * \internal
     * Binding used for outgoing characters.
     */
    struct terminal_binding *out_binding;

    /**
     * \internal
     * Binding used for configuration messages.
     */
    struct terminal_config_binding *conf_binding;

    /**
     * \internal
     * If we receive more characters in a single flounder message than the
     * user requested via term_client_blocking_read, the characters are buffered
     * in this buffer. Subsequent reads are first served from this buffer.
     */
    char *readbuf;

    /**
     * \internal
     * If the readbuf is not NULL, contains the number of characters in the
     * readbuf.
     */
    size_t readbuf_len;

    /**
     * \internal
     * If the readbuf is not NULL, points the next character not yet read by
     * the user domain via term_client_blocking_read.
     */
    char *readbuf_pos;

    /**
     * \internal
     * Linked-list of input filters that are applied to the raw characters
     * received via flounder.
     */
    collections_listnode *input_filters;

    /**
     * \internal
     * Linked-list of output filters that are applied to the characters
     * supplied by the user via term_client_write() or
     * term_client_write_non_block() before they are sent via flounder.
     */
    collections_listnode *output_filters;

    /**
     * \internal
     * Linked-list of echo filters that are applied to the raw characters
     * recevied via flounder before they are sent out via flounder again.
     * Note that neither the input nor the output filters are applied to the
     * characters echoed.
     */
    collections_listnode *echo_filters;

    /**
     * \internal
     * Maximum input filter id ever assigned.
     */
    term_filter_id_t max_input_filter_id;

    /**
     * \internal
     * Maximum output filter id ever assigned.
     */
    term_filter_id_t max_output_filter_id;

    /**
     * \internal
     * Maximum echo filter id ever assigned.
     */
    term_filter_id_t max_echo_filter_id;

    /**
     * \internal
     * Linked-list of character triggers. The triggers are processed on the
     * raw characters received via the flounder interface.
     */
    collections_listnode *triggers;

    /**
     * \internal
     * Maximum trigger id ever assigned.
     */
    term_trigger_id_t max_trigger_id;
};

#endif // LIBTERM_CLIENT_DEFS_H
