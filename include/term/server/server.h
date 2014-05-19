/**
 * \file
 * \brief Terminal server library (libterm_server) API.
 */

/*
 * Copyright (c) 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef LIBTERM_SERVER_SERVER_H
#define LIBTERM_SERVER_SERVER_H

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <collections/flipbuffer.h>
#include <if/terminal_defs.h>
#include <if/terminal_config_defs.h>
#include <if/terminal_session_defs.h>
#include <term/defs.h>

#include <stdbool.h>

/*
 * Continuation (callback) function types
 */
typedef void term_server_configuration_handler_fn(void *st,
                                                  terminal_config_option_t opt,
                                                  char *arguments);
typedef void term_server_new_session_fn(void *st, struct capref session_id);

struct term_server {
    /**
     * Arbitrary user state pointer. Passed to callback functions.
     */
    void *st;

    /**
     * \note read-only
     * Waitset used for the session interface. (terminal_session.if)
     */
    struct waitset *session_ws;

    /**
     * \note read-only
     * Waitset used for incoming characters. (terminal.if)
     */
    struct waitset *in_ws;

    /**
     * \note read-only
     * Waitset used for outgoing characters. (terminal.if)
     */
    struct waitset *out_ws;

    /**
     * \note read-only
     * Waitset used for configuration messages. (terminal_config.if)
     */
    struct waitset *conf_ws;

    /**
     * \internal
     * Callback when characters arrive.
     */
    term_characters_handler_fn *chars_cb;

    /**
     * \internal
     * Callback when a configuration message arrives.
     */
    term_server_configuration_handler_fn *conf_cb;

    /**
     * \internal
     * Callback when a new session is established.
     */
    term_server_new_session_fn *new_session_cb;

    /**
     * \internal
     * Callback when an asynchronous error happens.
     */
    term_async_err_handler_fn *err_cb;

    /**
     * \internal
     * Interface reference for session interface.
     */
    iref_t session_iref;

    /**
     * \internal
     * Interface reference for incoming characters.
     */
    iref_t in_iref;

    /**
     * \internal
     * Interface reference for outgoing characters.
     */
    iref_t out_iref;

    /**
     * \internal
     * Interface reference for configuration messages.
     */
    iref_t conf_iref;

    /**
     * \internal
     * Stack of terminal bindings (if/terminal.if) for outgoing characters.
     * Characters for the client (output for the server, input for the client)
     * are sent to the top element of the stack. If a new client connects its
     * binding is pushed to the stack. On disconnect the binding is poped from
     * the stack.
     */
    struct collections_stack *out_binding_stack;

    /**
     * \internal
     * Stack of terminal bindings (if/terminal.if) for incoming characters.
     */
    struct collections_stack *in_binding_stack;

    /**
     * \internal
     * Stack of configuration bindings (if/terminal_config.if) for configuration
     * messages.
     */
    struct collections_stack *conf_binding_stack;

    /**
     * \internal
     * Whether or not first client connected.
     */
    bool first_client_connected;

    /**
     * \internal
     * Session interface (if/terminal_session.if) binding.
     */
    struct terminal_session_binding *session_binding;

    /**
     * \internal
     * Session this terminal server is associated with.
     */
    struct capref session_id;

    /**
     * \internal
     * Flip-buffer used for sending.
     */
    struct collections_fbuf *fbuf;

    /**
     * \internal
     * Whether or not we're currently sending.
     */
    bool sending;
};

errval_t term_server_change_in_ws(struct term_server *server,
                                  struct waitset *ws);

errval_t term_server_change_out_ws(struct term_server *server,
                                   struct waitset *ws);

errval_t term_server_change_conf_ws(struct term_server *server,
                                    struct waitset *ws);

errval_t term_server_change_session_ws(struct term_server *server,
                                       struct waitset *ws);

errval_t term_server_init(struct term_server *server, iref_t *session_iref,
                          struct waitset *session_ws, struct waitset *in_ws,
                          struct waitset *out_ws, struct waitset *conf_ws,
                          term_characters_handler_fn chars_cb,
                          term_server_configuration_handler_fn conf_cb,
                          term_server_new_session_fn session_cb);

void term_server_send(struct term_server *server, const char *data,
                      size_t length);

void term_server_set_chars_handler(struct term_server *server,
                                   term_characters_handler_fn chars_cb);

void term_server_set_conf_handler(struct term_server *server,
                                  term_server_configuration_handler_fn *conf_cb);

void term_server_set_err_handler(struct term_server *server,
                                 term_async_err_handler_fn err_cb);

void term_server_set_new_session_handler(struct term_server *server,
                                         term_server_new_session_fn session_cb);

/**
 * \brief Whether or not terminal server is currently sending.
 *
 * \param Terminal server state.
 */
static inline bool term_server_sending(struct term_server *server)
{
    return server->sending;
}

#endif // LIBTERM_SERVER_SERVER_H
