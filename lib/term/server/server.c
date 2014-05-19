/**
 * \file
 * \brief Terminal server library (libterm_server) implementation.
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
#include <stdbool.h>

#include <barrelfish/barrelfish.h>
#include <collections/stack.h>
#include <collections/flipbuffer.h>
#include <if/monitor_defs.h>
#include <if/terminal_defs.h>
#include <if/terminal_config_defs.h>
#include <if/terminal_session_defs.h>
#include <term/server/server.h>

#include "term_debug.h"

/* internal functions */
static void default_err_handler(void *st, errval_t err);
static void tx_handler(void *arg);
static errval_t session_connect_cb(void *st,
                                   struct terminal_session_binding *b);
static void session_export_cb(void *st, errval_t err, iref_t iref);

/**
 * \brief Change waitset used for incoming characters.
 *
 * \param server Terminal server state.
 * \param ws New waitset.
 */
errval_t term_server_change_in_ws(struct term_server *server,
                                  struct waitset *ws)
{
    assert(server != NULL);
    assert(ws != NULL);

    errval_t err = SYS_ERR_OK;

    struct terminal_binding *b =
        collections_stack_top(server->in_binding_stack);
    assert(b != NULL);

    err = b->change_waitset(b, ws);
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_CHANGE_WAITSET);
    }
    return err;
}

/**
 * \brief Change waitset used for outgoing characters.
 *
 * \param server Terminal server state.
 * \param ws New waitset.
 */
errval_t term_server_change_out_ws(struct term_server *server,
                                   struct waitset *ws)
{
    assert(server != NULL);
    assert(ws != NULL);

    errval_t err = SYS_ERR_OK;

    struct terminal_binding *b =
        collections_stack_top(server->out_binding_stack);
    assert(b != NULL);

    err = b->change_waitset(b, ws);
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_CHANGE_WAITSET);
    }
    return err;
}

/**
 * \brief Change waitset used for configuration messages.
 *
 * \param server Terminal server state.
 * \param ws New waitset.
 */
errval_t term_server_change_conf_ws(struct term_server *server,
                                    struct waitset *ws)
{
    assert(server != NULL);
    assert(ws != NULL);

    errval_t err = SYS_ERR_OK;

    struct terminal_config_binding *b =
        collections_stack_top(server->conf_binding_stack);
    assert(b != NULL);

    err = b->change_waitset(b, ws);
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_CHANGE_WAITSET);
    }
    return err;
}

/**
 * \brief Change waitset used for the session interface.
 *
 * \param server Terminal server state.
 * \param ws New waitset.
 */
errval_t term_server_change_session_ws(struct term_server *server,
                                       struct waitset *ws)
{
    assert(server != NULL);
    assert(ws != NULL);

    errval_t err = SYS_ERR_OK;

    assert(server->session_binding != NULL);
    err = server->session_binding->change_waitset(server->session_binding, ws);
    if (err_is_fail(err)) {
        err_push(err, TERM_ERR_CHANGE_WAITSET);
    }
    return err;
}

/**
 * \brief Initialize terminal server and export session interface.
 *
 * \param server       Terminal server state, filled-in by function.
 * \param session_iref Interface reference of the session interface, filled-in
 *                     by function.
 * \param session_ws   Waitset used for the session interface.
 * \param in_ws        Waitset used for incoming characters.
 * \param out_ws       Waitset used for outgoing characters.
 * \param conf_ws      Waitset used for configuration messages.
 * \param chars_cb     Callback when new characters arrive.
 * \param conf_cb      Callback when configuration message arrive.
 * \param session_cb   Callback when new session is established.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_EXPORT_SESSION_INTERFACE if export of session inteface
 *                                           failed.
 */
errval_t term_server_init(struct term_server *server, iref_t *session_iref,
                          struct waitset *session_ws, struct waitset *in_ws,
                          struct waitset *out_ws, struct waitset *conf_ws,
                          term_characters_handler_fn chars_cb,
                          term_server_configuration_handler_fn conf_cb,
                          term_server_new_session_fn session_cb)
{
    errval_t err;
    struct monitor_binding *mb = get_monitor_binding();
    struct waitset *monitor_ws = mb->waitset;

    assert(server != NULL);
    assert(session_iref != NULL);
    assert(session_ws != NULL);
    assert(in_ws != NULL);
    assert(out_ws != NULL);
    assert(conf_ws != NULL);
    assert(chars_cb != NULL);
    assert(conf_cb != NULL);
    assert(session_cb != NULL);

    /* setup internal state */
    server->st = NULL;
    server->session_ws = session_ws;
    server->in_ws = in_ws;
    server->out_ws = out_ws;
    server->conf_ws = conf_ws;
    server->chars_cb = chars_cb;
    server->conf_cb = conf_cb;
    server->new_session_cb = session_cb;
    server->err_cb = default_err_handler;
    server->session_iref = NULL_IREF;
    server->out_iref = NULL_IREF;
    server->in_iref = NULL_IREF;
    server->conf_iref = NULL_IREF;
    server->out_binding_stack = NULL;
    server->in_binding_stack = NULL;
    server->conf_binding_stack = NULL;
    server->first_client_connected = false;
    server->session_binding = NULL;
    server->session_id = NULL_CAP;
    collections_fbuf_create(&server->fbuf);
    server->sending = false;

    /* export terminal session interface */
    err = terminal_session_export(server, session_export_cb, session_connect_cb,
                                  server->session_ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_EXPORT_SESSION_INTERFACE);
    }
    TERM_DEBUG("Exporting terminal session interface.\n");

    /* wait until session interface is exported */
    while (server->session_iref == NULL_IREF) {
        err = event_dispatch(monitor_ws);
        if (err_is_fail(err)) {
            return err_push(err, TERM_ERR_EXPORT_SESSION_INTERFACE);
        }
    }
    *session_iref = server->session_iref;

    return SYS_ERR_OK;
}

/**
 * \brief Enqueue characters for sending to the terminal client.
 *
 * \param server Terminal server state.
 * \param data   Characters to send.
 * \param length Number of characters to send.
 *
 * Function always completes without blocking. The actual sending might not
 * happen until the 'out_ws'-waitset is dispatched. The programmer can
 * check whether the send completed using term_server_sending().
 *
 * The function copies the supplied data to its internal buffers, so the
 * programmer can modify the data after the call.
 */
void term_server_send(struct term_server *server, const char *data,
                      size_t length)
{
    struct terminal_binding *b = collections_stack_top
                                    (server->out_binding_stack);

    collections_fbuf_append(server->fbuf, data, length);

    // try to send something, if we're not already doing so
    if (b != NULL && collections_fbuf_other_is_empty(server->fbuf)) {
        assert(!server->sending);
        server->sending = true;
        tx_handler(server);
    }
}

/**
 * \brief Set a handler called when new characters arrives.
 *
 * \param server Terminal server state.
 * \param chars_cb Characters handler.
 */
void term_server_set_chars_handler(struct term_server *server,
                                   term_characters_handler_fn chars_cb)
{
    assert(server != NULL);
    assert(chars_cb != NULL);

    server->chars_cb = chars_cb;
}

/**
 * \brief Set a handler called when a new configuration message arrives.
 *
 * \param server Terminal server state.
 * \param conf_cb Configuration message handler.
 */
void term_server_set_conf_handler(struct term_server *server,
                                  term_server_configuration_handler_fn *conf_cb)
{
    assert(server != NULL);
    assert(conf_cb != NULL);

    server->conf_cb = conf_cb;
}

/**
 * \brief Set an error handler if an asynchronous error occurs.
 *
 * \param server Terminal server state.
 * \param err_cb Error handler.
 */
void term_server_set_err_handler(struct term_server *server,
                                 term_async_err_handler_fn err_cb)
{
    assert(server != NULL);
    assert(err_cb != NULL);

    server->err_cb = err_cb;
}

/**
 * \brief Set an handler called when a new session is established.
 *
 * \param server Terminal server state.
 * \param session_cb New session handler.
 */
void term_server_set_new_session_handler(struct term_server *server,
                                         term_server_new_session_fn session_cb)
{
    assert(server != NULL);
    assert(session_cb != NULL);

    server->new_session_cb = session_cb;
}

/**
 * \privatesection
 * Internal function and binding code follows.
 */

/**
 * \brief Default asynchronous error handler.
 */
static void default_err_handler(void *st, errval_t err)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error in libterm_server.");
    }
}

static void tx_handler(void *arg)
{
    errval_t err;
    struct term_server *server = arg;
    struct terminal_binding *b = collections_stack_top
        (server->out_binding_stack);

    /* free previous send buffer */
    collections_fbuf_other_free(server->fbuf);

    /* bail out if there's nothing to send */
    if (collections_fbuf_is_empty(server->fbuf)) {
        server->sending = false;
        return;
    }

    /* try to send */
    err = b->tx_vtbl.characters(b, MKCONT(tx_handler, server),
                                collections_fbuf_get_data(server->fbuf),
                                collections_fbuf_get_length(server->fbuf));
    if (err_is_ok(err)) {
        /*
         * Flip the buffer
         * Flounder uses the back buffer to send the data. New data is filled
         * into the front buffer. The back buffer is freed once the send
         * completes and the tx_handler is entered again via the continuation.
         */
        collections_fbuf_flip(server->fbuf);
    } else if (err_is_fail(err)) {
        /* call async error callback */
        err = err_push(err, TERM_ERR_SEND_CHARS);
        server->err_cb(server->st, err);
    }
}

static void check_first_client_connected(struct term_server *server)
{
    if (server->first_client_connected == true) {
        return;
    }

    struct terminal_binding *in_b =
        collections_stack_top(server->in_binding_stack);
    struct terminal_binding *out_b =
        collections_stack_top(server->out_binding_stack);
    struct terminal_binding *conf_b =
        collections_stack_top(server->conf_binding_stack);

    if (in_b == NULL || out_b == NULL || conf_b == NULL) {
        return;
    }

    TERM_DEBUG("First client successfully connected to terminal server.\n");

    /* new session callback */
    server->new_session_cb(server->st, server->session_id);
    server->first_client_connected = true;
}

static void in_characters_handler(struct terminal_binding *b, char *buffer,
                                  size_t length)
{
    struct term_server *server = b->st;

    /*
     * Characters arrived at the interface for incoming characters, call
     * user supplied chars_cb.
     */
    server->chars_cb(server->st, buffer, length);
}

static errval_t in_connect_cb(void *st, struct terminal_binding *b)
{
    struct term_server *server = st;

    TERM_DEBUG("Client connected to interface for incoming characters.\n");

    b->rx_vtbl.characters = in_characters_handler;
    b->st = server;

    /* Puish binding to the top of the stack */
    TERM_DEBUG("Pushed binding to the incoming interface binding stack.\n");
    collections_stack_push(server->in_binding_stack, b);

    check_first_client_connected(server);

    return SYS_ERR_OK;
}

static void out_characters_handler(struct terminal_binding *b, char *buffer,
                                   size_t length)
{
    struct term_server *server = b->st;

    /*
     * It is an error if characters arrive at the interface for outgoing
     * characters.
     */
    server->err_cb(server->st, TERM_ERR_RECV_CHARS);
}

static errval_t out_connect_cb(void *st, struct terminal_binding *b)
{
    struct term_server *server = st;

    TERM_DEBUG("Client connected to interface for outgoing characters.\n");

    b->rx_vtbl.characters = out_characters_handler;
    b->st = server;

    /* Push binding to the top of the stack. */
    TERM_DEBUG("Pushed binding to the outgoing interface binding stack.\n");
    collections_stack_push(server->out_binding_stack, b);

    check_first_client_connected(server);

    return SYS_ERR_OK;
}

static void conf_disconnect_handler(struct terminal_config_binding *b)
{
    struct term_server *server = b->st;

    TERM_DEBUG("Poped binding from the outgoing interface binding stack.\n");
    collections_stack_pop(server->out_binding_stack);
    collections_stack_pop(server->in_binding_stack);
    collections_stack_pop(server->conf_binding_stack);
}

static void conf_configuration_handler(struct terminal_config_binding *b,
                                       terminal_config_option_t opt,
                                       char *arguments)
{
    struct term_server *server = b->st;

    /* If a configuration messages arrives, call user supplied conf_cb. */
    server->conf_cb(server->st, opt, arguments);
}

static errval_t conf_connect_cb(void *st, struct terminal_config_binding *b)
{
    struct term_server *server = st;

    TERM_DEBUG("Client connected to configuration interface.\n");

    b->rx_vtbl.disconnect = conf_disconnect_handler;
    b->rx_vtbl.configuration = conf_configuration_handler;
    b->st = server;

    /* Push binding to the top of the stack. */
    TERM_DEBUG("Pushed binding to the configuration interface binding stack.\n");
    collections_stack_push(server->conf_binding_stack, b);

    check_first_client_connected(server);

    return SYS_ERR_OK;
}

static void check_session_initialized(struct term_server *server)
{
    errval_t err;

    if (server->out_iref == NULL_IREF || server->in_iref == NULL_IREF ||
        server->conf_iref == NULL_IREF) {
        /* Not all three interfaces are already exported. */
        return;
    }

    /*
     * Send iref for outgoing characters, iref for incoming characters and
     * iref for configuration messages back as a reply to
     * 'session_associate_with_terminal'.
     *
     * Note that the interface reference for outgoing characters for the server
     * is the interface reference the client uses for incoming characters.
     */
    err = server->session_binding->
        tx_vtbl.session_associate_with_terminal_response
            (server->session_binding, NOP_CONT, server->out_iref,
             server->in_iref, server->conf_iref, SYS_ERR_OK);
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_ASSOCIATE_WITH_TERM_REPLY);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }

    TERM_DEBUG("All terminal interfaces sucessfully exported.\n");
}

/**
 * \brief Export callback for interface for incoming characters.
 */
static void in_export_cb(void *st, errval_t err, iref_t in_iref)
{
    struct term_server *server = st;

    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_IN_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }

    server->in_iref = in_iref;
    TERM_DEBUG("Terminal interface for incoming characters successfully "
               "exported (iref: %" PRIuIREF ").\n", in_iref);

    /* Check if all interfaces are already exported. */
    check_session_initialized(server);
}

/**
 * \brief Export callback for interface for outgoing characters.
 */
static void out_export_cb(void *st, errval_t err, iref_t out_iref)
{
    struct term_server *server = st;

    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_OUT_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }

    server->out_iref = out_iref;
    TERM_DEBUG("Terminal interface for outgoing characters successfully "
               "exported (iref: %" PRIuIREF ").\n", out_iref);

    /* Check if all interfaces are already exported. */
    check_session_initialized(server);
}

/**
 * \brief Export callback for configuration interface.
 */
static void conf_export_cb(void *st, errval_t err, iref_t conf_iref)
{
    struct term_server *server = st;

    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_CONF_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }

    server->conf_iref = conf_iref;
    TERM_DEBUG("Terminal configuration interface successfully exported (iref: %"
               PRIuIREF ").\n", conf_iref);

    /* Check if all interfaces are already exported. */
    check_session_initialized(server);
}

/**
 * \brief Function called if 'session_associate_with_terminal' message arrives.
 */
static void session_associate_with_terminal(struct terminal_session_binding *b,
                                            struct capref session_id)
{
    errval_t err;
    struct term_server *server = b->st;

    /* Check is terminal is not already associated with a session. */
    if (!capref_is_null(server->session_id)) {
        TERM_DEBUG("Terminal is already associated with a session. Sending "
                    "TERM_ERR_TERMINAL_IN_USE.\n");
        err = b->tx_vtbl.session_associate_with_terminal_response
            (b, NOP_CONT, NULL_IREF, NULL_IREF, NULL_IREF,
             TERM_ERR_TERMINAL_IN_USE);
        if (err_is_fail(err)) {
            err = err_push(err, TERM_ERR_ASSOCIATE_WITH_TERM_REPLY);
            /* call async err callback */
            server->err_cb(server->st, err);
            return;
        }
    }

    /* initialize session state */
    server->session_id = session_id;
    assert(server->out_binding_stack == NULL);
    assert(server->in_binding_stack == NULL);
    assert(server->conf_binding_stack == NULL);
    collections_stack_create(&server->out_binding_stack);
    collections_stack_create(&server->in_binding_stack);
    collections_stack_create(&server->conf_binding_stack);

    /* Export interface for incoming characters. (terminal.if) */
    err = terminal_export(server, in_export_cb, in_connect_cb, server->in_ws,
                          IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_IN_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }
    TERM_DEBUG("Exporting terminal interface for incoming characters.\n");

    /* Export interface for outgoing characters. (terminal.if) */
    err = terminal_export(server, out_export_cb, out_connect_cb, server->out_ws,
                          IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_OUT_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }
    TERM_DEBUG("Exporting terminal interface for outgoing characters.\n");

    /* Export interface for configuration messages. (terminal_config.if) */
    err = terminal_config_export(server, conf_export_cb, conf_connect_cb,
                                 server->conf_ws, IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_CONF_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }
    TERM_DEBUG("Exporting terminal configuration interface configuration "
               "messages.\n");

    /* wait until all interfaces are exported */
    struct monitor_binding *mb = get_monitor_binding();
    struct waitset *monitor_ws = mb->waitset;
    while (server->out_iref == NULL_IREF || server->in_iref == NULL_IREF ||
           server->conf_iref == NULL_IREF) {
        err = event_dispatch(monitor_ws);
        if (err_is_fail(err)) {
            /* call async error callback */
            DEBUG_ERR(err, "Error in event_dispatch.\n");
            server->err_cb(server->st, err);
        }
    }
}

/**
 * \brief Connect callback for terminal session interface.
 */
static errval_t session_connect_cb(void *st, struct terminal_session_binding *b)
{
    struct term_server *server = st;

    TERM_DEBUG("Client connected to terminal session interface.\n");

    server->session_binding = b;
    b->st = server;
    b->rx_vtbl.session_associate_with_terminal_call =
        session_associate_with_terminal;
    return SYS_ERR_OK;
}

/**
 * \brief Export callback for terminal session interface.
 */
static void session_export_cb(void *st, errval_t err, iref_t iref)
{
    struct term_server *server = st;

    /* check for error during export */
    if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_EXPORT_SESSION_INTERFACE);
        /* call async error callback */
        server->err_cb(server->st, err);
        return;
    }

    server->session_iref = iref;
}
