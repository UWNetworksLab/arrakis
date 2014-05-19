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

#include <barrelfish/barrelfish.h>
#include <term/client/client.h>

#include "filter_priv.h"

#include <assert.h>

/**
 * \brief Change the waitset used for incoming characters.
 *
 * \param client  Terminal client state.
 * \param read_ws New waitset to use.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_CHANGE_WAITSET on error.
 */
errval_t term_client_change_read_waitset(struct term_client *client,
                                         struct waitset *read_ws)
{
    errval_t err;

    assert(client != NULL);
    assert(read_ws != NULL);

    client->read_ws = read_ws;
    err = client->in_binding->change_waitset(client->in_binding, read_ws);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_CHANGE_WAITSET);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Change the waitset used for outgoing characters.
 *
 * \param client   Terminal client state.
 * \param write_ws New waitset to use.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_CHANGE_WAITSET on error.
 */
errval_t term_client_change_write_waitset(struct term_client *client,
                                          struct waitset *write_ws)
{
    errval_t err;

    assert(client != NULL);
    assert(write_ws != NULL);

    client->write_ws = write_ws;
    err = client->out_binding->change_waitset(client->out_binding, write_ws);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_CHANGE_WAITSET);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Change the waitset used for configuration messages.
 *
 * \param client  Terminal client state.
 * \param conf_ws New waitset to use.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_CHANGE_WAITSET on error.
 */
errval_t term_client_change_config_waitset(struct term_client *client,
                                           struct waitset *conf_ws)
{
    errval_t err;

    assert(client != NULL);
    assert(conf_ws != NULL);

    client->conf_ws = conf_ws;
    err = client->conf_binding->change_waitset(client->conf_binding, conf_ws);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_CHANGE_WAITSET);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Non-blocking write to a terminal.
 *
 * \param client  Terminal client state.
 * \param data    Buffer holding characters to write.
 * \param length  The number of characters to write.
 * \param cont    Continuation invoked once the write completes.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_TX_BUSY if another message is buffered but not yet sent.
 *         TERM_ERR_IO if an I/O error occurred.
 */
errval_t term_client_write(struct term_client *client, const char *data,
                           size_t length, struct event_closure cont)
{
    errval_t err = SYS_ERR_OK;
    char *outdata = NULL;

    assert(client != NULL);
    assert(data != NULL);
    assert(length > 0);

    /* Make a copy of the data, since the output filters might modify them. */
    outdata = malloc(length);
    assert(outdata != NULL);
    memcpy(outdata, data, length);

    /* apply output filters */
    term_filter_apply(client->output_filters, &outdata, &length);

    /* try to send characters */
    err = client->out_binding->tx_vtbl.characters(client->out_binding, cont,
                                                  outdata, length);
    if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        err = err_push(err, TERM_ERR_TX_BUSY);
        goto out;
    } else if (err_is_fail(err)) {
        err = err_push(err, TERM_ERR_IO);
        goto out;
    }

out:
    /* free data */
    free(outdata);
    return err;
}

/**
 * \brief Send a configuration command to the terminal server.
 *
 * \param client Terminal client state.
 * \param opt    Configuration option.
 * \param arg    Optional argument.
 * \param cont   Continuation invoked once the configuration message is sent.
 *
 * \return SYS_ERR_OK if successful.
 *         TERM_ERR_TX_BUSY if another message is buffered but not yet sent.
 *         TERM_ERR_UNKNOWN_CONFIG_OPT if opt is unknown.
 */
errval_t term_client_config(struct term_client *client,
                            terminal_config_option_t opt, char *arg,
                            struct event_closure cont)
{
    assert(!"NYI");
    return TERM_ERR_UNKNOWN_CONFIG_OPT;
}

/**
 * \brief Set an error handler, called when an asynchronous error occurs.
 *
 * \param client Terminal client state.
 * \param err_cb Error handler.
 */
void term_client_set_err_handler(struct term_client *client,
                                 term_async_err_handler_fn err_cb)
{
    assert(client != NULL);
    assert(err_cb != NULL);

    client->err_cb = err_cb;
}

/**
 * \brief Set handler that is called when new characters arrive.
 *
 * \param client   Terminal client state.
 * \param chars_cb Characters handler.
 */
void term_client_set_chars_handler(struct term_client *client,
                                   term_characters_handler_fn *chars_cb)
{
    assert(client != NULL);
    assert(chars_cb != NULL);

    client->chars_cb = chars_cb;
    client->non_blocking_read = true;
}
