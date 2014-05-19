/**
 * \file
 * \brief Terminal emulator.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <stdbool.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/terminal.h>
#include <term/client/client_blocking.h>

struct terminal_state {
    /**
     * Is domain part of a session or a daemon?
     */
    bool session_domain;

    /**
     * Terminal device used from stdin, stdout and stderr.
     */
    struct term_client client;
};

size_t terminal_write(const char *data, size_t length)
{
    errval_t err;
    size_t written = 0;
    struct terminal_state *state = get_terminal_state();

    if (state != NULL && state->session_domain) {
        err = term_client_blocking_write(&state->client, data, length,
                                         &written);
        assert(err_is_ok(err));
        return written;
    } else {
        sys_print(data, length);
        return length;
    }
}

size_t terminal_read(char *data, size_t count)
{
    errval_t err;
    size_t read = 0;
    struct terminal_state *state = get_terminal_state();

    if (state->session_domain) {
        err = term_client_blocking_read(&state->client, data, count, &read);
        assert(err_is_ok(err));
        return read;
    } else {
        /**
         * Only domains that are part of a session can read from a terminal
         * device.
         */
        assert(!"Daemons can't read from a terminal.");
        return 0;
    }
}

errval_t terminal_init(void)
{
    errval_t err;
    struct capability cap;

    /* Allocate and initialize dispatcher-specific state. */
    struct terminal_state *state = malloc(sizeof(struct terminal_state));
    if (!state) {
        return LIB_ERR_MALLOC_FAIL;
    }
    set_terminal_state(state);

    /* Check if domain is part of a session. */
    err = debug_cap_identify(cap_sessionid, &cap);
    if (err_is_ok(err)) {
        /* Initialize libterm_client. */
        err = term_client_blocking_init(&state->client, cap_sessionid);
        if (err_is_fail(err)) {
            return err;
        }

        state->session_domain = true;
        return SYS_ERR_OK;
    } else {
        state->session_domain = false;
        return SYS_ERR_OK;
    }
}

void terminal_exit(void)
{
    struct terminal_state *state = get_terminal_state();

    if (state != NULL && state->session_domain) {
        term_client_blocking_exit(&state->client);
    }
}
