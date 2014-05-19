/**
 * \file
 * \brief Glue code for old deprecated IDC API
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

void messages_wait_and_handle_next(void)
{
    errval_t err = event_dispatch(get_default_waitset());
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in event_dispatch for messages_wait_and_handle_next hack");
    }
}

void messages_handler_loop(void)
{
    while (1) {
        messages_wait_and_handle_next();
    }
}
