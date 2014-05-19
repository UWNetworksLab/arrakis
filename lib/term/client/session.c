/**
 * \file
 * \brief Session handling API for terminal client library.
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
#include <term/client/session.h>

/**
 * \brief Detach a domain from a session.
 *
 * \return SYS_ERR_OK if successful
 *         TERM_ERR_NOT_PART_OF_SESSION if domain is not part of a session
 */
errval_t daemonize(void)
{
    errval_t err;

    err = cap_delete(cap_sessionid);
    if (err_is_fail(err)) {
        return err_push(err, TERM_ERR_NOT_PART_OF_SESSION);
    }

    return SYS_ERR_OK;
}
