/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef LIBANGLER_ANGLER_H
#define LIBANGLER_ANGLER_H

#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <errors/errno.h>

errval_t angler_new_session(char *terminal, struct capref *session_id);

errval_t angler_new_session_with_iref(iref_t session_iref,
                                      struct capref *session_id);

#endif // LIBANGLER_ANGLER_H
