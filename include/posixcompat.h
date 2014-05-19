/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef POSIXCOMPAT_H
#define POSIXCOMPAT_H

#include <barrelfish/caddr.h>
#include <barrelfish/types.h>
#include <errors/errno.h>

#include <sys/cdefs.h>

__BEGIN_DECLS

errval_t spawn_setup_fds(struct capref *frame, int rfd);
errval_t posixcompat_unpack_fds(void);
iref_t posixcompat_pts_get_iref(int fd);

__END_DECLS

#endif
