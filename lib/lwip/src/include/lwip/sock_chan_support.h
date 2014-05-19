/**
 * \file
 * \brief Barrelfish waitset and channel support for LWIP.
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

#ifndef __LWIP_CHAN_SUPPORT_H__
#define __LWIP_CHAN_SUPPORT_H__

#include <barrelfish/waitset.h>
#include <errors/errno.h>

#include <stdbool.h>

bool lwip_sock_is_open(int socket);
bool lwip_sock_ready_read(int socket);
bool lwip_sock_ready_write(int socket);

errval_t lwip_sock_waitset_deregister_read(int socket);
errval_t lwip_sock_waitset_register_read(int socket, struct waitset *ws);
errval_t lwip_sock_waitset_deregister_write(int socket);
errval_t lwip_sock_waitset_register_write(int socket, struct waitset *ws);

#endif /* __LWIP_CHAN_SUPPORT_H__ */
