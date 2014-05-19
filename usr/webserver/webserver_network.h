/**
 * \file
 * \brief webserver network interface
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef WEBSERVER_NETWORK_H
#define WEBSERVER_NETWORK_H

void http_server_init(struct ip_addr server, const char *path);

#endif // WEBSERVER_NETWORK_H
