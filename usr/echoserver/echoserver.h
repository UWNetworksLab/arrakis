/**
 * \file
 * \brief echoserver local interface
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ECHOSERVER_H
#define ECHOSERVER_H

//void startlwip(char *card_name, uint64_t queueid);
int udp_echo_server_init(void);
int tcp_echo_server_init(void);

#endif // ECHOSERVER_H
