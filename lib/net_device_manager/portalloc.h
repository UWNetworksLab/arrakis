/**
 * \file
 * \brief Port allocator for netd
 *
 * This file is part of the net "daemon"
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: NetOS Group.
 */

#ifndef _PORTALLOC_H_
#define _PORTALLOC_H_

#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <if/net_ports_defs.h>

/*****************************************************************************
 * Constants
 ****************************************************************************/

#define TCP_LOCAL_PORT_RANGE_START 8192
#define TCP_LOCAL_PORT_RANGE_END   0x7fff
#define TCP_ARRAY_SIZE (((TCP_LOCAL_PORT_RANGE_END - \
                         0 + 1) / \
                                     sizeof(uint64_t)) + \
                        (((TCP_LOCAL_PORT_RANGE_END - \
                         0 + 1) % \
                                     sizeof(uint64_t)) != 0) \
                       )

#define UDP_LOCAL_PORT_RANGE_START 8192
#define UDP_LOCAL_PORT_RANGE_END   0x7fff
#define UDP_ARRAY_SIZE (((UDP_LOCAL_PORT_RANGE_END - \
                         0 + 1) / \
                                     sizeof(uint64_t)) + \
                        (((UDP_LOCAL_PORT_RANGE_END - \
                         0 + 1) % \
                                     sizeof(uint64_t)) != 0) \
                       )

//#define M64 ((1L << 64) - 1) // XXX
#define M64 0xffffffffffffffff
#define M32 0xffffffff

/*****************************************************************************
 * Prototypes
 ****************************************************************************/

void init_free_ports(void);

uint16_t alloc_tcp_port(void);
uint16_t alloc_udp_port(void);

uint16_t alloc_specific_port(uint16_t port, net_ports_port_type_t type);
void free_port(uint16_t port, net_ports_port_type_t type);

#endif
