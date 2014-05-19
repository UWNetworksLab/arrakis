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

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <if/net_ports_defs.h>
#include "portalloc.h"
#include "device_manager_debug.h"

/**
 * The arrays storing the current port allocation state
 */
static uint64_t free_tcp_ports[TCP_ARRAY_SIZE];
static uint64_t free_udp_ports[UDP_ARRAY_SIZE];


/**
 * @brief Initialization code for port allocator
 */
void init_free_ports(void)
{
    uint64_t i;

    for (i = 0; i < TCP_ARRAY_SIZE; i++) {
        free_tcp_ports[i] = M64;
    }
    for (i = 0; i < UDP_ARRAY_SIZE; i++) {
        free_udp_ports[i] = M64;
    }
}


/**
 * @brief Allocates the first free port found starting from pstart
 * @param free_ports the array of port allocation data
 * @param type UDP or TCP
 * @param pstart defines the port it starts searching from
 *
 * @return the proposed port number or 0 in case all ports are allocated
 */
static uint16_t alloc_port(uint64_t * free_ports, net_ports_port_type_t type,
                           uint16_t pstart)
{
    //k: asq did this to be generic?
    //uint32_t v32;
    //uint16_t v16;
    //uint8_t v8;
    int bitnr = 0;
    uint64_t m = M32, v;
    int s = 32;
    uint64_t len;
    uint64_t start;

    if (type == net_ports_PORT_TCP) {
        len = TCP_ARRAY_SIZE;
        start = TCP_LOCAL_PORT_RANGE_START / 64;
    } else {
        len = UDP_ARRAY_SIZE;
        start = UDP_LOCAL_PORT_RANGE_START / 64;
    }

    for (int i = start; i < len; i++) {
        //find a 64bit word which has at least 1 bit set (=1 free port)
        if (free_ports[i]) {
            v = free_ports[i];
            //binary search the 1-bit
            while (m > 0) {
                if (v & m) {
                    v = v & m;
                } else {
                    v = (v >> s) & m;
                    bitnr += s;
                }
                if (s != 1) {
                    s /= 2;
                }
                m >>= s;
            }
            //bitnr is now the bitposition within the current 64 bit word which
            //will be the allocated portnummer
            assert(bitnr >= 0 && bitnr <= 63);
            //mark the port as allocated
            free_ports[i] &= ~(1 << bitnr);
            //return the port number

            return (bitnr + i * sizeof(uint64_t) * 8 + pstart);
        }
    }
    return (0);                 //no port could be allocated
}

/**
 * @brief allocates a tcp port
 *
 */
uint16_t alloc_tcp_port(void)
{
    return alloc_port(free_tcp_ports, net_ports_PORT_TCP,
                      TCP_LOCAL_PORT_RANGE_START);
}

/**
 * @brief allocates a tcp port
 *
 */
uint16_t alloc_udp_port(void)
{
    return alloc_port(free_udp_ports, net_ports_PORT_UDP,
                      UDP_LOCAL_PORT_RANGE_START);
}

/**
 * @brief checks to see whether a given port is free for use
 *
 * @param free_ports the array of either tcp or udp allocation state
 * @param port the port number to be checked
 *
 * @return true in case the port is free and otherwise false
 */
static inline bool check_free(uint64_t * free_ports, uint16_t port)
{
    uint16_t pidx = port;

    return (free_ports[pidx / 64] & (1 << (pidx % 64)));
}

/**
 * @brief allocates a specific port. this is the backend function for bind
 *
 * @param port the port number
 * @param type UDP or TCP
 *
 * @return 0 in case port is in use and the port number other wise
 */
inline uint16_t alloc_specific_port(uint16_t port, net_ports_port_type_t type)
{
    uint16_t pidx = port;
    uint64_t *free_ports;

    NDM_DEBUG("allocating port %u with type %d\n", port, type);
    if (type == net_ports_PORT_TCP) {
        free_ports = free_tcp_ports;
    } else {
        free_ports = free_udp_ports;
    }
    if (free_ports[pidx / 64] & (1 << (pidx % 64))) {
        free_ports[pidx / 64] &= ~(1 << (pidx % 64));
        return port;
    } else {
        return (0);
    }
}

/**
 * @brief frees a port and does not care whether it is allocated before or not!
 *
 * @param port the port number
 * @param type UDP or TCP
 */
inline void free_port(uint16_t port, net_ports_port_type_t type)
{
    uint64_t *free_ports =
      (type == net_ports_PORT_TCP) ? free_tcp_ports : free_udp_ports;
    free_ports[port / 64] |= (1 << (port % 64));
}
