/**
 * \file
 * \brief Bfdmux twek options
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BFDMUX_H__
#define __BFDMUX_H__

#ifndef DOXYGEN
// exclude system headers from documentation

#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <netif/etharp.h>

#endif                          // DOXYGEN

#include "debug.h"

/*
 * Configs
 */
#define PROC_QUEUE_LEN 5                                /**< \brief Number of NIC buffers to queue for processing */

// Socket
#define BFDMUX_SOCK_PATH	"/tmp/.bfdmux.sock"     /**< \brief Location of the UNIX socket file */


#define FLUSH_AND_SYNC 1                /**< \brief Always call fflush and sync on file descriptor and communication channels. */

// Error constants
#define ERR_OK 0                        /**< \brief No error */
#define ERR_NONFATAL -1                 /**< \brief Error, but nonfatal */
#define ERR_FATAL -2                    /**< \brief Fatal error, shut down */
#define ERR_DISCONNECT -3               /**< \brief Disconnect error. Client-server connection is lost. */
#define ERR_DROPPED -4                  /**< \brief Application was not able to receive a new packet. So the packet was dropped. */

/*
 * IP TCP/UDP Definitions
 */
#define PROTO_TCP	0x06            /**< \brief TCP protocol number in IPv4 header */
#define PROTO_UDP	0x11a           /**< \brief UDP protocol number in IPv4 header */
#define PORT_ANY	0x00            /**< \brief Any UDP/TCP port */
#define BFDMUX_IP_ADDR_ANY	0x00            /**< \brief Any IPv4-Address */
#define IP_ADDR_LOCAL	0x7f000001      /**< \brief This is the localhost 127.0.0.1 IP-Address */
#define MAC_ANY(mac)	#mac={.addr = {0,0,0,0,0,0}}    // Any MAC

/*
 * Type defs
 */
typedef uint8_t prot_t;         /**< \brief Protocol type */
typedef uint32_t addr_t;        /**< \brief IP-Address type */
typedef uint16_t port_t;        /**< \brief Port type */

//typedef int8_t  err_t;                /**< \brief Error type */
typedef int32_t sock_t;         /**< \brief Socket type */
typedef int32_t mq_t;           /**< \brief Message queue type */
typedef uint8_t cmd_t;          /**< \brief Command type */
typedef uint32_t mqkey_t;       /**< \brief Message queue key type */
typedef uint32_t smkey_t;       /**< \brief Shared memory key type */
typedef int32_t filterid_t;     /**< \brief Filter id type. Negative values for errors. */

/* FIXME: Understand this hack
// k:
// this is a dirty hack like the whole
// stupid definitions like IP_ADDR_ANY
// in the bfdmux which prevents me to
// include the correct headers from lwip
#define MAC_ADDR_SIZE 6
struct eth_addr { uint8_t addr[MAC_ADDR_SIZE]; };
// had to fix all of them at the end
*/


bool demux(void *data, int len);

#endif
