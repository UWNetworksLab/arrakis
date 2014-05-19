/**
 * \file
 * \brief lwIP configuration file for Barrelfish.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LWIPOPTS_H
#define LWIPOPTS_H

#include <barrelfish/net_constants.h>
/// Build DHCP client
#define LWIP_DHCP               1

/// Build DNS client
#define LWIP_DNS		1

/// Don't want lwip POSIX socket wrappers
#define LWIP_POSIX_SOCKETS_IO_NAMES     0

/// We do not want LWIP to provide POSIX errno
#undef LWIP_PROVIDE_ERRNO

/// Don't want socket functions overriden by lwIP. We provide our own.
#define LWIP_COMPAT_SOCKETS     0

/// Use malloc() from libc
//#define MEM_LIBC_MALLOC         1

/// We want to be informed about interface up/down
#define LWIP_NETIF_STATUS_CALLBACK      1

/// Don't do ARP lookup to test IP
#define DHCP_DOES_ARP_CHECK     0

/// Disable locks (we lock the whole stack)
#define SYS_LIGHTWEIGHT_PROT    0


/// Number of simultaneously active TCP connections
#define MEMP_NUM_TCP_PCB        200
//#define MEMP_NUM_TCP_PCB        512



/// Number of TCP segments
#define MEMP_NUM_TCP_SEG        512
//#define MEMP_NUM_TCP_SEG        1024
//#define MEMP_NUM_TCP_SEG            4096

/// TCP window size
#define TCP_WND                 11680

/// TCP maximum segment size
#define TCP_MSS                 1460

/// TCP send buffer size
#define TCP_SND_BUF             8192

/// TCP send queue length (pbufs)
#define TCP_SND_QUEUELEN       (16 * (TCP_SND_BUF/TCP_MSS))

/// Enable debugging
// #define LWIP_DEBUG              1

/// Enable have loopif (localhost hostname translation)
#define LWIP_HAVE_LOOPIF 1

/* Place to control the LWIP debugging.
 Enable debugging of these subsystems */

#define ETHARP_DEBUG     LWIP_DBG_ON
#define NETIF_DEBUG      LWIP_DBG_ON
// #define PBUF_DEBUG       LWIP_DBG_ON
#define DHCP_DEBUG       LWIP_DBG_ON
#define UDP_DEBUG        LWIP_DBG_ON
#define IP_DEBUG         LWIP_DBG_ON
// #define TCP_DEBUG        LWIP_DBG_ON
// #define TCPIP_DEBUG      LWIP_DBG_ON
// #define TCP_INPUT_DEBUG  LWIP_DBG_ON
// #define TCP_OUTPUT_DEBUG LWIP_DBG_ON
#define SOCKETS_DEBUG    LWIP_DBG_ON

#ifndef CHECKSUM_GEN_IP
#define CHECKSUM_GEN_IP                 1
#endif

#ifndef CHECKSUM_GEN_UDP
#define CHECKSUM_GEN_UDP                1
#endif

#ifndef CHECKSUM_GEN_TCP
#define CHECKSUM_GEN_TCP                1
#endif

#ifndef CHECKSUM_CHECK_IP
#define CHECKSUM_CHECK_IP               1
#endif

#ifndef CHECKSUM_CHECK_UDP
#define CHECKSUM_CHECK_UDP              1
#endif

#ifndef CHECKSUM_CHECK_TCP
#define CHECKSUM_CHECK_TCP              1
#endif

#endif
