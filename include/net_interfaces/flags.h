/*
 * Copyright (c) 2007-2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NET_INTERFACES_FLAGS_H_
#define NET_INTERFACES_FLAGS_H_

#define NETIF_TXFLAG_IPCHECKSUM (1 << 0)
#define NETIF_TXFLAG_TCPCHECKSUM (1 << 1)
#define NETIF_TXFLAG_UDPCHECKSUM (1 << 2)
#define NETIF_TXFLAG_TCPHDRLEN_SHIFT (3)
#define NETIF_TXFLAG_TCPHDRLEN_MASK (0xf << NETIF_TXFLAG_TCPHDRLEN_SHIFT)



#define NETIF_RXFLAG_TYPE_IPV4 (1 << 0)
#define NETIF_RXFLAG_TYPE_UDP (1 << 1)
#define NETIF_RXFLAG_TYPE_TCP (1 << 2)

#define NETIF_RXFLAG_IPCHECKSUM (1 << 3)
#define NETIF_RXFLAG_IPCHECKSUM_GOOD (1 << 4)
#define NETIF_RXFLAG_L4CHECKSUM (1 << 5)
#define NETIF_RXFLAG_L4CHECKSUM_GOOD (1 << 6)

#endif // ndef NET_INTERFACES_FLAGS_H_

