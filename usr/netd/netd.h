/**
 * \file
 * \brief the main header file for the net "daemon"
 *
 * This file is part of the net "daemon"
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: NetOS Group.
 */

#ifndef _NETD_H_
#define _NETD_H_

#include <netif/bfeth.h>
#include <if/net_ARP_defs.h>

#include <contmng/contmng.h>
#include <procon/procon.h>

extern bool do_dhcp;
// IP information for static configuration
extern char *ip_addr_str;
extern char *netmask_str;
extern char *gateway_str;
extern char *dns_str;

typedef net_ARP_ipv4addr_t ipv4addr_t;

struct netif *netif_ptr;

/**
 * @brief initializes LWIP. not a lot left after I changed the subsystems
 *
 * @param card_name the of the card.
 */
void startlwip(char *card_name, uint64_t queueid);

int init_ARP_lookup_service(char *dev_name);
#endif // _NETD_H_

