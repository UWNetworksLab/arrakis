/**
 * \file
 * \brief Header file for helper and additional functions
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __TOOLS_H__
#define __TOOLS_H__

#ifndef DOXYGEN
// exclude system headers from documentation

//#include <strings.h>
#include <stdint.h>
#include <assert.h>

#endif                          // DOXYGEN

#include <bfdmuxtools/bfdmux.h>

int find_msb(uint64_t value);

//uint8_t        *parse_hex_input(char *str);
char *get_error_position_string(int pos);

// Filter

char *build_src_mac_filter(struct eth_addr src);
char *build_dst_mac_filter(struct eth_addr dst);
char *build_ipv4_filter(addr_t srcip, addr_t dstip);
char *build_icmp_filter(void);
char *build_tcp_filter(port_t srcport, port_t dstport);
char *build_udp_filter(port_t srcport, port_t dstport);
char *build_ipv4_icmp_filter(addr_t srcip, addr_t dstip);
char *build_ipv4_tcp_filter(addr_t srcip, addr_t dstip, port_t srcport,
                            port_t dstport);
char *build_ipv4_udp_filter(addr_t srcip, addr_t dstip, port_t srcport,
                            port_t dstport);
char *build_ether_dst_ipv4_udp_filter(struct eth_addr dst, addr_t srcip,
                                      addr_t dstip, port_t srcport,
                                      port_t dstport);
char *build_ether_dst_ipv4_tcp_filter(struct eth_addr dst, addr_t srcip,
                                      addr_t dstip, port_t srcport,
                                      port_t dstport);
char *build_ether_src_ipv4_udp_filter(struct eth_addr src, addr_t srcip,
                                      addr_t dstip, port_t srcport,
                                      port_t dstport);
char *build_ether_src_ipv4_tcp_filter(struct eth_addr src, addr_t srcip,
                                      addr_t dstip, port_t srcport,
                                      port_t dstport);
char *build_generic_arp_reply_filter(void);
char *build_arp_transmit_filter(struct eth_addr src);
#endif
