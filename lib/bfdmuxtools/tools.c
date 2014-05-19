/**
 * \file
 * \brief Helper functoin and additional tools used by libbfdmux
*/
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DOXYGEN
// exclude system headers from documentation

//#include <errno.h>
#include <stdlib.h>
#include <string.h>

#endif							// DOXYGEN

#include <barrelfish/barrelfish.h>

#include <bfdmuxtools/tools.h>

/**
 * \brief Finds the index of the most significant 1-bit in 'value'
 * @param value The integer to be analyzed
 * @return The index of the most significant 1-bit in value (bits numbered 1..64); 0 if value = 0.
 */

//inline int find_msb(uint64_t value);

inline int
find_msb(uint64_t value)
{
	int             msb;
	uint32_t        lo,
	                hi;

	msb = 0;
	lo = (uint32_t) value;
	hi = (uint32_t) (value >> 32);

	// Perform binary search for most significant bit
	if (hi) {
		msb |= 0x20;
		lo = hi & 0xFFFF;
		hi = hi >> 16;
	} else {
		hi = lo >> 16;
		lo = lo & 0xFFFF;
	}
	if (hi) {
		msb |= 0x10;
		lo = hi & 0x00FF;
		hi = hi >> 8;
	} else {
		hi = lo >> 8;
		lo = lo & 0x00FF;
	}
	if (hi) {
		msb |= 0x08;
		lo = hi & 0x000F;
		hi = hi >> 4;
	} else {
		hi = lo >> 4;
		lo = lo & 0x000F;
	}
	if (hi) {
		msb |= 0x04;
		lo = hi & 0x0003;
		hi = hi >> 2;
	} else {
		hi = lo >> 2;
		lo = lo & 0x0003;
	}
	if (hi) {
		msb |= 0x02;
		lo = hi & 0x0001;
		hi = hi >> 1;
	} else {
		hi = lo >> 1;
		lo = lo & 0x0001;
	}
	if (hi) {
		msb |= 0x01;
	}

	return msb + (value >> msb);	// Modifies bit numbering from 0..63
	// to range from 1..64
}

/**
 * \brief Returns a string with pos-1 spaces and a '^' character. Used to indicate error position in filter string!
 * @param pos The position to point at
 * @return A string with a '^' character at the given position. Caller should free memory after use!
 */
char           *
get_error_position_string(int pos)
{
	int             i = 0;
	char           *res = malloc(pos + 1);
	while ((i + 1) < pos) {
		res[i] = ' ';
		i++;
	}
	res[i] = '^';
	res[i + 1] = 0;
	return res;
}

/**
 * \brief Parses a string consisting of hex digits to a byte array
 * @param str The string to be parsed, e.g. "fe01abc9"
 * @return A byte array, e.g. 0xfe 0x01 0xab 0xc9. Caller should free the array after use!
 
uint8_t        *
parse_hex_input(char *str)
{
	char            hex[3];
	int             i;
	int             len = strlen(str);
	if ((len % 2) != 0) {
		return 0;
	}

	len = len / 2;
	char           *res = malloc(len);
	hex[2] = 0;
	for (i = 0; i < len; i++) {
		hex[0] = str[i * 2];
		hex[1] = str[i * 2 + 1];
		errno = 0;
		char           *endptr = (char *) &hex;
		uint8_t         val = strtol((char *) &hex, &endptr, 16);
		if ((errno == 0) && (endptr != (char *) &hex)) {
			res[i] = val;
		} else {
			free(res);
			return 0;
		}
	}
	return (uint8_t *) res;
}
*/

/*
 * Filter builder
 */

#define MAC_ADDR_SIZE 6

static inline bool if_any_mac(struct eth_addr addr){

    int i;
    for(i = 0; i < MAC_ADDR_SIZE; i++) {
	if(addr.addr[i] != 0xff) {
	    return false;
	}
    }
    return true;
}

/*
 * SrcMac field: size 48 bit offset = 0
 * DstMac field: size 48 bit offset = 48
 */
/*
 * This creates a Mac filter with a dst address
 */

char* build_dst_mac_filter(struct eth_addr dst)
{
    uint32_t first_four = 0;
    uint16_t last_two = 0;
    size_t max_len = 64;
    char *filter = malloc(max_len);
    filter[0] = 0x0;
    
    first_four = (uint32_t) dst.addr[3];
    first_four |= ((uint32_t) dst.addr[2]) << 8;
    first_four |= ((uint32_t) dst.addr[1]) << 16;
    first_four |= ((uint32_t) dst.addr[0]) << 24;
    last_two = (uint16_t) dst.addr[5];
    last_two |= ((uint16_t) dst.addr[4]) << 8;

    if(!if_any_mac(dst)) {
	snprintf(filter, max_len, "int32[0]==%"PRIu32"&&int16[4]==%"PRIu16,
	        first_four, last_two);
    }
    if (strlen(filter) == 0) {
	snprintf(filter, max_len, "1");
    }
    return filter;

}

char* build_src_mac_filter(struct eth_addr src)
{
    uint32_t first_four = 0;
    uint16_t last_two = 0;
    size_t max_len = 64;
    char *filter = malloc(max_len);
    filter[0] = 0x0;
    
    first_four = (uint32_t) src.addr[3];
    first_four |= ((uint32_t) src.addr[2]) << 8;
    first_four |= ((uint32_t) src.addr[1]) << 16;
    first_four |= ((uint32_t) src.addr[0]) << 24;
    last_two = (uint16_t) src.addr[5];
    last_two |= ((uint16_t) src.addr[4]) << 8;

    if(!if_any_mac(src)) {
	snprintf(filter, max_len, "int32[6]==%"PRIu32"&&int16[10]==%"PRIu16,
	        first_four, last_two);
    }
    if (strlen(filter) == 0) {
	snprintf(filter, max_len, "1");
    }
    return filter;

}

/*
 * SrcIP filed: size=32bit, offset=12Bytes
 * DstIP field: size=32bit, offset=16Bytes
 */
/**
 * \brief IPv4 filter template
 *
 * Create an IPv4 filter based on a source IP and a destination IP. The source IP is a 32bit field in the
 * IPv4 header starting at offset 12Bytes, the destination IP is also a 32bit field starting at 16Bytes.
 * @param srcip Filter packets coming from this source IP (BFDMUX_IP_ADDR_ANY for any source)
 * @param dstip Filter packets going to this destination IP (BFDMUX_IP_ADDR_ANY for any target)
 * @return A filter string. Caller has to free it after use.
 */
/* FIXME: It is assumed that if it is non-ARP packet then it should be IP packet.
 * So no check in Ethernet packet header to see if it is IP or not. PS */
char           *
build_ipv4_filter(addr_t srcip, addr_t dstip)
{
	size_t          max_len = 64;
	char           *filter = malloc(max_len);
	filter[0] = 0x0;			// strlen(filter) = 0;
	if (srcip != BFDMUX_IP_ADDR_ANY)
		snprintf(filter, max_len, "int32[26]==%"PRIu32"", (uint32_t) srcip);
	if (dstip != BFDMUX_IP_ADDR_ANY) {
		if (srcip != BFDMUX_IP_ADDR_ANY)
			snprintf(filter + strlen(filter), max_len, "&&");
		snprintf(filter + strlen(filter), max_len, "int32[30]==%"PRIu32"",
				 (uint32_t) dstip);
	}

	if (strlen(filter) == 0) {	// srcip == BFDMUX_IP_ADDR_ANY && dstip ==
		// BFDMUX_IP_ADDR_ANY
		snprintf(filter, max_len, "1");
	}
	return filter;
}

/*
 * Protocol field: size=8bit,  offset=9Bytes (in IP Header)
 * ICMP Protocol number: 0x01
 */
/**
 * \brief ICMP filter template
 *
 * Create a generic ICMP packet filter
 * @return A filter sting. Caller has to free it after use.
 */
char *
build_icmp_filter(void)
{
	size_t          max_len = 128;
	char           *filter = malloc(max_len);
	snprintf(filter, max_len, "int8[23]==%u", 0x01);

	return filter;
}

/**
 * \brief ICMP over IPv4 filter template
 *
 * This function build a ICMP over IPv4 filter based on the given arguments
 * using the build_tcp_filter and build_ipv4_filter helper functions.
 * @param srcip Source IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @param dstip Destination IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @return A filter string. Caller has to free it after use.
 */
char *
build_ipv4_icmp_filter(addr_t srcip, addr_t dstip)
{
	char *ip_filter = build_ipv4_filter(srcip, dstip);
	char *icmp_filter = build_icmp_filter();
	int filter_len = strlen(ip_filter) + strlen(icmp_filter) + 2 + 1;
	/* 2: &&, 1: zero byte */

	char *filter = malloc(filter_len);
	snprintf(filter, filter_len, "%s&&%s", ip_filter, icmp_filter);
	free(ip_filter);
	free(icmp_filter);
	return filter;
}




/*
 * Protocol field: size=8bit,  offset=9Bytes (in IP Header)
 * SrcPort field:  size=16bit, offset=20Bytes (with IP Header)
 * DstPort field:  size=16bit, offset=22Bytes (with IP Header)
 * TCP Protocol number: 0x06
 */
/**
 * \brief TCP filter template
 *
 * Create a TCP filter based on the source and destination TCP Port. This filter looks for the TCP protocol number (0x06)
 * in the IP header and sets the 16bit long source port field positioned at offset 20Bytes (with IP header) and the 16bit long
 * destination port filed positioned at offset 22Bytes to the given arguments.
 * @param srcport TCP source port to filter on (PORT_ANY for any port)
 * @param dstport TCP destination port to filter on (PORT_ANY for any port)
 * @return A filter sting. Caller has to free it after use.
 */
char           *
build_tcp_filter(port_t srcport, port_t dstport)
{
	size_t          max_len = 128;
	char           *filter = malloc(max_len);
	snprintf(filter, max_len, "int8[23]==%u", 0x06);

	if (srcport != PORT_ANY)
		snprintf(filter + strlen(filter), max_len, "&&int16[34]==%u",
				 (uint16_t) srcport);
	if (dstport != PORT_ANY) {
		snprintf(filter + strlen(filter), max_len, "&&int16[36]==%u",
				 (uint16_t) dstport);
	}

	return filter;
}

/*
 * Protocol field: size=8bit,  offset=9Bytes (in IP Header)
 * SrcPort field:  size=16bit, offset=20Bytes (with IP Header)
 * DstPort field:  size=16bit, offset=22Bytes (with IP Header)
 * UDP Protocol number: 0x11
 */
/**
 * \brief UDP filter template
 *
 * Create a UDP filter based on the source and destination UDP Port. This filter looks for the UDP protocol number (0x11)
 * in the IP header and sets the 16bit long source port field positioned at offset 20Bytes (with IP header) and the 16bit long
 * destination port filed positioned at offset 22Bytes to the given arguments.
 * @param srcport UDP source port to filter on (PORT_ANY for any port)
 * @param dstport UDP destination port to filter on (PORT_ANY for any port)
 * @return A filter sting. Caller has to free it after use.
 */
char           *
build_udp_filter(port_t srcport, port_t dstport)
{
	size_t          max_len = 128;
	char           *filter = malloc(max_len);
	snprintf(filter, max_len, "int8[23]==%u", 0x11);

	if (srcport != PORT_ANY)
		snprintf(filter + strlen(filter), max_len, "&&int16[34]==%u",
				 (uint16_t) srcport);
	if (dstport != PORT_ANY) {
		snprintf(filter + strlen(filter), max_len, "&&int16[36]==%u",
				 (uint16_t) dstport);
	}

	return filter;
}

/**
 * \brief TCP over IPv4 filter template
 *
 * This function build a TCP over IPv4 filter based on the given arguments using the build_tcp_filter and build_ipv4_filter
 * helper functions.
 * @param srcip Source IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @param dstip Destination IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @param srcport Source TCP port to filter for (PORT_ANY for any)
 * @param dstport Destination TCP port to filter for (PORT_ANY for any)
 * @return A filter string. Caller has to free it after use.
 */
char           *
build_ipv4_tcp_filter(addr_t srcip, addr_t dstip, port_t srcport,
					  port_t dstport)
{
	char           *ip_filter = build_ipv4_filter(srcip, dstip);
	char           *tcp_filter = build_tcp_filter(srcport, dstport);
	int filter_len = strlen(ip_filter) + strlen(tcp_filter) + 2 + 1;	// 2: 
																					// 
	// 
	// &&, 
	// 1: 
	// zero 
	// byte
	char           *filter = malloc(filter_len);
	snprintf(filter, filter_len, "%s&&%s", ip_filter, tcp_filter);
	free(ip_filter);
	free(tcp_filter);
	return filter;
}

/**
 * \brief UDP over IPv4 filter template
 *
 * This function build a UDP over IPv4 filter based on the given arguments using the build_tcp_filter and build_ipv4_filter
 * helper functions.
 * @param srcip Source IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @param dstip Destination IP-Address to filter for (BFDMUX_IP_ADDR_ANY for any)
 * @param srcport Source UDP port to filter for (PORT_ANY for any)
 * @param dstport Destination UDP port to filter for (PORT_ANY for any)
 * @return A filter string. Caller has to free it after use.
 */
char           *
build_ipv4_udp_filter(addr_t srcip, addr_t dstip, port_t srcport,
					  port_t dstport)
{
	char           *ip_filter = build_ipv4_filter(srcip, dstip);
	char           *udp_filter = build_udp_filter(srcport, dstport);
	size_t          filter_len = strlen(ip_filter) + strlen(udp_filter) + 2 + 1;	// 2: 
																					// 
	// 
	// &&, 
	// 1: 
	// zero 
	// byte
	char           *filter = malloc(filter_len);
	snprintf(filter, filter_len, "%s&&%s", ip_filter, udp_filter);
	free(ip_filter);
	free(udp_filter);
	return filter;
}

char* build_ether_dst_ipv4_udp_filter(struct eth_addr dst, addr_t srcip, 
	addr_t dstip, port_t srcport, port_t dstport)
{
    char *mac_filter = build_dst_mac_filter(dst);
    char *ip_filter = build_ipv4_filter(srcip, dstip);
    char *udp_filter = build_udp_filter(srcport, dstport);
    size_t filter_len = strlen(mac_filter) + strlen(ip_filter) + 
	strlen(udp_filter) + 4 + 1;	
    // 4: 2 * &&, 1: zero byte
    char *filter = malloc(filter_len);
    snprintf(filter, filter_len, "%s&&%s&&%s", mac_filter, ip_filter, 
	    udp_filter);
    free(mac_filter);
    free(ip_filter);
    free(udp_filter);
    return filter;

}

char* build_ether_dst_ipv4_tcp_filter(struct eth_addr dst, addr_t srcip, 
	addr_t dstip, port_t srcport, port_t dstport)
{
    char *mac_filter = build_dst_mac_filter(dst);	
    char *ip_filter = build_ipv4_filter(srcip, dstip);
    char *tcp_filter = build_tcp_filter(srcport, dstport);
    size_t filter_len = strlen(mac_filter) + strlen(ip_filter) + 
	strlen(tcp_filter) + 4 + 1;	
    // 4: 2 * &&, 1: zero byte
    char *filter = malloc(filter_len);
    snprintf(filter, filter_len, "%s&&%s&&%s", mac_filter, ip_filter, 
	    tcp_filter);
    free(mac_filter);
    free(ip_filter);
    free(tcp_filter);
    return filter;
}

char* build_ether_src_ipv4_udp_filter(struct eth_addr src, addr_t srcip, 
	addr_t dstip, port_t srcport, port_t dstport)
{
    char *mac_filter = build_src_mac_filter(src);
    char *ip_filter = build_ipv4_filter(srcip, dstip);
    char *udp_filter = build_udp_filter(srcport, dstport);
    size_t filter_len = strlen(mac_filter) + 
	strlen(ip_filter) + strlen(udp_filter) + 4 + 1;	
    // 4: 2 * &&, 1: zero byte
    char *filter = malloc(filter_len);
    snprintf(filter, filter_len, "%s&&%s&&%s", mac_filter, ip_filter, 
	    udp_filter);
    free(mac_filter);
    free(ip_filter);
    free(udp_filter);
    return filter;

}

char* build_ether_src_ipv4_tcp_filter(struct eth_addr src, addr_t srcip, 
	addr_t dstip, port_t srcport, port_t dstport)
{
    char *mac_filter = build_src_mac_filter(src);
    char *ip_filter = build_ipv4_filter(srcip, dstip);
    char *tcp_filter = build_tcp_filter(srcport, dstport);
    size_t filter_len = strlen(mac_filter) +
	strlen(ip_filter) + strlen(tcp_filter) + 4 + 1;	
    // 4: 2 * &&, 1: zero byte
    char *filter = malloc(filter_len);
    snprintf(filter, filter_len, "%s&&%s&&%s", mac_filter, ip_filter, 
	    tcp_filter);
    free(mac_filter);
    free(ip_filter);
    free(tcp_filter);
    return filter;
}

char* build_generic_arp_reply_filter(void)
{
    size_t max_len = 128;
    char *filter = malloc(max_len);
    assert(filter);
    memset(filter, 0, max_len);
    snprintf(filter, max_len, "int16[12]==%u", 0x0806 /* ETHTYPE_ARP */);
		/* FIXME: why following filter is broken?
		 * Question is, what is the correct location of value 0x0002? */
/*    snprintf(filter, max_len, "int16[12]==%u&&int16[20]==%u", (0x0806),
    		(0x0002));
*/

		/* The hardcoded values for protocol field are already
		 * in the network order. */
    return filter;
}

char* build_arp_transmit_filter(struct eth_addr src)
{
    char *mac = build_src_mac_filter(src);
    size_t max_len = 128;
    char *filter = malloc(max_len);
    assert(mac);
    assert(filter);
    memset(filter, 0, max_len);
    snprintf(filter, max_len, "int16[12]==%u&&%s", 0x0806, mac);
    /*FIXME: why these values are not htons()? */
    return filter;
}

