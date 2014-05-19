/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef ARRANET_DEBUG_H
#define ARRANET_DEBUG_H

//#define DEBUG_LATENCIES

//#include <lwip/udp.h>
#include <arranet_impl.h>
#include <barrelfish/sys_debug.h>

#define POSIX_TRANSA    1000

#define MIN(a,b)        ((a) < (b) ? (a) : (b))

#define	timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)

    /**
     * Definition of the header structure for a request packet.
     * See section 2
     */
    typedef union {
        struct {
            uint8_t magic;
            uint8_t opcode;
            uint16_t keylen;
            uint8_t extlen;
            uint8_t datatype;
            uint16_t reserved;
            uint32_t bodylen;
            uint32_t opaque;
            uint64_t cas;
        } request;
        uint8_t bytes[24];
    } protocol_binary_request_header;

    /**
     * Definition of the header structure for a response packet.
     * See section 2
     */
    typedef union {
        struct {
            uint8_t magic;
            uint8_t opcode;
            uint16_t keylen;
            uint8_t extlen;
            uint8_t datatype;
            uint16_t status;
            uint32_t bodylen;
            uint32_t opaque;
            uint64_t cas;
        } response;
        uint8_t bytes[24];
    } protocol_binary_response_header;

    /**
     * Definition of a request-packet containing no extras
     */
    typedef union {
        struct {
            protocol_binary_request_header header;
        } message;
        uint8_t bytes[sizeof(protocol_binary_request_header)];
    } protocol_binary_request_no_extras;

    /**
     * Definition of a response-packet containing no extras
     */
    typedef union {
        struct {
            protocol_binary_response_header header;
        } message;
        uint8_t bytes[sizeof(protocol_binary_response_header)];
    } protocol_binary_response_no_extras;

typedef struct {
  uint16_t req_id;
  uint16_t seq_id;
  uint16_t n_data;
  uint16_t extras;
} memcached_udp_header;

#define UDP_HEADLEN sizeof(memcached_udp_header)

static inline int get_time(void)
{
    uint64_t now = rdtsc();
    static uint64_t tscperms = 0;

    if(tscperms == 0) {
        errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
        assert(err_is_ok(err));
        assert(tscperms >= 100000);
    }

    uint64_t tod_us = now / (tscperms / 100000);

    return tod_us;
}

#endif
