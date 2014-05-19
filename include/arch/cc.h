/**
 * \file
 * \brief lwIP architecture configuration file for Barrelfish.
 */

/*
 * Copyright (c) 2007, 2008, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CC_H
#define CC_H

#include <string.h>
#include <stdint.h>
#include <machine/endian.h>

/* Define platform endianness */
#ifndef BYTE_ORDER
#define BYTE_ORDER LITTLE_ENDIAN
#endif /* BYTE_ORDER */

typedef uint8_t         u8_t;
typedef int8_t          s8_t;
typedef uint16_t        u16_t;
typedef int16_t         s16_t;
typedef uint32_t        u32_t;
typedef int32_t         s32_t;
typedef uintptr_t       mem_ptr_t;

// (sn)printf() format strings for integer types
#define U16_F "hu"
#define S16_F "hd"
#define X16_F "hx"
#define U32_F "u"
#define S32_F "d"
#define X32_F "x"

/* Compiler hints for packing structures */
#if __GNUC__ >= 4
/* GCC 4.1 doesn't like '__attribute__((packed))' after a field declaration */
#define PACK_STRUCT_FIELD(x) x
#define PACK_STRUCT_STRUCT __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END
#else
#define PACK_STRUCT_FIELD(x) x __attribute__((packed))
#define PACK_STRUCT_STRUCT __attribute__((packed))
#define PACK_STRUCT_BEGIN
#define PACK_STRUCT_END
#endif /* __GNUC__ */

#define INLINE  inline

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>

#define LWIP_PLATFORM_DIAG(x)   do {printf x;} while(0)
#define LWIP_PLATFORM_ASSERT(x) do {printf("Assertion \"%s\" failed at line %d in %s\n", \
                                     x, __LINE__, __FILE__); fflush(NULL); abort();} while(0)

// Declare [nh]to[nh][ls]() in this header, so we don't depend on liblwip.a
// when using them.

#define LWIP_PLATFORM_BYTESWAP 1

// Use optimized version if available
#ifdef __htonl

#define LWIP_PLATFORM_HTONS(x) __htons(x)
#define LWIP_PLATFORM_HTONL(x) __htonl(x)
#define LWIP_PLATFORM_NTOHS(x) __ntohs(x)
#define LWIP_PLATFORM_NTOHL(x) __ntohl(x)

#else

/**
 * Convert an u16_t from host- to network byte order.
 *
 * @param n u16_t in host byte order
 * @return n in network byte order
 */
static inline u16_t barrelfish_htons(u16_t n)
{
    return ((n & 0xff) << 8) | ((n & 0xff00) >> 8);
}

/**
 * Convert an u16_t from network- to host byte order.
 *
 * @param n u16_t in network byte order
 * @return n in host byte order
 */
static inline u16_t barrelfish_ntohs(u16_t n)
{
    return barrelfish_htons(n);
}

/**
 * Convert an u32_t from host- to network byte order.
 *
 * @param n u32_t in host byte order
 * @return n in network byte order
 */
static inline u32_t barrelfish_htonl(u32_t n)
{
    return ((n & 0xff) << 24) |
      ((n & 0xff00) << 8) |
      ((n & 0xff0000UL) >> 8) | ((n & 0xff000000UL) >> 24);
}

/**
 * Convert an u32_t from network- to host byte order.
 *
 * @param n u32_t in network byte order
 * @return n in host byte order
 */
static inline u32_t barrelfish_ntohl(u32_t n)
{
    return barrelfish_htonl(n);
}

#define LWIP_PLATFORM_HTONS(x) barrelfish_htons(x)
#define LWIP_PLATFORM_HTONL(x) barrelfish_htonl(x)
#define LWIP_PLATFORM_NTOHS(x) barrelfish_ntohs(x)
#define LWIP_PLATFORM_NTOHL(x) barrelfish_ntohl(x)

#endif

#endif
