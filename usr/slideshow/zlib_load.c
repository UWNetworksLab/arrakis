/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <zlib.h>

#include "zlib_load.h"

#define CHECK_ERR(err, msg, errmsg)                     \
    {                                                                   \
        if (err != Z_OK) {                                              \
            fprintf(stderr, "slideshow: %s error: %s (%d)\n", msg, errmsg, err); \
            exit(EXIT_FAILURE);                                         \
        }                                                               \
    }

int zlib_load(void *dst, size_t dstsize, size_t *size, void *src,
              size_t srcsize)
{
    int err;
    z_stream d_stream; /* decompression stream */

    d_stream.zalloc = (alloc_func)0;
    d_stream.zfree = (free_func)0;
    d_stream.opaque = (voidpf)0;
    d_stream.next_in  = src;
    d_stream.avail_in = srcsize;
    d_stream.avail_out = dstsize;
    d_stream.next_out = dst;

    err = inflateInit2(&d_stream, MAX_WBITS + 32);
    CHECK_ERR(err, "inflateInit", d_stream.msg);

    while(d_stream.total_out < dstsize && d_stream.total_in < srcsize) {
        err = inflate(&d_stream, Z_NO_FLUSH);
        if(err == Z_STREAM_END) {
            break;
        }
        CHECK_ERR(err, "inflate", d_stream.msg);
    }

    *size = d_stream.total_out;

    err = inflateEnd(&d_stream);
    CHECK_ERR(err, "inflateEnd", d_stream.msg);

    return 0;
}
