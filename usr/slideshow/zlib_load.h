/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ZLIB_LOAD_H
#define ZLIB_LOAD_H

int zlib_load(void *dst, size_t dstsize, size_t *size, void *src,
              size_t srcsize);

#endif
