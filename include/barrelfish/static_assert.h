/**
 * \file
 * \brief Top-level header for convenient inclusion of standard
 * libbarrelfish headers.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_STATIC_ASSERT_H
#define LIBBARRELFISH_STATIC_ASSERT_H

/*
 * Variant based on Padraig Brady's implementation
 * http://www.pixelbeat.org/programming/gcc/static_assert.html
 */

#define ASSERT_CONCAT_(a, b) a##b
#define ASSERT_CONCAT(a, b) ASSERT_CONCAT_(a, b)

#ifdef __COUNTER__
  /* Microsoft compiler. */
  #define STATIC_ASSERT(e,m) \
    enum { ASSERT_CONCAT(static_assert_, __COUNTER__) = 1/(!!(e)) }
#else
  /* This can't be used twice on the same line so ensure if using in headers
   * that the headers are not included twice (by wrapping in #ifndef...#endif)
   * Note it doesn't cause an issue when used on same line of separate modules
   * compiled with gcc -combine -fwhole-program.  */
  #define STATIC_ASSERT(e,m) \
    enum { ASSERT_CONCAT(assert_line_, __LINE__) = 1/(!!(e)) }
#endif

#define STATIC_ASSERT_SIZEOF(tname,n)                           \
    STATIC_ASSERT(sizeof(tname) == n,                           \
                  ASSERT_CONCAT("Size mismatch:", tname)        \
                 )

#define sa_offsetof(x,y) ((size_t)(((void*)&(((x*)0)->y)) - (void*)(x*)0))

#define STATIC_ASSERT_OFFSETOF(tname, field, offset)            \
    STATIC_ASSERT(sa_offsetof(tname, field) == offset,          \
                  ASSERT_CONCAT("Offset mismatch:", field)      \
                 )

#endif // LIBBARRELFISH_STATIC_ASSERT_H

