/**
 * \file
 * \brief Definitions of standard Barrelfish userland types.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_TYPES_H
#define BARRELFISH_TYPES_H

#include <barrelfish_kpi/types.h>

/// Cycle count type
#ifdef __i386__
typedef uint64_t cycles_t; // rdtsc() is 64-bit on i386
#define PRIuCYCLES PRIu64
#define PRIxCYCLES PRIx64
#define PRIXCYCLES PRIX64
#else
typedef size_t cycles_t;
#define PRIuCYCLES "zu"
#define PRIxCYCLES "zx"
#define PRIXCYCLES "zX"
#endif

typedef uint32_t iref_t;

#define NULL_IREF 0

#define PRIxIREF PRIx32
#define PRIuIREF PRIu32

#define BYTES_IN_IREF (sizeof(iref_t) / sizeof(uint8_t))

/// Relative delay time (in microseconds)
typedef uint64_t delayus_t;
#define PRIuDELAYUS PRIu64
#define PRIxDELAYUS PRIx64
#define PRIXDELAYUS PRIX64

/// PCI addresses
typedef uint64_t pciaddr_t;
#define PRIxPCIADDR PRIx64
#define PRIuPCIADDR PRIu64
/// PCI size
typedef uint64_t pcisize_t;
#define PRIxPCISIZE PRIx64
#define PRIuPCISIZE PRIu64

/// MAX_CPUS is a legacy name for max core ID
#ifdef MAX_CPUS
STATIC_ASSERT(MAX_CPUS == MAX_COREID, "MAX_CPUS != MAX_COREID")
#else
#define MAX_CPUS MAX_COREID
#endif

/** \brief Core ID bitmask type
 *
 * XXX: the coremask_t type is deprecated and should not be used for new code.
 * For a replacement, see <barrelfish/coreset.h>
 */

#ifndef DIVIDE_ROUND_UP
#define DIVIDE_ROUND_UP(n, size)    (((n) + (size) - 1) / (size))
#define _DIVIDE_ROUND_UP_DEFINED 1
#endif

typedef uint64_t _coremask_word_t;
#define _COREMASK_BITS_PER_WORD (sizeof(_coremask_word_t) * NBBY)
#define _COREMASK_WORDS DIVIDE_ROUND_UP(MAX_COREID, _COREMASK_BITS_PER_WORD)

typedef struct {
    _coremask_word_t bits[_COREMASK_WORDS];
} coremask_t;

#ifdef _DIVIDE_ROUND_UP_DEFINED
#undef DIVIDE_ROUND_UP
#undef _DIVIDE_ROUND_UP_DEFINED
#endif

#endif // TYPES_H
