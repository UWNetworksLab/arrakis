/**
 * \file
 * \brief Definitions of standard Barrelfish types.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_TYPES_H
#define BARRELFISH_KPI_TYPES_H

/* Number of bits in a byte */
#define NBBY            8

/// Capability NULL pointer
#define CPTR_NULL       ((capaddr_t)0)

#ifndef __ASSEMBLER__

#include <stdint.h>
#include <stddef.h>
#include <inttypes.h>
#include <barrelfish/static_assert.h>

/* natural machine word size */
typedef signed int      word_t;
typedef unsigned int    uword_t;

/* Local physical address. */
typedef uintptr_t lpaddr_t;
#define PRIuLPADDR PRIuPTR
#define PRIxLPADDR PRIxPTR

/* Global (system-wide) physical address, currently 64 bits */
typedef uint64_t genpaddr_t;
#define PRIuGENPADDR PRIu64
#define PRIxGENPADDR PRIx64

/* Global (system-wide) size type, currently 64 bits */
typedef uint64_t gensize_t;
#define PRIuGENSIZE PRIu64
#define PRIxGENSIZE PRIx64

/* Local virtual address */
typedef uintptr_t lvaddr_t;
#define PRIuLVADDR PRIuPTR
#define PRIxLVADDR PRIxPTR

/* Global (system-wide) virtual address, currently 64 bits */
typedef uint64_t genvaddr_t;
#define PRIuGENVADDR PRIu64
#define PRIxGENVADDR PRIx64

/* A virtual address in a foreign address space. */
typedef genvaddr_t forvaddr_t;
#define PRIuFORVADDR PRIuGENVADDR
#define PRIxFORVADDR PRIxGENVADDR

/* capability addresses */
typedef uint32_t capaddr_t;
#define PRIuCADDR PRIu32
#define PRIxCADDR PRIx32

/// Number of bits in a cspace address
#define CPTR_BITS       (sizeof(capaddr_t) * NBBY)

/* slot number */
typedef capaddr_t cslot_t;
#define PRIuCSLOT PRIuCADDR
#define PRIxCSLOT PRIxCADDR

/* core id type */
// This is also used as a count of cores, so the maximum core ID must be
// one less than the maximum value representable here.
typedef uint8_t	coreid_t;
#define PRIuCOREID  PRIu8
#define PRIxCOREID  PRIx8

#ifndef MAX_COREID
#define MAX_COREID  255 // limit of coreid_t type (see comment above)
#endif

/* ID capability ID */
// Returned if IDCmd_Identify is invoked on an ID capability.
typedef uint64_t idcap_id_t;
#define PRIuIDCAPID PRIu64
#define PRIxIDCAPID PRIx64

/* Resource id */
typedef uint32_t rsrcid_t;
#define PRIuRSRCID  PRIu32
#define PRIxRSRCID  PRIx32

/* Domain ID */
typedef uint32_t domainid_t;
#define PRIuDOMAINID    PRIu32
#define PRIxDOMAINID    PRIx32

/* Performance counter */
// Performance counter
typedef uint8_t perfmon_counter_t;
// Performance event
typedef uint64_t perfmon_event_t;
// Performance mask
typedef uint64_t perfmon_mask_t;

/// Absolute system time (in microseconds)
typedef uint64_t systime_t;
#define PRIuSYSTIME PRIu64
#define PRIxSYSTIME PRIx64
#define PRIXSYSTIME PRIX64

#define PRIxERRV    PRIxPTR
#define PRIuERRV    PRIuPTR

#endif // __ASSEMBLER__

#endif // BARRELFISH_KPI_TYPES_H
