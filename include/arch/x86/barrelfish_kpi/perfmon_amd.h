/**
 * \file
 * \brief AMD performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_PERFMON_H
#define BARRELFISH_KPI_PERFMON_H

#define EVENT_L2_LINES_IN       0x24

/*
 * The following are derived from the AMD "BIOS and Kernel Developer's
 * Guide (family 10h)", Section 3.14.
 */

// 3.14.3 Data Cache Events

// Instructions retired
#define EVENT_AMD_INSTRUCTIONS_RETIRED          0xC0

// DRAM Access
#define EVENT_AMD_DRAM_ACCESS                   0xE0

// L1 DTLB MISS AND L2 DTLB HIT
#define EVENT_AMD_L1DTLB_MISS_L2_DTLB_HIT       0x45

// L1 DTLB MISS AND L2 DTLB MISS
#define EVENT_AMD_L1DTLB_MISS_L2_DTLB_MISS      0x46

// Data Cache Misses
#define EVENT_AMD_DATA_CACHE_MISSES             0x41

// Interrupts taken
#define EVENT_AMD_INTERRUPTS_TAKEN              0xCF

// Dispatch Stalls
#define EVENT_AMD_DISPATCH_STALLS               0xD1

// Dispatch Stall Reorder buffer full
#define EVENT_AMD_DISPATCH_STALL_REORDER_BUF_FULL 0xD5

// Check if SMM came up
#define EVENT_AMD_SMM_INTERRUPTS                  0x2B

// Data Cache Refills from L2 or Northbridge
#define EVENT_AMD_DATA_CACHE_REFILLS_L2_NB      0x42

#define UMASK_AMD_DATA_CACHE_REFILLS_NB                 0x1
#define UMASK_AMD_DATA_CACHE_REFILLS_L2_SHARED          0x2
#define UMASK_AMD_DATA_CACHE_REFILLS_L2_EXCLUSIVE       0x4
#define UMASK_AMD_DATA_CACHE_REFILLS_L2_OWNED           0x8
#define UMASK_AMD_DATA_CACHE_REFILLS_L2_MODIFIED        0x10

// Data Cache Refills from Northbridge
#define EVENT_AMD_DATA_CACHE_REFILLS_NB         0x43

// UMASK: See MOESI generic count UMASK_COUNT_*

// Data Cache Lines Evicted
#define EVENT_AMD_DATA_CACHE_LINES_EVICTED      0x44

// MOESI generic UMASK_COUNT_*, and
#define UMASK_AMD_DATA_CACHE_EVICTED_PREFETCHNTA        0x20
#define UMASK_AMD_DATA_CACHE_EVICTED_NO_PREFETCHNTA     0x40

// Misaligned accesses
#define EVENT_AMD_MISALIGNED_ACCESSES           0x47

// DCACHE misses by locked instructions
#define EVENT_AMD_DCACHE_MISS_LOCKED            0x4c

#define UMASK_AMD_DCACHE_MISS_LOCKED            0x2

// 3.14.4 L2 Cache and System Interface Events

// Memory requests by type
#define EVENT_AMD_MEM_REQ_TYPES                 0x65

#define UMASK_AMD_MEM_REQ_TYPE_UC               0x1
#define UMASK_AMD_MEM_REQ_TYPE_WC               0x2
#define UMASK_AMD_MEM_REQ_TYPE_SS               0x80

// Northbridge read responses by coherency state
#define EVENT_AMD_NB_RESPONSE_STATE             0x6c

#define UMASK_AMD_NB_RESPONSE_STATE_EXCLUSIVE   0x1
#define UMASK_AMD_NB_RESPONSE_STATE_MODIFIED    0x2
#define UMASK_AMD_NB_RESPONSE_STATE_SHARED      0x4
#define UMASK_AMD_NB_RESPONSE_STATE_DATA_ERROR  0x10

// Octwords written to system
#define EVENT_AMD_OCTWORDS_TO_SYSTEM            0x6d

#define UMASK_AMD_OCTWORDS_WRITE_TRANSFER       0x1

// Requests to L2 Cache
#define EVENT_AMD_L2_REQUESTS                   0x7d

#define UMASK_AMD_L2_REQUEST_IC_FILL            0x1
#define UMASK_AMD_L2_REQUEST_DC_FILL            0x2
#define UMASK_AMD_L2_REQUEST_TLB_FILL           0x4
#define UMASK_AMD_L2_REQUEST_TAG_SNOOP          0x8
#define UMASK_AMD_L2_REQUEST_CANCELLED          0x10
#define UMASK_AMD_L2_REQUEST_HW_PREFETCH_DC     0x20

// L2 Cache Misses
#define EVENT_AMD_L2_CACHE_MISSES               0x7e

#define UMASK_AMD_L2_CACHE_MISS_IC_FILL                 0x1
#define UMASK_AMD_L2_CACHE_MISS_DC_FILL                 0x2
#define UMASK_AMD_L2_CACHE_MISS_TLB_PAGE_TABLE_WALK     0x4
#define UMASK_AMD_L2_CACHE_MISS_HW_PREFETCH_DC          0x8

// L2 Fill/Writeback
#define EVENT_AMD_L2_FILL_WRITEBACK             0x7f

#define UMASK_AMD_L2_FILL_WRITEBACK_FILLS       0x1
#define UMASK_AMD_L2_FILL_WRITEBACK_WRITEBACKS  0x2

// Instruction Cache Events
#define EVENT_AMD_INSTRUCTION_CACHE_MISSES      0x81

// 3.14.7 Memory Controller Events

// CPU/IO Requests to Memory/IO
// Cache Block Commands
// Probe Responses and Upstream Requests
// Memory Controller Requests

// 3.14.8 Crossbar Events

// CPU to DRAM Requests to Target Node
// CPU Read Command Latency to Target Node 0-3
// CPU Read Command Requests to Target Node 0-3
// CPU Read Command Latency to Target Node 4-7
// CPU Read Command Requests to Target Node 4-7
// CPU Command Latency to Target Node 0-3/4-7
// CPU Requests to Target Node 0-3/4-7

// 3.14.9 Link Events

#define EVENT_AMD_HYPERTRANSPORT_LINK0_BANDWIDTH        0xf6
#define EVENT_AMD_HYPERTRANSPORT_LINK1_BANDWIDTH        0xf7
#define EVENT_AMD_HYPERTRANSPORT_LINK2_BANDWIDTH        0xf8
#define EVENT_AMD_HYPERTRANSPORT_LINK3_BANDWIDTH        0x1f9

// HyperTransport Link 0 Transmit Bandwidth
// HyperTransport Link 1 Transmit Bandwidth
// HyperTransport Link 2 Transmit Bandwidth
// HyperTransport Link 3 Transmit Bandwidth

// 3.14.10 L3 Cache Events

// Read Request to L3 Cache

// L3 Cache Misses
#define EVENT_AMD_L3_CACHE_MISSES               0x4e1

#define UMASK_AMD_L3_MISSES_READ_BLOCK_EXCLUSIVE        0x1
#define UMASK_AMD_L3_MISSES_READ_BLOCK_SHARED           0x2
#define UMASK_AMD_L3_MISSES_READ_BLOCK_MODIFY           0x4
#define UMASK_AMD_L3_MISSES_ALL                         0x7
// See UMASK_AMD_CORE_*_SELECT for core select

// L3 Fills caused by L2 evictions
#define EVENT_AMD_L3_FILLS_L2_EVICT             0x4e2

#define UMASK_AMD_L3_FILLS_L2_EVICT_SHARED      0x1
#define UMASK_AMD_L3_FILLS_L2_EVICT_EXCLUSIVE   0x2
#define UMASK_AMD_L3_FILLS_L2_EVICT_OWNED       0x4
#define UMASK_AMD_L3_FILLS_L2_EVICT_MODIFIED    0x8
// See UMASK_AMD_CORE_*_SELECT for core select

// L3 Evictions
#define EVENT_AMD_L3_EVICTIONS                  0x4e3

#define UMASK_AMD_L3_EVICTIONS_SHARED      0x1
#define UMASK_AMD_L3_EVICTIONS_EXCLUSIVE   0x2
#define UMASK_AMD_L3_EVICTIONS_OWNED       0x4
#define UMASK_AMD_L3_EVICTIONS_MODIFIED    0x8

// HyperTransport

#define EVENT_AMD_HYPERTRANSPORT_LINK0_BANDWIDTH        0xf6
#define EVENT_AMD_HYPERTRANSPORT_LINK1_BANDWIDTH        0xf7
#define EVENT_AMD_HYPERTRANSPORT_LINK2_BANDWIDTH        0xf8

///////////////////////////////////////////////////////////////

// Generic UMASKs
#define UMASK_ALL_CORES         (3 << 6)
#define UMASK_THIS_CORE         (1 << 6)

#define UMASK_ALL_AGENTS        (1 << 5)

#define UMASK_ALL_INCLUSIVE     (3 << 4)
#define UMASK_PREFETCH_ONLY     (1 << 4)
#define UMASK_NO_PREFETCH       (0 << 4)

// Generic UMASK for MOESI counts
#define UMASK_COUNT_MODIFIED    0x10
#define UMASK_COUNT_OWNED       0x8
#define UMASK_COUNT_EXCLUSIVE   0x4
#define UMASK_COUNT_SHARED      0x2
#define UMASK_COUNT_INVALID     0x1
#define UMASK_COUNT_ALL         (UMASK_COUNT_INVALID | UMASK_COUNT_SHARED | \
                                 UMASK_COUNT_EXCLUSIVE | UMASK_COUNT_OWNED | \
                                 UMASK_COUNT_MODIFIED)

// Generic UMASK for core select
#define UMASK_AMD_CORE_0_SELECT               0x10
#define UMASK_AMD_CORE_1_SELECT               0x20
#define UMASK_AMD_CORE_2_SELECT               0x40
#define UMASK_AMD_CORE_3_SELECT               0x80

#define UMASK_AMD_L2_MISSES     0x1

#endif
