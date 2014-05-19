/**
 * @file
 * Dynamic pool memory manager
 *
 * lwIP has dedicated pools for many structures (netconn, protocol control blocks,
 * packet buffers, ...). All these pools are managed here.
 */

/*
 * Copyright (c) 2001-2004 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */

#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/udp.h"
#include "lwip/raw.h"
#include "lwip/tcp.h"
#include "lwip/igmp.h"
#include "lwip/api.h"
#include "lwip/api_msg.h"
#include "lwip/tcpip.h"
#include "lwip/sys.h"
#include "lwip/stats.h"
#include "netif/etharp.h"
#include "lwip/ip_frag.h"

#include <string.h>

#if !MEMP_MEM_MALLOC            /* don't build if not configured for use in lwipopts.h */

struct memp {
    struct memp *next;
#if MEMP_OVERFLOW_CHECK
    const char *file;
    int line;
#endif                          /* MEMP_OVERFLOW_CHECK */
};

#if MEMP_OVERFLOW_CHECK
/* if MEMP_OVERFLOW_CHECK is turned on, we reserve some bytes at the beginning
 * and at the end of each element, initialize them as 0xcd and check
 * them later. */
/* If MEMP_OVERFLOW_CHECK is >= 2, on every call to memp_malloc or memp_free,
 * every single element in each pool is checked!
 * This is VERY SLOW but also very helpful. */
/* MEMP_SANITY_REGION_BEFORE and MEMP_SANITY_REGION_AFTER can be overridden in
 * lwipopts.h to change the amount reserved for checking. */
#ifndef MEMP_SANITY_REGION_BEFORE
#define MEMP_SANITY_REGION_BEFORE  16
#endif                          /* MEMP_SANITY_REGION_BEFORE */
#if MEMP_SANITY_REGION_BEFORE > 0
#define MEMP_SANITY_REGION_BEFORE_ALIGNED    LWIP_MEM_ALIGN_SIZE(MEMP_SANITY_REGION_BEFORE)
#else
#define MEMP_SANITY_REGION_BEFORE_ALIGNED    0
#endif                          /* MEMP_SANITY_REGION_BEFORE */
#ifndef MEMP_SANITY_REGION_AFTER
#define MEMP_SANITY_REGION_AFTER   16
#endif                          /* MEMP_SANITY_REGION_AFTER */
#if MEMP_SANITY_REGION_AFTER > 0
#define MEMP_SANITY_REGION_AFTER_ALIGNED     LWIP_MEM_ALIGN_SIZE(MEMP_SANITY_REGION_AFTER)
#else
#define MEMP_SANITY_REGION_AFTER_ALIGNED     0
#endif                          /* MEMP_SANITY_REGION_AFTER */

/* MEMP_SIZE: save space for struct memp and for sanity check */
#define MEMP_SIZE          (LWIP_MEM_ALIGN_SIZE(sizeof(struct memp)) + MEMP_SANITY_REGION_BEFORE_ALIGNED)
#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x) + MEMP_SANITY_REGION_AFTER_ALIGNED)

#else                           /* MEMP_OVERFLOW_CHECK */

/* No sanity checks
 * We don't need to preserve the struct memp while not allocated, so we
 * can save a little space and set MEMP_SIZE to 0.
 */
#define MEMP_SIZE           0
#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x))

#endif                          /* MEMP_OVERFLOW_CHECK */

/** This array holds the first free element of each pool.
 *  Elements form a linked list. */
static struct memp *memp_tab[MEMP_MAX];

#else                           /* MEMP_MEM_MALLOC */

#define MEMP_ALIGN_SIZE(x) (LWIP_MEM_ALIGN_SIZE(x))

#endif                          /* MEMP_MEM_MALLOC */

/** This array holds the element sizes of each pool. */
#if !MEM_USE_POOLS && !MEMP_MEM_MALLOC
static
#endif
const u16_t memp_sizes[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  MEMP_ALIGN_SIZE(size),
#include "lwip/memp_std.h"
};

/**
 * Array of indexes of pools, ordered by their element size in descending
 * order.
 */
static size_t memp_sorted[MEMP_MAX];

#if !MEMP_MEM_MALLOC            /* don't build if not configured for use in lwipopts.h */

/** This array holds the number of elements in each pool. */
static const u16_t memp_num[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  (num),
#include "lwip/memp_std.h"
};


/** This array holds a textual description of each pool. */
#ifdef LWIP_DEBUG
static const char *memp_desc[MEMP_MAX] = {
#define LWIP_MEMPOOL(name,num,size,desc)  (desc),
#include "lwip/memp_std.h"
};
#endif                          /* LWIP_DEBUG */

#if 0
static u8_t memp_memory_orig[MEM_ALIGNMENT - 1
#define LWIP_MEMPOOL(name,num,size,desc) + ( (num) * (MEMP_SIZE + MEMP_ALIGN_SIZE(size) ) )
#include "lwip/memp_std.h"
  ];
#endif

/* This is the size of memory required by all the pools. */
/*static const size_t memp_memory_size = (MEM_ALIGNMENT - 1
#define LWIP_MEMPOOL(name,num,size,desc) + ( (num) * (MEMP_SIZE + MEMP_ALIGN_SIZE(size) ) )
#include "lwip/memp_std.h"
  );*/

static u8_t *memp_memory = 0;
u8_t *mem_barrelfish_alloc(uint8_t buf_index, uint32_t size);
u8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size);

#if MEMP_SANITY_CHECK
/**
 * Check that memp-lists don't form a circle
 */
static int memp_sanity(void)
{
    s16_t i, c;
    struct memp *m, *n;

    for (i = 0; i < MEMP_MAX; i++) {
        for (m = memp_tab[i]; m != NULL; m = m->next) {
            c = 1;
            for (n = memp_tab[i]; n != NULL; n = n->next) {
                if (n == m && --c < 0) {
                    return 0;
                }
            }
        }
    }
    return 1;
}
#endif                          /* MEMP_SANITY_CHECK */
#if MEMP_OVERFLOW_CHECK
/**
 * Check if a memp element was victim of an overflow
 * (e.g. the restricted area after it has been altered)
 *
 * @param p the memp element to check
 * @param memp_size the element size of the pool p comes from
 */
static void memp_overflow_check_element(struct memp *p, u16_t memp_size)
{
    u16_t k;
    u8_t *m;

#if MEMP_SANITY_REGION_BEFORE_ALIGNED > 0
    m = (u8_t *) p + MEMP_SIZE - MEMP_SANITY_REGION_BEFORE_ALIGNED;
    for (k = 0; k < MEMP_SANITY_REGION_BEFORE_ALIGNED; k++) {
        if (m[k] != 0xcd) {
            LWIP_ASSERT("detected memp underflow!", 0);
        }
    }
#endif
#if MEMP_SANITY_REGION_AFTER_ALIGNED > 0
    m = (u8_t *) p + MEMP_SIZE + memp_size;
    for (k = 0; k < MEMP_SANITY_REGION_AFTER_ALIGNED; k++) {
        if (m[k] != 0xcd) {
            LWIP_ASSERT("detected memp overflow!", 0);
        }
    }
#endif
}

/**
 * Do an overflow check for all elements in every pool.
 *
 * @see memp_overflow_check_element for a description of the check
 */
static void memp_overflow_check_all(void)
{
    u16_t i, j;
    struct memp *p;

    p = LWIP_MEM_ALIGN(memp_memory);
    for (i = 0; i < MEMP_MAX; ++i) {
        p = p;
        for (j = 0; j < memp_num[i]; ++j) {
            memp_overflow_check_element(p, memp_sizes[i]);
            p =
              (struct memp *) ((u8_t *) p + MEMP_SIZE + memp_sizes[i] +
                               MEMP_SANITY_REGION_AFTER_ALIGNED);
        }
    }
}

/**
 * Initialize the restricted areas of all memp elements in every pool.
 */
static void memp_overflow_init(void)
{
    u16_t i, j;
    struct memp *p;
    u8_t *m;

    p = LWIP_MEM_ALIGN(memp_memory);
    for (i = 0; i < MEMP_MAX; ++i) {
        p = p;
        for (j = 0; j < memp_num[i]; ++j) {
#if MEMP_SANITY_REGION_BEFORE_ALIGNED > 0
            m = (u8_t *) p + MEMP_SIZE - MEMP_SANITY_REGION_BEFORE_ALIGNED;
            memset(m, 0xcd, MEMP_SANITY_REGION_BEFORE_ALIGNED);
#endif
#if MEMP_SANITY_REGION_AFTER_ALIGNED > 0
            m = (u8_t *) p + MEMP_SIZE + memp_sizes[i];
            memset(m, 0xcd, MEMP_SANITY_REGION_AFTER_ALIGNED);
#endif
            p =
              (struct memp *) ((u8_t *) p + MEMP_SIZE + memp_sizes[i] +
                               MEMP_SANITY_REGION_AFTER_ALIGNED);
        }
    }
}
#endif                          /* MEMP_OVERFLOW_CHECK */

static u16_t pbuf_pool_counter = 0;


#if MEMP_OVERFLOW_CHECK
#error   "Overflow checking is not supported at the moment, as it screws up "\
         "our alignment assumptions"
#endif

/**
 * Comparator function for ordering pools in descending order of their element
 * sizes.
 */
static int memp_size_comp(const void *a, const void *b)
{
    const size_t *as = a, *bs = b;
    if (memp_sizes[*bs] < memp_sizes[*as]) {
        return -1;
    } else if (memp_sizes[*bs] == memp_sizes[*as]) {
        return 0;
    } else {
        return 1;
    }
}

/** Initialize memp_sorted */
static void initialize_memp_sorted(void)
{
    size_t i;
    for (i = 0; i < MEMP_MAX; i++) {
        memp_sorted[i] = i;
    }
    qsort(memp_sorted, MEMP_MAX, sizeof(size_t), memp_size_comp);
}

/**
 * Try and figure out how much memory is needed for the pool. This is not
 * completly trivial as we try to align the pool elements to their size.
 *
 * @param max_el_size Pointer to location where maximal element size should be
 *                    stored (first element size).
 */
static size_t memp_memory_needed(void)
{
    size_t memp_memory_size = 0;
    size_t cursz, curtotal, i, idx;
    size_t maxsz = 1;

    for (i = 0; i < MEMP_MAX; ++i) {
        idx = memp_sorted[i];
        cursz = memp_sizes[idx];
        curtotal = memp_num[idx];
        if (memp_memory_size % cursz != 0) {
            curtotal += cursz - (memp_memory_size % cursz);
        }
        memp_memory_size += curtotal;

        if (cursz > maxsz) {
            maxsz = cursz;
        }
    }

    // Add a little more so we can align the buffer
    memp_memory_size += maxsz;

    return memp_memory_size;
}

/**
 * Initialize this module.
 *
 * Carves out memp_memory into linked lists for each pool-type.
 */
void memp_init(void)
{
    size_t memp_memory_size;
//    printf("memp_init: allocating %zx memory for index %d\n", memp_memory_size,
//           RX_BUFFER_ID);

    initialize_memp_sorted();
    memp_memory_size = memp_memory_needed();

    memp_memory =
      mem_barrelfish_alloc(RX_BUFFER_ID, memp_memory_size);
    if (memp_memory == 0) {
        fprintf(stderr, "could not allocate memory");
        abort();
    }

//    printf("memp_init: allocated memory is at VA [%p]\n", memp_memory);

    memp_initialize_pbuf_list();
    mem_barrelfish_register_buf(RX_BUFFER_ID, memp_memory_size);
}


void memp_initialize_pbuf_list(void)
{
    assert(memp_memory != NULL);
    struct memp *memp;
    uintptr_t uimemp;
    u16_t k, i, j;
    for (i = 0; i < MEMP_MAX; ++i) {
        MEMP_STATS_AVAIL(used, i, 0);
        MEMP_STATS_AVAIL(max, i, 0);
        MEMP_STATS_AVAIL(err, i, 0);
        MEMP_STATS_AVAIL(avail, i, memp_num[i]);
    }
    memp = LWIP_MEM_ALIGN(memp_memory);
/*    printf("memp_init: total types of pools %d\n", MEMP_MAX );
    printf("memp_init: total types of pools %d, memp_mem %p\n",
            MEMP_MAX, memp_memory);
    printf("memp_init: total types of pools %d, memp %p\n", MEMP_MAX, memp);
*/
    memp->next = NULL;
    /* for every pool: */
    for (k = 0; k < MEMP_MAX; ++k) {
        i = memp_sorted[k];
        memp_tab[i] = NULL;


        // Align memp to element size
        uimemp = (uintptr_t) memp;
        if (uimemp % memp_sizes[i] > 0) {
            uimemp += memp_sizes[i] - (uimemp % memp_sizes[i]);
        }
        memp = (struct memp *) uimemp;

        /* create a linked list of memp elements */
        for (j = 0; j < memp_num[i]; ++j) {
            memp->next = NULL;
            memp->next = memp_tab[i];
            memp_tab[i] = memp;
            memp = (struct memp *) ((u8_t *) memp + memp_sizes[i]);
        }
    }
    // Set how many free pbuf_pools are there
//    printf("memp_num[PBUF_POOL] %" PRIu16 "\n", memp_num[MEMP_MAX - 1]);
    pbuf_pool_counter = 0;
#if MEMP_OVERFLOW_CHECK
    memp_overflow_init();
    /* check everything a first time to see if it worked */
    memp_overflow_check_all();
#endif                          /* MEMP_OVERFLOW_CHECK */

//    mem_barrelfish_register_buf(RX_BUFFER_ID, memp_memory_size);
}

// Returns the count of free pbufs available
u16_t memp_pbuf_peek(void)
{
    return (memp_num[MEMP_MAX - 1] - pbuf_pool_counter);
}

#define INSTRUMENT_PBUF_CALLS  1

#if INSTRUMENT_PBUF_CALLS
void show_pbuf_free_stats(void);
void show_pbuf_alloc_stats(void);
#endif // INSTRUMENT_PBUF_CALLS


extern uint64_t chained_pbuf_count;
extern uint64_t outgoing_packet_count;
extern uint64_t incoming_packet_count;
extern uint64_t pbuf_free_tx_done_counter;
extern uint64_t pbuf_free_incoming_counter;
extern uint64_t pbuf_realloc_called;
extern uint64_t pbuf_free_called_all;


extern uint64_t pbuf_alloc_all;
extern uint64_t pbuf_alloc_pool;
extern uint64_t pbuf_alloc_ram;
extern uint64_t pbuf_free_all;
extern uint64_t pbuf_free_pool;
extern uint64_t pbuf_free_ram;
extern uint64_t pbuf_free_all_called;
extern uint64_t pbuf_free_pool_called;
extern uint64_t pbuf_free_ram_called;


extern uint64_t pbuf_free_RX_packets;
extern uint64_t pbuf_free_TX_packets;
extern uint64_t pbuf_alloc_RX_packets;
extern uint64_t pbuf_alloc_RX_packets_2;
extern uint64_t pbuf_alloc_TX_packets;

static int64_t pbuf_pool_inuse_13 = 0;
static uint64_t memp_called_counter_13 = 0;
static uint64_t free_counter_13 = 0;
static uint64_t free_counter = 0;
static uint64_t memp_called_counter = 0;


static __attribute__((unused)) void print_stats(void)
{
    int64_t memp_diff = memp_called_counter_13 - free_counter_13;
        // This should be same as pbuf_pool_inuse_13,

    int64_t pbuf_pool_diff = pbuf_alloc_pool - pbuf_free_pool;
    int64_t pbuf_ram_diff = pbuf_alloc_ram - pbuf_free_ram;
    int64_t pbuf_diff_all = pbuf_alloc_all - pbuf_free_all;


    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "memp_diff", memp_diff,
        "memp_alloc", memp_called_counter_13,
        "memp_free", free_counter_13);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "pbuf_pool_diff", pbuf_pool_diff,
        "pbuf_alloc_pool", pbuf_alloc_pool,
        "pbuf_free_pool", pbuf_free_pool);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "RX_pkt_diff", (pbuf_alloc_RX_packets - pbuf_free_RX_packets),
        "pbuf_alloc_RX_packets", pbuf_alloc_RX_packets,
        "pbuf_free_RX_packets", pbuf_free_RX_packets);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "RX_pkt_diff", (pbuf_alloc_RX_packets_2 - pbuf_free_RX_packets),
        "pbuf_alloc_RX_packets_2", pbuf_alloc_RX_packets_2,
        "pbuf_free_RX_packets", pbuf_free_RX_packets);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "pbuf_ram_diff", pbuf_ram_diff,
        "pbuf_alloc_ram", pbuf_alloc_ram,
        "pbuf_free_ram", pbuf_free_ram);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "TX_pkt_diff", (pbuf_alloc_TX_packets - pbuf_free_TX_packets),
        "pbuf_alloc_TX_packets", pbuf_alloc_TX_packets,
        "pbuf_free_TX_packets", pbuf_free_TX_packets);


    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] - %-15s [%-8"PRIu64"]\n",
        "pbuf_diff_all", pbuf_diff_all,
        "pbuf_alloc_all", pbuf_alloc_all,
        "pbuf_free_all", pbuf_free_all);

    printf("assert memp_diff  [%"PRId64"] ==  pbuf_diff_all [%"PRId64"] =="
            " [%"PRId64"] (pbuf_pool_diff [%"PRId64"]  + pbuf_ram_diff  [%"PRId64"] )\n",
            memp_diff, pbuf_diff_all,  (pbuf_pool_diff  + pbuf_ram_diff),
            pbuf_pool_diff, pbuf_ram_diff);

    printf("%-15s [%-8"PRId64"] = %-15s [%-8"PRIu64"] + %-15s [%-8"PRIu64"] ==  [%-8"PRIu64"]\n",
        "pbuf_free_all_called", pbuf_free_all_called,
        "pbuf_free_pool_called", pbuf_free_pool_called,
        "pbuf_free_ram_called", pbuf_free_ram_called,
        pbuf_free_pool_called + pbuf_free_ram_called);

    printf("packet_accounting: allocations (incoming packets [%"PRIu64 "] + "
            "outgoing packets [%"PRIu64"] = [%"PRIu64"]) - "
            "free_counter_13 [%"PRIu64"] = unaccounted[%"PRId64"] \n",
            incoming_packet_count, outgoing_packet_count,
            (incoming_packet_count + outgoing_packet_count),
            free_counter_13,
            (free_counter_13 -
             (incoming_packet_count + outgoing_packet_count))
          );

    printf("packet_accounting: (outgoing pbufs[%"PRIu64 "], "
            "chained pbufs[%"PRIu64"]\n", outgoing_packet_count,
            chained_pbuf_count);

#if INSTRUMENT_PBUF_CALLS
    show_pbuf_free_stats();
    show_pbuf_alloc_stats();
#endif // INSTRUMENT_PBUF_CALLS

    // compare pbuf_diff_all with memp_diff
    return;
}

#if 0
static __attribute__((unused)) void print_stats_old(void)
{

    printf("memp_malloc: failed type: inUse_13: %"PRId64
            ", allocations_13: %"PRIu64", free_counter_13: %"PRIu64"\n",
            pbuf_pool_inuse_13, memp_called_counter_13,
            free_counter_13);

    printf("pbuf_free: pbuf_realloc [%"PRIu64"], "
            "(pbuf_alloc [%"PRIu64 "] - "
            "pbuf_free [%"PRIu64", (ALL: %"PRIu64")] = unaccounted [%"PRId64"]\n",
            pbuf_realloc_called, pbuf_alloc_called,
            pbuf_free_called, (pbuf_free_called_all) ,
            (pbuf_free_called - (pbuf_alloc_called ))
          );



    printf("pbuf_free_all:  (pbuf_alloc [%"PRIu64 "] + pbuf_realloc [%"PRIu64"] = "
            " [%"PRIu64"] ) -  pbuf_free_all [%"PRIu64", (called: %"PRIu64")] "
            "= unaccounted [%"PRId64"]\n",
            pbuf_alloc_called, pbuf_realloc_called,
            (pbuf_alloc_called + pbuf_realloc_called),
            (pbuf_free_called_all), pbuf_free_called,
            (pbuf_free_called_all -
             (pbuf_alloc_called + pbuf_realloc_called))
          );

    printf("memp_malloc: allocations (incoming packets [%"PRIu64 "] + "
            "outgoing packets [%"PRIu64"] = [%"PRIu64"]) - "
            "allcations_13 [%"PRIu64"] = unaccounted[%"PRId64"] \n",
            incoming_packet_count, outgoing_packet_count,
            (incoming_packet_count + outgoing_packet_count),
            memp_called_counter_13,
            (memp_called_counter_13 -
             (incoming_packet_count + outgoing_packet_count))
          );


    printf("memp_malloc: frees ( TX_done_frees [%"PRIu64 "] + "
            "incoming packets_frees [%"PRIu64"] = [%"PRIu64"]) - "
            "free_calls [%"PRIu64"] = unaccounted[%"PRId64"] \n",
            pbuf_free_tx_done_counter,
            pbuf_free_incoming_counter,
            (pbuf_free_tx_done_counter + pbuf_free_incoming_counter),
            free_counter_13,
            (free_counter_13 -
             (pbuf_free_tx_done_counter + pbuf_free_incoming_counter))
          );

#if INSTRUMENT_PBUF_CALLS
    show_pbuf_free_stats();
#endif // INSTRUMENT_PBUF_CALLS


}

#endif // 0


/**
 * Get an element from a specific pool.
 *
 * @param type the pool to get an element from
 *
 * the debug version has two more parameters:
 * @param file file name calling this function
 * @param line number of line where this function is called
 *
 * @return a pointer to the allocated memory or a NULL pointer on error
 */
void *
#if !MEMP_OVERFLOW_CHECK

memp_malloc(memp_t type)
#else
memp_malloc_fn(memp_t type, const char *file, const int line)
#endif
{
    struct memp *memp;
    memp_called_counter++;
    SYS_ARCH_DECL_PROTECT(old_level);

    LWIP_ERROR("memp_malloc: type < MEMP_MAX", (type < MEMP_MAX), return NULL;
      );

    SYS_ARCH_PROTECT(old_level);
#if MEMP_OVERFLOW_CHECK >= 2
    memp_overflow_check_all();
#endif                          /* MEMP_OVERFLOW_CHECK >= 2 */

    memp = memp_tab[type];

    if (memp != NULL) {
        memp_tab[type] = memp->next;
        ++pbuf_pool_counter;
        if (type == 13) {
            memp_called_counter_13++;
            pbuf_pool_inuse_13++;
        }

//    printf("memp_malloc: %s %"PRIu16" %"PRIu16" \n",
//            disp_name(), type, pbuf_pool_counter);
#if MEMP_OVERFLOW_CHECK
        memp->next = NULL;
        memp->file = file;
        memp->line = line;
#endif                          /* MEMP_OVERFLOW_CHECK */
        MEMP_STATS_INC_USED(used, type);
        LWIP_ASSERT("memp_malloc: memp properly aligned",
                    ((mem_ptr_t) memp % MEM_ALIGNMENT) == 0);
        memp = (struct memp *) ((u8_t *) memp + MEMP_SIZE);
    } else {
        LWIP_DEBUGF(MEMP_DEBUG | 2,
                    ("memp_malloc: out of memory in pool %s\n",
                     memp_desc[type]));
        MEMP_STATS_INC(err, type);
    }

    SYS_ARCH_UNPROTECT(old_level);

    if (memp == NULL) {
#if !MEMP_OVERFLOW_CHECK
        // NOTE: no prints here because its OK to fail in allocation.
        // Higher levels will deal with failure.
        // Use prints here only for debugging.
        if (type == 13) {
            printf("memp_malloc: failed type: %" PRIu16 ", count: %"PRIu16
                ", memp_called: %"PRIu64", free_counter: %"PRIu64", currently free: %"PRIu16" \n",
                type, pbuf_pool_counter, memp_called_counter, free_counter, memp_pbuf_peek() );

           // print_stats_old();
            print_stats();
            //abort();
        }
#else
        printf("memp_malloc_fn failed :\n");
#endif
    }

    return memp;
}

/**
 * Put an element back into its pool.
 *
 * @param type the pool where to put mem
 * @param mem the memp element to free
 */
void memp_free(memp_t type, void *mem)
{
    ++free_counter;
/*    printf("memp_free %s called for type %"PRIu16" with counter "
            "%"PRIu16", free counter %"PRIu64"\n",
           disp_name(), type, pbuf_pool_counter, free_counter);
*/
    struct memp *memp;

    SYS_ARCH_DECL_PROTECT(old_level);

    if (mem == NULL) {
        printf("memp_free: mem is NULL\n");
        return;
    }
    LWIP_ASSERT("memp_free: mem properly aligned",
                ((mem_ptr_t) mem % MEM_ALIGNMENT) == 0);

    memp = (struct memp *) ((u8_t *) mem - MEMP_SIZE);

    SYS_ARCH_PROTECT(old_level);
#if MEMP_OVERFLOW_CHECK
#if MEMP_OVERFLOW_CHECK >= 2
    memp_overflow_check_all();
#else
    memp_overflow_check_element(memp, Nemp_sizes[type]);
#endif                          /* MEMP_OVERFLOW_CHECK >= 2 */
#endif                          /* MEMP_OVERFLOW_CHECK */

    MEMP_STATS_DEC(used, type);

    memp->next = memp_tab[type];
    memp_tab[type] = memp;
    assert(pbuf_pool_counter > 0);
    --pbuf_pool_counter;

    if (type == 13) {
        --pbuf_pool_inuse_13;
        ++free_counter_13;
    }

#if MEMP_SANITY_CHECK
    LWIP_ASSERT("memp sanity", memp_sanity());
#endif                          /* MEMP_SANITY_CHECK */

    SYS_ARCH_UNPROTECT(old_level);
}

#endif                          /* MEMP_MEM_MALLOC */
