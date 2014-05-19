/**
 * @file
 * Packet buffer management
 *
 * Packets are built from the pbuf data structure. It supports dynamic
 * memory allocation for packet contents or can reference externally
 * managed packet contents both in RAM and ROM. Quick allocation for
 * incoming packets is provided through pools with fixed sized pbufs.
 *
 * A packet may span over multiple pbufs, chained as a singly linked
 * list. This is called a "pbuf chain".
 *
 * Multiple packets may be queued, also using this singly linked list.
 * This is called a "packet queue".
 *
 * So, a packet queue consists of one or more pbuf chains, each of
 * which consist of one or more pbufs. CURRENTLY, PACKET QUEUES ARE
 * NOT SUPPORTED!!! Use helper structs to queue multiple packets.
 *
 * The differences between a pbuf chain and a packet queue are very
 * precise but subtle.
 *
 * The last pbuf of a packet has a ->tot_len field that equals the
 * ->len field. It can be found by traversing the list. If the last
 * pbuf of a packet has a ->next field other than NULL, more packets
 * are on the queue.
 *
 * Therefore, looping through a pbuf of a single packet, has an
 * loop end condition (tot_len == p->len), NOT (next == NULL).
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

#include <barrelfish/barrelfish.h>

#include "lwip/opt.h"

#include "lwip/stats.h"
#include "lwip/def.h"
#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/sys.h"
#include "arch/perf.h"
#if TCP_QUEUE_OOSEQ
#include "lwip/tcp.h"
#endif

#include <string.h>
#include <assert.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>


/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define LWIP_TRACE_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE



#define SIZEOF_STRUCT_PBUF        LWIP_MEM_ALIGN_SIZE(sizeof(struct pbuf))
/* Since the pool is created in memp, PBUF_POOL_BUFSIZE will be automatically
   aligned there. Therefore, PBUF_POOL_BUFSIZE_ALIGNED can be used here. */
#define PBUF_POOL_BUFSIZE_ALIGNED LWIP_MEM_ALIGN_SIZE(PBUF_POOL_BUFSIZE)

#if TCP_QUEUE_OOSEQ
#define ALLOC_POOL_PBUF(p) do { (p) = alloc_pool_pbuf(); } while (0)
#else
#define ALLOC_POOL_PBUF(p) do { (p) = NULL } while (0)
#endif


uint64_t pbuf_free_RX_packets = 0;
uint64_t pbuf_free_TX_packets = 0;
uint64_t pbuf_alloc_RX_packets = 0;
uint64_t pbuf_alloc_TX_packets = 0;


#define INSTRUMENT_PBUF_CALLS  1

#if INSTRUMENT_PBUF_CALLS

#define MAX_INSTRUMENTED_CALLS  256
#define MAX_INSTRUMENTED_STATS 8

struct func_call_list {
    char        func_name[256];
    uint64_t    stats[MAX_INSTRUMENTED_STATS];
};

typedef struct func_call_list * func_call_list_t;

struct func_call_list pbuf_free_calls[MAX_INSTRUMENTED_CALLS];
struct func_call_list pbuf_alloc_calls[MAX_INSTRUMENTED_CALLS];

static void show_list(func_call_list_t list_name)
{
    int i = 0, j = 0, k = 0;

    for (i = 0; i < MAX_INSTRUMENTED_CALLS; ++i) {
        if (i == 0 || list_name[i].stats[0] == 1 ) {
            printf("%4d %-35s ",  i, (i == 0)? "TOTAL" :list_name[i].func_name);

            for (j = 1; j < MAX_INSTRUMENTED_STATS; ++j  ) {
                bool print_value = false;
                if (list_name[i].stats[j] == 0) {
                    for (k = j; k < MAX_INSTRUMENTED_STATS; ++k) {
                        if (list_name[i].stats[k] != 0) {
                            print_value = true;
                            break;
                        }
                    }
                } else {
                    print_value = true;
                }
                if (print_value) {
                    printf(" %7"PRIu64" ", list_name[i].stats[j]);
                }
            }
            printf("\n");
        }
    }
}

void show_pbuf_alloc_stats(void);
void show_pbuf_alloc_stats(void)
{
    func_call_list_t list_name = pbuf_alloc_calls;
    pbuf_alloc_TX_packets = list_name[0].stats[3];
    pbuf_alloc_RX_packets = list_name[0].stats[4];
    printf("\n\n");
    printf("%4s %-35s " " %7s "  " %7s " " %7s " " %7s \n" ,
            "idx", "pbuf_alloc_stats", "pbfref", "RAM_T", "POOL_S", "RAM_S");

    show_list(list_name);
    printf("\n\n");
}

void show_pbuf_free_stats(void);
void show_pbuf_free_stats(void)
{
    func_call_list_t list_name = pbuf_free_calls;
    pbuf_free_TX_packets = list_name[0].stats[3];
    pbuf_free_RX_packets = list_name[0].stats[4];
    printf("\n\n");
    printf("%4s %-35s " " %7s "  " %7s " " %7s " " %7s \n" ,
            "idx", "pbuf_free stats", "POOL_T", "RAM_T", "POOL_S", "RAM_S");
    show_list(list_name);
    printf("\n\n");
}

static int locate_key(func_call_list_t list_name, char *key)
{
    int i = 0;
    for (i = 1; i < MAX_INSTRUMENTED_CALLS; ++i) {
        if (list_name[i].stats[0] != 1) {
            continue;
        }
        if (strncmp(list_name[i].func_name, key, 256) == 0) {
            return i;
        }
    }
    return -1;
}

static int add_key(func_call_list_t list_name, char *key)
{
    int i = 0;
    for (i = 1; i < MAX_INSTRUMENTED_CALLS; ++i) {
        if (list_name[i].stats[0] != 1) {
            list_name[i].stats[0] = 1;
            strncpy(list_name[i].func_name, key, 256);
            return i;
        }
    }
    return -1;
}

static void increment_stats(func_call_list_t list_name, int idx, int type)
{
    assert(type < MAX_INSTRUMENTED_STATS);
    list_name[idx].stats[type] =  list_name[idx].stats[type] + 1;

    // Increasing total sum
    list_name[0].stats[type] =  list_name[0].stats[type] + 1;
}



static void increment_calls(func_call_list_t list_name, int type,
        const char *func_name, int line_no)
{
    char key[256];
    snprintf(key, sizeof(key), "%s:%d", func_name, line_no);
    int idx = locate_key(list_name, key);
    if (idx == -1) {
        idx = add_key(list_name, key);
    }
    if (idx == -1) {
        USER_PANIC("Function call tracking table is full!, dropping key %s\n", key);
        abort();
    }
    increment_stats(list_name, idx, type);
}

#endif // INSTRUMENT_PBUF_CALLS




#if TCP_QUEUE_OOSEQ

static bool try_free_segs(void)
{
    struct tcp_pcb *pcb;
    for (pcb = tcp_active_pcbs; NULL != pcb; pcb = pcb->next) {
        if (NULL != pcb->ooseq) {
            tcp_segs_free(pcb->ooseq);
            pcb->ooseq = NULL;
            return true;
        }
    }
    return false;
}

/**
 * Attempt to reclaim some memory from queued out-of-sequence TCP segments
 * if we run out of pool pbufs. It's better to give priority to new packets
 * if we're running out.
 *
 * @return the allocated pbuf.
 */
static struct pbuf *alloc_pool_pbuf(void)
{
    struct pbuf *p = NULL;
    void *payload = NULL;
    bool try_again = false;
    bool alloc_failed = false;


    do {
        if (payload == NULL) {
            payload = memp_malloc(MEMP_PBUF_POOL);
            assert((uintptr_t) payload % PBUF_POOL_BUFSIZE == 0);
        }
        if (p == NULL) {
            p = memp_malloc(MEMP_PBUF);
        }

        alloc_failed = (p == NULL || payload == NULL);

        if (alloc_failed) {
            if (p == NULL) {
                printf("p = memp_malloc(MEMP_PBUF) failed\n");
            }

            if (payload == NULL) {
                printf("payload = memp_malloc(MEMP_PBUF_POOL) failed\n");
            }
            try_again = try_free_segs();
        }
    } while (alloc_failed && try_again);

    if (alloc_failed) {
        if (p != NULL) {
            memp_free(MEMP_PBUF, p);
            p = NULL;
        }
        if (payload != NULL) {
            memp_free(MEMP_PBUF_POOL, payload);
        }
        //USER_PANIC("alloc_pool_pbuf: failed!");
        printf("alloc_pool_pbuf: failed!\n");
        return NULL;
    }

    p->payload = payload;
    return p;
}
#endif                          /* TCP_QUEUE_OOSEQ */


uint16_t free_pbuf_pool_count(void)
{
    return memp_pbuf_peek();
}


#define PBUF_FIXED_SIZE		1
/* FIXME: get rid of PBUF_FIXED_SIZE */

uint64_t pbuf_alloc_all = 0;
uint64_t pbuf_alloc_pool = 0;
uint64_t pbuf_alloc_ram = 0;
uint64_t pbuf_free_all = 0;
uint64_t pbuf_free_pool = 0;
uint64_t pbuf_free_ram = 0;
uint64_t pbuf_free_all_called = 0;
uint64_t pbuf_free_pool_called = 0;
uint64_t pbuf_free_ram_called = 0;


uint64_t pbuf_realloc_called = 0;

/**
 * Allocates a pbuf of the given type (possibly a chain for PBUF_POOL type).
 *
 * The actual memory allocated for the pbuf is determined by the
 * layer at which the pbuf is allocated and the requested size
 * (from the size parameter).
 *
 * @param layer flag to define header size
 * @param length size of the pbuf's payload
 * @param type this parameter decides how and where the pbuf
 * should be allocated as follows:
 *
 * - PBUF_RAM: buffer memory for pbuf is allocated as one large
 *             chunk. This includes protocol headers as well.
 * - PBUF_ROM: no buffer memory is allocated for the pbuf, even for
 *             protocol headers. Additional headers must be prepended
 *             by allocating another pbuf and chain in to the front of
 *             the ROM pbuf. It is assumed that the memory used is really
 *             similar to ROM in that it is immutable and will not be
 *             changed. Memory which is dynamic should generally not
 *             be attached to PBUF_ROM pbufs. Use PBUF_REF instead.
 * - PBUF_REF: no buffer memory is allocated for the pbuf, even for
 *             protocol headers. It is assumed that the pbuf is only
 *             being used in a single thread. If the pbuf gets queued,
 *             then pbuf_take should be called to copy the buffer.
 * - PBUF_POOL: the pbuf is allocated as a pbuf chain, with pbufs from
 *              the pbuf pool that is allocated during pbuf_init().
 *
 * @return the allocated pbuf. If multiple pbufs where allocated, this
 * is the first pbuf of a pbuf chain.
 */
//struct pbuf *pbuf_alloc(pbuf_layer layer, u16_t length, pbuf_type type)
struct pbuf *pbuf_alloc_tagged(pbuf_layer layer, u16_t length, pbuf_type type,
       const char *func_name, int line_no)
{
    struct pbuf *p, *q, *r;
    u16_t offset;
    s32_t rem_len;              /* remaining length */

    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3,
                ("pbuf_alloc(length=%" U16_F ")\n", length));

#ifdef PBUF_FIXED_SIZE
    //  printf("pbuf_alloc(length=%"U16_F")\n", length);
    assert(length <= PBUF_PKT_SIZE);    /* It is typically equal to 1514, but adding extra for safety */
#endif                          // PBUF_FIXED_SIZE
    /* determine header offset */
    p = q = r = NULL;

    offset = 0;
    switch (layer) {
        case PBUF_TRANSPORT:
            /* add room for transport (often TCP) layer header */
            offset += PBUF_TRANSPORT_HLEN;
            /* FALLTHROUGH */
        case PBUF_IP:
            /* add room for IP layer header */
            offset += PBUF_IP_HLEN;
            /* FALLTHROUGH */
        case PBUF_LINK:
            /* add room for link layer header */
            offset += PBUF_LINK_HLEN;
            break;
        case PBUF_RAW:
            break;
        default:
            LWIP_ASSERT("pbuf_alloc: bad pbuf layer", 0);
            return NULL;
    }

    switch (type) {
        case PBUF_POOL:
            /* allocate head of pbuf chain into p */
            ALLOC_POOL_PBUF(p);
            LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3,
                        ("pbuf_alloc: allocated pbuf %p\n", (void *) p));
            if (p == NULL) {

                printf("\npbuf_alloc(): no more memory available.\n");
                return NULL;
            }
#if INSTRUMENT_PBUF_CALLS
            increment_calls(pbuf_alloc_calls, 3, func_name, line_no);
#endif  // INSTRUMENT_PBUF_CALLS
            p->type = type;
            p->next = NULL;
            /* make the payload pointer point 'offset' bytes into pbuf data memory */
            p->payload =
              LWIP_MEM_ALIGN((void *) ((u8_t *) p->payload + offset));
            LWIP_ASSERT("pbuf_alloc: pbuf p->payload properly aligned",
                        ((mem_ptr_t) p->payload % MEM_ALIGNMENT) == 0);
            /* the total length of the pbuf chain is the requested size */
            p->tot_len = length;
#ifdef PBUF_FIXED_SIZE
            p->buff_len = PBUF_PKT_SIZE;        // This doesn't matter that much as this if PBUF_POOL
#endif                          // PBUF_FIXED_SIZE
            /* set the length of the first pbuf in the chain */
            p->len =
              LWIP_MIN(length,
                       PBUF_POOL_BUFSIZE_ALIGNED - LWIP_MEM_ALIGN_SIZE(offset));
            LWIP_ASSERT("check p->payload + p->len does not overflow pbuf",
                        (offset + p->len <= PBUF_POOL_BUFSIZE_ALIGNED));
            LWIP_ASSERT("PBUF_POOL_BUFSIZE must be bigger than MEM_ALIGNMENT",
                        (PBUF_POOL_BUFSIZE_ALIGNED -
                         LWIP_MEM_ALIGN_SIZE(offset)) > 0);
            /* set reference count (needed here in case we fail) */
            p->ref = 1;

            // stats about how many successfull allocs have happened
            ++pbuf_alloc_pool;
            ++pbuf_alloc_all;
            /* now allocate the tail of the pbuf chain */

            /* remember first pbuf for linkage in next iteration */
            r = p;
            /* remaining length to be allocated */
            rem_len = length - p->len;
            assert(rem_len <= 0); // making sure that there is no fragmentation
            LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3,
                        ("pbuf_alloc: remaining length to be allocated %" PRIu32
                         "\n", rem_len));
            /* any remaining pbufs to be allocated? */
            while (rem_len > 0) {
                ALLOC_POOL_PBUF(q);
                if (q == NULL) {
                    /* free chain so far allocated */
                    pbuf_free(p);
                    /* bail out unsuccesfully */
                    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3,
                                ("pbuf_alloc: remaining length alloc failed %"
                                 PRIu32 "\n", rem_len));

                    return NULL;
                }
                q->type = type;
                q->flags = 0;
                q->next = NULL;
                /* make previous pbuf point to this pbuf */
                r->next = q;
                /* set total length of this pbuf and next in chain */
                LWIP_ASSERT("rem_len < max_u16_t", rem_len < 0xffff);
                q->tot_len = (u16_t) rem_len;
                /* this pbuf length is pool size, unless smaller sized tail */
                q->len = LWIP_MIN((u16_t) rem_len, PBUF_POOL_BUFSIZE_ALIGNED);
                LWIP_ASSERT("pbuf_alloc: pbuf q->payload properly aligned",
                            ((mem_ptr_t) q->payload % MEM_ALIGNMENT) == 0);
                LWIP_ASSERT("check p->payload + p->len does not overflow pbuf",
                            ((u8_t *) p->payload + p->len <=
                             (u8_t *) p + SIZEOF_STRUCT_PBUF +
                             PBUF_POOL_BUFSIZE_ALIGNED));
                q->ref = 1;
                /* calculate remaining length to be allocated */
                rem_len -= q->len;
                /* remember this pbuf for linkage in next iteration */
                r = q;
            }
            /* end of chain */
            /*r->next = NULL; */

            break;
        case PBUF_RAM:
            /* If pbuf is to be allocated in RAM, allocate memory for it. */
#ifdef PBUF_FIXED_SIZE
            assert(length + offset <= PBUF_POOL_BUFSIZE_ALIGNED);
            p = alloc_pool_pbuf();
#else                           // PBUF_FIXED_SIZE
            p =
              (struct pbuf *)
              mem_malloc(LWIP_MEM_ALIGN_SIZE(SIZEOF_STRUCT_PBUF + offset) +
                         LWIP_MEM_ALIGN_SIZE(length));
#endif                          // PBUF_FIXED_SIZE
            if (p == NULL) {
                return NULL;
            }

#if INSTRUMENT_PBUF_CALLS
            increment_calls(pbuf_alloc_calls, 4, func_name, line_no);

#endif // INSTRUMENT_PBUF_CALLS
            /* Set up internal structure of the pbuf. */
            p->payload = LWIP_MEM_ALIGN((u8_t *) p->payload + offset);
            p->len = p->tot_len = length;
            p->next = NULL;
            p->type = type;
#ifdef PBUF_FIXED_SIZE
            p->buff_len = PBUF_PKT_SIZE;
#else
            p->buff_len = length;
#endif                          // PBUF_FIXED_SIZE

            ++pbuf_alloc_ram;
            ++pbuf_alloc_all;

            LWIP_ASSERT("pbuf_alloc: pbuf->payload properly aligned",
                        ((mem_ptr_t) p->payload % MEM_ALIGNMENT) == 0);
            break;
            /* pbuf references existing (non-volatile static constant) ROM payload? */
        case PBUF_ROM:
            /* pbuf references existing (externally allocated) RAM payload? */
        case PBUF_REF:
            /* only allocate memory for the pbuf structure */
            p = memp_malloc(MEMP_PBUF);
            if (p == NULL) {
                LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 2,
                            ("pbuf_alloc: Could not allocate MEMP_PBUF for PBUF_%s.\n",
                             (type == PBUF_ROM) ? "ROM" : "REF"));
                return NULL;
            }
            /* caller must set this field properly, afterwards */
            p->payload = NULL;
            p->len = p->tot_len = length;
            p->next = NULL;
            p->type = type;
            break;
        default:
            LWIP_ASSERT("pbuf_alloc: erroneous type", 0);
            return NULL;
    }
    /* set reference count */
    p->ref = 1;
    /* set flags */
    p->flags = 0;
    p->nicflags = 0;
/*
  LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3, ("pbuf_alloc(length=%"U16_F") == %p\n", length, (void *)p));
*/
#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_LWIPPBA2, (uint32_t)(uintptr_t)p);
#endif // LWIP_TRACE_MODE

    return p;
}


/**
 * Shrink a pbuf chain to a desired length.
 *
 * @param p pbuf to shrink.
 * @param new_len desired new length of pbuf chain
 *
 * Depending on the desired length, the first few pbufs in a chain might
 * be skipped and left unchanged. The new last pbuf in the chain will be
 * resized, and any remaining pbufs will be freed.
 *
 * @note If the pbuf is ROM/REF, only the ->tot_len and ->len fields are adjusted.
 * @note May not be called on a packet queue.
 *
 * @note Despite its name, pbuf_realloc cannot grow the size of a pbuf (chain).
 */
void pbuf_realloc(struct pbuf *p, u16_t new_len)
{
    struct pbuf *q;
    u16_t rem_len;              /* remaining length */
    s32_t grow;

    LWIP_ASSERT("pbuf_realloc: p != NULL", p != NULL);
    LWIP_ASSERT("pbuf_realloc: sane p->type", p->type == PBUF_POOL ||
                p->type == PBUF_ROM ||
                p->type == PBUF_RAM || p->type == PBUF_REF);

    /* desired length larger than current length? */
    if (new_len >= p->tot_len) {
        /* enlarging not yet supported */
        return;
    }
#ifdef PBUF_FIXED_SIZE
    /* FIXME: this code should not be used in new buff management. */
    assert(new_len <= PBUF_PKT_SIZE);
#endif                          // PBUF_FIXED_SIZE
    /* the pbuf chain grows by (new_len - p->tot_len) bytes
     * (which may be negative in case of shrinking) */
    grow = new_len - p->tot_len;

    /* first, step over any pbufs that should remain in the chain */
    rem_len = new_len;
    q = p;
    /* should this pbuf be kept? */
    while (rem_len > q->len) {
        /* decrease remaining length by pbuf length */
        rem_len -= q->len;
        /* decrease total length indicator */
        LWIP_ASSERT("grow < max_u16_t", grow < 0xffff);
        q->tot_len += (u16_t) grow;
        /* proceed to next pbuf in chain */
        q = q->next;
        LWIP_ASSERT("pbuf_realloc: q != NULL", q != NULL);
    }
    /* we have now reached the new last pbuf (in q) */
    /* rem_len == desired length for pbuf q */

    /* shrink allocated memory for PBUF_RAM */
    /* (other types merely adjust their length fields */
    if ((q->type == PBUF_RAM) && (rem_len != q->len)) {
        /* reallocate and adjust the length of the pbuf that will be split */
        q = mem_realloc(q, (u8_t *) q->payload - (u8_t *) q + rem_len);
        LWIP_ASSERT("mem_realloc give q == NULL", q != NULL);
    }
    /* adjust length fields for new last pbuf */
    q->len = rem_len;
    q->tot_len = q->len;

    /* any remaining pbufs in chain? */
    if (q->next != NULL) {
        /* free remaining pbufs in chain */
        pbuf_free(q->next);
    }
    /* q is last packet in chain */
    q->next = NULL;
    ++pbuf_realloc_called;
}

/**
 * Adjusts the payload pointer to hide or reveal headers in the payload.
 *
 * Adjusts the ->payload pointer so that space for a header
 * (dis)appears in the pbuf payload.
 *
 * The ->payload, ->tot_len and ->len fields are adjusted.
 *
 * @param p pbuf to change the header size.
 * @param header_size_increment Number of bytes to increment header size which
 * increases the size of the pbuf. New space is on the front.
 * (Using a negative value decreases the header size.)
 * If hdr_size_inc is 0, this function does nothing and returns succesful.
 *
 * PBUF_ROM and PBUF_REF type buffers cannot have their sizes increased, so
 * the call will fail. A check is made that the increase in header size does
 * not move the payload pointer in front of the start of the buffer.
 * @return non-zero on failure, zero on success.
 *
 */
u8_t pbuf_header(struct pbuf *p, s16_t header_size_increment)
{
    u16_t type;
    void *payload;
    u16_t increment_magnitude;

    LWIP_ASSERT("p != NULL", p != NULL);
    if ((header_size_increment == 0) || (p == NULL))
        return 0;

    if (header_size_increment < 0) {
        increment_magnitude = -header_size_increment;
        /* Check that we aren't going to move off the end of the pbuf */

        if (increment_magnitude > p->len) {
            printf("ERROR: incr mag %u <= len %d tot_len %u tp %u, fg %u\n",
                   increment_magnitude, p->len, p->tot_len, p->type, p->flags);
//      abort();
            return -1;
        }
        /* Following error is converted into above if condition with abort */
        LWIP_ERROR("increment_magnitude <= p->len",
                   (increment_magnitude <= p->len), return 1;
          );


    } else {
        increment_magnitude = header_size_increment;
#if 0
        /* Can't assert these as some callers speculatively call
           pbuf_header() to see if it's OK.  Will return 1 below instead. */
        /* Check that we've got the correct type of pbuf to work with */
        LWIP_ASSERT("p->type == PBUF_RAM || p->type == PBUF_POOL",
                    p->type == PBUF_RAM || p->type == PBUF_POOL);
        /* Check that we aren't going to move off the beginning of the pbuf */
        LWIP_ASSERT
          ("p->payload - increment_magnitude >= p + SIZEOF_STRUCT_PBUF",
           (u8_t *) p->payload - increment_magnitude >=
           (u8_t *) p + SIZEOF_STRUCT_PBUF);
#endif
    }

    type = p->type;
    /* remember current payload pointer */
    payload = p->payload;

    /* pbuf types containing payloads? */
    if (type == PBUF_RAM || type == PBUF_POOL) {
        /* set new payload pointer */
        p->payload = (u8_t *) p->payload - header_size_increment;
        /* boundary check fails? */
        if ((uintptr_t) payload / PBUF_POOL_BUFSIZE !=
            ((uintptr_t) p->payload) / PBUF_POOL_BUFSIZE)
        {
            LWIP_DEBUGF(PBUF_DEBUG | 2,
                        ("pbuf_header: failed as %p < %p (not enough space for new header size)\n",
                         (void *) p->payload, (void *) (p + 1)));
            /* restore old payload pointer */
            p->payload = payload;
            /* bail out unsuccesfully */
            return 1;
        }
        /* pbuf types refering to external payloads? */
    } else if (type == PBUF_REF || type == PBUF_ROM) {
        /* hide a header in the payload? */
        if ((header_size_increment < 0) && (increment_magnitude <= p->len)) {
            /* increase payload pointer */
            p->payload = (u8_t *) p->payload - header_size_increment;
        } else {
            /* cannot expand payload to front (yet!)
             * bail out unsuccesfully */
            return 1;
        }
    } else {
        /* Unknown type */
        LWIP_ASSERT("bad pbuf type", 0);
        return 1;
    }
    /* modify pbuf length fields */
    p->len += header_size_increment;
    p->tot_len += header_size_increment;

    LWIP_DEBUGF(PBUF_DEBUG, ("pbuf_header: old %p new %p (%" S16_F ")\n",
                             (void *) payload, (void *) p->payload,
                             header_size_increment));

    return 0;
}

/**
 * Dereference a pbuf chain or queue and deallocate any no-longer-used
 * pbufs at the head of this chain or queue.
 *
 * Decrements the pbuf reference count. If it reaches zero, the pbuf is
 * deallocated.
 *
 * For a pbuf chain, this is repeated for each pbuf in the chain,
 * up to the first pbuf which has a non-zero reference count after
 * decrementing. So, when all reference counts are one, the whole
 * chain is free'd.
 *
 * @param p The pbuf (chain) to be dereferenced.
 *
 * @return the number of pbufs that were de-allocated
 * from the head of the chain.
 *
 * @note MUST NOT be called on a packet queue (Not verified to work yet).
 * @note the reference counter of a pbuf equals the number of pointers
 * that refer to the pbuf (or into the pbuf).
 *
 * @internal examples:
 *
 * Assuming existing chains a->b->c with the following reference
 * counts, calling pbuf_free(a) results in:
 *
 * 1->2->3 becomes ...1->3
 * 3->3->3 becomes 2->3->3
 * 1->1->2 becomes ......1
 * 2->1->1 becomes 1->1->1
 * 1->1->1 becomes .......
 *
 */
//u8_t pbuf_free(struct pbuf * p)
u8_t pbuf_free_tagged(struct pbuf * p, const char *func_name, int line_no)
{
    u16_t type;
    struct pbuf *q;
    u8_t count;
#if LWIP_TRACE_MODE
    struct pbuf *p_bak = p;
#endif // LWIP_TRACE_MODE

    if (p == NULL) {
        LWIP_ASSERT("p != NULL", p != NULL);
        /* if assertions are disabled, proceed with debug output */
        LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 2,
                    ("pbuf_free(p == NULL) was called.\n"));
        return 0;
    }
    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3,
                ("pbuf_free(%p)\n", (void *) p));

    PERF_START;

    LWIP_ASSERT("pbuf_free: sane type",
                p->type == PBUF_RAM || p->type == PBUF_ROM ||
                p->type == PBUF_REF || p->type == PBUF_POOL);

#if INSTRUMENT_PBUF_CALLS
    if (p->type == PBUF_POOL) {
            ++pbuf_free_pool_called;
            ++pbuf_free_all_called;
        increment_calls(pbuf_free_calls, 1, func_name, line_no);
    }
    if (p->type == PBUF_RAM) {
            ++pbuf_free_ram_called;
            ++pbuf_free_all_called;
        increment_calls(pbuf_free_calls, 2, func_name, line_no);
    }
#endif // INSTRUMENT_PBUF_CALLS

    count = 0;
    /* de-allocate all consecutive pbufs from the head of the chain that
     * obtain a zero reference count after decrementing*/
    while (p != NULL) {
        u16_t ref;

        SYS_ARCH_DECL_PROTECT(old_level);
        /* Since decrementing ref cannot be guaranteed to be a single machine operation
         * we must protect it. We put the new ref into a local variable to prevent
         * further protection. */
        SYS_ARCH_PROTECT(old_level);
        /* all pbufs in a chain are referenced at least once */

        /* decrease reference count (number of pointers to pbuf) */
        if (p->ref <= 0) {
            printf("pbuf_free:[%p] p->ref value is %u\n",p, p->ref);
            printf("callstack: %p %p %p %p\n",
	         __builtin_return_address(0),
	         __builtin_return_address(1),
	         __builtin_return_address(2),
	         __builtin_return_address(3));
            /* FIXME: This state represents that something is seriously wrong,
             * This may lead to releasing the memory twice
             * or invalid memory accesses in future. */
            /* abort(); */
            //    LWIP_ASSERT("pbuf_free: p->ref > 0", p->ref > 0);
            ref = 0;
        } else {
            ref = --(p->ref);
        }

//   printf("pbuf_free: p->ref value is %u and type %u(%u)\n",
//          p->ref, p->type, PBUF_POOL);
        SYS_ARCH_UNPROTECT(old_level);
        /* this pbuf is no longer referenced to? */
        if (ref == 0) {
            /* remember next pbuf in chain for next iteration */
            q = p->next;
            LWIP_DEBUGF(PBUF_DEBUG | 2,
                        ("pbuf_free: deallocating %p\n", (void *) p));
            type = p->type;
            p->nicflags = 0;
//            printf("pbuf_free: deallocating %p\n", (void *) p);
            /* is this a pbuf from the pool? */
            if (type == PBUF_POOL || type == PBUF_RAM) {
//                printf("pbuf_free: %p: PBUF_POOL\n", (void *) p);
                // Is a bit hacky, but it should work as long as we allocate
                // objects aligned to their size, is necessary because of the
                // possible offset of the payload.
//                assert(type == PBUF_POOL);
                uintptr_t pl = (uintptr_t) p->payload;
                pl -= pl % PBUF_POOL_BUFSIZE;

                memp_free(MEMP_PBUF_POOL, (void*) pl);
                memp_free(MEMP_PBUF, p);
                ++pbuf_free_all;
#if INSTRUMENT_PBUF_CALLS
                if (p->type == PBUF_POOL) {
                    ++pbuf_free_pool;
                    increment_calls(pbuf_free_calls, 3, func_name, line_no);
                }
                if (p->type == PBUF_RAM) {
                    ++pbuf_free_ram;
                    increment_calls(pbuf_free_calls, 4, func_name, line_no);
                }
#endif // INSTRUMENT_PBUF_CALLS

                /* is this a ROM or RAM referencing pbuf? */
            } else if (type == PBUF_ROM || type == PBUF_REF) {
//                printf("pbuf_free: %p: PBUF_ROM || PBUF_REF\n", (void *) p);
                memp_free(MEMP_PBUF, p);
                /* type == PBUF_RAM */
            } else {
                assert(!"Should never be executed");
            }
            count++;
            /* proceed to next pbuf */
            p = q;
            /* p->ref > 0, this pbuf is still referenced to */
            /* (and so the remaining pbufs in chain as well) */
        } else {
            LWIP_DEBUGF(PBUF_DEBUG | 2,
                        ("pbuf_free: %p has ref %" U16_F ", ending here.\n",
                         (void *) p, ref));
//            printf("pbuf_free: %p has ref %" U16_F ", ending here.\n",
//                         (void *) p, ref);
            /* stop walking through the chain */
            p = NULL;
        }
    }
    PERF_STOP("pbuf_free");
    /* return number of de-allocated pbufs */
//    printf("pbuf_free: finished with [%p] and count %"PRIu8"\n", p, count);

#if LWIP_TRACE_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_LWIPPBF2, (uint32_t)(uintptr_t)p_bak);
#endif // LWIP_TRACE_MODE

    return count;
}

/**
 * Count number of pbufs in a chain
 *
 * @param p first pbuf of chain
 * @return the number of pbufs in a chain
 */

u8_t pbuf_clen(struct pbuf * p)
{
    u8_t len;

    len = 0;
    while (p != NULL) {
        ++len;
        p = p->next;
    }
    return len;
}

/**
 * Increment the reference count of the pbuf.
 *
 * @param p pbuf to increase reference counter of
 *
 */
//void pbuf_ref(struct pbuf *p)
void pbuf_ref_tagged(struct pbuf *p, const char *func_name, int line_no)
{
    SYS_ARCH_DECL_PROTECT(old_level);
    /* pbuf given? */
    if (p != NULL) {
        SYS_ARCH_PROTECT(old_level);
        ++(p->ref);
#if INSTRUMENT_PBUF_CALLS
        increment_calls(pbuf_alloc_calls, 1, func_name, line_no);
#endif // INSTRUMENT_PBUF_CALLS
        SYS_ARCH_UNPROTECT(old_level);
    }
}

/**
 * Concatenate two pbufs (each may be a pbuf chain) and take over
 * the caller's reference of the tail pbuf.
 *
 * @note The caller MAY NOT reference the tail pbuf afterwards.
 * Use pbuf_chain() for that purpose.
 *
 * @see pbuf_chain()
 */

void pbuf_cat(struct pbuf *h, struct pbuf *t)
{
    struct pbuf *p;

    LWIP_ERROR("(h != NULL) && (t != NULL) (programmer violates API)",
               ((h != NULL) && (t != NULL)), return;
      );

    /* proceed to last pbuf of chain */
    for (p = h; p->next != NULL; p = p->next) {
        /* add total length of second chain to all totals of first chain */
        p->tot_len += t->tot_len;
    }
    /* { p is last pbuf of first h chain, p->next == NULL } */
    LWIP_ASSERT("p->tot_len == p->len (of last pbuf in chain)",
                p->tot_len == p->len);
    LWIP_ASSERT("p->next == NULL", p->next == NULL);
    /* add total length of second chain to last pbuf total of first chain */
    p->tot_len += t->tot_len;
    /* chain last pbuf of head (p) with first of tail (t) */
    p->next = t;
    /* p->next now references t, but the caller will drop its reference to t,
     * so netto there is no change to the reference count of t.
     */
}

/**
 * Chain two pbufs (or pbuf chains) together.
 *
 * The caller MUST call pbuf_free(t) once it has stopped
 * using it. Use pbuf_cat() instead if you no longer use t.
 *
 * @param h head pbuf (chain)
 * @param t tail pbuf (chain)
 * @note The pbufs MUST belong to the same packet.
 * @note MAY NOT be called on a packet queue.
 *
 * The ->tot_len fields of all pbufs of the head chain are adjusted.
 * The ->next field of the last pbuf of the head chain is adjusted.
 * The ->ref field of the first pbuf of the tail chain is adjusted.
 *
 */
void pbuf_chain(struct pbuf *h, struct pbuf *t)
{
    pbuf_cat(h, t);
    /* t is now referenced by h */
    pbuf_ref(t);
    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_FRESH | 2,
                ("pbuf_chain: %p references %p\n", (void *) h, (void *) t));
}

/**
 * Dechains the first pbuf from its succeeding pbufs in the chain.
 *
 * Makes p->tot_len field equal to p->len.
 * @param p pbuf to dechain
 * @return remainder of the pbuf chain, or NULL if it was de-allocated.
 * @note May not be called on a packet queue.
 */
struct pbuf *pbuf_dechain(struct pbuf *p)
{
    struct pbuf *q;
    u8_t tail_gone = 1;

    /* tail */
    q = p->next;
    /* pbuf has successor in chain? */
    if (q != NULL) {
        /* assert tot_len invariant: (p->tot_len == p->len + (p->next? p->next->tot_len: 0) */
        LWIP_ASSERT("p->tot_len == p->len + q->tot_len",
                    q->tot_len == p->tot_len - p->len);
        /* enforce invariant if assertion is disabled */
        q->tot_len = p->tot_len - p->len;
        /* decouple pbuf from remainder */
        p->next = NULL;
        /* total length of pbuf p is its own length only */
        p->tot_len = p->len;
        /* q is no longer referenced by p, free it */
        LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_STATE,
                    ("pbuf_dechain: unreferencing %p\n", (void *) q));
        tail_gone = pbuf_free(q);
        if (tail_gone > 0) {
            LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_STATE,
                        ("pbuf_dechain: deallocated %p (as it is no longer referenced)\n",
                         (void *) q));
        }
        /* return remaining tail or NULL if deallocated */
    }
    /* assert tot_len invariant: (p->tot_len == p->len + (p->next? p->next->tot_len: 0) */
    LWIP_ASSERT("p->tot_len == p->len", p->tot_len == p->len);
    return ((tail_gone > 0) ? NULL : q);
}

/**
 *
 * Create PBUF_RAM copies of pbufs.
 *
 * Used to queue packets on behalf of the lwIP stack, such as
 * ARP based queueing.
 *
 * @note You MUST explicitly use p = pbuf_take(p);
 *
 * @note Only one packet is copied, no packet queue!
 *
 * @param p_to pbuf destination of the copy
 * @param p_from pbuf source of the copy
 *
 * @return ERR_OK if pbuf was copied
 *         ERR_ARG if one of the pbufs is NULL or p_to is not big
 *                 enough to hold p_from
 */
err_t pbuf_copy(struct pbuf * p_to, struct pbuf * p_from)
{
    u16_t offset_to = 0, offset_from = 0, len;

    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 3, ("pbuf_copy(%p, %p)\n",
                                                  (void *) p_to,
                                                  (void *) p_from));

    /* is the target big enough to hold the source? */
    LWIP_ERROR("pbuf_copy: target not big enough to hold source",
               ((p_to != NULL) && (p_from != NULL)
                && (p_to->tot_len >= p_from->tot_len)), return ERR_ARG;
      );

    /* iterate through pbuf chain */
    do {
        LWIP_ASSERT("p_to != NULL", p_to != NULL);
        /* copy one part of the original chain */
        if ((p_to->len - offset_to) >= (p_from->len - offset_from)) {
            /* complete current p_from fits into current p_to */
            len = p_from->len - offset_from;
        } else {
            /* current p_from does not fit into current p_to */
            len = p_to->len - offset_to;
        }
        MEMCPY((u8_t *) p_to->payload + offset_to,
               (u8_t *) p_from->payload + offset_from, len);
        offset_to += len;
        offset_from += len;
        LWIP_ASSERT("offset_to <= p_to->len", offset_to <= p_to->len);
        if (offset_to == p_to->len) {
            /* on to next p_to (if any) */
            offset_to = 0;
            p_to = p_to->next;
        }
        LWIP_ASSERT("offset_from <= p_from->len", offset_from <= p_from->len);
        if (offset_from >= p_from->len) {
            /* on to next p_from (if any) */
            offset_from = 0;
            p_from = p_from->next;
        }

        if ((p_from != NULL) && (p_from->len == p_from->tot_len)) {
            /* don't copy more than one packet! */
            LWIP_ERROR("pbuf_copy() does not allow packet queues!\n",
                       (p_from->next == NULL), return ERR_VAL;
              );
        }
        if ((p_to != NULL) && (p_to->len == p_to->tot_len)) {
            /* don't copy more than one packet! */
            LWIP_ERROR("pbuf_copy() does not allow packet queues!\n",
                       (p_to->next == NULL), return ERR_VAL;
              );
        }
    } while (p_from);
    LWIP_DEBUGF(PBUF_DEBUG | LWIP_DBG_TRACE | 1,
                ("pbuf_copy: end of chain reached.\n"));
    return ERR_OK;
}

/**
 * Copy (part of) the contents of a packet buffer
 * to an application supplied buffer.
 *
 * @param buf the pbuf from which to copy data
 * @param dataptr the application supplied buffer
 * @param len length of data to copy (dataptr must be big enough). No more
 * than buf->tot_len will be copied, irrespective of len
 * @param offset offset into the packet buffer from where to begin copying len bytes
 * @return the number of bytes copied, or 0 on failure
 */
u16_t
pbuf_copy_partial(struct pbuf * buf, void *dataptr, u16_t len, u16_t offset)
{
    struct pbuf *p;
    u16_t left;
    u16_t buf_copy_len;
    u16_t copied_total = 0;

    LWIP_ERROR("pbuf_copy_partial: invalid buf", (buf != NULL), return 0;
      );
    LWIP_ERROR("pbuf_copy_partial: invalid dataptr", (dataptr != NULL),
               return 0;
      );

    left = 0;

    if ((buf == NULL) || (dataptr == NULL)) {
        return 0;
    }

    /* Note some systems use byte copy if dataptr or one of the pbuf payload pointers are unaligned. */
    for (p = buf; len != 0 && p != NULL; p = p->next) {
        if ((offset != 0) && (offset >= p->len)) {
            /* don't copy from this buffer -> on to the next */
            offset -= p->len;
        } else {
            /* copy from this buffer. maybe only partially. */
            buf_copy_len = p->len - offset;
            if (buf_copy_len > len)
                buf_copy_len = len;
            /* copy the necessary parts of the buffer */
            MEMCPY(&((char *) dataptr)[left], &((char *) p->payload)[offset],
                   buf_copy_len);
            copied_total += buf_copy_len;
            left += buf_copy_len;
            len -= buf_copy_len;
            offset = 0;
        }
    }
    return copied_total;
}

/**
 * Copy application supplied data into a pbuf.
 * This function can only be used to copy the equivalent of buf->tot_len data.
 *
 * @param buf pbuf to fill with data
 * @param dataptr application supplied data buffer
 * @param len length of the application supplied data buffer
 *
 * @return ERR_OK if successful, ERR_MEM if the pbuf is not big enough
 */
err_t pbuf_take(struct pbuf * buf, const void *dataptr, u16_t len)
{
    struct pbuf *p;
    u16_t buf_copy_len;
    u16_t total_copy_len = len;
    u16_t copied_total = 0;

    LWIP_ERROR("pbuf_take: invalid buf", (buf != NULL), return 0;
      );
    LWIP_ERROR("pbuf_take: invalid dataptr", (dataptr != NULL), return 0;
      );

    if ((buf == NULL) || (dataptr == NULL) || (buf->tot_len < len)) {
        return ERR_ARG;
    }

    /* Note some systems use byte copy if dataptr or one of the pbuf payload pointers are unaligned. */
    for (p = buf; total_copy_len != 0; p = p->next) {
        LWIP_ASSERT("pbuf_take: invalid pbuf", p != NULL);
        buf_copy_len = total_copy_len;
        if (buf_copy_len > p->len) {
            /* this pbuf cannot hold all remaining data */
            buf_copy_len = p->len;
        }
        /* copy the necessary parts of the buffer */
        MEMCPY(p->payload, &((char *) dataptr)[copied_total], buf_copy_len);
        total_copy_len -= buf_copy_len;
        copied_total += buf_copy_len;
    }
    LWIP_ASSERT("did not copy all data", total_copy_len == 0
                && copied_total == len);
    return ERR_OK;
}

/**
 * Creates a single pbuf out of a queue of pbufs.
 *
 * @remark: The source pbuf 'p' is not freed by this function because that can
 *          be illegal in some places!
 *
 * @param p the source pbuf
 * @param layer pbuf_layer of the new pbuf
 *
 * @return a new, single pbuf (p->next is NULL)
 *         or the old pbuf if allocation fails
 */
struct pbuf *pbuf_coalesce(struct pbuf *p, pbuf_layer layer)
{
    struct pbuf *q;
    err_t err;

    if (p->next == NULL) {
        return p;
    }
    q = pbuf_alloc(layer, p->tot_len, PBUF_RAM);
    if (q == NULL) {
        /* @todo: what do we do now? */
        return p;
    }
    err = pbuf_copy(q, p);
    LWIP_ASSERT("pbuf_copy failed", err == ERR_OK);
    pbuf_free(p);
    return q;
}
