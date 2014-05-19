/**
 * \file
 * \brief Buffer memory for LWIP using caps
 *
 * This file provides caps for the buffers in LWIP. Needed because the network
 * stack should pass a cap to a buffer to the network device driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/sys_debug.h>
#include <barrelfish/waitset.h>
#include <contmng/netbench.h>

#include <assert.h>
#include <stdlib.h>
#include "lwip/init.h"
#include "lwip/def.h"
#include "lwip/pbuf.h"
#include "mem_barrelfish.h"
#include "idc_barrelfish.h"
#include <procon/procon.h>

#include "lwip_barrelfish_debug.h"



//XXX: the rx descriptor in the e1000 card always assumes a buffer of size 2048
//     bytes. So this works only if we assume that no packet longer than 1518
//     bytes will be received, otherwise a buffer overflow occurs. On the other
//     hand, we don't want smaller buffers and having the card split larger
//     packets to multiple buffers, because otherwise we'd had to create a
//     pbuf chain.
//#define RECEIVE_PBUF_SIZE 1514
#define RECEIVE_PBUF_SIZE 2048


static inline void *binding_to_buffer(uint8_t binding_index);


// antoinek: Come from benchmark interface
errval_t buffer_rx_add(size_t idx);

extern void *buffer_base;
extern size_t buffer_size;
extern size_t buffer_count;




struct pbuf_desc {
    struct pbuf *p;
};

// Is used to map from buffer ids (benchmark if) to pbufs (lwip if)
static struct pbuf_desc *pbufs;


uint64_t pbuf_alloc_RX_packets_2 = 0;

struct pbuf * get_pbuf_for_packet(void)
{
    struct pbuf *p;
    // We allocate a pbuf chain of pbufs from the pool.
    p = pbuf_alloc(PBUF_RAW, RECEIVE_PBUF_SIZE, PBUF_POOL);

    if (p == NULL) {
        return NULL;
    }

    ++pbuf_alloc_RX_packets_2;
    // Some sanity checks on the pbuf
    assert(p != NULL);
    assert(p->next == NULL);   //make sure there is no chain for now...
    assert(p->tot_len == RECEIVE_PBUF_SIZE);
    assert(p->len == RECEIVE_PBUF_SIZE);
    return p;
}

/** Add a pbuf to the rx ring. */
static errval_t add_pbuf_to_rx_ring(struct pbuf *p)
{
    ptrdiff_t offset;
    size_t idx;
    errval_t err = 1; // FIXME: assuming 1 is for failure

    // We allocate a pbuf chain of pbufs from the pool.
    //p = get_pbuf_for_packet();

    if (p == NULL) {
        USER_PANIC("NULL pbuf given for insertion\n");
        abort();
        return err;
    }

    // Some sanity checks on the pbuf
    assert(p != NULL);
    assert(p->next == NULL);   //make sure there is no chain for now...
    assert(p->tot_len == RECEIVE_PBUF_SIZE);
    assert(p->len == RECEIVE_PBUF_SIZE);


    offset = p->payload - binding_to_buffer(RX_BUFFER_ID);
    //printf("offset=%"PRIu64"\n", offset);
    assert(offset % buffer_size == 0);
    assert(offset < buffer_count * buffer_size);

    idx = mem_barrelfish_put_pbuf(p);
    err = buffer_rx_add(idx);

    // FIXME: shouldn't this be done by calling function?
    if (err != SYS_ERR_OK) {
        pbuf_free(p);
        p = NULL;
    }
    return err;
}

/** Populate RX ring initially with pbufs. */
static void rx_populate_ring(void)
{
    size_t i;

    // Allocate pbuf map
    assert(pbufs == NULL);
    pbufs = calloc(buffer_count, sizeof(struct pbuf_desc));

    struct pbuf *added_pbuf = NULL;
    // Fill ring
    for (i = 0; i < RECEIVE_BUFFERS; i++) {

        added_pbuf = get_pbuf_for_packet();
        if (added_pbuf == NULL) {
            // FIXME: Here, maybe application should just for some other events
            USER_PANIC("Not enough pbufs to initialize the ring with %d pbufs.\n"
                    "Only %zu inserted till now\n", RECEIVE_BUFFERS, i);
            abort();
        }
        errval_t err =  add_pbuf_to_rx_ring(added_pbuf);
        if (err != SYS_ERR_OK) {
            pbuf_free(added_pbuf);
            USER_PANIC("Could not push pbuf (%zu) into driver\n", i);
            abort();
        }

    } // end for:  for every slot
} // end rx_populate_ring


/**
 * Return the buffer size for a particular binding
 */
static inline size_t binding_to_buffersize(uint8_t binding_index)
{
    return (buffer_count / 2) * buffer_size;
}

/**
 * Return the buffer address for a particular binding.
 * The idea is that we use the first half of the buffers for RX and the second
 * half for TX.
 */
static inline void *binding_to_buffer(uint8_t binding_index)
{
    uint8_t *buf = buffer_base;

    if (binding_index == TX_BUFFER_ID) {
        buf += binding_to_buffersize(RX_BUFFER_ID);
    }

    return buf;
}

/**
 * Used to allocate memory for RX/TX buffers in lwip.
 * At the moment it must * only be called once for each binding (2 total).
 */
uint8_t *mem_barrelfish_alloc(uint8_t binding_index, uint32_t size)
{
    static bool rx_done = false;
    static bool tx_done = false;

    LWIPBF_DEBUG("@@@@@@ mem alloc %" PRIx32 " for index %d\n", size,
                 binding_index);

    // Make sure it's only called once for each direction
    assert((binding_index != RX_BUFFER_ID) || !rx_done);
    assert((binding_index != TX_BUFFER_ID) || !tx_done);

    // Check the size parameter
    assert(size <= binding_to_buffersize(binding_index));

    if (binding_index == TX_BUFFER_ID) {
        tx_done = true;
    } else {
        rx_done = true;
    }

    return binding_to_buffer(binding_index);
}

uint8_t *mem_barrelfish_register_buf(uint8_t binding_index, uint32_t size)
{
    // Here we don't have to register the buffer, as it is already registered by
    // the benchmark interface. the only thing we have to do is populating the
    // RX ring.

    if (binding_index == RX_BUFFER_ID) {
        rx_populate_ring();
    }

    return binding_to_buffer(binding_index);
}

/**
 * Add a new pbuf to the rx ring to replace an existing one, in which a packet
 * has been received.
 */
errval_t mem_barrelfish_replace_pbuf(struct pbuf *p)
{
    return add_pbuf_to_rx_ring(p);
}

/**
 * Resolve a pbuf id into a pbuf handle.
 */
struct pbuf *mem_barrelfish_get_pbuf(uint64_t pbuf_id)
{
    return (pbufs[pbuf_id].p);
}

/**
 * Register a pbuf to get an id for it. The pbuf has to be allocated using
 * mem_barrelfish_alloc!
 */
uint64_t mem_barrelfish_put_pbuf(struct pbuf *pbuf)
{
    size_t idx;
    ptrdiff_t offset = pbuf->payload - buffer_base;
    assert(offset < buffer_size * buffer_count);
    assert(offset % buffer_size + pbuf->len <= buffer_size);

    idx = offset / buffer_size;
    pbufs[idx].p = pbuf;
    return idx;
}


