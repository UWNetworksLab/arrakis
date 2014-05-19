/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef E10K_QUEUE_H_
#define E10K_QUEUE_H_

#include <string.h>
#include <stdlib.h>
#include <arch/x86/barrelfish_kpi/asm_inlines_arch.h>

#include "e10k_q_dev.h"

struct e10k_queue_ops {
    errval_t (*update_txtail)(void*, size_t);
    errval_t (*update_rxtail)(void*, size_t);
};

/**
 * Context structure for RX descriptors. This is needed to implement RSC, since
 * we need to be able to chain buffers together. */
struct e10k_queue_rxctx {
    void                    *opaque;
    struct e10k_queue_rxctx *previous;
    bool                    used;
};

struct e10k_queue {
    // FIXME: Look for appropriate type for the _head/tail/size fields
    e10k_q_tdesc_adv_wb_array_t*    tx_ring;
    void**                          tx_opaque;
    bool*                           tx_isctx;
    size_t                          tx_head;
    size_t                          tx_tail, tx_lasttail;
    size_t                          tx_size;
    uint32_t*                       tx_hwb;

    e10k_q_rdesc_adv_wb_array_t*    rx_ring;
    struct e10k_queue_rxctx*        rx_context;
    size_t                          rx_head;
    size_t                          rx_tail;
    size_t                          rx_size;

    struct e10k_queue_ops           ops;
    void*                           opaque;
};

typedef struct e10k_queue e10k_queue_t;

static inline e10k_queue_t* e10k_queue_init(void* tx, size_t tx_size,
    uint32_t* tx_hwb, void* rx, size_t rx_size, struct e10k_queue_ops* ops,
    void* opaque)
{
    e10k_queue_t* q = malloc(sizeof(*q));

    q->tx_ring = tx;
    q->tx_opaque = calloc(tx_size, sizeof(void*));
    q->tx_isctx = calloc(tx_size, sizeof(bool));
    q->tx_head = 0;
    q->tx_tail = q->tx_lasttail = 0;
    q->tx_size = tx_size;
    q->tx_hwb = tx_hwb;

    q->rx_ring = rx;
    q->rx_context = calloc(rx_size, sizeof(*q->rx_context));
    q->rx_head = 0;
    q->rx_tail = 0;
    q->rx_size = rx_size;

    q->ops = *ops;
    q->opaque = opaque;

    // Initialize ring memory with zero
    memset(tx, 0, tx_size * e10k_q_tdesc_adv_wb_size);
    memset(rx, 0, rx_size * e10k_q_rdesc_adv_wb_size);

    return q;
}

static inline int e10k_queue_add_txcontext(e10k_queue_t* q, uint8_t idx,
    uint8_t maclen, uint16_t iplen, uint8_t l4len, e10k_q_l4_type_t l4t)
{
    e10k_q_tdesc_adv_ctx_t d;
    size_t tail = q->tx_tail;

    memset(q->tx_ring[tail], 0, e10k_q_tdesc_adv_wb_size);

    // TODO: Check if there is room in the queue
    q->tx_isctx[tail] = true;
    d = q->tx_ring[tail];

    e10k_q_tdesc_adv_rd_dtyp_insert(d, e10k_q_adv_ctx);
    e10k_q_tdesc_adv_rd_dext_insert(d, 1);

    /* e10k_q_tdesc_adv_ctx_bcntlen_insert(d, 0x3f); */
    e10k_q_tdesc_adv_ctx_idx_insert(d, idx);
    e10k_q_tdesc_adv_ctx_maclen_insert(d, maclen);
    e10k_q_tdesc_adv_ctx_iplen_insert(d, iplen);
    e10k_q_tdesc_adv_ctx_ipv4_insert(d, 1);
    e10k_q_tdesc_adv_ctx_l4len_insert(d, l4len);
    e10k_q_tdesc_adv_ctx_l4t_insert(d, l4t);

    q->tx_lasttail = q->tx_tail;
    q->tx_tail = (tail + 1) % q->tx_size;
    return 0;
}


static inline int e10k_queue_add_txbuf_ctx(e10k_queue_t* q, uint64_t phys,
    size_t len, void* opaque, int first, int last, size_t totallen,
    uint8_t ctx, bool ixsm, bool txsm)
{
    e10k_q_tdesc_adv_rd_t d;
    size_t tail = q->tx_tail;

    memset(q->tx_ring[tail], 0, e10k_q_tdesc_adv_wb_size);

    // TODO: Check if there is room in the queue
    q->tx_isctx[tail] = false;
    q->tx_opaque[tail] = opaque;
    d = q->tx_ring[tail];

    e10k_q_tdesc_adv_rd_buffer_insert(d, phys);
    e10k_q_tdesc_adv_rd_dtalen_insert(d, len);
    if (first) {
        e10k_q_tdesc_adv_rd_paylen_insert(d, totallen);
    }
    e10k_q_tdesc_adv_rd_dtyp_insert(d, e10k_q_adv_data);
    e10k_q_tdesc_adv_rd_dext_insert(d, 1);
    e10k_q_tdesc_adv_rd_rs_insert(d, (last == 1));
    e10k_q_tdesc_adv_rd_ifcs_insert(d, 1);
    e10k_q_tdesc_adv_rd_eop_insert(d, last);

    if (ctx != -1U) {
        e10k_q_tdesc_adv_rd_idx_insert(d, ctx);
        e10k_q_tdesc_adv_rd_cc_insert(d, 1);
        e10k_q_tdesc_adv_rd_ixsm_insert(d, ixsm);
        e10k_q_tdesc_adv_rd_txsm_insert(d, txsm);
    }

    q->tx_lasttail = q->tx_tail;
    q->tx_tail = (tail + 1) % q->tx_size;
    return 0;
}

static inline int e10k_queue_add_txbuf(e10k_queue_t* q, uint64_t phys,
    size_t len, void* opaque, int first, int last, size_t totallen)
{
    return e10k_queue_add_txbuf_ctx(q, phys, len, opaque, first, last, totallen,
            -1, false, false);
}

/*
 * Reclaim 1 packet from the TX queue once it's handled by the
 * card. Call multiple times to reclaim more packets.
 *
 * \param       q       Queue to check
 * \param       opaque  Contains opaque data of reclaimed packet, if any
 *
 * \return 1 if no packet can be reclaimed, 0 otherwise.
 */
static inline int e10k_queue_get_txbuf(e10k_queue_t* q, void** opaque)
{
    /* e10k_q_tdesc_adv_wb_t d; */
    size_t head = q->tx_head;
    int result = 1;

    // If HWB is enabled, we can skip reading the descriptor if nothing happened
    if (q->tx_hwb && *q->tx_hwb == head) {
        return 1;
    }

    if(!q->tx_hwb) {
        size_t idx = head;

        // Skip over context and non-EOP descriptors
        while(idx != q->tx_tail && q->tx_isctx[idx] && !e10k_q_tdesc_adv_wb_dd_extract(q->tx_ring[idx])) {
            idx = (idx + 1) % q->tx_size;
        }

        /* d = q->tx_ring[head]; */
        /* d = q->tx_ring[idx]; */
        /* if (!e10k_q_tdesc_adv_wb_dd_extract(d)) { */
        /*     return 1; */
        /* } */

        if(idx == q->tx_tail) {
            return 1;
        }
    }

    // Check the last written packet, which should be marked with a
    // write-back indicator (RS bit)
    /* if (!q->tx_hwb) { */
    /*     assert(!q->tx_isctx[q->tx_lasttail]); */
    /*     if(!e10k_q_tdesc_adv_wb_dd_extract(q->tx_ring[q->tx_lasttail])) { */
    /*         return 1; */
    /*     } */
    /* } */

    // That last packet got written out, now go reclaim from the head pointer.
    if (!q->tx_isctx[head]) {
        *opaque = q->tx_opaque[head];
        result = 0;
    }

    /* memset(q->tx_ring[head], 0, e10k_q_tdesc_adv_wb_size); */
    q->tx_head = (head + 1) % q->tx_size;
    return result;
}

static inline errval_t e10k_queue_bump_txtail(e10k_queue_t* q)
{
    return q->ops.update_txtail(q->opaque, q->tx_tail);
}

static inline size_t e10k_queue_free_txslots(e10k_queue_t* q)
{
    size_t head = q->tx_head;
    size_t tail = q->tx_tail;
    size_t size = q->tx_size;

    if (tail >= head) {
        return size - (tail - head) - 1; // TODO: could this be off by 1?
    } else {
        return size - (tail + size - head) - 1; // TODO: off by 1?
    }

}

#include <stdio.h>
static inline int e10k_queue_add_rxbuf(e10k_queue_t* q, uint64_t phys,
    void* opaque)
{
    e10k_q_rdesc_adv_rd_t d;
    size_t tail = q->rx_tail;
    struct e10k_queue_rxctx *ctx;

    ctx = q->rx_context + tail;
    if (ctx->used) {
        printf("e10k: Already used!\n");
        return 1;
    }

    // TODO: Check if there is room in the queue
    ctx->opaque = opaque;
    ctx->used = true;
    d = (e10k_q_rdesc_adv_rd_t) q->rx_ring[tail];

    e10k_q_rdesc_adv_rd_buffer_insert(d, phys);
    // TODO: Does this make sense for RSC?
    e10k_q_rdesc_adv_rd_hdr_buffer_insert(d, 0);

    q->rx_tail = (tail + 1) % q->rx_size;

    return 0;
}

static inline uint64_t e10k_queue_convert_rxflags(e10k_q_rdesc_adv_wb_t d)
{
    uint64_t flags = 0;

    // IP checksum
    if (e10k_q_rdesc_adv_wb_ipcs_extract(d)) {
        flags |= NETIF_RXFLAG_IPCHECKSUM;
        if (!e10k_q_rdesc_adv_wb_ipe_extract(d)) {
            flags |= NETIF_RXFLAG_IPCHECKSUM_GOOD;
        }
    }

    // L4 checksum
    if (e10k_q_rdesc_adv_wb_l4i_extract(d)) {
        flags |= NETIF_RXFLAG_L4CHECKSUM;
        if (!e10k_q_rdesc_adv_wb_l4e_extract(d)) {
            flags |= NETIF_RXFLAG_L4CHECKSUM_GOOD;
        }
    }

    // Packet type
    if (e10k_q_rdesc_adv_wb_pt_ipv4_extract(d)) {
        flags |= NETIF_RXFLAG_TYPE_IPV4;
    }
    if (e10k_q_rdesc_adv_wb_pt_tcp_extract(d)) {
        flags |= NETIF_RXFLAG_TYPE_TCP;
    }
    if (e10k_q_rdesc_adv_wb_pt_udp_extract(d)) {
        flags |= NETIF_RXFLAG_TYPE_UDP;
    }

    return flags;
}

static inline size_t e10k_queue_get_rxbuf(e10k_queue_t* q, void** opaque,
    size_t* len, int* last, uint64_t *flags)
{
    e10k_q_rdesc_adv_wb_t d;
    size_t head = q->rx_head;
    struct e10k_queue_rxctx *ctx;
    struct e10k_queue_rxctx *ctx_next;
    size_t nextp;

    d = q->rx_ring[head];
    ctx = q->rx_context + head;

    if (!e10k_q_rdesc_adv_wb_dd_extract(d)) {
        return 1;
    }

    // Barrier needed according to linux driver to make sure nothing else is
    // read before the dd bit TODO: make sure
    lfence();

    if (e10k_q_rdesc_adv_wb_rsccnt_extract(d)) {
        printf("e10k.q0: Part of a large receive\n");
    }

    // RSC: we've already received parts of the large receive, but haven't
    // returned them to the caller, so they will be returned first
    if (ctx->previous && e10k_q_rdesc_adv_wb_eop_extract(d)) {
        printf("e10k: Return part RSC\n");
        // Look for first buffer in chain
        do {
            ctx_next = ctx;
            ctx = ctx->previous;
        } while (ctx->previous != NULL);
        ctx_next->previous = NULL;

        head = ctx - q->rx_context;
        d = q->rx_ring[head];

        // TODO: Extract status (okay/error)
        *last = 0;
        *len = e10k_q_rdesc_adv_wb_pkt_len_extract(d);
        *opaque = ctx->opaque;

        ctx->used = false;
        memset(d, 0, e10k_q_rdesc_adv_wb_size);
        return 0;
    }

    // RSC: We just received part of a large receive (not the last packet)
    // chain this buffer to the indicated next one
    if (e10k_q_rdesc_adv_wb_rsccnt_extract(d) &&
            !e10k_q_rdesc_adv_wb_eop_extract(d))
    {
        printf("e10k: RSC chained\n");
        e10k_q_rdesc_adv_wb_nl_t n = d;
        nextp = e10k_q_rdesc_adv_wb_nl_nextp_extract(n);
        assert(nextp < q->rx_size);

        ctx_next = q->rx_context + nextp;
        assert(ctx_next->used);
        ctx_next->previous = ctx;

         q->rx_head = (head + 1) % q->rx_size;
         return 1;
    }

    *flags = 0;
    // Set flags if it this is a descriptor with EOP
    // TODO: with multi-part packets, we want these flags on the first packet
    if (e10k_q_rdesc_adv_wb_eop_extract(d)) {
        *flags = e10k_queue_convert_rxflags(d);
    }

    // TODO: Extract status (okay/error)
    *last = e10k_q_rdesc_adv_wb_eop_extract(d);
    *len = e10k_q_rdesc_adv_wb_pkt_len_extract(d);
    *opaque = ctx->opaque;

    ctx->used = false;
    memset(d, 0, e10k_q_rdesc_adv_wb_size);

    q->rx_head = (head + 1) % q->rx_size;
    return 0;
}

static inline errval_t e10k_queue_bump_rxtail(e10k_queue_t* q)
{
    return q->ops.update_rxtail(q->opaque, q->rx_tail);
}

static inline size_t e10k_queue_free_rxslots(e10k_queue_t* q)
{
    size_t head = q->rx_head;
    size_t tail = q->rx_tail;
    size_t size = q->rx_size;

    if (tail >= head) {
        return size - (tail - head) - 1; // TODO: could this be off by 1?
    } else {
        return size - (tail + size - head) - 1; // TODO: off by 1?
    }
}


#endif // ndef E10K_QUEUE_H_
