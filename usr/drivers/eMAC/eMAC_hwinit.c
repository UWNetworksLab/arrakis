/**
 * \file
 * \brief Intel e1000 driver: Initialize the hardware
 *
 *
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */



#include <barrelfish/barrelfish.h>
#include <string.h>
// Following two are needed for call "sys_debug_feign_frame_cap"
#define  ENABLE_FEIGN_FRAME_CAP
#include <barrelfish/sys_debug.h>
#include <barrelfish/inthandler.h>
#include <net_queue_manager/net_queue_manager.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>


#include "eMAC_dev.h"
#include "eMAC_driver.h"

#include "eMAC_debug.h"

/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE


#define MIN(a, b) (((a) < (b)) ? (a) : (b))

static eMAC_t emac;

#define EMAC_CFG_REGS_BASE         (0xF9000000)
#define EMAC_CFG_REGS_SIZE         (0x10000)

#define EMAC_IRQ_CONFIG 1       /// Pin on core to use (1 == LINT1)
/*
#define EMAC0_IRQ       0x1     /// IRQ of eMAC #2
#define EMAC1_IRQ       0x2     /// IRQ of eMAC #2
#define EMAC2_IRQ       0x4     /// IRQ of eMAC #2
#define EMAC3_IRQ       0x8     /// IRQ of eMAC #2
*/

#define CSIZE           (32)
#define SLOTS           (1 << 16)
#define BUFF_SIZE       (SLOTS * CSIZE)
#define TX_MAX_PKT_SIZE 1536


static struct capref RX_capframe;
static void *RX_internal_memory_pa = NULL;
static void *RX_internal_memory_va = NULL;
static uint32_t volatile RX_read_index = 0;

static struct capref TX_capframe;
static void *TX_internal_memory_pa = NULL;
static void *TX_internal_memory_va = NULL;
static uint32_t volatile TX_write_index = 0;
static uint32_t TX_max_slots = SLOTS - 1;


// For measurement of interrupts
extern uint64_t interrupt_counter;

/* MAC address */
extern uint64_t eMAC_mac;

/* set at hwinit */
static uint8_t eMAC_PHY_id = 0; /* Which emac PHY you are using? */
//static interrupt_handler_fn high_level_handler = NULL;


/* FIXME: allocate following buffer with malloc (or get it from user apps) */

#define MAX_FRAME_SIZE  1600
static uint8_t dummy_pkt[MAX_FRAME_SIZE];

/* FIXME: remove these */
static volatile uint8_t *rb_RX = NULL;
static volatile uint8_t *rb_TX = NULL;
static volatile uint16_t *rb_TX_header = NULL;
static volatile uint16_t *rb_RX_header = NULL;


static unsigned long long rx_tile_offset = 0;
static int rx_pos = 0;
static int rx_mode = 0;

static uint8_t core_id = 0;
//static uint8_t core_id = 0;
static uint64_t call_counter = 0;
static uint64_t pkt_counter = 0;

static size_t get_cachelines(uint64_t data_size)
{
    size_t cachelines = (data_size)/ CSIZE;
    if (((data_size)%CSIZE) > 0) {
        ++cachelines;
    }
    return cachelines;
}

/*
static void print_cline(uint8_t *cline)
{
    EMAC_DEBUG("Cacheline [");
    for(int j = 0; j < 32; ++j) {
        printf("%x,", cline[j]);
    }
    printf("]\n");
}
*/
/*
static void increment_TX_write_index(void)
{
    ++TX_write_index;
    if (TX_write_index > TX_max_slots ) {
        TX_write_index = 1;
    }
} // end function: increment_TX_write_index
*/

static void increment_RX_read_index(void)
{
    ++RX_read_index;
    if (RX_read_index > RX_max_slots || RX_read_index == 0) {
        RX_read_index = 1;
    }
} /* end function: increment_read_index */

/*
static void my_memcpy(uint8_t *dst, uint8_t *src, size_t len)
{
    for(int i = 0; i < len; ++i){
        dst[i] = src[i];
    }
}
*/

static void show_packet(uint8_t *pkt, size_t len)
{
/*
    printf("\n[");
    for(int i = 0; i < len; ++i) {
        if(i % 16 == 0){
            printf("\n%04x", i);
        }
        printf(" %02x", pkt[i]);
    }
    printf("\n] = %u\n", len);
*/
} /* end function: show_packet */

static void receive_frame_data(void)
{
    /* 9.6.5.4 Receiving Frame Data */
    /*
        1. Check for updated write index in head of buffer
                (or FPGA register) (RX buffer+0x00)
            a. Read 4 bytes from RX buffer at address 0 and compare
                the write index value to the internal read index value stored
                in the driver
    */

    eMAC_RX_Buffer_read_index_t idx_r;
    eMAC_RX_Buffer_write_index_t idx_w;
    /*
    uint16_t rid;
    uint16_t wid;
*/

    cl1flushmb();
    uint16_t volatile wid = 0;
    uint8_t volatile *buf = (uint8_t *) RX_internal_memory_va;
    void *pkt_to_upload = NULL;
    ++call_counter;

    uint16_t volatile wid1 = 0;
    uint16_t volatile rid1 = 0;
    idx_r = eMAC_eMAC2_RX_Buffer_read_index_rd(&emac, core_id);
    idx_w = eMAC_eMAC2_RX_Buffer_write_index_rd(&emac, core_id);
    rid1 = idx_r.rid;
    wid1 = idx_w.wid;

    if (rid1 != wid1 ){
        EMAC_DEBUG("######## we have a packet! rid [%"PRIx16"] wid[%"PRIx16"]\n",
                rid1, wid1);
        EMAC_DEBUG("old values %"PRIx32", %"PRIx16", %"PRIx16"\n",
                RX_read_index, rb_RX_header[0], rb_RX_header[1]);
//        abort();
    }

    wid = rb_RX_header[0];
    wid = wid1;
    RX_read_index = rid1;
    if (RX_read_index == wid) {
        return;
    }


    assert( wid <= RX_max_slots);

/*    EMAC_DEBUG("%" PRIx64 ": new pkt [rid1 (%u) (%u) == wid1(%u) (%u)]\n",
            call_counter, rid1, rb_RX_header[1],  wid1, rb_RX_header[0]);
*/
    EMAC_DEBUG("MOVEMENT: %" PRIx64 " [rid (%x) != wid (%x)]\n",
            call_counter, RX_read_index,  wid);

    do {
        increment_RX_read_index();

        uint16_t frame_len = 0;
        uint16_t *frame_len_ptr;
        uint32_t copied = 0;

        uint8_t *pkt_ptr = (uint8_t *)(buf + (RX_read_index * 32));
//        show_packet(pkt_ptr, 90);
        EMAC_DEBUG("buf location (%p) + index (%x) = pkt location (%p)\n",
                buf, RX_read_index * 32, pkt_ptr);
        frame_len_ptr = (uint16_t *)pkt_ptr;
        frame_len = *frame_len_ptr;
        if(frame_len == 0){
            /* printf("%d:invalid packet received, %x %x\n", RX_read_index, wid1); */
            RX_read_index = wid1;
            goto finish;
        }
        EMAC_DEBUG("[RX pkt len (%u)]\n", frame_len);
        uint32_t clines = get_cachelines(frame_len + 2);
        EMAC_DEBUG("[clines (%u)]\n", clines);
        if(RX_read_index + clines - 1 <= RX_max_slots) {

            pkt_to_upload = pkt_ptr + 2;
            copied = frame_len;
            RX_read_index = RX_read_index + clines - 1;
            EMAC_DEBUG("[directly passed the pkt of len (%u), new [rid (%x) != wid (%x)]]\n",
                    copied, RX_read_index, wid);
        } else {

            EMAC_DEBUG("Broken packet received\n");
            memcpy_fast(dummy_pkt, pkt_ptr + 2, CSIZE - 2);
            copied = CSIZE - 2;

            while (copied < frame_len) {
                if(RX_read_index == wid) {
                    /* FIXME: complete packet is not there. */
                    /* discard the half packet, and return */
                    printf("ERROR: Half packet received!!!, dropping it\n");
                    return;
                }
                increment_RX_read_index();
                pkt_ptr = (uint8_t *)(buf + (RX_read_index * 32));
                uint16_t to_copy = MIN((frame_len - copied), CSIZE);
                memcpy_fast(dummy_pkt + copied, pkt_ptr, to_copy);
                copied = copied + to_copy;
            }
            pkt_to_upload = dummy_pkt;
        } /* end else: copy the pkt */

        EMAC_DEBUG("Following pkt received\n");
        show_packet(pkt_to_upload, copied);
        process_received_packet(pkt_to_upload, copied, true);

    } while (RX_read_index != wid);

 finish:
    /* processing the packet */
    eMAC_eMAC2_RX_Buffer_read_index_rid_wrf(&emac, core_id, RX_read_index);
    rb_RX_header[1] = RX_read_index;
    ++pkt_counter;
    EMAC_DEBUG("pkt no %" PRIx64 " processed. \n", pkt_counter);
    return;

} /* end function: receive_frame_data */


static void polling(void)
{
    printf("started polling.....\n");
    while(1){
        receive_frame_data();
    }
}


static void read_out_regs(void)
{
    char s[1000];
    s[0] = 0;
    EMAC_DEBUG("reading the registers\n");
//    eMAC_eMAC_start_IP_SCC_network_pr();
//    eMAC_eMAC_start_IP_SCC_network_reg_pr(s, 999, &emac);
    EMAC_DEBUG("### details[%s]\n", s);

    eMAC_eMAC_host_IP_addr_reg_pr(s, 999, &emac);
    EMAC_DEBUG("### details[%s]\n", s);

    eMAC_eMAC_host_GW_addr_reg_pr(s, 999, &emac);
    EMAC_DEBUG("### details[%s]\n", s);

    eMAC_eMAC_MAC_base_addr_upper_reg_pr(s, 999, &emac);
    EMAC_DEBUG("### MAC upper[%s]\n", s);


    eMAC_eMAC_MAC_base_addr_lower_reg_pr(s, 999, &emac);
    EMAC_DEBUG("### MAC lower[%s]\n", s);

} /* end function: read_out_regs */


/*****************************************************************
 * Transmit logic
 ****************************************************************/
/*
 * pbuf_list_memcpy(addr + 2, skb->data, skb->len);
 * pbuf_list_memcpy(addr + 2, skb->data, bytes_to_copy);
 * pbuf_list_memcpy(addr, skb->data + bytes_to_copy, bytes_left);
 * */
static void pbuf_list_memcpy(uint8_t *dst, struct client_closure *cl,
        size_t start_offset, size_t to_copy)
{
    int pbuf_offset = 0;
    int copying = 0;
    int data_left = 0;
    int already_copied = 0;
    uint64_t pbuf_len = 0;
    struct shared_pool_private *spp = cl->spp_ptr;
    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
    uint64_t rtpbuf = sld->no_pbufs;

    struct buffer_descriptor *buffer = find_buffer(sld->buffer_id);

    for (int idx = 0; idx < rtpbuf; idx++) {
        sld = &spp->sp->slot_list[cl->tx_index + idx].d;
        assert(buffer->buffer_id == sld->buffer_id);
//        paddr = (uint64_t) buffer->pa + sld->offset;

        /* check if this pbuf contains any data that is to be copied */
        if((pbuf_len + sld->len) < start_offset) {
            pbuf_len = (pbuf_len + sld->len);
            continue;
        }
        if(already_copied == 0) {
            /* Start offset lies somewhere in this pbuf.
             * So, this is the first pbuf where some data will get copied. */
            pbuf_offset = start_offset - pbuf_len;
            data_left = sld->len - pbuf_offset;
        } else {
            // copying already started. so, just continue onwards in this pbuf
            pbuf_offset = 0;
            data_left = sld->len;
        }
        if(pbuf_offset < 0){
            EMAC_DEBUG("idx %d, pbufs = %u, to copy %zu\n",
                    idx, rtpbuf, to_copy);
            EMAC_DEBUG("start offset %zu, to_copy %zu\n", start_offset,
                    to_copy);
            EMAC_DEBUG("pbuf_offset %d = start_offset(%zu) - "
                    "pbuf_len (%"PRIu64")\n",
                    pbuf_offset, start_offset, pbuf_len);

            EMAC_DEBUG("pbuflen (%"PRIu64") + (%"PRIu64") < "
                    "start_offset(%zu)\n",
                    pbuf_len, sld->len, start_offset);
            EMAC_DEBUG("prev pbuf len (%"PRIu64"), already copied %d, "
                    "left %d\n", spp->sp->slot_list[cl->tx_index].d->len,
                    already_copied, data_left);
        }
        assert(pbuf_offset >= 0);
        assert(data_left >= 0);

        copying = MIN(to_copy, data_left);

        uint8_t *src =((uint8_t *)buffer->va) + sld->offset
                                + pbuf_offset;

        // FIXME: may be I should use memcpy_fast here!!
        memcpy_fast(dst + already_copied, src, copying);
        already_copied = already_copied + copying;

        if(already_copied == to_copy) {
            return;
        }
        pbuf_len = (pbuf_len + sld->len);
    } // end for:

    EMAC_DEBUG("ERROR: pbuf_list_memcpy: not enough data [%zu] in "
            "client_closure\n", to_copy);
    EMAC_DEBUG("pbufs = %u\n", rtpbuf);
    EMAC_DEBUG("already copied %d, left %d\n", already_copied, data_left);
    for (int idx = 0; idx < rtpbuf; idx++) {
        EMAC_DEBUG(" %d: pbuflen (%"PRIu64")\n", idx,
                spp->sp->slot_list[cl->tx_index + idx].d->len);
    }
    EMAC_DEBUG("start offset %zu, to_copy %zu\n", start_offset, to_copy);

    assert(!"Not enough data in pbuf_list to send");
} // end function: pbuf_list_memcpy


static uint64_t TX_pkt_counter = 0;

// FIXME: dynamically calcluate this ring size
#define eMAC_TX_RING_SIZE 1000
uint64_t get_tx_free_slots_count(void)
{
    // FIXME: dynamically calcluate this ring size
    return eMAC_TX_RING_SIZE;
}

errval_t transmit_pbuf_list(struct client_closure *cl)
{
    uint8_t *addr = NULL;
    uint16_t read_offset = 0;
    int rest = 0;
    int packets = 0;

    assert(!"Using older version of communication library\n");
    abort();
    struct shared_pool_private *spp = cl->spp_ptr;
    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
    uint64_t rtpbuf = sld->no_pbufs;

    // Find the length of entire packet
    uint64_t pkt_len = 0;
    for (int idx = 0; idx < rtpbuf; idx++) {
        pkt_len += spp->sp->slot_list[cl->tx_index + idx].d.len;
    }


//    assert(cl->rtpbuf == 1);
    if (pkt_len > TX_MAX_PKT_SIZE) {
        printf("ERROR: pkt too big (0x%"PRIu64")> %u\n",
                pkt_len, TX_MAX_PKT_SIZE);
        /* FIXME: maintain the stats of packet dropping */
        /* FIXME: define better error to return here */
        return ETHERSRV_ERR_CANT_TRANSMIT;
    }
    ++TX_pkt_counter;


    ++TX_write_index;
    if (TX_write_index > TX_max_slots ) {
        TX_write_index = 1;
    }
    packets = get_cachelines(pkt_len + 2);
    EMAC_DEBUG("^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
    EMAC_DEBUG("PKT_no:%"PRIu64", packet len: %"PRIu64", "
            "no. pbufs %"PRIu16", clines %d\n",
            TX_pkt_counter, pkt_len, rtpbuf, packets);

    eMAC_TX_Buffer_read_index_t idx_r;
    idx_r = eMAC_eMAC2_TX_Buffer_read_index_rd(&emac, core_id);
    read_offset = idx_r.rid;

    uint16_t volatile wid = 0;
    uint16_t volatile rid = 0;

    rid = rb_TX_header[0];
    wid = rb_TX_header[1];

    EMAC_DEBUG("TX rid: 0x%x,  wid 0x%x\n", read_offset, TX_write_index);
    EMAC_DEBUG("TX_rid: 0x%x, _wid 0x%x\n", rid, wid);

    EMAC_DEBUG("TX rid: 0x%x, wid 0x%x\n", read_offset, TX_write_index);

    /* checking for overflow */
/*    if (TX_write_index == read_offset){
        printf("ERROR: no space left as write_index(%u) == read_index(%u)\n",
                TX_write_index, read_offset);
        return -1;
    }
*/

#ifdef OVERFLOW_CHECK
again:
    int sum = 0;
    if (read_offset < TX_write_index) {
        sum = TX_max_slots - TX_write_index + read_offset - 1;
    } else if (read_offset > TX_write_index) {
        sum = read_offset - TX_write_index - 1;
    }

    if (sum < packets) {
        EMAC_DEBUG( "Warning: not enough space available. Retrying\n");
        goto again;
    }
#endif // OVERFLOW_CHECK

    addr = TX_internal_memory_va + (TX_write_index * 32);

    /* Set frame length */
    ((uint8_t*)addr)[0] = pkt_len % 256;
    ((uint8_t*)addr)[1] = pkt_len / 256;

    size_t already_copied = 0;
    if (TX_write_index + packets - 1 <= TX_max_slots) {
        /* enough space, just copy */
        EMAC_DEBUG("######## TX:  Simple case, just copy whole pkt ########\n");
        pbuf_list_memcpy(addr + 2, cl, 0, pkt_len);
//        my_memcpy(addr + 2, src, pkt_len);
        already_copied = already_copied + pkt_len;
        /* increment write ptr */
        TX_write_index += packets - 1;
    } else {
        /* wrap in offsets. first copy to the end, second at the starting
         * point
         */
        int bytes_left = pkt_len;
        int bytes_to_copy = (TX_max_slots - TX_write_index + 1) * 32 - 2;

        if (bytes_left < bytes_to_copy) {
            bytes_to_copy = bytes_left;
        }

        EMAC_DEBUG("#### special case: copy last %d bytes ####\n",
                bytes_to_copy);

        pbuf_list_memcpy(addr + 2, cl, 0, bytes_to_copy);
        //my_memcpy(addr + 2, src, bytes_to_copy);
        already_copied = already_copied + bytes_to_copy;
        bytes_left -= bytes_to_copy;

        if (bytes_left != 0) {
            TX_write_index = 1;
            addr = TX_internal_memory_va + 32;

            EMAC_DEBUG("#### special case: copy remaining %d bytes\n",
                    bytes_left);
            pbuf_list_memcpy(addr, cl, already_copied, bytes_left);
            //my_memcpy(addr, src + bytes_to_copy, bytes_left);
            already_copied = already_copied + bytes_left;

            rest = bytes_left % 32;
            if (rest != 0) {
                rest = 32 - rest;
            }
            EMAC_DEBUG("#### Rest is %d\n", rest);
            TX_write_index += ((bytes_left + rest)/CSIZE) - 1;
        }
    }
    ((uint32_t *)TX_internal_memory_va)[0] = 2;
//    writel(2, priv->tx_buffer); // FIXME: what and why is this???

    /* set new write offset */
    EMAC_DEBUG("##### Update tx write offset: %d (read offset %d)\n",
            TX_write_index, read_offset);
    eMAC_eMAC2_TX_Buffer_write_index_wid_wrf(&emac, core_id, TX_write_index);
    cl1flushmb();

    EMAC_DEBUG("packet len: %"PRIu64", clines %d\n", pkt_len, packets);
    char s[1000];
    eMAC_eMAC2_TX_Buffer_read_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX read index [%s]\n", s);

    eMAC_eMAC2_TX_Buffer_write_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX write index [%s]\n", s);

    rid = rb_TX_header[0];
    wid = rb_TX_header[1];
/*    printf("END TX rid: 0x%x,  wid 0x%x\n", read_offset, TX_write_index);
    printf("END TX_rid: 0x%x, _wid 0x%x\n", rid, wid);
*/
    /* FIXME: update the stats about successfull packet transmissions */


    // Tell the client we sent them!!!
    for (int i = 0; i < rtpbuf; i++) {
        assert(!"FIXME: handle_tx_done should send back the opaque pointer");
        abort();
        // associated with buffer sent.
        //handle_tx_done(cl->app_connection, (cl->tx_index + i));
        handle_tx_done(NULL);
    } // end for:


#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NO_S,
            (uint32_t)cl);
#endif // TRACE_ETHERSRV_MODE


    return SYS_ERR_OK;
} /* end function: transmit_pbuf_list */


static void initialize_TX(void)
{

    char s[1000];

    /* 9.6.5.3 Initialization TX */
    uint8_t volatile *buf = TX_internal_memory_va;

    rb_TX = (uint8_t *)buf;
    rb_TX_header = (uint16_t *)rb_TX;

    EMAC_DEBUG("TX buffer mem: lva [%p] == lpa [%p]\n", TX_internal_memory_va,
            TX_internal_memory_pa);

    uint32_t physical_addr = (uint32_t)TX_internal_memory_pa;
    /* getting 34 bit address from 32 bit address */
    /* ref: SCC_EAS.pdf (section 8.2: System Address Lookup Table (LUT)) */

    /* step 1: get 24 bits from physical address */
//    physical_addr = physical_addr & 0x00FFFFFF;


    /* step 2: Get the address part generated by LUT */
    struct scc_frame_identity sf;
    errval_t err = invoke_scc_frame_identify(TX_capframe, &sf);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "invoke_scc_frame_identify(TX_capframe): ");
    }
    EMAC_DEBUG("TX route details: route [%x], subdest [%x], addr[%x]\n",
            sf.route, sf.subdest, sf.addrbits);

    /* append */
    uint64_t big_address = ((sf.addrbits & 0x3FF) << 24) + physical_addr;
    lpaddr_t tmp_addr_p = 0;
    tmp_addr_p = (uint32_t)(big_address >> 5);
    EMAC_DEBUG("#### TX big addr %"PRIx64", in reg [%"PRIxLPADDR"]\n",
            big_address, tmp_addr_p);

    uint32_t tmp;
    tmp = (uint32_t)TX_internal_memory_pa;
    unsigned long long addr_offset = rx_tile_offset + tmp;
    EMAC_DEBUG("##### TX adder_offset [%llx]\n", addr_offset);
    addr_offset >>= 5;
    EMAC_DEBUG("##### TX in reg [%llx]\n", addr_offset);


    eMAC_eMAC2_TX_Buffer_start_addr_wr_raw(&emac, core_id, addr_offset);

    /*
        1. Set TX buffer address (GRB+0x9900):
            Shift 5 times right and set address.
    */
//    eMAC_eMAC2_TX_Buffer_start_addr_ADDR_wrf(&emac, core_id, tmp_addr_p);

    /*
        4. Set TX last index (GRB+0x9C00):
            Set last index to define buffer size (size = last index * 32).
    */
    eMAC_eMAC2_TX_Buffer_last_index_lid_wrf(&emac, core_id, TX_max_slots);

    s[0] = 0;
    eMAC_eMAC2_TX_Buffer_start_addr_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX start addr = %s\n", s);

    s[0] = 0;
    eMAC_eMAC2_TX_Buffer_last_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX write index = %s\n", s);

    cl1flushmb();

    /*
        2. Read TX read index (GRB+0x9A00):
            Read TX read index (note that the read index is read only for SW,
            thus cannot be initialized).
    */
    s[0] = 0;
    eMAC_eMAC2_TX_Buffer_read_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX read index = %s\n", s);

    eMAC_TX_Buffer_read_index_t idx2;
    idx2 = eMAC_eMAC2_TX_Buffer_read_index_rd(&emac, core_id);
    uint16_t rid = idx2.rid;

    /*
        3. Write TX write index (GRB+0x9B00):
            Set write index (both point to the same index now, thus the
            buffer is empty) and store value as driver internal write index.
    */
    eMAC_eMAC2_TX_Buffer_write_index_wid_wrf(&emac, core_id, rid);
    EMAC_DEBUG("TX rid (%u) == wid(%u)\n", rid, rid);

    s[0] = 0;
    eMAC_eMAC2_TX_Buffer_write_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX write index = %s\n", s);

    cl1flushmb();


    /* Write the value of Write-pointer, Read-pointer in the first block */
    rb_TX_header[0] = rid;
    rb_TX_header[1] = rid;
    TX_write_index = rid;

//    print_cline((uint8_t *)buf);

    /*
        5. Set TX route/destination (GRB+0x9D00):
            Set route and destination depending on selected core
            (e.g. for core0 0x600).
        0x0600 = 0b11000000000
                    110 00000000
    */
    eMAC_eMAC2_TX_routing_broute_wrf(&emac, core_id, sf.route);
    assert(sf.subdest <= 0b111);
    eMAC_eMAC2_TX_routing_bdest_wrf(&emac, core_id, (sf.subdest &0b111));

    s[0] = 0;
    eMAC_eMAC2_TX_routing_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX routing = %s\n", s);

    /*
         6. Activate TX network port (GRB+0x9E00): Set to 1.
     */
    eMAC_eMAC2_TX_net_port_enable_enable_wrf(&emac, core_id, 1);

    s[0] = 0;
    eMAC_eMAC2_TX_net_port_enable_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("TX port enable = %s\n", s);

} /* end function: initialize_TX */

/*
Determining the right MAC addresses can be done in different ways. The easiest way is to use the
MAC Address Base Register (0x07e00 to 0x07e04) in the FPGA and to calculate the MAC address
*/
/* MAC address of core 0 00:45:4D:41:48:31 */
/* [6] = {0x0, 0x45, 0x4D, 0x41, 0x48, 0x31};  MAC address */

static void display_mac_address(uint8_t *mac)
{
    printf("[%02x", mac[0]);
    for(int i = 1; i < 6; ++i){
        printf(":%02x", mac[i]);
    }
    printf("]\n");
}



static uint64_t get_mac_address(void)
{
    uint64_t mac = 0;

//    uint8_t base_mac_addr[6];
    eMAC_eMAC_MAC_base_addr_upper_t mac_hi =
            eMAC_eMAC_MAC_base_addr_upper_reg_rd(&emac);
    mac = mac_hi.mac_upper;
    mac = mac << 32;

    eMAC_eMAC_MAC_base_addr_lower_t mac_low =
            eMAC_eMAC_MAC_base_addr_lower_reg_rd(&emac);
    mac = mac + mac_low.mac_lower;

    printf("Base mac address:");
    display_mac_address((uint8_t *)&mac);


    /* NOTE: This formula is used so that, MAC's generated will match the
     * SCC Linux distribution's MAC address. */
    mac = mac + (((1 << eMAC_PHY_id) * 0x100) + core_id);
    /* FIXME: core1 should get IP even with following line,
     * investigate why does it not get it?? */
//    mac = mac + (((1 << eMAC_PHY_id) * 0x100));
    eMAC_mac = mac;
    printf("Core MAC address:");
    display_mac_address((uint8_t *)&eMAC_mac);
    return eMAC_mac;
} /* end function: get_mac_address */



static int route[24][2] = {
        {0, 0},
        {0, 1},
        {0, 2},
        {0, 3},
        {0, 4},
        {0, 5},
        {1, 0},
        {1, 1},
        {1, 2},
        {1, 3},
        {1, 4},
        {1, 5},
        {2, 0},
        {2, 1},
        {2, 2},
        {2, 3},
        {2, 4},
        {2, 5},
        {3, 0},
        {3, 1},
        {3, 2},
        {3, 3},
        {3, 4},
        {3, 5}
};


static void initialize_RX(void)
{
    char s[1000];

    /* 9.6.5.2 Initialization RX */
    uint8_t volatile *buf = (uint8_t *) RX_internal_memory_va;

    rb_RX = buf;
    rb_RX_header = (uint16_t *)rb_RX;

    EMAC_DEBUG("RX buffer mem: lva [%p] == lpa [%p]\n", RX_internal_memory_va,
            RX_internal_memory_pa);

    uint32_t physical_addr = (uint32_t)RX_internal_memory_pa;
    /* getting 34 bit address from 32 bit address */
    /* ref: SCC_EAS.pdf (section 8.2: System Address Lookup Table (LUT)) */

    /* step 1: get 24 bits from physical address */
//    physical_addr = physical_addr & 0x00FFFFFF;


    /* step 2: Get the address part generated by LUT */
    struct scc_frame_identity sf;
    errval_t err = invoke_scc_frame_identify(RX_capframe, &sf);
    if(err_is_fail(err)){
        USER_PANIC_ERR(err, "invoke_scc_frame_identify(TX_capframe): ");
    }
    EMAC_DEBUG("RX route details: route [%x], subdest [%x], addr[%x]\n",
            sf.route, sf.subdest, sf.addrbits);

    printf("#### phywsical addr %"PRIxLPADDR"\n", physical_addr);
    /* append */
    uint64_t big_address = ((sf.addrbits & 0x3FF) << 24) + physical_addr;

    /*
    1. Set RX buffer address (GRB+0x9000):
        Shift 5 times to the right and set address.
    */
    lpaddr_t tmp_addr_p = 0;
    tmp_addr_p = (uint32_t)(big_address >> 5);
    EMAC_DEBUG("#### big addr %"PRIx64", in reg [%"PRIxLPADDR"]\n",
            big_address, tmp_addr_p);

    uint32_t tmp;
    tmp = (uint32_t)RX_internal_memory_pa;
    unsigned long long addr_offset = rx_tile_offset + tmp;
    EMAC_DEBUG("##### adder_offset [%llx]\n", addr_offset);
    addr_offset >>= 5;
    EMAC_DEBUG("##### in reg [%llx]\n", addr_offset);

    eMAC_eMAC2_RX_Buffer_start_addr_wr_raw(&emac, core_id, addr_offset);

//    eMAC_eMAC2_RX_Buffer_start_addr_wr_raw(&emac, core_id, (uint32_t)tmp_addr_p);
//    eMAC_eMAC2_RX_Buffer_start_addr_ADDR_wrf(&emac, core_id, tmp_addr_p);

    /*
    4. Set RX last index (GRB+0x9300):
        Set last index to define buffer size (size = last index * 32).
    */
    eMAC_eMAC2_RX_Buffer_last_index_lid_wrf(&emac, core_id, RX_max_slots);

    s[0] = 0;
    eMAC_eMAC2_RX_Buffer_start_addr_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX start addr = %s\n", s);

    s[0] = 0;
    eMAC_eMAC2_RX_Buffer_last_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX write index = %s\n", s);

    cl1flushmb();

    /*
    2. Read RX write index (GRB+0x9200):
            Read write index (note that the write index registers are read
            only by SW, thus cannot be set to an initial value).
    */

    s[0] = 0;
    eMAC_eMAC2_RX_Buffer_write_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX write index = %s\n", s);
//    uint16_t idx = eMAC_eMAC2_RX_Buffer_write_index_wid_rdf(&emac, core_id);
    eMAC_RX_Buffer_write_index_t idx2;
    idx2 = eMAC_eMAC2_RX_Buffer_write_index_rd(&emac, core_id);
    uint16_t wid = idx2.wid;
    /*
    3. Set RX write index to RX read index (GRB+0x9100):
        Set read index (both point to the same index now, thus the buffer is empty) and store value as
        driver internal read index.
    */
    eMAC_eMAC2_RX_Buffer_read_index_rid_wrf(&emac, core_id, wid);
    EMAC_DEBUG("RX rid (%u) == wid(%u)\n", wid, wid);

    s[0] = 0;
    eMAC_eMAC2_RX_Buffer_read_index_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX write index = %s\n", s);

    cl1flushmb();

    /* Write the value of Write-pointer, Read-pointer in the first block */
    rb_RX_header[0] = wid;
    rb_RX_header[1] = wid;
    RX_read_index = wid;

//    print_cline((uint8_t *)buf);

    /*
    5. Set RX route/destination (GRB+0x9500):
        Set route and destination depending on selected core
        (e.g. for core0 0x600).
        0x0600 = 0b11000000000
                    110 000 00000
    */



    uint8_t route_val = (route[core_id/2][0] << 4) |(route[core_id/2][1]);
    assert(sf.subdest <= 0b111);


    uint32_t rr = 0;

    rr = rr + ((core_id & 1) <<        (32 - (5 + 3)));
    rr = rr + ((route_val & 0xFF) <<   (32 - (5 + 3 + 8)));
    rr = rr + ((sf.subdest & 0b111) << (32 - (5 + 3 + 8 + 5 + 3)));
    rr = rr + ((sf.route) <<           (32 - (5 + 3 + 8 + 5 + 3 + 8)));
/*
    eMAC_eMAC2_RX_routing_broute_wrf(&emac, core_id, sf.route);

    eMAC_eMAC2_RX_routing_bdest_wrf(&emac, core_id, (sf.subdest & 0b111));


    eMAC_eMAC2_RX_routing_iroute_wrf(&emac, core_id, route_val);
    eMAC_eMAC2_RX_routing_idest_wrf(&emac, core_id, (core_id & 1));
*/
    eMAC_eMAC2_RX_routing_wr_raw(&emac, core_id, rr);
    s[0] = 0;
    uint32_t rrr = eMAC_eMAC2_RX_routing_rd_raw(&emac, core_id);
    eMAC_eMAC2_RX_routing_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX routing = %s\n", s);
    if(rr != rrr){
        EMAC_DEBUG("WARNING: Prepared values do not match set values\n");
    }
    EMAC_DEBUG( "Raw value = [%"PRIx32"] and prepared[%"PRIx32"]\n", rrr, rr);
    /*
    6. Set RX hi MAC address (GRB+0x9600):
        Set high MAC address (e.g. 0x0045).
     */
    uint64_t mac_addr_holder = get_mac_address();
    uint16_t mac_hi = (mac_addr_holder >> 32) & 0xFFFF;
    eMAC_eMAC2_RX_net_port_MAC_high_mac_hi_wrf(&emac, core_id, mac_hi);

    /*
    7. Set RX lo MAC address (GRB+0x9700):
        Set low MAC address (e.g. 0x414D4500).
    */
    uint32_t mac_low = mac_addr_holder & 0xFFFFFFFF;
    eMAC_eMAC2_RX_net_port_MAC_low_mac_lo_wrf(&emac, core_id, mac_low);

    s[0] = 0;
    eMAC_eMAC2_RX_net_port_MAC_high_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX mac high = [%s]\n", s);

    s[0] = 0;
    eMAC_eMAC2_RX_net_port_MAC_low_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX mac low = [%s]\n", s);

    /*
    8. Activate RX network port (GRB+0x9800):
        Set to 1.
    */
    eMAC_eMAC2_RX_net_port_enable_enable_wrf(&emac, core_id, 1);

    s[0] = 0;
    eMAC_eMAC2_RX_net_port_enable_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("RX port enable = %s\n", s);

} /* end function: initialize_RX */



/* CRB TILEID */
#define RCK_TILEID                              0x0100
static long local_crb_offset = 0xF8000000;

static uint32_t read_32(void *base, int x)
{
    uint32_t *addr = (uint32_t *)(base + x);
    return(*addr);
}

static void rx_init_wrapper(void)
{
    unsigned long long offset = 0;
    int tmp = 0;
    int x = 0;
    int y = 0;
    int z = 0;
    int position = 0;
    int lmode = 0;
    int subdest = 0;
    int lroute = 0;
    uint16_t framesize_bits = 13;


    /******** mapping the device ***********/
    struct capref frame;
    errval_t err = slot_alloc(&frame);
    assert(err_is_ok(err));
    EMAC_DEBUG("rck:calling system call to get cap for rck registers\n");
    EMAC_DEBUG("rck:with values %lx, %x\n", local_crb_offset, framesize_bits);
    err = sys_debug_feign_frame_cap(frame, local_crb_offset, framesize_bits);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "rck:feign_frame_cap failed:");
        assert(!"rck:feign_frame_cap failed");
    }
    assert(err_is_ok(err));

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    EMAC_DEBUG("rck:device register base = %" PRIxGENPADDR ", size = %u\n",
            id.base, id.bits);


    void *driver_memory_va = NULL;
    EMAC_DEBUG("rck:frame alloc done\n");
    errval_t r = vspace_map_one_frame_attr(&driver_memory_va,
            (1<<framesize_bits), frame,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(r)) {
        assert(!"rck:vspace_map_one_frame failed");
    }
    EMAC_DEBUG("rck:vspace done\n");

    assert(driver_memory_va != NULL);

    /******** mapping the device ***********/

    /* Read tile id */
    tmp = read_32(driver_memory_va, RCK_TILEID);
    /* bits 06:03 */
    x = (tmp >> 3) & 0x0f;
    /* bits 10:07 */
    y = (tmp >> 7) & 0x0f;
    /* bits 02:00 */
    z = (tmp) & 0x07;

    position = 12 * y + 2 * x + z;
    assert(position == core_id);
    EMAC_DEBUG("Location:\n");
    EMAC_DEBUG("  X: %d Y: %d, Z: %d => Position: %d\n", x, y, z, position);

    /* Depending on core location read own private data
     * (offset, subdest, route)
     */
    if (z == 0) {
        tmp = read_32(driver_memory_va, 0x800);
    } else {
        tmp = read_32(driver_memory_va, 0x1000);
    }
    EMAC_DEBUG("#####read success %x, address bits[%x]\n", tmp, (tmp & 0x3FF));

    offset = (unsigned long long)((unsigned long long) tmp & 0x3FF) << 24;
    subdest = (tmp >> 10) & 0x07;
    lroute = (tmp >> 13) & 0xFF;
    lmode = (subdest << 8) + lroute;
    rx_tile_offset = offset;
    rx_mode = lmode;
    rx_pos = (y << 4) | x;

    EMAC_DEBUG("Using offset: %llx, route[%x], subdest[%x], mode[%x]\n",
            offset, lroute, subdest, lmode);
}






static void init_Xilinx_IP_block_eMAC2(void)
{
    /* 9.6.5.1 Initialization Xilinx IP (block eMAC2) */

    /* In Xilinx IP Disable flow control */
    eMAC_eMAC2_flow_control_conf_RX_FC_enable_wrf(&emac, 0);
    eMAC_eMAC2_flow_control_conf_TX_FC_enable_wrf(&emac, 0);

    /* In Xilinx, enable transmitter, receiver */
    eMAC_eMAC2_receiver_conf_1_RX_wrf(&emac, 1);
    eMAC_eMAC2_transmiter_conf_TX_wrf(&emac, 1);

    /* In Xilinx, enable full duplex for transmitter, receiver */
    eMAC_eMAC2_receiver_conf_1_HD_wrf(&emac, 0);
    eMAC_eMAC2_transmiter_conf_HD_wrf(&emac, 0);

    /* FIXME: use 1GB as it was shown in example */
    /* In Xilinx, set MAC speed (setting to 10 Mb/s )*/
    eMAC_eMAC2_ethernet_mac_conf_LINK_SPEED_wrf(&emac, 0b10);

    /* Set promiscuous mode */
//    eMAC_eMAC2_address_filter_mode_PM_wrf(&emac, 1);

} /* end function: init_Xilinx_IP_block_eMAC2 */




/*****************************************************************
* initialize the card
 *****************************************************************/


static void *setup_internal_memory(void)
{
    struct frame_identity frameid;
    lpaddr_t mem;
    errval_t r;

    EMAC_DEBUG("Setting up internal memory for receive\n");

    ram_set_affinity(0x13000000, 0x14000000);

    r = frame_alloc(&RX_capframe, BUFF_SIZE, NULL);
    if(err_is_fail(r)) {
        assert(!"frame_alloc for RX failed");
    }

    r = frame_alloc(&TX_capframe, BUFF_SIZE, NULL);
    if(err_is_fail(r)) {
        assert(!"frame_alloc for TX failed");
    }
    ram_set_affinity(0x0, 0x0);

    /********* Setting for RX memory ***********/
    r = invoke_frame_identify(RX_capframe, &frameid);
    assert(err_is_ok(r));
    mem = frameid.base;

    RX_internal_memory_pa = (void*)mem;

    r = vspace_map_one_frame_attr(&RX_internal_memory_va,
        BUFF_SIZE, RX_capframe,
        VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(r)) {
        assert(!"vspace_map_one_frame failed for RX");
    }

    assert(RX_internal_memory_pa);
    assert(RX_internal_memory_va);

    memset(RX_internal_memory_va, 0x00, CSIZE);
    memset(RX_internal_memory_va + CSIZE, 0xDA, CSIZE);
    EMAC_DEBUG("setup_internal_mem (RX) (size 0x%x), lpa[%p] = lva[%p]\n",
            BUFF_SIZE, RX_internal_memory_pa, RX_internal_memory_va);


    /***** Setting for TX memory *****/
    r = invoke_frame_identify(TX_capframe, &frameid);
    assert(err_is_ok(r));
    mem = frameid.base;

    TX_internal_memory_pa = (void*)mem;

    r = vspace_map_one_frame_attr(&TX_internal_memory_va,
        BUFF_SIZE, TX_capframe,
        VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(r)) {
        assert(!"vspace_map_one_frame failed for TX");
    }

    assert(TX_internal_memory_pa);
    assert(TX_internal_memory_va);

    memset(TX_internal_memory_va, 0x00, CSIZE);
    memset(TX_internal_memory_va + CSIZE, 0xDA, CSIZE);
    EMAC_DEBUG("setup_internal_mem (TX) (size 0x%x), lpa[%p] = lva[%p]\n",
            BUFF_SIZE, TX_internal_memory_pa, TX_internal_memory_va);


    return RX_internal_memory_va;
}


static bool can_handle_packet(void)
{

    if(RX_internal_memory_pa == NULL || RX_internal_memory_va == NULL) {
        EMAC_DEBUG("no internal memory yet#####.\n");
        return false;
    }
//    return true;

    // Mask local APIC IRQ (prob not needed)
    if(waiting_for_netd()){
        EMAC_DEBUG("netd not connected\n");
        return false;
    }

    return true;
} /* end function: can_handle_packet */



static void interrupt_handler(void *dummy)
{
    // Check that interrupt came from my device
    /* status = readl(RA(IRQ_STATUS, priv->pid * 2)); */
    if(!(eMAC_PIC_irq_status_rd(&emac, core_id) & (1 << eMAC_PHY_id))) {
        printf("interrupt from wrong device!\n");
        /* assuming that someone else will handle that error */
        /* abort(); */
        return;
    }

    EMAC_DEBUG("######## interrupt came!\n");
    if(can_handle_packet()){
        // Process packets
        EMAC_DEBUG("#### interrupt handling!\n");
        ++interrupt_counter;
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NET, TRACE_EVENT_NET_NI_I, 0);
#endif // TRACE_ETHERSRV_MODE
        receive_frame_data();
    }

    // EOI
    eMAC_PIC_irq_reset_wr(&emac, core_id, (1 << eMAC_PHY_id));
}

#if 0
static void clear_interrupt(void)
{
    // TODO:
    // Unset APIC mask (probably in kernel, prob not needed)
    // Unset interrupt bit in local CRB (prob in kernel)
    // Reset interrupt for eMAC2

    // For reference:
    /* /\* Set APIC mask *\/ */
    /* #define EMAC_LVT		APIC_LVT1 */
    /* unset_lapic_mask(EMAC_LVT, dev->irq == 3); */

    /* /\* Set interrupt bit *\/ */
    /* #define EMAC_IRQ_MASK	0x00000001 */
    /* tmp = readl((void*)priv->irq_address); */
    /* tmp &= ~(EMAC_IRQ_MASK); */
    /* writel(tmp, (void*)priv->irq_address); */

    eMAC_PIC_irq_reset_wr(&emac, core_id, (1 << eMAC_PHY_id));

    /* /\* Reset *\/ */
    /* #define EMAC2		0x04 */
    /* tmp = priv->device == EMAC2; */
    /* writel(tmp, RA(IRQ_RESET, priv->pid * 2)); */
}
#endif


static void eMAC_set_interrupt(void)
{
    errval_t r;

    // Setup interrupt handling
    // Send EOI
    /* FIXME: set IRQ based on which EMAC, instead of hardcoding it*/
    eMAC_PIC_irq_reset_wr(&emac, core_id, (1 << eMAC_PHY_id));

    // Unmask IRQ
    uint64_t irqmask = eMAC_PIC_irq_mask_rd(&emac, core_id);

    /* tmp = readl(RA(IRQ_MASK, priv->pid * 2)); */
    /* FIXME: Don't hardcode EMAC2_IRQ */
    eMAC_PIC_irq_mask_wr(&emac, core_id, irqmask & ~((1 << eMAC_PHY_id)));
    /* eMAC_PIC_irq_mask_wr(&emac, core_id, 0); */

    char s[1000];
    eMAC_PIC_irq_status_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("irq status [%s]\n", s);

    // Configure which pin to use on our core (always LINT1)
    //    eMAC_PIC_irq_config_wr(&emac, core_id, EMAC_IRQ_CONFIG);
    eMAC_PIC_irq_config_wr(&emac, core_id, EMAC_IRQ_CONFIG);
    /* writel(EMAC_IRQ_CONFIG, RA(IRQ_CONFIG, priv->pid)); */

    // Register interrupt handler
    uint32_t vector;
    r = inthandler_setup(interrupt_handler, NULL, &vector);
    if(err_is_fail(r)) {
        USER_PANIC_ERR(r, "inthandler_setup");
    }
    assert(vector == 0);
    EMAC_DEBUG("Interrupt set\n");

    cl1flushmb();

    eMAC_PIC_irq_status_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("irq status [%s]\n", s);
    eMAC_PIC_irq_mask_pri(s, 999, &emac, core_id);
    EMAC_DEBUG("irq mask [%s]\n", s);

    /* writel(tmp & ~(priv->device), RA(IRQ_MASK, priv->pid * 2)); */

} /* end function: eMAC_set_interrupt */



/**
 * Initialize RCK driver by mapping in config registers and MPBs.
 */
void eMAC_hwinit(uint8_t phy_id)
{
    errval_t r;
//    void *emac_base = NULL;

    RX_max_slots = SLOTS - 1;  // initializing the value


    core_id = disp_get_core_id();
    eMAC_PHY_id = phy_id;
    EMAC_DEBUG("eMAC driver was called for eMAC%x on core %d(0x%x)\n",
            eMAC_PHY_id, core_id, core_id);
    /* Map the needed address-space on memory. */

    struct capref frame;
    errval_t err = slot_alloc(&frame);
    assert(err_is_ok(err));
    EMAC_DEBUG("calling system call to get cap for device registers\n");
    EMAC_DEBUG("with values %x, %x\n", EMAC_CFG_REGS_BASE, 16);
    err = sys_debug_feign_frame_cap(frame, EMAC_CFG_REGS_BASE,
            16);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "feign_frame_cap failed:");
        assert(!"feign_frame_cap failed");
    }
    assert(err_is_ok(err));

    struct frame_identity id;
    err = invoke_frame_identify(frame, &id);
    assert(err_is_ok(err));

    EMAC_DEBUG("device register base = %" PRIxGENPADDR ", size = %u\n",
            id.base, id.bits);


    void *driver_memory_va = NULL;
    EMAC_DEBUG("frame alloc done\n");
    r = vspace_map_one_frame_attr(&driver_memory_va,
            EMAC_CFG_REGS_SIZE, frame,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(r)) {
        assert(!"vspace_map_one_frame failed");
    }
    EMAC_DEBUG("vspace done\n");

    assert(driver_memory_va != NULL);

    eMAC_initialize(&emac, driver_memory_va);

    setup_internal_memory();
    EMAC_DEBUG("internal memory set\n");


//    test_address_conversion();
    read_out_regs();
//    return;

    /*
     * The software driver is responsible for configuring
     * and enabling the network ports.
     * Each driver does this independently for its own register sets.
     */

    /* Reset the register values */
    /* FIXME: should I do this? as it is not told in manual. */
/*
    eMAC_eMAC2_receiver_conf_1_RST_wrf(&emac, 1);
    eMAC_eMAC2_transmiter_conf_RST_wrf(&emac, 1);
*/

//    EMAC_DEBUG("eMAC driver resetted\n");
    /* Initialize the Xilinx block */
    init_Xilinx_IP_block_eMAC2();

    EMAC_DEBUG("### eMAC driver initiated\n");
    /* Initialize RX mode */
    rx_init_wrapper();
    initialize_RX();
    EMAC_DEBUG("eMAC driver: initialized RX\n");

    initialize_TX();
    EMAC_DEBUG("eMAC driver: initialized TX\n");

//    high_level_handler = handler;
    eMAC_set_interrupt();
    if (core_id == 55 )polling();
} /* end function: eMAC_init */

