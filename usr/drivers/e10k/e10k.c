/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <net_queue_manager/net_queue_manager.h>
#include <pci/pci.h>
#include <ipv4/lwip/inet.h>
#include <barrelfish/debug.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>

#include "e10k.h"

//#define DEBUG(x...) printf("e10k: " x)
#define DEBUG(x...) do {} while(0)



static e10k_t* d;
static e10k_queue_t* q[2];
uint64_t d_mac;
static int initialized = 0;
static int use_interrupts = 1;
struct txbuf* txbufs;

static uint64_t assumed_queue_id = 0; // queue_id that will be initialized

#define NTXDESCS 256
#define NRXDESCS 256
#define RXBUFSZ 2048

struct rxbuf {
    uint64_t phys;
    void* virt;
};

struct txbuf {
    struct net_queue_manager_binding* eb;
    uint64_t data;
    uint64_t spp_index;
    uint64_t ts;
};


void stats_dump(void);
void stats_dump(void)
{
    uint32_t crcerrs, illerrc, errbc, rxmpc0, rxmpc1, rxmpc2, rxmpc3, rxmpc4,
        rxmpc5, rxmpc6, rxmpc7, mlfc, mrfc, rlec, gprc, rxdgpc, gptc, txdgpc,
        ruc, rfc, roc, rjc, tpr, tpt;


    DEBUG("stats:");
    crcerrs = e10k_crcerrs_rd(d);
    if (crcerrs) printf(" crcerrs=%x", crcerrs);
    illerrc = e10k_illerrc_rd(d);
    if (illerrc) printf(" illerrc=%x", illerrc);
    errbc = e10k_errbc_rd(d);
    if (errbc) printf(" errbc=%x", errbc);
    rxmpc0 = e10k_rxmpc_rd(d, 0);
    if (rxmpc0) printf(" rxmpc0=%x", rxmpc0);
    rxmpc1 = e10k_rxmpc_rd(d, 1);
    if (rxmpc1) printf(" rxmpc1=%x", rxmpc1);
    rxmpc2 = e10k_rxmpc_rd(d, 2);
    if (rxmpc2) printf(" rxmpc2=%x", rxmpc2);
    rxmpc3 = e10k_rxmpc_rd(d, 3);
    if (rxmpc3) printf(" rxmpc3=%x", rxmpc3);
    rxmpc4 = e10k_rxmpc_rd(d, 4);
    if (rxmpc4) printf(" rxmpc4=%x", rxmpc4);
    rxmpc5 = e10k_rxmpc_rd(d, 5);
    if (rxmpc5) printf(" rxmpc5=%x", rxmpc5);
    rxmpc6 = e10k_rxmpc_rd(d, 6);
    if (rxmpc6) printf(" rxmpc6=%x", rxmpc6);
    rxmpc7 = e10k_rxmpc_rd(d, 7);
    if (rxmpc7) printf(" rxmpc7=%x", rxmpc7);
    mlfc = e10k_mlfc_rd(d);
    if (mlfc) printf(" mlfc=%x", mlfc);
    mrfc = e10k_mrfc_rd(d);
    if (mrfc) printf(" mrfc=%x", mrfc);
    rlec = e10k_rlec_rd(d);
    if (rlec) printf(" rlec=%x", rlec);
    gprc = e10k_gprc_rd(d);
    if (gprc) printf(" gprc=%x", gprc);
    rxdgpc = e10k_rxdgpc_rd(d);
    if (rxdgpc) printf(" rxdgpc=%x", rxdgpc);
    gptc = e10k_gptc_rd(d);
    if (gptc) printf(" gptc=%x", gptc);
    txdgpc = e10k_txdgpc_rd(d);
    if (txdgpc) printf(" txdgpc=%x", txdgpc);
    ruc = e10k_ruc_rd(d);
    if (ruc) printf(" ruc=%x", ruc);
    rfc = e10k_rfc_rd(d);
    if (rfc) printf(" rfc=%x", rfc);
    roc = e10k_roc_rd(d);
    if (roc) printf(" roc=%x", roc);
    rjc = e10k_rjc_rd(d);
    if (rjc) printf(" rjc=%x", rjc);
    tpr = e10k_tpr_rd(d);
    if (tpr) printf(" tpr=%x", tpr);
    tpt = e10k_tpt_rd(d);
    if (tpt) printf(" tpt=%x", tpt);
    printf("\n");
}

void device_dump(void);
void device_dump(void)
{
    size_t size = 1024 * 1024;
    char* buffer = malloc(size);
    e10k_pr(buffer, size, d);
    puts(buffer);
    free(buffer);
}

/** Callback to get card's MAC address */
static void get_mac_address_fn(uint8_t* mac)
{
    DEBUG("Get MAC address\n");
    memcpy(mac, &d_mac, 6);
}

/** Callback to add a buffer to TX ring. */
static errval_t transmit_pbuf_list_fn(struct client_closure* cl)
{
    int i;
    uint64_t paddr;
    struct txbuf* buf;
//    uint64_t client_data = 0;
    struct shared_pool_private *spp = cl->spp_ptr;
    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
    uint64_t rtpbuf = sld->no_pbufs;

    struct buffer_descriptor *buffer = find_buffer(sld->buffer_id);
    DEBUG("Add buffer callback %"PRIu64":\n", rtpbuf);

    // TODO: Make sure there is room in TX queue
    for (i = 0; i < rtpbuf; i++) {
        sld = &spp->sp->slot_list[cl->tx_index + i].d;
        assert(buffer->buffer_id == sld->buffer_id);
        paddr = (uint64_t) buffer->pa + sld->offset;

        // Add info to free memory
        // TODO: Is this copy really necessary?
        buf = txbufs + q[0]->tx_tail;
        buf->eb = cl->app_connection;
        buf->spp_index = cl->tx_index + i;
//        client_data = sld->client_data;
//        buf->ts = pbuf->ts;

        e10k_queue_add_txbuf(q[0], paddr, sld->len, buf,
            (i == rtpbuf - 1));
    }

    e10k_queue_bump_txtail(q[0]);

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVADD,
                (uint32_t)0);
#endif // TRACE_ONLY_SUB_NNET

    return SYS_ERR_OK;
}


static uint64_t find_tx_free_slot_count_fn(void)
{
    e10k_queue_t* queue = q[0]; // FIXME
    return e10k_queue_free_txslots(queue);
}

static bool handle_free_tx_slot_fn(void)
{
    int qi = 0; // FIXME
    e10k_queue_t* queue = q[qi];
    void* op;
    int last;
    struct txbuf* buf;

    if (e10k_queue_get_txbuf(queue, &op, &last) != 0) {
        return false;
    }

    DEBUG("Packet done (q=%d)\n", qi);

    buf = op;

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVSEE,
                (uint32_t) buf->data);
#endif // TRACE_ONLY_SUB_NNET

    handle_tx_done(buf->eb, buf->spp_index);

    return true;
}

static void check_for_free_txbufs(int qi)
{
    // Stop if not initialized
    if (!initialized) return;

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    while (handle_free_tx_slot_fn()) { }
}

static void check_for_new_packets(int qi)
{
    size_t len;
    void* op;
    struct rxbuf* buf;
    int last;
    size_t count;
    e10k_queue_t* queue = q[qi];

    // Stop if not initialized
    if (!initialized) return;

    //stats_dump();

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    count = 0;
    while (e10k_queue_get_rxbuf(queue, &op, &len, &last) == 0) {
        buf = op;

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXDRVSEE,
                    (uint32_t) len);
#endif // TRACE_ONLY_SUB_NNET

        DEBUG("New packet (q=%d)\n", qi);

        if(waiting_for_netd()){
            DEBUG("still waiting for netd to register buffers\n");
            return;
        }

        process_received_packet(buf->virt, len);
        e10k_queue_add_rxbuf(queue, buf->phys, buf);

        count++;
    }

    if (count > 0) e10k_queue_bump_rxtail(queue);
}

static void interrupt_handler(void* arg)
{
    e10k_eicr_t eicr = e10k_eicr_rd(d);
    int i;

    e10k_eicr_wr(d, eicr);

    if (eicr >> 16 || !use_interrupts) {
        DEBUG("Interrupt: %x\n", eicr);
    }

    for (i = 0; i < 1; i++) {
        check_for_new_packets(i);
        check_for_free_txbufs(i);
    }
}




/* Helpers for queue manager */
static errval_t update_txtail(void* opaque, size_t tail)
{
    int* n = opaque;
    e10k_tdt_wr(d, *n, tail);
    return SYS_ERR_OK;
}

static errval_t update_rxtail(void* opaque, size_t tail)
{
    int* n = opaque;
    e10k_rdt_1_wr(d, *n, tail);
    return SYS_ERR_OK;
}


/** Allocate queue n and return handle for queue manager */
static e10k_queue_t* setup_queue(int n, int enable_global)
{
    e10k_queue_t* queue;
    struct capref frame;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    struct e10k_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail };
    errval_t r;
    int i;
    uint8_t* b;
    int* np;

    void* tx_virt;
    uint64_t tx_phys;
    size_t tx_size;

    void* rx_virt;
    uint64_t rx_phys;
    size_t rx_size;


    // Allocate memory for descriptor rings
    tx_size = e10k_q_tdesc_legacy_size * NTXDESCS;
//    tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, tx_size,
    tx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, tx_size,
        &frame);
    assert(tx_virt != NULL);
    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));
    tx_phys = frameid.base;

    txbufs = calloc(NTXDESCS, sizeof(*txbufs));

    rx_size = e10k_q_rdesc_legacy_size * NRXDESCS;
//    rx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE_NOCACHE, rx_size,
    rx_virt = alloc_map_frame(VREGION_FLAGS_READ_WRITE, rx_size,
        &frame);
    assert(rx_virt != NULL);
    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));
    rx_phys = frameid.base;


    // Initialize queue manager
    np = malloc(sizeof(*np));
    *np = n;
    queue = e10k_queue_init(tx_virt, NTXDESCS, rx_virt, NRXDESCS, &ops, np);


    // Initialize RX queue in HW
    e10k_rdbal_1_wr(d, n, rx_phys);
    e10k_rdbah_1_wr(d, n, rx_phys >> 32);
    e10k_rdlen_1_wr(d, n, rx_size);

    e10k_srrctl_1_bsz_pkt_wrf(d, n, RXBUFSZ / 1024);
    e10k_srrctl_1_desctype_wrf(d, n, e10k_legacy);

    e10k_rdt_1_wr(d, n, 0);
    e10k_rdh_1_wr(d, n, 0);

    e10k_rxdctl_1_enable_wrf(d, n, 1);
    while (e10k_rxdctl_1_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    i = n / 2;
    if ((n % 2) == 0) {
        e10k_ivar_i_alloc0_wrf(d, i, n);
        e10k_ivar_i_allocval0_wrf(d, i, 1);
        e10k_ivar_i_alloc1_wrf(d, i, n);
        e10k_ivar_i_allocval1_wrf(d, i, 1);
    } else {
        e10k_ivar_i_alloc2_wrf(d, i, n);
        e10k_ivar_i_allocval2_wrf(d, i, 1);
        e10k_ivar_i_alloc3_wrf(d, i, n);
        e10k_ivar_i_allocval3_wrf(d, i, 1);
    }


    // Enable RX
    if (enable_global) {
        e10k_secrxctrl_rx_dis_wrf(d, 1);
        while (e10k_secrxstat_sr_rdy_rdf(d) == 0); // TODO: Timeout
        e10k_rxctrl_rxen_wrf(d, 1);
        e10k_secrxctrl_rx_dis_wrf(d, 0);
    }


    // Add RX Buffers
    b = alloc_map_frame(VREGION_FLAGS_READ_WRITE,
        RXBUFSZ * (NRXDESCS - 1), &frame);
    assert(b != NULL);
    r = invoke_frame_identify(frame, &frameid);
    assert(err_is_ok(r));

    for (i = 0; i < NRXDESCS - 1; i++) {
        struct rxbuf* buf = malloc(sizeof(*buf));
        buf->virt = b + (i * RXBUFSZ);
        buf->phys = frameid.base + i * RXBUFSZ;

        e10k_queue_add_rxbuf(queue, buf->phys, buf);
    }
    e10k_queue_bump_rxtail(queue);

    // Initialize TX queue in HW
    e10k_tdbal_wr(d, n, tx_phys);
    e10k_tdbah_wr(d, n, tx_phys >> 32);
    e10k_tdlen_wr(d, n, tx_size);

    e10k_tdh_wr(d, n, 0x0);
    e10k_tdt_wr(d, n, 0x0);

    if (enable_global) e10k_dmatxctl_txen_wrf(d, 1);

    e10k_txdctl_enable_wrf(d, n, 1);
    while (e10k_txdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);


    // Some initialization stuff from BSD driver
    e10k_dca_txctrl_txdesc_wbro_wrf(d, n, 0);

    return queue;
}

void e10k_flt_etype_setup(int filter, int queue, uint16_t etype);

void e10k_flt_etype_setup(int filter, int queue, uint16_t etype)
{
    // Clear existing values
    e10k_etqf_wr(d, filter, 0x0);
    e10k_etqs_wr(d, filter, 0x0);

    e10k_etqs_rx_queue_wrf(d, filter, queue);
    e10k_etqs_queue_en_wrf(d, filter, 1);

    e10k_etqf_etype_wrf(d, filter, etype);
    e10k_etqf_filter_en_wrf(d, filter, 1);
}

enum filter_l4type {
    L4_OTHER,
    L4_UDP,
    L4_TCP,
    L4_SCTP
};

enum filter_mask {
    MASK_L4PROTO    = (1 << 0),
    MASK_SRCIP      = (1 << 1),
    MASK_DSTIP      = (1 << 2),
    MASK_SRCPORT    = (1 << 3),
    MASK_DSTPORT    = (1 << 4),
};

struct e10k_filter {
    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;

    uint16_t mask;
    uint16_t l4_type;
};

void e10k_flt_ftqf_setup(int idx, int queue,
    struct e10k_filter* filter, int priority);

void e10k_flt_ftqf_setup(int idx, int queue,
    struct e10k_filter* filter, int priority)
{
    uint16_t m = filter->mask;
    e10k_l4_proto_t p = 0;

    // Write filter data
    if (!(m & MASK_SRCIP))
        e10k_saqf_wr(d, idx, htonl(filter->src_ip));
    if (!(m & MASK_DSTIP))
        e10k_daqf_wr(d, idx, htonl(filter->dst_ip));
    if (!(m & MASK_SRCPORT))
        e10k_sdpqf_src_port_wrf(d, idx, htons(filter->src_port));
    if (!(m & MASK_DSTPORT))
        e10k_sdpqf_dst_port_wrf(d, idx, htons(filter->dst_port));
    if (!(m & MASK_L4PROTO)) {
        switch (filter->l4_type) {
            case L4_OTHER:  p = e10k_l4other; break;
            case L4_UDP:    p = e10k_l4udp; break;
            case L4_TCP:    p = e10k_l4tcp; break;
            case L4_SCTP:   p = e10k_l4sctp; break;
            default: assert(0);
        }
        e10k_ftqf_protocol_wrf(d, idx, p);
    }

    // Write mask bits
    e10k_ftqf_m_srcaddr_wrf(d, idx, !!(m & MASK_SRCIP));
    e10k_ftqf_m_dstaddr_wrf(d, idx, !!(m & MASK_DSTIP));
    e10k_ftqf_m_srcport_wrf(d, idx, !!(m & MASK_SRCPORT));
    e10k_ftqf_m_dstport_wrf(d, idx, !!(m & MASK_DSTPORT));
    e10k_ftqf_m_protocol_wrf(d, idx, !!(m & MASK_L4PROTO));

    // Configure destination queue and enable filter
    e10k_l34timir_rx_queue_wrf(d, idx, queue);
    e10k_ftqf_priority_wrf(d, idx, priority);
    e10k_ftqf_pool_mask_wrf(d, idx, 1);
    e10k_ftqf_queue_en_wrf(d, idx, 1);
}

static void stop_device(void)
{
    int i = 0;

    DEBUG("Stopping device\n");

    // Disable RX and TX
    e10k_rxctrl_rxen_wrf(d, 0x0);
    e10k_dmatxctl_txen_wrf(d, 0x0);

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Disable each RX and TX queue
    for (i = 0; i < 128; i++) {
        e10k_txdctl_wr(d, i, e10k_txdctl_swflsh_insert(0x0, 1));

        if (i < 64) {
            e10k_rxdctl_1_wr(d, i, 0x0);
        } else {
            e10k_rxdctl_2_wr(d, i - 64, 0x0);
        }

    }

    // From BSD driver (not in spec)
    milli_sleep(2);

    // Master disable procedure
    e10k_ctrl_pcie_md_wrf(d, 1);
    while (e10k_status_pcie_mes_rdf(d) != 0); // TODO: Timeout
    DEBUG("Stopping device done\n");
}

/** Acquire SWFW semaphore */
static bool e10k_swfwsem_acquire(void)
{
    while (e10k_swsm_smbi_rdf(d) != 0); // TODO: Timeout
    e10k_swsm_swesmbi_wrf(d, 1);
    while (e10k_swsm_swesmbi_rdf(d) == 0); // TODO: Timeout
    return true;
}

/** Release SWFW semaphore */
static void e10k_swfwsem_release(void)
{
    e10k_swsm_swesmbi_wrf(d, 0);
    e10k_swsm_smbi_wrf(d, 0);
}


static bool e10k_swfwlock_phy(void)
{
    bool good = false;

    // See 10.5.4
again:
    if (!e10k_swfwsem_acquire()) {
        return false;
    }

    if ((e10k_status_lan_id_rdf(d) == 0) &&
        (e10k_swfw_sync_sw_physm0_rdf(d) == 0) &&
        (e10k_swfw_sync_fw_physm0_rdf(d) == 0))
    {
        e10k_swfw_sync_sw_physm0_wrf(d, 1);
        good = true;
    } else if ((e10k_swfw_sync_sw_physm1_rdf(d) == 0) &&
        (e10k_swfw_sync_fw_physm1_rdf(d) == 0))
    {
        e10k_swfw_sync_sw_physm1_wrf(d, 1);
        good = true;
    }
    e10k_swfwsem_release();

    if (!good) {
        DEBUG("Failed, try again\n");
        milli_sleep(20);
        goto again;
    }

    return true;
}

static bool e10k_swfwunlock_phy(void)
{
    if (!e10k_swfwsem_acquire()) {
        return false;
    }

    if (e10k_status_lan_id_rdf(d) == 0) {
        e10k_swfw_sync_sw_physm0_wrf(d, 0);
    } else {
        e10k_swfw_sync_sw_physm1_wrf(d, 0);
    }

    e10k_swfwsem_release();

    // Technically this is only necessary in the case that the semaphore is
    // acquired again.
    milli_sleep(10);
    return true;
}

#if 0
static bool e10k_mdi_command(uint16_t mdi, uint8_t dev, uint8_t phy,
    e10k_mdi_opcode_t op)
{
    e10k_msca_t msca = e10k_msca_default;

    msca = e10k_msca_mdiadd_insert(msca, mdi);
    msca = e10k_msca_devadd_insert(msca, dev);
    msca = e10k_msca_phyadd_insert(msca, phy);
    msca = e10k_msca_opcode_insert(msca, op);
    msca = e10k_msca_stcode_insert(msca, e10k_new_proto);
    msca = e10k_msca_mdicmd_insert(msca, 1);


    // Issue command
    e10k_msca_wr(d, msca);

    // Wait for completion
    while (e10k_msca_mdicmd_rdf(d) != 0); // TODO: Timeout

    return true;
}

static bool e10k_phy_readreg(uint16_t mdi, uint8_t dev, uint8_t phy,
    uint16_t* value)
{
    bool success = true;

    if (!e10k_swfwlock_phy()) {
        return false;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_addr_cycle)) {
        success = false;
        goto out;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_read_op)) {
        success = false;
        goto out;
    }
    *value = e10k_msrwd_mdirddata_rdf(d);

out:
    e10k_swfwunlock_phy();
    return success;
}

static bool e10k_phy_writereg(uint16_t mdi, uint8_t dev, uint8_t phy,
    uint16_t value)
{
    bool success = true;

    if (!e10k_swfwlock_phy()) {
        return false;
    }

    if (!e10k_mdi_command(mdi, dev, phy, e10k_addr_cycle)) {
        success = false;
        goto out;
    }

    e10k_msrwd_mdiwrdata_wrf(d, value);
    if (!e10k_mdi_command(mdi, dev, phy, e10k_read_op)) {
        success = false;
        goto out;
    }

out:
    e10k_swfwunlock_phy();
    return success;
}

static bool e10k_phy_validate_address(uint8_t phy)
{
    bool success;
    uint16_t value;
    uint16_t v;

    /* IXGBE_MDIO_PHY_ID_HIGH */ /* IXGBE_MDIO_PMA_PMD_DEV_TYPE */
    success = e10k_phy_readreg(0x2, 0x1, phy, &value);
    if (!success) {
        DEBUG("Error reading\n");
    }

    uint8_t i;

    for (i = 0; i < 32; i++) {
        e10k_phy_readreg(i, 0x1, phy, &v);
        DEBUG("    [%x][%x] %x\n", phy, i, v);
    }

    return success && (value != 0xFFFF) && (value != 0x0);
}

static bool e10k_phy_read_id(uint8_t phy, uint16_t* id)
{
    /* IXGBE_MDIO_PHY_ID_HIGH */ /* IXGBE_MDIO_PMA_PMD_DEV_TYPE */
    return e10k_phy_readreg(0x2, 0x1, phy, id);
}

static bool e10k_phy_identify(void)
{
    uint8_t i;
    uint16_t id;
    for (i = 0; i < 32; i++) {
        if (e10k_phy_validate_address(i)) {
            if (!e10k_phy_read_id(i, &id)) {
                return false;
            }
            DEBUG("Found PHY addr=%x id=%x\n", i, id);
            return true;
        }
    }
    return false;
}
#endif

static uint16_t e10k_eeprom_read(uint16_t offset)
{
    e10k_eerd_t eerd = e10k_eerd_default;

    eerd = e10k_eerd_start_insert(eerd, 1);
    eerd = e10k_eerd_addr_insert(eerd, offset);
    e10k_eerd_wr(d, eerd);

    while (e10k_eerd_done_rdf(d) == 0); // TODO: Timeout

    return e10k_eerd_data_rdf(d);
}

static void e10k_phy_init(void)
{
    /* IXGBE_PHY_INIT_OFFSET_NL */
    uint16_t list_offset;
    uint16_t data_offset = 0x0;
    uint16_t data_value;
    uint16_t sfp_id;
    uint16_t sfp_type = 0x4; /* SPF_DA_CORE1 */
    e10k_autoc_t autoc;

    list_offset = e10k_eeprom_read(0x002B);
    DEBUG("list_offset=%x\n", list_offset);
    if ((list_offset == 0x0) || (list_offset == 0xFFFF)) {
        return;
    }

    list_offset++;

    sfp_id = e10k_eeprom_read(list_offset);
    DEBUG("sfp_id = %x\n", sfp_id);
    while (sfp_id != 0xFFFF) {
        if (sfp_id == sfp_type) {
            list_offset++;
            data_offset = e10k_eeprom_read(list_offset);
            if ((data_offset == 0x0) || (data_offset == 0xFFFF)) {
                DEBUG("sfp init failed\n");
                return;
            } else {
                break;
            }
        } else {
            list_offset += 2;
            sfp_id = e10k_eeprom_read(list_offset);
        }
        list_offset++;
    }

    if (sfp_id == 0xFFFF) {
        DEBUG("sfp init failed\n");
        return;
    }

    DEBUG("data_offset=%x\n", data_offset);

    e10k_swfwlock_phy();
    data_value = e10k_eeprom_read(++data_offset);
    while (data_value != 0xFFFF) {
        DEBUG(" v=%x\n", data_value);
        e10k_corectl_wr(d, data_value);
        data_value = e10k_eeprom_read(++data_offset);
    }
    e10k_swfwunlock_phy();

    milli_sleep(50);


    autoc = e10k_autoc_rd(d);
    autoc = e10k_autoc_lms_insert(autoc, 0x0);
    autoc = e10k_autoc_restart_an_insert(autoc, 1);
    e10k_autoc_wr(d, autoc);
    while (e10k_anlp1_anas_rdf(d) != 0); // TODO: Timeout


    autoc = e10k_autoc_rd(d);
    autoc = e10k_autoc_lms_insert(autoc, e10k_l10g_sfi);
    autoc = e10k_autoc_restart_an_insert(autoc, 1);
    e10k_autoc_wr(d, autoc);
    while (e10k_autoc_restart_an_rdf(d) != 0); // TODO: Timeout

    DEBUG("PHY init done\n");
}

/** Init callback from pci. */
static void e10k_init(struct device_mem* bar_info, int bar_count)
{
    int i;
    e10k_ctrl_t ctrl;
    d = malloc(sizeof(*d));

    // Map first BAR for register access
    assert(bar_count >= 1);
    map_device(&bar_info[0]);
    DEBUG("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    // Initialize Mackerel binding
    e10k_initialize(d, (void*) bar_info[0].vaddr);


    milli_sleep(200);

    stop_device();

    // Make a double reset to be sure
    for (i = 0; i < 2; i++) {
        // Issue Global reset
        ctrl = e10k_ctrl_rd(d);
        ctrl = e10k_ctrl_lrst_insert(ctrl, 1);
        ctrl = e10k_ctrl_rst_insert(ctrl, 1);
        e10k_ctrl_wr(d, ctrl);
        while ((e10k_ctrl_rst_rdf(d) != 0) ||
               (e10k_ctrl_lrst_rdf(d) != 0)); // TODO: Timeout

        // Spec says 10, fbsd driver 50
        milli_sleep(50);
    }
    milli_sleep(200);
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Initialize flow-control registers
    for (i = 0; i < 8; i++) {
        if (i < 4) e10k_fcttv_wr(d, i, 0x0);
        e10k_fcrtl_wr(d, i, 0x0);
        e10k_fcrth_wr(d, i, 0x0);
    }
    e10k_fcrtv_wr(d, 0x0);
    e10k_fccfg_wr(d, 0x0);

    // Initialize Phy
    e10k_phy_init();

    // Wait for EEPROM auto read
    while (e10k_eec_auto_rd_rdf(d) == 0); // TODO: Timeout
    DEBUG("EEPROM auto read done\n");

    milli_sleep(200);

    // Wait for DMA initialization
    // Is in spec, but hangs
    //while (e10k_rdrxctl_dma_initok_rdf(d) == 0); // TODO: Timeout
    //DEBUG("DMA initialization done\n");


    d_mac = e10k_ral_ral_rdf(d, 0) | ((uint64_t) e10k_rah_rah_rdf(d, 0) << 32);
    DEBUG("mac valid = %x\n", e10k_rah_av_rdf(d, 0));

    // Setup Link
    /*e10k_autoc_restart_an_wrf(d, 1);
    while (e10k_autoc_restart_an_rdf(d) != 0); // TODO: Timeout
    DEBUG("ANEG done\n");*/

    // Wait for link to come up
    while (e10k_links_lnk_up_rdf(d) == 0); // TODO: Timeout
    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_eicr_wr(d, 0xffffffff);
    if (use_interrupts) {
        e10k_gpie_eimen_wrf(d, 1);

        e10k_eimc_wr(d, e10k_eims_rd(d));
        e10k_eims_cause_wrf(d, 0x7fffffff);
    }


    // Initialize RX filters
    for (i = 1; i < 128; i++) {
        e10k_ral_wr(d, i, 0);
        e10k_rah_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_mta_bit_vec_wrf(d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_vfta_vlan_flt_wrf(d, i, 0);
    for (i = 0; i < 64; i++)
        e10k_pfvlvf_vi_en_wrf(d, i, 0);
    for (i = 0; i < 256; i++)
        e10k_mpsar_pool_ena_wrf(d, i, 0);
    for (i = 0; i < 128; i++) {
        e10k_ftqf_wr(d, i, 0);
        e10k_saqf_wr(d, i, 0);
        e10k_daqf_wr(d, i, 0);
        e10k_sdpqf_wr(d, i, 0);
    }
    e10k_fctrl_bam_wrf(d, 1);
    /*e10k_fctrl_mpe_wrf(d, 1); PROMISC
    e10k_fctrl_upe_wrf(d, 1);*/
    for (i = 0; i < 128; i++) {
        e10k_fhft_1_wr(d, i, 0);
        if (i < 64) {
            e10k_fhft_2_wr(d, i, 0);
        }
    }
    for (i = 0; i < 128; i++)
        e10k_pfuta_wr(d, i, 0);
    e10k_mcstctrl_mfe_wrf(d, 0);



    /* From BSD code, but somehow prevents this driver from working
    for (i = 0; i < 128; i++) {
        e10k_rttdqsel_txdq_idx_wrf(d, i);
        e10k_rttbcnrc_wr(d, 0);
    }*/

    // disable relaxed ordering
    for (i = 0; i < 128; i++) {
        e10k_dca_txctrl_txdesc_wbro_wrf(d, i, 0);
        if (i < 64) {
            e10k_dca_rxctrl_1_rxhdr_ro_wrf(d, i, 0);
            e10k_dca_rxctrl_1_rxdata_wrro_wrf(d, i, 0);
        } else {
            e10k_dca_rxctrl_2_rxhdr_ro_wrf(d, i - 64, 0);
            e10k_dca_rxctrl_2_rxdata_wrro_wrf(d, i - 64, 0);
        }
    }


    //stats_dump();
    //device_dump();

    // Initialize queue and RX/TX
    q[0] = setup_queue(0, 1);


    milli_sleep(200);

    DEBUG("Card initialized\n");

    initialized = 1;
    ethersrv_init("e10k", assumed_queue_id,  get_mac_address_fn,
            transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
            handle_free_tx_slot_fn);
}

/** Polling loop. */
static void polling_loop(void)
{
    struct waitset *ws = get_default_waitset();
    while (1) {
        event_dispatch_non_block(ws);
        do_pending_work_for_all();
        if (!use_interrupts) {
            check_for_new_packets(0);
            check_for_free_txbufs(0);
        }
    }
}

int main(int argc, char** argv)
{
    errval_t r;
    int pci_function = 0;
    int i;

    DEBUG("Started\n");

    sleep_init();

    // Parse commandline parameters
    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "function=", strlen("function=") - 1) == 0) {
            pci_function = atol(argv[i] + strlen("function="));
        }
        if (strncmp(argv[i], "polling", strlen("polling") - 1) == 0) {
            use_interrupts = 0;
        }
    }

    // Register our device driver
    r = pci_client_connect();
    assert(err_is_ok(r));
    DEBUG("connected to pci\n");

    r = pci_register_driver_irq(e10k_init, PCI_CLASS_ETHERNET, PCI_DONT_CARE,
        PCI_DONT_CARE, PCI_VENDOR_INTEL, E10K_PCI_DEVID, PCI_DONT_CARE,
        PCI_DONT_CARE, pci_function, interrupt_handler, NULL);

    polling_loop();
}

