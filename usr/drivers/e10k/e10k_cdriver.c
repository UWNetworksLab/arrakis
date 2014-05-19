/*
 * Copyright (c) 2007-2011, 2013, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#include <net_device_manager/net_device_manager.h>
#include <pci/pci.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/debug.h>
#include <ipv4/lwip/inet.h>
#ifdef LIBRARY
#       include <netif/e1000.h>
#endif

#include <if/e10k_defs.h>
#include <if/e10k_vf_defs.h>
#include <dev/e10k_dev.h>

#include "e10k.h"
#include "sleep.h"
#include "helper.h"

//#define VTON_DCBOFF
//#define DCA_ENABLED

//#define DEBUG(x...) printf("e10k: " x)
#define DEBUG(x...) do {} while (0)

#define QUEUE_INTRX 0
#define QUEUE_INTTX 1

struct queue_state {
    bool enabled;
    struct e10k_binding *binding;

    struct capref tx_frame;
    struct capref txhwb_frame;
    struct capref rx_frame;
    uint32_t rxbufsz;

    size_t msix_index;
    int16_t msix_intvec;
    uint8_t msix_intdest;
    bool use_irq;
    bool use_rsc;

    uint64_t rx_head;
    uint64_t tx_head;
    lvaddr_t tx_va;
    lvaddr_t rx_va;
    lvaddr_t txhwb_va;
};

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
    bool enabled;
    uint8_t priority;
    uint8_t queue;

    uint32_t src_ip;
    uint32_t dst_ip;
    uint16_t src_port;
    uint16_t dst_port;

    uint16_t mask;
    uint16_t l4_type;
};

union macentry {
    uint8_t as8[6];
    uint64_t as64;
};

static union macentry mactable[128] = {
    { .as8 = "\x0\x0\x0\x0\x0\x0" },      // First MAC is never set (loaded from card EEPROM)

    { .as8 = "\x22\xc9\xfc\x96\x83\xfc" },
    { .as8 = "\xce\x43\x5b\xf7\x3e\x60" },
    { .as8 = "\x6a\xb0\x62\xf6\xa7\x21" },
    { .as8 = "\xb2\xdf\xf9\x39\xc6\x10" },
    { .as8 = "\x92\x77\xe7\x3f\x80\x30" },
    { .as8 = "\xd6\x88\xd6\x86\x4a\x22" },
    { .as8 = "\x7e\x64\xe9\x2e\xbe\x4b" },
    { .as8 = "\xba\xac\x49\xd6\x3c\x77" },

    // We set the rest to all zeroes

    // Last MAC (127) never set (loaded from card EEPROM ... at least, it's already there)
};

static uint16_t credit_refill[128];
static uint32_t tx_rate[128];

// Hack for monolithic driver
void qd_main(void) __attribute__((weak));
void qd_argument(const char *arg) __attribute__((weak));
void qd_interrupt(bool is_rx, bool is_tx) __attribute__((weak));
void qd_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr) __attribute__((weak));
void qd_queue_memory_registered(struct e10k_binding *b) __attribute__((weak));
void qd_write_queue_tails(struct e10k_binding *b) __attribute__((weak));


void cd_request_device_info(struct e10k_binding *b);
void cd_register_queue_memory(struct e10k_binding *b,
                              uint8_t queue,
                              struct capref tx,
                              struct capref txhwb,
                              struct capref rx,
                              uint32_t rxbufsz,
                              int16_t msix_intvec,
                              uint8_t msix_intdest,
                              bool use_interrupts,
                              bool use_rsc,
			      lvaddr_t tx_va,
			      lvaddr_t rx_va,
			      lvaddr_t txhwb_va);
void cd_set_interrupt_rate(struct e10k_binding *b,
                           uint8_t queue,
                           uint16_t rate);


static void idc_write_queue_tails(struct e10k_binding *b);
static void stop_device(void);

static void device_init(void);
static void queue_hw_init(uint8_t n);
//static void queue_hw_stop(uint8_t n);
static void interrupt_handler_msix(void* arg);
//static void interrupt_handler_msix_b(void* arg);

static void e10k_flt_ftqf_setup(int index, struct e10k_filter *filter);
//static void e10k_flt_etype_setup(int filter, int queue, uint16_t etype);



static const char *service_name = "e10k";
static int initialized = 0;
static e10k_t *d = NULL;
static struct capref *regframe;
static bool msix = true;

/** Specifies if RX/TX is currently enabled on the device. */
static bool rxtx_enabled = false;

// Management of MSI-X vectors
static struct bmallocator msix_alloc;
/** MSI-X vector used by cdriver */
static size_t cdriver_msix = -1;
static uint8_t cdriver_vector;


// State of queues and filters
static struct queue_state queues[128];
static struct e10k_filter filters[128];

static char buf[4096];

/* PCI device address passed on command line */
static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_function = 0;
static uint32_t pci_deviceid = E10K_PCI_DEVID;


static void e10k_flt_ftqf_setup(int idx, struct e10k_filter* filter)
{
    uint16_t m = filter->mask;
    e10k_l4_proto_t p;
    e10k_ftqf_t ftqf = 0;
    e10k_l34timir_t timir = 0;
    e10k_sdpqf_t sdpqf = 0;


    // Write filter data
    if (!(m & MASK_SRCIP))
        e10k_saqf_wr(d, idx, htonl(filter->src_ip));
    if (!(m & MASK_DSTIP))
        e10k_daqf_wr(d, idx, htonl(filter->dst_ip));
    if (!(m & MASK_SRCPORT))
        sdpqf = e10k_sdpqf_src_port_insert(sdpqf, htons(filter->src_port));
    if (!(m & MASK_DSTPORT))
        sdpqf = e10k_sdpqf_dst_port_insert(sdpqf, htons(filter->dst_port));
    e10k_sdpqf_wr(d, idx, sdpqf);


    if (!(m & MASK_L4PROTO)) {
        switch (filter->l4_type) {
            case L4_OTHER:  p = e10k_l4other; break;
            case L4_UDP:    p = e10k_l4udp; break;
            case L4_TCP:    p = e10k_l4tcp; break;
            case L4_SCTP:   p = e10k_l4sctp; break;
            default: assert(0); return;
        }
        ftqf = e10k_ftqf_protocol_insert(ftqf, p);
    }

    // Write mask bits
    ftqf = e10k_ftqf_m_srcaddr_insert(ftqf, !!(m & MASK_SRCIP));
    ftqf = e10k_ftqf_m_dstaddr_insert(ftqf, !!(m & MASK_DSTIP));
    ftqf = e10k_ftqf_m_srcport_insert(ftqf, !!(m & MASK_SRCPORT));
    ftqf = e10k_ftqf_m_dstport_insert(ftqf, !!(m & MASK_DSTPORT));
    ftqf = e10k_ftqf_m_protocol_insert(ftqf, !!(m & MASK_L4PROTO));


    // Configure destination queue and enable filter
    timir = e10k_l34timir_rx_queue_insert(timir, filter->queue);
    e10k_l34timir_wr(d, idx, timir);

    ftqf = e10k_ftqf_priority_insert(ftqf, filter->priority);
    ftqf = e10k_ftqf_pool_mask_insert(ftqf, 1);
    ftqf = e10k_ftqf_queue_en_insert(ftqf, 1);
    e10k_ftqf_wr(d, idx, ftqf);
}

#ifndef LIBRARY
static int ftqf_index = 0;
static int ftqf_alloc(void)
{
    // FIXME: Do this reasonably
    return ftqf_index++;
}

static errval_t reg_ftfq_filter(struct e10k_filter* f, uint64_t* fid)
{
    int i;

    DEBUG("reg_ftfq_filter: called\n");

    if ((i = ftqf_alloc()) < 0) {
        return FILTER_ERR_NOT_ENOUGH_MEMORY;
    }


    filters[i] = *f;
    filters[i].enabled = true;

    e10k_flt_ftqf_setup(i, f);

    *fid = i + 1;

    return SYS_ERR_OK;
}
#endif


#if 0
static void e10k_flt_etype_setup(int filter, int queue, uint16_t etype)
{
    // Clear existing values
    e10k_etqf_wr(d, filter, 0x0);
    e10k_etqs_wr(d, filter, 0x0);

    e10k_etqs_rx_queue_wrf(d, filter, queue);
    e10k_etqs_queue_en_wrf(d, filter, 1);

    e10k_etqf_etype_wrf(d, filter, etype);
    e10k_etqf_filter_en_wrf(d, filter, 1);
}


static errval_t arp_filter(uint64_t qid, uint64_t* fid)
{
    e10k_flt_etype_setup(0, (int) qid, 0x0806);
    *fid = 0;
    DEBUG("reg_arp_filter: called\n");
    return SYS_ERR_OK;
}

static errval_t reg_ftfq_filter(struct e10k_filter* f, uint64_t* fid)
{
    int i;

    DEBUG("reg_ftfq_filter: called\n");

    if ((i = ftqf_alloc()) < 0) {
        return ETHERSRV_ERR_NOT_ENOUGH_MEM;
    }


    filters[i] = *f;
    filters[i].enabled = true;

    e10k_flt_ftqf_setup(i, f);

    *fid = i + 1;

    return SYS_ERR_OK;
}

static errval_t ipv4_tcp_port(uint64_t qid, uint16_t port, uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = L4_TCP,
        .priority = 1,
        .queue = qid,
    };

    DEBUG("ipv4_tcp_port: called\n");
    return reg_ftfq_filter(&f, fid);
}

static errval_t ipv4_udp_port(uint64_t qid, uint16_t port, uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = L4_UDP,
        .priority = 1,
        .queue = qid,
    };

    DEBUG("ipv4_udp_port: called\n");
    return reg_ftfq_filter( &f, fid);
}

static errval_t ipv4_tcp_conn(uint64_t qid,
                              uint32_t l_ip, uint16_t l_port,
                              uint32_t r_ip, uint16_t r_port,
                              uint64_t* fid)
{
    struct e10k_filter f = {
        .dst_ip = l_ip,
        .dst_port = l_port,
        .src_ip = r_ip,
        .src_port = r_port,
        .mask = 0,
        .l4_type = L4_TCP,
        .priority = 0,
        .queue = qid,
    };

    DEBUG("ipv4_tcp_conn: called\n");
    return reg_ftfq_filter(&f, fid);
}

static errval_t deregister_filter(uint64_t fid)
{
    DEBUG("deregister_filter: called\n");
    return LIB_ERR_NOT_IMPLEMENTED;
}

#endif


/** Enable RX operation for whole card. */
static void rx_enable(void)
{
    e10k_secrxctrl_rx_dis_wrf(d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(d, 1);
    e10k_secrxctrl_rx_dis_wrf(d, 0);
}

/** Disable RX operation for whole card. */
static void rx_disable(void)
{
    e10k_secrxctrl_rx_dis_wrf(d, 1);
    while (e10k_secrxstat_sr_rdy_rdf(d) == 0); // TODO: Timeout
    e10k_rxctrl_rxen_wrf(d, 0);
    e10k_secrxctrl_rx_dis_wrf(d, 0);
}

/** Enable TX operation for whole card. */
static void tx_enable(void)
{
    e10k_dmatxctl_txen_wrf(d, 1);
}

/** Disable TX operation for whole card. */
static void tx_disable(void)
{
    e10k_dmatxctl_txen_wrf(d, 0);
    while (e10k_dmatxctl_txen_rdf(d) != 0); // TODO: timeout
}


static void setup_interrupt(size_t *msix_index, uint8_t core, uint8_t vector)
{
    bool res;
    errval_t err;
    uint8_t dest;

    res = bmallocator_alloc(&msix_alloc, msix_index);
    assert(res);

    err = get_apicid_from_core(core, &dest);
    assert(err_is_ok(err));

    err = pci_msix_vector_init(*msix_index, dest, vector);
    assert(err_is_ok(err));

    DEBUG("e10k: MSI-X vector setup index=%"PRIx64", core=%d apic=%d swvec=%x\n",
            *msix_index, core, dest, vector);
}

/**
 * Initialize hardware registers.
 * Is also called after a reset of the device.
 */
static void device_init(void)
{
    int i;
    e10k_ctrl_t ctrl;
    e10k_pfqde_t pfqde;
    errval_t err;
    bool initialized_before = initialized;

    initialized = 0;

    stop_device();

    if (initialized_before) {
        // Save queue heads and tails
        for (i = 0; i < 128; i++) {
            if (queues[i].enabled) {
                queues[i].tx_head = e10k_tdh_rd(d, i);
                if (i < 64) {
                    queues[i].rx_head = e10k_rdh_1_rd(d, i);
                } else {
                    queues[i].rx_head = e10k_rdh_2_rd(d, i - 64);
                }
            }
        }
    }

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
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_eimc_cause_wrf(d, 0x7FFFFFFF);
    e10k_eicr_rd(d);

    // Let firmware know that we have taken over
    e10k_ctrl_ext_drv_load_wrf(d, 1);

    // NO Snoop disable (from FBSD)
    // Without this, the driver only works on sbrinz1 if the receive buffers are
    // mapped non cacheable. If the buffers are mapped cacheable, sometimes we
    // seem to read old buffer contents, not sure exactly why, as far as
    // understood this, No snoop should only be enabled by the device if it is
    // save...
    // TODO: Also check performance implications of this on gottardo and other
    // machnies where it works without this.
    e10k_ctrl_ext_ns_dis_wrf(d, 1);

    // Initialize flow-control registers
    for (i = 0; i < 8; i++) {
        if (i < 4) e10k_fcttv_wr(d, i, 0x0);
        e10k_fcrtl_wr(d, i, 0x0);
        e10k_fcrth_wr(d, i, 0x0);
    }
    e10k_fcrtv_wr(d, 0x0);
    e10k_fccfg_wr(d, 0x0);

    // Initialize Phy
    e10k_phy_init(d);

    // Wait for EEPROM auto read
    while (e10k_eec_auto_rd_rdf(d) == 0); // TODO: Timeout
    DEBUG("EEPROM auto read done\n");

    // Wait for DMA initialization
    while (e10k_rdrxctl_dma_initok_rdf(d) == 0); // TODO: Timeout

    // Wait for link to come up
    while (e10k_links_lnk_up_rdf(d) == 0); // TODO: Timeout
    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_eicr_wr(d, 0xffffffff);
    if (msix) {
        // Switch to MSI-X mode
        e10k_gpie_msix_wrf(d, 1);
        e10k_gpie_pba_sup_wrf(d, 1);
        e10k_gpie_ocd_wrf(d, 1);

        // Allocate msix vector for cdriver and set up handler
        if (cdriver_msix == -1) {
            err = pci_setup_inthandler(interrupt_handler_msix, NULL, &cdriver_vector);
            assert(err_is_ok(err));

            setup_interrupt(&cdriver_msix, disp_get_core_id(), cdriver_vector);
        }

        // Map management interrupts to our vector
        e10k_ivar_misc_i_alloc0_wrf(d, cdriver_msix);
        e10k_ivar_misc_i_alloc1_wrf(d, cdriver_msix);
        e10k_ivar_misc_i_allocval0_wrf(d, 1);
        e10k_ivar_misc_i_allocval1_wrf(d, 1);

        // Enable auto masking of interrupt
        e10k_gpie_eiame_wrf(d, 1);
        e10k_eiamn_wr(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));

        // Set no interrupt delay
        e10k_eitr_l_wr(d, cdriver_msix, 0);
        e10k_gpie_eimen_wrf(d, 1);

        // Enable interrupt
        e10k_eimsn_wr(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));
    } else {
        // Set no Interrupt delay
        e10k_eitr_l_wr(d, 0, 0);
        e10k_gpie_eimen_wrf(d, 1);

        // Enable all interrupts
        e10k_eimc_wr(d, e10k_eims_rd(d));
        e10k_eims_cause_wrf(d, 0x7fffffff);
    }

    // Just a guess for RSC delay
    e10k_gpie_rsc_delay_wrf(d, 2);

    // Initialize multiple register tables (MAC 0 and 127 are not set)
    for (i = 0; i < 128; i++) {
        /* uint64_t mac = e10k_ral_ral_rdf(d, i) | ((uint64_t) e10k_rah_rah_rdf(d, i) << 32); */
        /* uint8_t *m = (uint8_t *)&mac; */
        /* DEBUG("Old MAC %d: %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx ... mac valid = %x\n", */
        /*       i, m[0], m[1], m[2], m[3], m[4], m[5], e10k_rah_av_rdf(d, 0)); */

        if(i > 0 && i < 127) {
            e10k_ral_wr(d, i, mactable[i].as64 & 0xffffffff);
            e10k_rah_wr(d, i, mactable[i].as64 >> 32);
            e10k_rah_av_wrf(d, i, 1);

            /* mac = e10k_ral_ral_rdf(d, i) | ((uint64_t) e10k_rah_rah_rdf(d, i) << 32); */
            /* m = (uint8_t *)&mac; */
            /* DEBUG("New MAC %d: %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx ... mac valid = %x\n", */
            /*       i, m[0], m[1], m[2], m[3], m[4], m[5], e10k_rah_av_rdf(d, 0)); */
        }
    }
    for (i = 0; i < 128; i++)
        e10k_mta_bit_vec_wrf(d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_vfta_vlan_flt_wrf(d, i, 0);
    for (i = 0; i < 128; i++)
        e10k_pfvlvfb_wr(d, i, 0);
    for (i = 0; i < 64; i++) {
#ifdef VTON_DCBOFF
        e10k_pfvlvf_vi_en_wrf(d, i, 1);
#else
        e10k_pfvlvf_vi_en_wrf(d, i, 0);
#endif
        e10k_psrtype_wr(d, i, 0);
    }
    for (i = 0; i < 128; i++)
        e10k_pfuta_wr(d, i, 0);
    for (i = 0; i < 256; i++)
        e10k_mpsar_pool_ena_wrf(d, i, 0);

    // Program direct match MAC forwarding rules
    // This setup will assign the first 64 MAC addresses each to a different
    // RX pool. This assumes we have 64 VFs. The rest is set to filtered.
    for(i = 0; i < 128; i++) {
        if(i < 32) {
            // Pools < 32 (low bits)
            e10k_mpsar_pool_ena_wrf(d, 2 * i, 1 << i);
            e10k_mpsar_pool_ena_wrf(d, 2 * i + 1, 0);
        } else if(i < 64) {
            // Pools >= 32 and < 64 (high bits)
            e10k_mpsar_pool_ena_wrf(d, 2 * i, 0);
            e10k_mpsar_pool_ena_wrf(d, 2 * i + 1, 1 << (i - 32));
        } else {
            // Pools >= 64 -> DROP
            e10k_mpsar_pool_ena_wrf(d, 2 * i, 0);
            e10k_mpsar_pool_ena_wrf(d, 2 * i + 1, 0);
        }
    }

    for (i = 0; i < 128; i++) {
        e10k_fhft_1_wr(d, i, 0);
        if (i < 64) {
            e10k_fhft_2_wr(d, i, 0);
        }
    }

#ifdef VTON_DCBOFF
    // Disallow per-queue RSC (not supported in SR-IOV mode)
    e10k_rfctl_rsc_dis_wrf(d, 1);
#else
    // Allow for per-queue RSC
    e10k_rfctl_rsc_dis_wrf(d, 0);
#endif

    // Initialize RX filters
    for (i = 0; i < 128; i++) {
        e10k_ftqf_wr(d, i, 0);
        e10k_saqf_wr(d, i, 0);
        e10k_daqf_wr(d, i, 0);
        e10k_sdpqf_wr(d, i, 0);
    }
    for (i = 0; i < 32; i++)
        e10k_reta_wr(d, i, 0);
    e10k_mcstctrl_mfe_wrf(d, 0);

    // Accept broadcasts
    e10k_fctrl_bam_wrf(d, 1);

    // Make sure Rx CRC strip is consistently enabled in HLREG0 and RDRXCTL
    e10k_hlreg0_rxcrcstrp_wrf(d, 1);
    // Note: rscfrstsz has to be set to 0 (is mbz)
    e10k_rdrxctl_t rdrxctl = e10k_rdrxctl_rd(d);
    rdrxctl = e10k_rdrxctl_crcstrip_insert(rdrxctl, 1);
    e10k_rdrxctl_wr(d, rdrxctl);


    // Configure buffers etc. according to specification
    // Section 4.6.11.3.4 (DCB, virtualization, no RSS)
    // 1:1 from spec, though not sure if everything is necessary, but since
    // initialization is still buggy, I'd rather be conservative and set some
    // additional flags, even if they aren't strictly necessary.
    e10k_rttdcs_arbdis_wrf(d, 1);

#ifdef VTON_DCBOFF
    e10k_rxpbsize_size_wrf(d, 0, 0x200);
    e10k_txpbsize_size_wrf(d, 0, 0xA0);
    e10k_txpbthresh_thresh_wrf(d, 0, 0xA0);
    for (i = 1; i < 8; i++) {
        e10k_rxpbsize_size_wrf(d, i, 0x0);
        e10k_txpbsize_size_wrf(d, i, 0x0);
        e10k_txpbthresh_thresh_wrf(d, i, 0x0);
    }

    e10k_mrqc_mrque_wrf(d, e10k_vrt_only);
    e10k_mtqc_rt_en_wrf(d, 0);

    e10k_mtqc_vt_en_wrf(d, 1);
    e10k_mtqc_num_tc_wrf(d, 1);
    e10k_pfvtctl_vt_en_wrf(d, 1);
#else
    for (i = 0; i < 8; i++) {
        e10k_rxpbsize_size_wrf(d, i, 0x40);
        e10k_txpbsize_size_wrf(d, i, 0x14);
        e10k_txpbthresh_thresh_wrf(d, i, 0x14);
    }

    e10k_mrqc_mrque_wrf(d, e10k_vrt_only);
    e10k_mtqc_rt_en_wrf(d, 1);
    e10k_mtqc_vt_en_wrf(d, 1);
    e10k_mtqc_num_tc_wrf(d, 2);
    e10k_pfvtctl_vt_en_wrf(d, 1);
#endif
    e10k_rtrup2tc_wr(d, 0);
    e10k_rttup2tc_wr(d, 0);

#ifdef VTON_DCBOFF
    e10k_dtxmxszrq_max_bytes_wrf(d, 0xFFF);
#else
    e10k_dtxmxszrq_max_bytes_wrf(d, 0x010);
#endif

    e10k_rttdcs_arbdis_wrf(d, 0);

    for (i = 0; i < 128; i++) {
        pfqde = e10k_pfqde_queue_idx_insert(0x0, i);
        pfqde = e10k_pfqde_we_insert(pfqde, 1);
        // XXX: Might want to set drop enable here
        /* pfqde = e10k_pfqde_qde_insert(pfqde, 1); */
        e10k_pfqde_wr(d, pfqde);
    }

#ifdef VTON_DCBOFF
    e10k_mflcn_rpfce_wrf(d, 0);
    e10k_mflcn_rfce_wrf(d, 0);
    e10k_fccfg_tfce_wrf(d, e10k_lfc_en);
#else
    e10k_mflcn_rpfce_wrf(d, 1);
    e10k_mflcn_rfce_wrf(d, 0);
    e10k_fccfg_tfce_wrf(d, e10k_pfc_en);
#endif

    /* Causes ECC error (could be same problem as with l34timir (see e10k.dev) */
    for (i = 0; i < 128; i++) {
        e10k_rttdqsel_txdq_idx_wrf(d, i);
	e10k_rttdt1c_wr(d, credit_refill[i]);   // Credit refill x 64 bytes
        e10k_rttbcnrc_wr(d, 0);
        if(tx_rate[i] != 0) {
            // Turn on rate scheduler for this queue and set rate factor
            e10k_rttbcnrc_t rttbcnrc = 0;
            // XXX: Assuming 10Gb/s link speed. Change if that's not correct.
            uint32_t tx_factor = (10000 << 14) / tx_rate[i];

            rttbcnrc = e10k_rttbcnrc_rf_dec_insert(rttbcnrc, tx_factor & 0x3fff);
            rttbcnrc = e10k_rttbcnrc_rf_int_insert(rttbcnrc, tx_factor >> 14);
            rttbcnrc = e10k_rttbcnrc_rs_ena_insert(rttbcnrc, 1);
            e10k_rttbcnrc_wr(d, rttbcnrc);

            printf("Setting rate for queue %d to %u\n", i, tx_rate[i]);
        }
    }

    for (i = 0; i < 8; i++) {
        e10k_rttdt2c_wr(d, i, 0);
        e10k_rttpt2c_wr(d, i, 0);
        e10k_rtrpt4c_wr(d, i, 0);
    }

#ifdef VTON_DCBOFF
    e10k_rttdcs_tdpac_wrf(d, 0);
    e10k_rttdcs_vmpac_wrf(d, 1);        // Remember to set RTTDT1C >= MTU when this is 1
    e10k_rttdcs_tdrm_wrf(d, 0);
    e10k_rttdcs_bdpm_wrf(d, 1);
    e10k_rttdcs_bpbfsm_wrf(d, 0);
    e10k_rttpcs_tppac_wrf(d, 0);
    e10k_rttpcs_tprm_wrf(d, 0);
    e10k_rttpcs_arbd_wrf(d, 0x224);
    e10k_rtrpcs_rac_wrf(d, 0);
    e10k_rtrpcs_rrm_wrf(d, 0);
#else
    e10k_rttdcs_tdpac_wrf(d, 1);
    e10k_rttdcs_vmpac_wrf(d, 1);
    e10k_rttdcs_tdrm_wrf(d, 1);
    e10k_rttdcs_bdpm_wrf(d, 1);
    e10k_rttdcs_bpbfsm_wrf(d, 0);
    e10k_rttpcs_tppac_wrf(d, 1);
    e10k_rttpcs_tprm_wrf(d, 1);
    e10k_rttpcs_arbd_wrf(d, 0x004);
    e10k_rtrpcs_rac_wrf(d, 1);
    e10k_rtrpcs_rrm_wrf(d, 1);

    e10k_sectxminifg_sectxdcb_wrf(d, 0x1f);
#endif

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

    // disable all queues
    for (i = 0; i < 128; i++) {
        e10k_txdctl_enable_wrf(d, i, 0);
        if (i < 64) {
            e10k_rxdctl_1_enable_wrf(d, i, 0);
        } else {
            e10k_rxdctl_2_enable_wrf(d, i - 64, 0);
        }
    }

    for(i = 0; i < 64; i++) {
        e10k_pfvml2flt_mpe_wrf(d, i, 1);
        e10k_pfvml2flt_bam_wrf(d, i, 1);
        e10k_pfvml2flt_aupe_wrf(d, i, 1);
    }

#ifdef DCA_ENABLED
    // Enable DCA (Direct Cache Access)
    {
        e10k_dca_ctrl_t dca_ctrl = 0;
        dca_ctrl = e10k_dca_ctrl_dca_mode_insert(dca_ctrl, e10k_dca10);
        e10k_dca_ctrl_wr(d, dca_ctrl);
    }

    printf("DCA globally enabled\n");
#endif

    DEBUG("Card initialized (%d)\n", initialized_before);


    // Restore configuration
    if (initialized_before) {
        // Restoring filters
        for (i = 0; i < 128; i++) {
            if (filters[i].enabled) {
                e10k_flt_ftqf_setup(i, filters + i);
            }
        }

        // Restoring queues
        for (i = 0; i < 128; i++) {
            if (queues[i].enabled) {
                queue_hw_init(i);
            }
        }

        DEBUG("Configuration restored\n");
    }

    initialized = 1;
}

/** Initialize hardware queue n. */
static void queue_hw_init(uint8_t n)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    uint64_t tx_phys, txhwb_phys, rx_phys;
    size_t tx_size, rx_size;
    bool enable_global = !rxtx_enabled;

    // Get physical addresses for rx/tx rings
    r = invoke_frame_identify(queues[n].tx_frame, &frameid);
    assert(err_is_ok(r));
    tx_phys = frameid.base;
    tx_size = 1 << frameid.bits;

    r = invoke_frame_identify(queues[n].rx_frame, &frameid);
    assert(err_is_ok(r));
    rx_phys = frameid.base;
    rx_size = 1 << frameid.bits;

    DEBUG("tx.phys=%"PRIx64" tx.size=%"PRIu64"\n", tx_phys, tx_size);
    DEBUG("rx.phys=%"PRIx64" rx.size=%"PRIu64"\n", rx_phys, rx_size);


    // Initialize RX queue in HW
    if (queues[n].rx_va) {
        e10k_rdbal_1_wr(d, n, queues[n].rx_va);
        e10k_rdbah_1_wr(d, n, (queues[n].rx_va) >> 32);
    } else {
        e10k_rdbal_1_wr(d, n, rx_phys);
        e10k_rdbah_1_wr(d, n, rx_phys >> 32);
    }
    e10k_rdlen_1_wr(d, n, rx_size);

    e10k_srrctl_1_bsz_pkt_wrf(d, n, queues[n].rxbufsz / 1024);
    e10k_srrctl_1_bsz_hdr_wrf(d, n, 128 / 64); // TODO: Do 128 bytes suffice in
                                               //       all cases?
    e10k_srrctl_1_desctype_wrf(d, n, e10k_adv_1buf);
    e10k_srrctl_1_drop_en_wrf(d, n, 1);

    // Set RSC status
    if (queues[n].use_rsc) {
        USER_PANIC("RSC not supported in SR-IOV mode!\n");
        e10k_rscctl_1_maxdesc_wrf(d, n, 3);
        e10k_rscctl_1_rsc_en_wrf(d, n, 1);
        // TODO: (how) does this work for queues >=64?
        e10k_psrtype_split_tcp_wrf(d, n, 1);
    } else {
        e10k_rscctl_1_maxdesc_wrf(d, n, 0);
        e10k_rscctl_1_rsc_en_wrf(d, n, 0);
    }

    // Initialize queue pointers (empty)
    e10k_rdt_1_wr(d, n, queues[n].rx_head);
    e10k_rdh_1_wr(d, n, queues[n].rx_head);

#ifdef VTON_DCBOFF
    // Open virtualization pool gate (assumes 64 VF mapping)
    e10k_pfvfre_wr(d, n / 64, e10k_pfvfre_rd(d, n / 64) | (1 << ((n / 2) % 32)));
#endif

    e10k_rxdctl_1_enable_wrf(d, n, 1);
    while (e10k_rxdctl_1_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    if (queues[n].use_irq) {
        uint8_t rxv, txv;
        // Look for interrupt vector
        if (queues[n].msix_intvec != 0) {
            if (queues[n].msix_index == -1) {
                setup_interrupt(&queues[n].msix_index, queues[n].msix_intdest,
                                queues[n].msix_intvec);
            }
            rxv = txv = queues[n].msix_index;
        } else {
            rxv = QUEUE_INTRX;
            txv = QUEUE_INTTX;
        }
        DEBUG("rxv=%d txv=%d\n", rxv, txv);

        // Setup mapping queue Rx/Tx -> interrupt
        uint8_t i = n / 2;
        if ((n % 2) == 0) {
            e10k_ivar_i_alloc0_wrf(d, i, rxv);
            e10k_ivar_i_allocval0_wrf(d, i, 1);
            e10k_ivar_i_alloc1_wrf(d, i, txv);
            e10k_ivar_i_allocval1_wrf(d, i, 1);
        } else {
            e10k_ivar_i_alloc2_wrf(d, i, rxv);
            e10k_ivar_i_allocval2_wrf(d, i, 1);
            e10k_ivar_i_alloc3_wrf(d, i, txv);
            e10k_ivar_i_allocval3_wrf(d, i, 1);
        }
        if (queues[n].msix_intvec != 0) {
            e10k_eitr_l_wr(d, rxv, 0);

            // Enable autoclear (higher ones are always auto cleared)
            if (rxv < 16) {
                e10k_eiac_rtxq_wrf(d, e10k_eiac_rtxq_rdf(d) | (1 << rxv));
            }

            // Enable interrupt
            e10k_eimsn_wr(d, rxv / 32, (1 << (rxv % 32)));
        }
        if (rxv < 16) {
            // Make sure interrupt is cleared
            e10k_eicr_wr(d, 1 << rxv);
        }
    }

    // Enable RX
    if (enable_global) {
        DEBUG("[%x] Enabling RX globally...\n", n);
        rx_enable();
        DEBUG("[%x] RX globally enabled\n", n);
    }

#ifdef DCA_ENABLED
    {
        // Enable DCA for this queue
        e10k_dca_rxctrl_t dca_rxctrl = 0;

        dca_rxctrl = e10k_dca_rxctrl_rxdca_desc_insert(dca_rxctrl, 1);
        dca_rxctrl = e10k_dca_rxctrl_rxdca_hdr_insert(dca_rxctrl, 1);
        dca_rxctrl = e10k_dca_rxctrl_rxdca_payl_insert(dca_rxctrl, 1);

        uint8_t my_apic_id;
        errval_t err = sys_debug_get_apic_id(&my_apic_id);
        assert(err_is_ok(err));

        dca_rxctrl = e10k_dca_rxctrl_cpuid_insert(dca_rxctrl, my_apic_id);

        if(n < 64) {
            e10k_dca_rxctrl_1_wr(d, n, dca_rxctrl);
        } else {
            e10k_dca_rxctrl_2_wr(d, n - 64, dca_rxctrl);
        }

        printf("DCA enabled on queue %d with APIC ID %d\n", n, my_apic_id);
    }
#endif

    // Initialize TX queue in HW
    if (queues[n].rx_va) {
        e10k_tdbal_wr(d, n, queues[n].tx_va);
        e10k_tdbah_wr(d, n, (queues[n].tx_va) >> 32);
    } else {
        e10k_tdbal_wr(d, n, tx_phys);
        e10k_tdbah_wr(d, n, tx_phys >> 32);
    }
    e10k_tdlen_wr(d, n, tx_size);

    // Initialize TX head index write back
    if (!capref_is_null(queues[n].txhwb_frame)) {
        r = invoke_frame_identify(queues[n].txhwb_frame, &frameid);
        assert(err_is_ok(r));
        txhwb_phys = frameid.base;
	if (queues[n].rx_va) {
	    e10k_tdwbal_headwb_low_wrf(d, n, (queues[n].txhwb_va) >> 2);
	    e10k_tdwbah_headwb_high_wrf(d, n, (queues[n].txhwb_va) >> 32);
	} else {
	    e10k_tdwbal_headwb_low_wrf(d, n, txhwb_phys >> 2);
	    e10k_tdwbah_headwb_high_wrf(d, n, txhwb_phys >> 32);
	}
        e10k_tdwbal_headwb_en_wrf(d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    e10k_tdh_wr(d, n, queues[n].tx_head);
    e10k_tdt_wr(d, n, queues[n].tx_head);

    // Configure prefetch and writeback threshhold
    e10k_txdctl_pthresh_wrf(d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_txdctl_hthresh_wrf(d, n, 0);
    e10k_txdctl_wthresh_wrf(d, n, 0);

    if (enable_global) {
        DEBUG("[%x] Enabling TX globally...\n", n);
        tx_enable();
        rxtx_enabled = true;
        DEBUG("[%x] TX globally enabled\n", n);
    }

#ifdef VTON_DCBOFF
    // Open virtualization pool gate (assumes 64 VF mapping)
    e10k_pfvfte_wr(d, n / 64, e10k_pfvfte_rd(d, n / 64) | (1 << ((n / 2) % 32)));
#endif

    e10k_txdctl_enable_wrf(d, n, 1);
    while (e10k_txdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);

    // Some initialization stuff from BSD driver
    e10k_dca_txctrl_txdesc_wbro_wrf(d, n, 0);

    idc_write_queue_tails(queues[n].binding);

}

#ifndef LIBRARY
/** Stop queue. */
static void queue_hw_stop(uint8_t n)
{
    // This process is described in 4.6.7.1.2

    // Disable TX for this queue
    e10k_txdctl_enable_wrf(d, n, 0);

    // TODO: Flush packet buffers
    // TODO: Remove all filters
    // TODO: With RSC we have to wait here (see spec), not used atm

    // Disable RX for this queue
    e10k_rxdctl_1_enable_wrf(d, n, 0);
    while (e10k_rxdctl_1_enable_rdf(d, n) != 0); // TODO: Timeout

    // A bit too much, but make sure memory is not used anymore
    milli_sleep(1);
}
#endif


/** Stop whole device. */
static void stop_device(void)
{
    int i = 0;

    DEBUG("Stopping device\n");

    // Disable RX and TX
    rx_disable();
    tx_disable();
    rxtx_enabled = false;

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

static void management_interrupt(e10k_eicr_t eicr)
{
    if (e10k_eicr_ecc_extract(eicr)) {
        DEBUG("##########################################\n");
        DEBUG("ECC Error, resetting device :-/\n");
        DEBUG("##########################################\n");
        device_init();
    } else if (eicr >> 16) {
        DEBUG("Interrupt: %x\n", eicr);
        e10k_eicr_prtval(buf, sizeof(buf), eicr);
        puts(buf);
    } else if (msix) {
        DEBUG("Weird management interrupt without cause: eicr=%x\n", eicr);
    }
}

static void interrupt_handler_msix(void* arg)
{
    DEBUG("e10k: MSI-X management interrupt\n");
    e10k_eicr_t eicr = e10k_eicr_rd(d);

    eicr &= ~(1 << cdriver_msix);
    management_interrupt(eicr);

    // Ensure management MSI-X vector is cleared
    e10k_eicr_wr(d, (1 << cdriver_msix));

    // Reenable interrupt
    e10k_eimsn_cause_wrf(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));
}

/** Here are the global interrupts handled. */
static void interrupt_handler(void* arg)
{
    e10k_eicr_t eicr = e10k_eicr_rd(d);

    if (eicr >> 16) {
        management_interrupt(eicr);
    }
    if (eicr & ((1 << QUEUE_INTRX) | (1 << QUEUE_INTTX))) {
        e10k_eicr_wr(d, eicr);
        qd_interrupt(!!(eicr & (1 << QUEUE_INTRX)),
                     !!(eicr & (1 << QUEUE_INTTX)));
    }
}





/******************************************************************************/
/* Management interface implemetation */

/** Send register cap and mac address to queue driver. */
static void idc_queue_init_data(struct e10k_binding *b,
                                struct capref registers,
                                uint64_t macaddr)
{
    errval_t r;
    r = e10k_queue_init_data__tx(b, NOP_CONT, registers, macaddr);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Tell queue driver that we are done initializing the queue. */
static void idc_queue_memory_registered(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_queue_memory_registered__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send request to queue driver to rewrite the tail pointers of its queues. */
static void idc_write_queue_tails(struct e10k_binding *b)
{
    errval_t r;
    if (b == NULL) {
        qd_write_queue_tails(b);
        return;
    }

    r = e10k_write_queue_tails__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

#ifndef LIBRARY
/** Signal queue driver that the queue is stopped. */
static void idc_queue_terminated(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_queue_terminated__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send response about filter registration to device manager */
static void idc_filter_registered(struct e10k_binding *b,
                                  uint64_t buf_id_rx,
                                  uint64_t buf_id_tx,
                                  errval_t err,
                                  uint64_t filter)
{
    errval_t r;
    r = e10k_filter_registered__tx(b, NOP_CONT, buf_id_rx, buf_id_tx, err,
                                   filter);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send response about filter deregistration to device manager */
static void idc_filter_unregistered(struct e10k_binding *b,
                                    uint64_t filter,
                                    errval_t err)
{
    errval_t r;
    r = e10k_filter_unregistered__tx(b, NOP_CONT, filter, err);
    // TODO: handle busy
    assert(err_is_ok(r));
}
#endif

/** Request from queue driver for register memory cap */
void cd_request_device_info(struct e10k_binding *b)
{
    assert(initialized);
#ifdef LIBRARY
    uint64_t d_mac = e10k_ral_ral_rdf(d, qi) | ((uint64_t) e10k_rah_rah_rdf(d, qi) << 32);
    DEBUG("mac valid = %x\n", e10k_rah_av_rdf(d, qi));
#else
    uint64_t d_mac = e10k_ral_ral_rdf(d, 0) | ((uint64_t) e10k_rah_rah_rdf(d, 0) << 32);
    DEBUG("mac valid = %x\n", e10k_rah_av_rdf(d, 0));
#endif

    if (b == NULL) {
        struct capref cr;
        errval_t err = slot_alloc(&cr);
        assert(err_is_ok(err));
        err = cap_copy(cr, *regframe);
        assert(err_is_ok(err));
        qd_queue_init_data(b, cr, d_mac);
        return;
    }
    idc_queue_init_data(b, *regframe, d_mac);
}

/** Request from queue driver to initialize hardware queue. */
void cd_register_queue_memory(struct e10k_binding *b,
                              uint8_t n,
                              struct capref tx_frame,
                              struct capref txhwb_frame,
                              struct capref rx_frame,
                              uint32_t rxbufsz,
                              int16_t msix_intvec,
                              uint8_t msix_intdest,
                              bool use_irq,
                              bool use_rsc,
			      lvaddr_t tx_va,
			      lvaddr_t rx_va,
			      lvaddr_t txhwb_va)
{
    DEBUG("register_queue_memory(%"PRIu8")\n", n);
    // TODO: Make sure that rxbufsz is a power of 2 >= 1024

    if (use_irq && msix_intvec != 0 && !msix) {
        printf("e10k: Queue %d requests MSI-X, but MSI-X is not enabled "
                " card driver. Ignoring queue\n", n);
        return;
    }
    // Save state so we can restore the configuration in case we need to do a
    // reset
    queues[n].enabled = true;
    queues[n].tx_frame = tx_frame;
    queues[n].txhwb_frame = txhwb_frame;
    queues[n].rx_frame = rx_frame;
    queues[n].tx_head = 0;
    queues[n].rx_head = 0;
    queues[n].rxbufsz = rxbufsz;
    queues[n].msix_index = -1;
    queues[n].msix_intvec = msix_intvec;
    queues[n].msix_intdest = msix_intdest;
    queues[n].binding = b;
    queues[n].use_irq = use_irq;
    queues[n].use_rsc = use_rsc;
    queues[n].tx_va = tx_va;
    queues[n].rx_va = rx_va;
    queues[n].txhwb_va = txhwb_va;

    queue_hw_init(n);

    if (b == NULL) {
        qd_queue_memory_registered(b);
        return;
    }
    idc_queue_memory_registered(b);
}

/** Request from queue driver to initialize hardware queue. */
void cd_set_interrupt_rate(struct e10k_binding *b,
                           uint8_t n,
                           uint16_t rate)
{
    DEBUG("set_interrupt_rate(%"PRIu8")\n", n);

    uint8_t i;
    e10k_eitrn_t eitr = 0;
    eitr = e10k_eitrn_itr_int_insert(eitr, rate);

    i = (queues[n].msix_index == -1 ? 0 : queues[n].msix_index);
    if (i < 24) {
        e10k_eitr_l_wr(d, i, eitr);
    } else {
        e10k_eitr_h_wr(d, i - 24, eitr);
    }
}

#ifndef LIBRARY
/**
 * Request from queue driver to stop hardware queue and free everything
 * associated with that queue.
 */
static void idc_terminate_queue(struct e10k_binding *b, uint8_t n)
{
    DEBUG("idc_terminate_queue(q=%d)\n", n);

    queue_hw_stop(n);

    queues[n].enabled = false;
    queues[n].binding = NULL;

    // TODO: Do we have to free the frame caps, or destroy the binding?
    idc_queue_terminated(b);
}

static void idc_register_port_filter(struct e10k_binding *b,
                                     uint64_t buf_id_rx,
                                     uint64_t buf_id_tx,
                                     uint8_t queue,
                                     e10k_port_type_t type,
                                     uint16_t port)
{
    struct e10k_filter f = {
        .dst_port = port,
        .mask = MASK_SRCIP | MASK_DSTIP | MASK_SRCPORT,
        .l4_type = (type == e10k_PORT_TCP ? L4_TCP : L4_UDP),
        .priority = 1,
        .queue = queue,
    };
    errval_t err;
    uint64_t fid = -1ULL;

    DEBUG("idc_register_port_filter: called (q=%d t=%d p=%d)\n",
            queue, type, port);

    err = reg_ftfq_filter(&f, &fid);
    DEBUG("filter registered: err=%"PRIu64", fid=%"PRIu64"\n", err, fid);

    idc_filter_registered(b, buf_id_rx, buf_id_tx, err, fid);
}

static void idc_unregister_filter(struct e10k_binding *b,
                                  uint64_t filter)
{
    DEBUG("unregister_filter: called (%"PRIx64")\n", filter);
    idc_filter_unregistered(b, filter, LIB_ERR_NOT_IMPLEMENTED);
}

static struct e10k_rx_vtbl rx_vtbl = {
    .request_device_info = cd_request_device_info,
    .register_queue_memory = cd_register_queue_memory,
    .set_interrupt_rate = cd_set_interrupt_rate,
    .terminate_queue = idc_terminate_queue,

    .register_port_filter = idc_register_port_filter,
    .unregister_filter = idc_unregister_filter,
};


static void export_cb(void *st, errval_t err, iref_t iref)
{
    const char *suffix = "_e10kmng";
    char name[strlen(service_name) + strlen(suffix) + 1];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("Management interface exported\n");
}

static errval_t connect_cb(void *st, struct e10k_binding *b)
{
    DEBUG("New connection on management interface\n");
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_mngif(void)
{
    errval_t r;

    r = e10k_export(NULL, export_cb, connect_cb, get_default_waitset(),
                    IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}
#endif

/****** VF/PF server interface *******/

static void init_done_vf(struct e10k_vf_binding *b, uint8_t vfn)
{
    assert(vfn < 64);

    DEBUG("VF %d init done\n", vfn);

    // Enable correct pool for VF
    e10k_pfvfre_wr(d, vfn / 32, e10k_pfvfre_rd(d, vfn / 32) | (1 << (vfn % 32)));
    e10k_pfvfte_wr(d, vfn / 32, e10k_pfvfte_rd(d, vfn / 32) | (1 << (vfn % 32)));

    if(vfn < 32) {
        e10k_pfvflrec_wr(d, 0, 1 << vfn);
    } else {
        e10k_pfvflrec_wr(d, 1, 1 << (vfn - 32));
    }

    errval_t err = b->tx_vtbl.init_done_response(b, NOP_CONT);
    assert(err_is_ok(err));
}

static void get_mac_address_vf(struct e10k_vf_binding *b, uint8_t vfn)
{
    assert(initialized);
    uint64_t d_mac = e10k_ral_ral_rdf(d, vfn) | ((uint64_t) e10k_rah_rah_rdf(d, vfn) << 32);
    errval_t err = b->tx_vtbl.get_mac_address_response(b, NOP_CONT, d_mac);
    assert(err_is_ok(err));
}

static struct e10k_vf_rx_vtbl vf_rx_vtbl = {
    .get_mac_address_call = get_mac_address_vf,
    .init_done_call = init_done_vf,
};

static void vf_export_cb(void *st, errval_t err, iref_t iref)
{
    const char *suffix = "_vf";
    char name[strlen(service_name) + strlen(suffix) + 1];

    assert(err_is_ok(err));

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    err = nameservice_register(name, iref);
    assert(err_is_ok(err));
    DEBUG("VF/PF interface [%s] exported\n", name);
}

static errval_t vf_connect_cb(void *st, struct e10k_vf_binding *b)
{
    DEBUG("New connection on VF/PF interface\n");
    b->rx_vtbl = vf_rx_vtbl;
    return SYS_ERR_OK;
}

/**
 * Initialize management interface for queue drivers.
 * This has to be done _after_ the hardware is initialized.
 */
static void initialize_vfif(void)
{
    errval_t r;

    r = e10k_vf_export(NULL, vf_export_cb, vf_connect_cb, get_default_waitset(),
		       IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

/******************************************************************************/
/* Initialization code for driver */

/** Callback from pci to initialize a specific PCI device. */
static void pci_init_card(struct device_mem* bar_info, int bar_count)
{
    errval_t err;
    bool res;

    assert(!initialized);

    d = malloc(sizeof(*d));

    // Map first BAR for register access
    assert(bar_count >= 1);
    map_device(&bar_info[0]);
    regframe = bar_info[0].frame_cap;
    DEBUG("BAR[0] mapped (v=%llx p=%llx l=%llx)\n",
            (unsigned long long) bar_info[0].vaddr,
            (unsigned long long) bar_info[0].paddr,
            (unsigned long long) bar_info[0].bytes);

    // Initialize Mackerel binding
    e10k_initialize(d, (void*) bar_info[0].vaddr);

    DEBUG("STATUS = %x\n", e10k_status_rd(d));

    // Initialize manager for MSI-X vectors
    if (msix) {
        DEBUG("Enabling MSI-X interrupts\n");
        uint16_t msix_count = 0;
        err = pci_msix_enable(&msix_count);
        assert(err_is_ok(err));
        assert(msix_count > 0);
        DEBUG("MSI-X #vecs=%d\n", msix_count);

        res = bmallocator_init(&msix_alloc, msix_count);
        assert(res);
    } else {
        DEBUG("Using legacy interrupts\n");
    }

    // Initialize hardware registers etc.
    DEBUG("Initializing hardware\n");
    device_init();

    assert(initialized);

#ifdef VTON_DCBOFF
    DEBUG("SR-IOV device up routine\n");

    // Setup support for 64 VFs
    e10k_gcr_ext_vtmode_wrf(d, e10k_vt_64);
    e10k_gpie_vtmode_wrf(d, e10k_vt_64);

    // Enable virtualization, disable default pool, replication enable
    e10k_pfvtctl_t pfvtctl = e10k_pfvtctl_rd(d);
    pfvtctl = e10k_pfvtctl_vt_en_insert(pfvtctl, 1);
    pfvtctl = e10k_pfvtctl_def_pl_insert(pfvtctl, 0);
    pfvtctl = e10k_pfvtctl_dis_def_pl_insert(pfvtctl, 1);
    pfvtctl = e10k_pfvtctl_rpl_en_insert(pfvtctl, 1);
    e10k_pfvtctl_wr(d, pfvtctl);

    // Enable L2 loopback
    e10k_pfdtxgswc_lbe_wrf(d, 1);

    // TODO: Accept untagged packets in all VMDQ pools
    // TODO: Broadcast accept mode
    // TODO: Accept packets matching PFUTA table
    // TODO: Accept packets matching MTA table
    // TODO: Accept untagged packets enable
    // TODO: Strip VLAN tag for incoming packets

    DEBUG("STATUS = %x\n", e10k_status_rd(d));

    e10k_ctrl_ext_pfrstd_wrf(d, 1);
#endif

#ifndef LIBRARY
    // Now we initialize the management interface
    DEBUG("Initializing management interface\n");
    initialize_mngif();
#endif

    DEBUG("Initializing VF/PF interface\n");
    initialize_vfif();
    DEBUG("Done with initialization\n");
}


/** Register with PCI */
static void pci_register(void)
{
    errval_t r;

    r = pci_client_connect();
    assert(err_is_ok(r));
    DEBUG("connected to pci\n");

    r = pci_register_driver_irq(pci_init_card, PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
                                PCI_VENDOR_INTEL, pci_deviceid,
                                pci_bus, pci_device, pci_function,
                                interrupt_handler, NULL);
    assert(err_is_ok(r));
}

static void parse_cmdline(int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=")) == 0) {
            service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "bus=", strlen("bus=")) == 0) {
            pci_bus = atol(argv[i] + strlen("bus="));
        } else if (strncmp(argv[i], "device=", strlen("device=")) == 0) {
            pci_device = atol(argv[i] + strlen("device="));
        } else if (strncmp(argv[i], "function=", strlen("function=")) == 0) {
            pci_function = atol(argv[i] + strlen("function="));
        } else if (strncmp(argv[i], "deviceid=", strlen("deviceid=")) == 0) {
            pci_deviceid = strtoul(argv[i] + strlen("deviceid="), NULL, 0);
        } else if (strncmp(argv[i], "msix=", strlen("msix=")) == 0) {
            msix = !!atol(argv[i] + strlen("msix="));
            // also pass this to queue driver
            qd_argument(argv[i]);
	} else if (strncmp(argv[i], "credit_refill[", strlen("credit_refill[") - 1) == 0) {
            // Controls the WRR (weighted round-robin) scheduler's credit refill rate
            // This seems to be per VM pool
            unsigned int entry, val;
            int r = sscanf(argv[i], "credit_refill[%u]=%u", &entry, &val);
            assert(r == 2);
            assert(entry < 128);
            assert(val < 0x3fff);
            credit_refill[entry] = val;
	} else if (strncmp(argv[i], "tx_rate[", strlen("tx_rate[") - 1) == 0) {
            // This is specified in Mbits/s and must be >= 10 and <= link speed (typically 10,000)
            // This seems to be per Tx queue
            unsigned int entry, val;
            int r = sscanf(argv[i], "tx_rate[%u]=%u", &entry, &val);
            assert(r == 2);
            assert(entry < 128);
            assert(val >= 10 && val <= 10000);
            tx_rate[entry] = val;
        } else {
            qd_argument(argv[i]);
        }
    }
}

#ifndef LIBRARY
static void eventloop(void)
{
    struct waitset *ws;

    ws = get_default_waitset();
    while (1) {
        event_dispatch(ws);
    }
}

void qd_main(void)
{
    eventloop();
}

void qd_argument(const char *arg) { }
void qd_interrupt(bool is_rx, bool is_tx) { }
void qd_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr) { }
void qd_queue_memory_registered(struct e10k_binding *b) { }
void qd_write_queue_tails(struct e10k_binding *b) { }

int main(int argc, char **argv)
#else
int e1000n_driver_init(int argc, char *argv[])
#endif
{
    DEBUG("PF driver started\n");
    // credit_refill value must be >= 1 for a queue to be able to send.
    // Set them all to 1 here. May be overridden via commandline.
    for(int i = 0; i < 128; i++) {
        credit_refill[i] = 1;
    }

    memset(tx_rate, 0, sizeof(tx_rate));

    parse_cmdline(argc, argv);
    pci_register();

    while (!initialized) {
        event_dispatch(get_default_waitset());
    }
    qd_main();
    return 1;
}
