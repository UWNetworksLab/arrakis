/*
 * Copyright (c) 2013, University of Washington.
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

#include <if/e10k_defs.h>
#include <if/e10k_vf_defs.h>
#include <if/e10k_vf_rpcclient_defs.h>
#include <dev/e10k_vf_dev.h>

#include "sleep.h"
#include "helper.h"

#define E10K_PCI_DEVID 0x10ed

//#define DEBUG(x...) printf("e10k_vf: " x)
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

//static void e10k_flt_ftqf_setup(int index, struct e10k_filter *filter);
//static void e10k_flt_etype_setup(int filter, int queue, uint16_t etype);

static const char *service_name = "e10k";
uint64_t d_mac;
static int initialized = 0;
static e10k_vf_t *d = NULL;
static struct capref *regframe;
static bool msix = true;

// Management of MSI-X vectors
static struct bmallocator msix_alloc;
/** MSI-X vector used by cdriver */
static size_t cdriver_msix = -1;
static uint8_t cdriver_vector;
static bool use_interrupts = false;

// State of queues and filters
static struct queue_state queues[8];
//static struct e10k_filter filters[128];

//static char buf[4096];

static int vf_num = 0;

/* PCI device address passed on command line */
static uint32_t pci_bus = PCI_DONT_CARE;
static uint32_t pci_device = PCI_DONT_CARE;
static uint32_t pci_function = 0;

static struct e10k_vf_rpc_client *e10k_vf_client = NULL;

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
    errval_t err;
    bool initialized_before = initialized;

    initialized = 0;

    stop_device();

#if 0
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
#else
    assert(!initialized_before);
#endif

    // Issue Global reset
    e10k_vf_vfctrl_rst_wrf(d, 1);
    // Spec says 10, fbsd driver 50
    milli_sleep(50);
    DEBUG("Global reset done\n");

    // Disable interrupts
    e10k_vf_vfeimc_msix_wrf(d, 7);
    e10k_vf_vfeicr_rd(d);

    // Wait for link to come up
    DEBUG("Waiting for Link\n");
    while (e10k_vf_vflinks_lnk_up_rdf(d) == 0); // TODO: Timeout
    DEBUG("Link Up\n");
    milli_sleep(50);

    // Initialize interrupts
    e10k_vf_vfeicr_wr(d, 7);
    e10k_vf_vfeitr_wr(d, 0, 0x0);
    e10k_vf_vfeitr_wr(d, 1, 0x0);

    if (msix) {
        // Allocate msix vector for cdriver and set up handler
        if (cdriver_msix == -1) {
            err = pci_setup_inthandler(interrupt_handler_msix, NULL, &cdriver_vector);
            assert(err_is_ok(err));

            setup_interrupt(&cdriver_msix, disp_get_core_id(), cdriver_vector);
        }

        // Map management interrupts to our vector
        e10k_vf_vfivar_misc_i_alloc0_wrf(d, cdriver_msix);
        e10k_vf_vfivar_misc_i_allocval0_wrf(d, 1);

        // Enable interrupt
        e10k_vf_vfeitr_wr(d, cdriver_msix / 32, (1 << (cdriver_msix % 32)));
    } else {
        // Enable all interrupts
        e10k_vf_vfeimc_wr(d, e10k_vf_vfeims_rd(d));
        e10k_vf_vfeims_msix_wrf(d, 7);
    }

    // Other stuff
    e10k_vf_vfpsrtype_wr(d, 0);

    // disable relaxed ordering
    for (i = 0; i < 8; i++) {
        e10k_vf_vfdca_txctrl_txdesc_wbro_wrf(d, i, 0);
        e10k_vf_vfdca_rxctrl_rxhdr_ro_wrf(d, i, 0);
        e10k_vf_vfdca_rxctrl_rxdata_wrro_wrf(d, i, 0);
    }

    // enable all queues
    for (i = 0; i < 2; i++) {
        e10k_vf_vftxdctl_enable_wrf(d, i, 1);
    }
#if 0
    for (i = 0; i < 8; i++) {
        e10k_vf_vfrxdctl_enable_wrf(d, i, 0);
    }
#endif

    DEBUG("VF initialized (%d)\n", initialized_before);

#if 0
    // Restore configuration
    if (initialized_before) {
        // Restoring filters
        for (i = 0; i < 128; i++) {
            if (filters[i].enabled) {
                e10k_vf_flt_ftqf_setup(i, filters + i);
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
#endif

    initialized = 1;
}

/** Initialize hardware queue n. */
static void queue_hw_init(uint8_t n)
{
    errval_t r;
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    uint64_t tx_phys, txhwb_phys, rx_phys;
    size_t tx_size, rx_size;

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
        e10k_vf_vfrdbal_wr(d, n, queues[n].rx_va);
        e10k_vf_vfrdbah_wr(d, n, (queues[n].rx_va) >> 32);
    } else {
        e10k_vf_vfrdbal_wr(d, n, rx_phys);
        e10k_vf_vfrdbah_wr(d, n, rx_phys >> 32);
    }
    e10k_vf_vfrdlen_wr(d, n, rx_size);

    e10k_vf_vfsrrctl_bsz_pkt_wrf(d, n, queues[n].rxbufsz / 1024);
    e10k_vf_vfsrrctl_bsz_hdr_wrf(d, n, 128 / 64); // TODO: Do 128 bytes suffice in
                                               //       all cases?
    e10k_vf_vfsrrctl_desctype_wrf(d, n, e10k_vf_adv_1buf);
    e10k_vf_vfsrrctl_drop_en_wrf(d, n, 1);

    // Initialize queue pointers (empty)
    e10k_vf_vfrdt_wr(d, n, queues[n].rx_head);
    e10k_vf_vfrdh_wr(d, n, queues[n].rx_head);

    e10k_vf_vfrxdctl_enable_wrf(d, n, 1);
    while (e10k_vf_vfrxdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] RX queue enabled\n", n);

    // Setup Interrupts for this queue
    if (queues[n].use_irq) {
        DEBUG("[%x] Setting up interrupts\n", n);
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
            e10k_vf_vfivar_i_alloc0_wrf(d, i, rxv);
            e10k_vf_vfivar_i_allocval0_wrf(d, i, 1);
            e10k_vf_vfivar_i_alloc1_wrf(d, i, txv);
            e10k_vf_vfivar_i_allocval1_wrf(d, i, 1);
        } else {
            e10k_vf_vfivar_i_alloc2_wrf(d, i, rxv);
            e10k_vf_vfivar_i_allocval2_wrf(d, i, 1);
            e10k_vf_vfivar_i_alloc3_wrf(d, i, txv);
            e10k_vf_vfivar_i_allocval3_wrf(d, i, 1);
        }
        if (queues[n].msix_intvec != 0) {
            // Enable interrupt
            e10k_vf_vfeitr_wr(d, rxv / 32, (1 << (rxv % 32)));
        }
        if (rxv < 16) {
            // Make sure interrupt is cleared
            e10k_vf_vfeicr_wr(d, 1 << rxv);
        }
    }

    // We only have 4 TX queues
    assert(n < 4);

    // Initialize TX queue in HW
    if (queues[n].rx_va) {
        e10k_vf_vftdbal_wr(d, n, queues[n].tx_va);
        e10k_vf_vftdbah_wr(d, n, (queues[n].tx_va) >> 32);
    } else {
        e10k_vf_vftdbal_wr(d, n, tx_phys);
        e10k_vf_vftdbah_wr(d, n, tx_phys >> 32);
    }
    e10k_vf_vftdlen_wr(d, n, tx_size);

    // Initialize TX head index write back
    if (!capref_is_null(queues[n].txhwb_frame)) {
        r = invoke_frame_identify(queues[n].txhwb_frame, &frameid);
        assert(err_is_ok(r));
        txhwb_phys = frameid.base;
	if (queues[n].rx_va) {
	    e10k_vf_vftdwbal_headwb_low_wrf(d, n, (queues[n].txhwb_va) >> 2);
	    e10k_vf_vftdwbah_headwb_high_wrf(d, n, (queues[n].txhwb_va) >> 32);
	} else {
	    e10k_vf_vftdwbal_headwb_low_wrf(d, n, txhwb_phys >> 2);
	    e10k_vf_vftdwbah_headwb_high_wrf(d, n, txhwb_phys >> 32);
	}
        e10k_vf_vftdwbal_headwb_en_wrf(d, n, 1);
    }

    // Initialized by queue driver to avoid race conditions
    // Initialize queue pointers
    assert(queues[n].tx_head == 0);
    e10k_vf_vftdh_wr(d, n, queues[n].tx_head);
    e10k_vf_vftdt_wr(d, n, queues[n].tx_head);

    // Configure prefetch and writeback threshhold
    e10k_vf_vftxdctl_pthresh_wrf(d, n, 8); // FIXME: Figure out what the right number
                                      //        is here.
    e10k_vf_vftxdctl_hthresh_wrf(d, n, 0);
    e10k_vf_vftxdctl_wthresh_wrf(d, n, 0);      // Needs to be 0 for TXHWB

    e10k_vf_vftxdctl_enable_wrf(d, n, 1);

    while (e10k_vf_vftxdctl_enable_rdf(d, n) == 0); // TODO: Timeout
    DEBUG("[%x] TX queue enabled\n", n);

    // Some initialization stuff from BSD driver
    e10k_vf_vfdca_txctrl_txdesc_wbro_wrf(d, n, 0);

    idc_write_queue_tails(queues[n].binding);
}

#if 0
/** Stop queue. */
static void queue_hw_stop(uint8_t n)
{
#if 0
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
#else
    assert(!"NYI");
#endif
}
#endif


/** Stop whole device. */
static void stop_device(void)
{
    DEBUG("Stopping device\n");

    // Disable interrupts
    e10k_vf_vfeimc_msix_wrf(d, 7);
    e10k_vf_vfeicr_rd(d);

    // Disable each RX and TX queue
    for(int i = 0; i < 4; i++) {
        e10k_vf_vftxdctl_wr(d, i, e10k_vf_vftxdctl_swflsh_insert(0x0, 1));
    }
    for(int i = 0; i < 8; i++) {
        e10k_vf_vfrxdctl_wr(d, i, 0x0);
    }

    // From BSD driver (not in spec)
    milli_sleep(2);
}

static void interrupt_handler_msix(void* arg)
{
    DEBUG("MSI-X management interrupt\n");
    e10k_vf_vfeicr_t eicr = e10k_vf_vfeicr_rd(d);

    eicr &= ~(1 << cdriver_msix);

    // Ensure management MSI-X vector is cleared
    e10k_vf_vfeicr_wr(d, 1 << cdriver_msix);

    // Reenable interrupt
    e10k_vf_vfeimc_msix_wrf(d, 1 << (cdriver_msix % 32));
}

/** Here are the global interrupts handled. */
static void interrupt_handler(void* arg)
{
    e10k_vf_vfeicr_t eicr = e10k_vf_vfeicr_rd(d);

    if (eicr & ((1 << QUEUE_INTRX) | (1 << QUEUE_INTTX))) {
        e10k_vf_vfeicr_wr(d, eicr);
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

#if 0
/** Signal queue driver that the queue is stopped. */
static void idc_queue_terminated(struct e10k_binding *b)
{
    errval_t r;
    r = e10k_queue_terminated__tx(b, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}
#endif

/** Request from queue driver for register memory cap */
void cd_request_device_info(struct e10k_binding *b)
{
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

    assert(n < 8);

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

#if 0
    // XXX: interrupt throttle not supported on VF???
    uint8_t i;
    e10k_eitrn_t eitr = 0;
    eitr = e10k_eitrn_itr_int_insert(eitr, rate);

    i = (queues[n].msix_index == -1 ? 0 : queues[n].msix_index);
    e10k_vf_vfeitr_wr(d, eitr);
    if (i < 24) {
        e10k_eitr_l_wr(d, i, eitr);
    } else {
        e10k_eitr_h_wr(d, i - 24, eitr);
    }
#endif
}

#if 0
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

static struct e10k_rx_vtbl rx_vtbl = {
    .request_device_info = cd_request_device_info,
    .register_queue_memory = cd_register_queue_memory,
    .set_interrupt_rate = cd_set_interrupt_rate,
    .terminate_queue = idc_terminate_queue,
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
    e10k_vf_initialize(d, (void*) bar_info[0].vaddr);

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

    DEBUG("STATUS = %x\n", e10k_vf_vfstatus_rd(d));

    // Initialize hardware registers etc.
    DEBUG("Initializing hardware\n");
    device_init();

    assert(initialized);

    // Tell PF driver
    err = e10k_vf_client->vtbl.init_done(e10k_vf_client, vf_num);
    assert(err_is_ok(err));

#if 0
    // Now we initialize the management interface
    DEBUG("Initializing management interface\n");
    initialize_mngif();
#endif
}


/** Register with PCI */
static void pci_register(void)
{
    errval_t r;

    r = pci_client_connect();
    assert(err_is_ok(r));
    DEBUG("connected to pci\n");

    interrupt_handler_fn inthandler;

    if(use_interrupts) {
        inthandler = interrupt_handler;
    } else {
        inthandler = NULL;
    }

    r = pci_register_driver_irq(pci_init_card, PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
                                PCI_VENDOR_INTEL, E10K_PCI_DEVID,
                                pci_bus, pci_device, pci_function,
                                inthandler, NULL);
    assert(err_is_ok(r));
}

static void parse_cmdline(int argc, char **argv)
{
    int i;

    for (i = 1; i < argc; i++) {
        if (strncmp(argv[i], "cardname=", strlen("cardname=") - 1) == 0) {
            service_name = argv[i] + strlen("cardname=");
        } else if (strncmp(argv[i], "bus=", strlen("bus=") - 1) == 0) {
            pci_bus = atol(argv[i] + strlen("bus="));
        } else if (strncmp(argv[i], "device=", strlen("device=") - 1) == 0) {
            pci_device = atol(argv[i] + strlen("device="));
        } else if (strncmp(argv[i], "function=", strlen("function=") - 1) == 0){
            pci_function = atol(argv[i] + strlen("function="));
        } else if (strncmp(argv[i], "vf=", strlen("vf=") - 1) == 0){
            vf_num = atoi(argv[i] + strlen("vf="));
        } else if (strncmp(argv[i], "msix=", strlen("msix=") - 1) == 0) {
            msix = !!atol(argv[i] + strlen("msix="));
            // also pass this to queue driver
            qd_argument(argv[i]);
        } else if (strncmp(argv[i], "interrupts=", strlen("interrupts=") - 1) == 0) {
            use_interrupts = !!atol(argv[i] + strlen("interrupts="));
            qd_argument(argv[i]);
        } else {
            qd_argument(argv[i]);
        }
    }
}

static void vf_bind_cont(void *st, errval_t err, struct e10k_vf_binding *b)
{
  assert(err_is_ok(err));

  struct e10k_vf_rpc_client *r = malloc(sizeof(*r));
  assert(r != NULL);
  err = e10k_vf_rpc_client_init(r, b);
  if (err_is_ok(err)) {
    e10k_vf_client = r;
  } else {
    free(r);
  }
}

static errval_t e10k_vf_client_connect(void)
{
    iref_t iref;
    errval_t err, err2 = SYS_ERR_OK;

    /* Connect to the pci server */
    err = nameservice_blocking_lookup("e10k_vf", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    assert(iref != 0);

    /* Setup flounder connection with pci server */
    err = e10k_vf_bind(iref, vf_bind_cont, &err2, get_default_waitset(),
                   IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        return err;
    }

    /* XXX: Wait for connection establishment */
    while (e10k_vf_client == NULL && err2 == SYS_ERR_OK) {
        messages_wait_and_handle_next();
    }

    err = e10k_vf_client->vtbl.get_mac_address(e10k_vf_client, vf_num, &d_mac);
    assert(err_is_ok(err));

    return err2;
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
int e1000n_driver_init(int argc, char *argv[]);
int e1000n_driver_init(int argc, char *argv[])
#endif
{
    DEBUG("VF driver started\n");
    parse_cmdline(argc, argv);

    DEBUG("Connecting to PF driver...\n");
    e10k_vf_client_connect();

    pci_register();

    while (!initialized) {
        event_dispatch(get_default_waitset());
    }
    qd_main();
    return 1;
}
