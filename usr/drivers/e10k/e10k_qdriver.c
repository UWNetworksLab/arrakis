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
#include <stdarg.h>

#include <net_queue_manager/net_queue_manager.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/spawn_client.h>
#include <barrelfish/debug.h>
#include <skb/skb.h>
#include <trace/trace.h>

#include <if/e10k_defs.h>
#ifndef VF
#       include <dev/e10k_dev.h>
#else
#       include <dev/e10k_vf_dev.h>
#endif
#include <dev/e10k_q_dev.h>
#include <pci/pci.h>

#ifdef LIBRARY
#       include <netif/e1000.h>
#endif

#include "helper.h"
#include "e10k_queue.h"

#ifdef PRINT_QUEUES
static void *glbl_rx_virt = NULL;
static size_t glbl_rx_size = 0;
#endif

/******************************************************************************/
/* Compile time parameters */

/* Size of the TX and RX rings */
/* #define NTXDESCS 1024 */
#define NTXDESCS 2048
#define NRXDESCS 2048
//#define NRXDESCS 512

/* Size of RX buffers */
#define RXBUFSZ 2048

/* Maximal number of buffers per packet */
#define MAXDESCS 16

// Enable Debugging for packet transfers
#define DEBUG_ENABLE 0

// Enable Debugging for intialization code
#define INITDEBUG_ENABLE 0

#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE





/******************************************************************************/
/* Prototypes */

static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx_frame,
                                      struct capref txhwb_frame,
                                      struct capref rx_frame,
                                      uint32_t rxbufsz,
                                      int16_t msix_intvec,
                                      uint8_t msix_intdest,
				      lvaddr_t tx_va,
				      lvaddr_t rx_va,
				      lvaddr_t txhwb_va);
static void idc_set_interrupt_rate(uint8_t queue, uint16_t rate);
static void idc_terminate_queue(void);

// Hack for monolithic driver
void cd_request_device_info(struct e10k_binding *b) __attribute__((weak));
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
			      lvaddr_t txhwb_va) __attribute__((weak));
void cd_set_interrupt_rate(struct e10k_binding *b,
                           uint8_t queue,
                           uint16_t rate) __attribute__((weak));


void qd_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr);
void qd_queue_memory_registered(struct e10k_binding *b);
void qd_write_queue_tails(struct e10k_binding *b);

void qd_argument(const char *arg);
static void interrupt_handler(void *);
void qd_interrupt(bool is_rx, bool is_tx);
void qd_main(void);
int main(int argc, char **argv) __attribute__((weak));




/******************************************************************************/
/* Global state */

/** Service name */
static const char* service_name = "e10k";

/** Indicates if this queue driver is running as a standalone process */
static bool standalone = false;

/** Binding to the internal e10k management service */
static struct e10k_binding *binding = NULL;

/** Queue index for this manager instance */
int qi = -1;

/** Mackerel handle for device */
#ifndef VF
static e10k_t *d = NULL;
#else
static e10k_vf_t *d = NULL;
#endif

/** Queue handle for queue management library */
static e10k_queue_t *q;

/** MAC address to be used */
static uint64_t mac_address = 0;

/** Indicates if the initialization is done */
static int initialized = 0;

/**
 * Indicates whether we should rely on cache coherence for the descriptor
 * rings.
 */
static bool cache_coherence = true;

/** Indicates whether TX head index write back should be used */
#ifndef VF
static bool use_txhwb = true;
#else
// XXX: TX HWB doesn't work on the VF. Probably a bug in the
// driver. It stalls the output queue after a few packets have gone
// out. The Linux VF driver doesn't seem to use TX HWB.
// If you fix this, you'll get a 5% performance boost.
static bool use_txhwb = false;
#endif

/** Indicates whether Interrupts should be used */
static bool use_interrupts = false;

/** Indicates whether RSC should be used */
static bool use_rsc = false;

/** Indicates whether MSI-X should be used */
static bool use_msix = true;

/** Indicates whether or the VT-d should be used for DMA remapping.*/
static bool use_vtd = true; 

/** Minimal delay between interrupts in us */
static uint16_t interrupt_delay = 0;

/** Capability for hardware TX ring */
static struct capref tx_frame;

/** Capability for hardware TX ring */
static struct capref rx_frame;

/** Capability for head index write back feature */
static struct capref txhwb_frame;

/******************************************************************************/
/* Debugging code, etc. */

#if DEBUG_ENABLE
#  define DEBUG queue_debug
#else
#  define DEBUG(x...) do { } while (0)
#endif
#if INITDEBUG_ENABLE
#  define INITDEBUG queue_debug
#else
#  define INITDEBUG(x...) do { } while (0)
#endif


#if DEBUG_ENABLE || INITDEBUG_ENABLE
static void queue_debug(const char* fmt, ...)
{
    va_list va;
    va_start(va, fmt);
    printf("e10k.q%d: ", qi);
    vprintf(fmt, va);
    va_end(va);
}

/* Helper code for debugging that dumps the statistics registers that are != 0.
 * Most registers are cleared uppon a read. */
#ifdef STATS_DUMP
#ifndef VF
#define prnonz(x) uint32_t x = e10k_##x##_rd(d); if (x) snprintf(str[cnt++], 32, #x "=%x", x)
#define prnonzary(x, n)			      \
  for(int i = 0; i < n; i++) {		      \
    uint32_t reg = e10k_##x##_rd(d, i);	      \
    if (reg) snprintf(str[cnt++], 32, #x ".%d=%x", i, reg);	\
  }
static void stats_dump(void)
{
  char str[256][32];
  int cnt = 0;
  memset(str, 0, 256 * 32);

  prnonzary(pfvfte, 2);
  /* prnonzary(pfvfspoof, 8); */
  /* prnonz(rttbcnrc); */
  /* prnonzary(mpsar, 256); */
  prnonz(picause);
  prnonz(rttdcs);

  prnonz(fwsm);
  prnonz(eicr);

  prnonz(dmatxctl);
  prnonzary(tdbal, 128);
  prnonzary(tdbah, 128);
  prnonzary(tdlen, 128);
  prnonzary(tdh, 128);
  prnonzary(tdt, 128);
  prnonzary(txdctl, 128);
  prnonzary(tdwbal, 128);
  prnonzary(tdwbah, 128);
  prnonz(mtqc);

  /* prnonzary(pfvflre, 2); */

    prnonz(crcerrs);
    prnonz(illerrc);
    prnonz(errbc);
    prnonzary(rxmpc, 8);
    prnonz(mlfc);
    prnonz(mrfc);
    prnonz(rlec);
    prnonz(ssvpc);
    // ...
    prnonz(gprc);
    prnonz(gorcl);
    prnonz(gorch);
    prnonz(rxnfgpc);
    // ...
    prnonz(rxdgpc);
    // ...
    prnonz(gptc);
    prnonz(gotcl);
    prnonz(gotch);
    prnonz(txdgpc);
    // ...
    prnonz(ruc);
    prnonz(rfc);
    prnonz(roc);
    prnonz(rjc);
    prnonz(mngprc);
    prnonz(mngpdc);
    prnonz(torl);
    prnonz(torh);
    prnonz(tpr);
    prnonz(tpt);
    // ...
    prnonz(mspdc);
    // ...
    prnonzary(qprc, 16);
    prnonzary(qprdc, 16);
    // ...

    if(cnt > 0) {
      queue_debug("");
      for(int i = 0; i < cnt; i++) {
	printf("%s ", str[i]);
      }
      printf("\n");
    }
}
#else   // VF
#define prnonz(x)                                               \
    uint32_t x = e10k_vf_##x##_rd(d);                           \
    static uint32_t last_##x = 0;                               \
    if (x != last_##x) {                                        \
      snprintf(str[cnt++], 32, #x "=%x", x);                      \
      last_##x = x;                                               \
    }
static void stats_dump(void)
{
  char str[256][32];
  int cnt = 0;
  memset(str, 0, 256 * 32);

    prnonz(vfgprc);
    prnonz(vfgptc);
    prnonz(vfgorc_lsb);
    prnonz(vfgorc_msb);
    prnonz(vfgotc_lsb);
    prnonz(vfgotc_msb);
    prnonz(vfmprc);

    if(cnt > 0) {
      queue_debug("");
      for(int i = 0; i < cnt; i++) {
	printf("%s ", str[i]);
      }
      printf("\n");
    }
}
#endif

#else

static void stats_dump(void) {}

#endif

#else

static void stats_dump(void) {}

#endif





/******************************************************************************/
/* Transmit path */

#define ETHHDR_LEN 14
#define IPHDR_LEN 20
#define UDPHDR_LEN 8



static inline bool buf_use_tcpxsm(struct driver_buffer *buffers)
{
    return (buffers->flags & NETIF_TXFLAG_TCPCHECKSUM);
}

static inline bool buf_use_udpxsm(struct driver_buffer *buffers)
{
    return (buffers->flags & NETIF_TXFLAG_UDPCHECKSUM);
}

static inline bool buf_use_ipxsm(struct driver_buffer *buf)
{
    return (buf->flags & NETIF_TXFLAG_IPCHECKSUM) ||
        buf_use_tcpxsm(buf) || buf_use_udpxsm(buf);
}

static inline uint8_t buf_tcphdrlen(struct driver_buffer *buf)
{
    return ((buf->flags & NETIF_TXFLAG_TCPHDRLEN_MASK) >>
        NETIF_TXFLAG_TCPHDRLEN_SHIFT) * 4;
}

#ifdef LIBRARY
bool e1000n_queue_empty(void)
{
    uint32_t tail, head;

#ifndef VF
    tail = e10k_tdt_rd(d, qi);
    head = e10k_tdh_rd(d, qi);
#else
    tail = e10k_vf_vftdt_rd(d, qi);
    head = e10k_vf_vftdh_rd(d, qi);
#endif

    return (head == tail);
}
#endif

/*
 * This function transmits a packet via the network.
 *
 * It has to make sure to properly mark the last packet of an array of
 * packets, so we can properly collect fully transmitted packets
 * asynchronously later on. Only the last packet is marked with a
 * write-back indication and we scan for that to know the packet was
 * written out.
 */
static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count)
{
    size_t i;
    size_t totallen = 0;
    size_t start = 0;
    DEBUG("Add buffer callback %d:\n", count);

    // TODO: Make sure there is room in TX queue
    for (i = 0; i < count; i++) {
        totallen += buffers[i].len;
    }

    // Prepare checksum offload
    if (buf_use_ipxsm(buffers)) {
        e10k_q_l4_type_t l4t = 0;
        uint8_t l4len = 0;

        if (buf_use_tcpxsm(buffers)) {
            l4t = e10k_q_tcp;
            l4len = buf_tcphdrlen(buffers);
        } else if (buf_use_udpxsm(buffers)) {
            l4t = e10k_q_udp;
            l4len = UDPHDR_LEN;
        }
        e10k_queue_add_txcontext(q, 0, ETHHDR_LEN, IPHDR_LEN, l4len, l4t);
	if (use_vtd) {
	    e10k_queue_add_txbuf_ctx(q, (lvaddr_t)buffers[0].va, buffers[0].len,
				     buffers[0].opaque, 1, (count == 1), totallen, 0, true, l4len != 0);
	} else {
	    e10k_queue_add_txbuf_ctx(q, buffers[0].pa, buffers[0].len,
				     buffers[0].opaque, 1, (count == 1), totallen, 0, true, l4len != 0);
	}
        start++;
   }

    for (i = start; i < count; i++) {
        if (use_vtd) {
	    e10k_queue_add_txbuf(q, (lvaddr_t)buffers[i].va, buffers[i].len,
				 buffers[i].opaque, (i == 0), (i == count - 1), totallen);
	} else {
	    e10k_queue_add_txbuf(q, buffers[i].pa, buffers[i].len,
				 buffers[i].opaque, (i == 0), (i == count - 1), totallen);
	}
    }

    e10k_queue_bump_txtail(q);
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_DRV_SEE, 0);
#endif // TRACE_ETHERSRV_MODE

    return SYS_ERR_OK;
}


static uint64_t find_tx_free_slot_count_fn(void)
{
    return e10k_queue_free_txslots(q);
}

static bool handle_free_tx_slot_fn(void)
{
    void *op;

    if (e10k_queue_get_txbuf(q, &op) != 0) {
        return false;
    }

    DEBUG("handle_free_tx_slot_fn: Packet %p done\n", op);

    stats_dump();

#if TRACE_ETHERSRV_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_DRVTXDONE, 0);
#endif // TRACE_ETHERSRV_MODE

    handle_tx_done(op);

    return true;
}





/******************************************************************************/
/* Receive path */

static void check_for_free_txbufs(void)
{
    if (!initialized) return;

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    while (handle_free_tx_slot_fn()) { }
}

static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{
    /* printf("Got %p from stack\n", opaque); */
    DEBUG("register_rx_buffer_fn: called\n");
    if (use_vtd) {
        e10k_queue_add_rxbuf(q, (lvaddr_t)vaddr, opaque);
    } else {
        e10k_queue_add_rxbuf(q, paddr, opaque);
    }
    e10k_queue_bump_rxtail(q);

    DEBUG("register_rx_buffer_fn: terminated\n");
    return SYS_ERR_OK;
}

static uint64_t find_rx_free_slot_count_fn(void)
{
    return e10k_queue_free_rxslots(q);
}

static size_t check_for_new_packets(int num)
{
    size_t len;
    void *op;
    int last = 1;
    size_t count;
    size_t pkt_cnt;
    struct driver_rx_buffer buf[MAXDESCS];
    uint64_t flags = 0;
    int res;

    if (!initialized) return 0;

    /* stats_dump(); */

#ifdef PRINT_QUEUES
    uint64_t *tv = glbl_rx_virt;
    size_t cnt = 0;
    for(size_t i = 0; i < glbl_rx_size / 8; i++) {
        if(tv[i] != 0) {
            printf("%zx: %lx, ", i, tv[i]);
            cnt++;
        }
        if(cnt % 16 == 0) {
            printf("\n");
            cnt++;
        }
    }
    printf("\n");
#endif

#ifndef VF
#if 0
    // Check if queue runs too full
    int rdtp = e10k_rdt_1_rd(d, qi);
    int rdhp = e10k_rdh_1_rd(d, qi);
    if(rdtp < rdhp) {
        if(NRXDESCS - (rdhp - rdtp) < 10) {
            printf("Running low on receive buffers! rdhp = %u, rdtp = %u\n", rdhp, rdtp);
        }
    } else {
        if(rdtp - rdhp < 10) {
            printf("Running low on receive buffers! rdhp = %u, rdtp = %u\n", rdhp, rdtp);
        }
    }
#endif
#endif

    // TODO: This loop can cause very heavily bursty behaviour, if the packets
    // arrive faster than they can be processed.
    count = 0;
    pkt_cnt = 0;
    while ((res = e10k_queue_get_rxbuf(q, &op, &len, &last, &flags)) == 0 ||
           pkt_cnt != 0)
    {
        if (res != 0) continue;
#if TRACE_ETHERSRV_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_DRVRX, 0);
#endif // TRACE_ETHERSRV_MODE


        DEBUG("New packet (q=%d f=%"PRIx64")\n", qi, flags);

        buf[pkt_cnt].opaque = op;
        buf[pkt_cnt].len = len;
        pkt_cnt++;
        if (last) {
            process_received_packet(buf, pkt_cnt, flags);
            pkt_cnt = 0;
            if(num != 0) {
                count++;
                flags = 0;
                break;
            }
        } else {
            assert(pkt_cnt < MAXDESCS - 1);
        }
        count++;
        flags = 0;
    }

    if (count > 0) e10k_queue_bump_rxtail(q);
    return count;
}




/******************************************************************************/
/* Misc */


/* Helper call-backs for queue manager */
static errval_t update_txtail(void *opaque, size_t tail)
{
    assert(d != NULL);

#ifndef VF
    e10k_tdt_wr(d, qi, tail);
#else
    e10k_vf_vftdt_wr(d, qi, tail);
#endif
    return SYS_ERR_OK;
}

static errval_t update_rxtail(void *opaque, size_t tail)
{
    assert(d != NULL);

#ifndef VF
    e10k_rdt_1_wr(d, qi, tail);
#else
    e10k_vf_vfrdt_wr(d, qi, tail);
#endif
    return SYS_ERR_OK;
}

/**
 * Callback to pass MAC address to queue manager library.
 */
static void get_mac_addr_fn(uint8_t *mac)
{
    memcpy(mac, &mac_address, 6);
}




/******************************************************************************/
/* Device/queue initialization */

/** Allocate queue n and return handle for queue manager */
static void setup_queue(void)
{
    struct e10k_queue_ops ops = {
        .update_txtail = update_txtail,
        .update_rxtail = update_rxtail };

    size_t tx_size, txhwb_size, rx_size;
    void *tx_virt, *txhwb_virt, *rx_virt;
    vregion_flags_t flags;
    uint8_t vector, core;
    errval_t err;



    INITDEBUG("setup_queue\n");

    // Decide on which flags to use for the mappings
    flags = (cache_coherence ? VREGION_FLAGS_READ_WRITE :
                               VREGION_FLAGS_READ_WRITE_NOCACHE);

    // Allocate memory for descriptor rings
    tx_size = e10k_q_tdesc_legacy_size * NTXDESCS;
    tx_virt = alloc_map_frame(flags, tx_size, &tx_frame);
    assert(tx_virt != NULL);

    rx_size = e10k_q_rdesc_legacy_size * NRXDESCS;
    rx_virt = alloc_map_frame(flags, rx_size, &rx_frame);
    assert(rx_virt != NULL);

#ifdef PRINT_QUEUES
    glbl_rx_virt = rx_virt;
    glbl_rx_size = rx_size;
#endif

    // Register memory with device manager
    txhwb_virt = NULL;
    if (use_txhwb) {
        INITDEBUG("Using transmit write-back\n");
        txhwb_size = BASE_PAGE_SIZE;
        txhwb_virt = alloc_map_frame(flags, txhwb_size, &txhwb_frame);
        assert(txhwb_virt != NULL);
        memset(txhwb_virt, 0, sizeof(uint32_t));
        assert(txhwb_virt != NULL);
    }

    // Initialize queue manager
    q = e10k_queue_init(tx_virt, NTXDESCS, txhwb_virt, rx_virt, NRXDESCS, &ops,
                        NULL);



    if (use_interrupts && use_msix) {
        INITDEBUG("Enabling MSI-X interrupts\n");
        err = pci_setup_inthandler(interrupt_handler, NULL, &vector);
        assert(err_is_ok(err));
        core = disp_get_core_id();
    } else {
        if (use_interrupts) {
            INITDEBUG("Enabling legacy interrupts\n");
        }
        vector = 0;
        core = 0;
    }


    if (use_vtd) {
      err = skb_client_connect();
      assert(err_is_ok(err));
      err = skb_execute_query("vtd_enabled(0,_).");
      if (err_is_fail(err)) {
          use_vtd = false;
      }
    }

    if (use_vtd) {
        idc_register_queue_memory(qi, tx_frame, txhwb_frame, rx_frame, RXBUFSZ,
				  vector, core, (lvaddr_t)tx_virt, (lvaddr_t)rx_virt, (lvaddr_t)txhwb_virt);
    } else {
        idc_register_queue_memory(qi, tx_frame, txhwb_frame, rx_frame, RXBUFSZ,
				  vector, core, 0, 0, 0);
    }
}

/** Hardware queue initialized in card driver */
static void hwqueue_initialized(void)
{
    idc_set_interrupt_rate(qi, interrupt_delay);
}

/** Terminate this queue driver */
static void terminate_queue_fn(void)
{
    idc_terminate_queue();
}





/******************************************************************************/
/* Management interface implemetation */

/** Request device register cap from card driver */
static void idc_request_device_info(void)
{

    errval_t r;
    INITDEBUG("idc_request_device_info()\n");

    if (!standalone) {
        cd_request_device_info(NULL);
        return;
    }

    r = e10k_request_device_info__tx(binding, NOP_CONT);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Send memory caps to card driver */
static void idc_register_queue_memory(uint8_t queue,
                                      struct capref tx,
                                      struct capref txhwb,
                                      struct capref rx,
                                      uint32_t rxbufsz,
                                      int16_t msix_intvec,
                                      uint8_t msix_intdest,
				      lvaddr_t tx_va,
				      lvaddr_t rx_va,
				      lvaddr_t txhwb_va)
{

    errval_t r;
    INITDEBUG("idc_register_queue_memory()\n");

    if (!standalone) {
        cd_register_queue_memory(NULL, queue, tx, txhwb, rx, rxbufsz,
				 msix_intvec, msix_intdest, use_interrupts, use_rsc,
				 tx_va, rx_va, txhwb_va);

        return;
    }

    r = e10k_register_queue_memory__tx(binding, NOP_CONT, queue,
                                       tx, txhwb, rx, rxbufsz, msix_intvec,
                                       msix_intdest, use_interrupts, use_rsc,
				       tx_va, rx_va, txhwb_va);
    // TODO: handle busy
    assert(err_is_ok(r));
}

/** Modify interrupt rate for queue */
static void idc_set_interrupt_rate(uint8_t queue, uint16_t rate)
{
    errval_t r;

    INITDEBUG("idc_set_interrupt_rate()\n");

    if (!standalone) {
        cd_set_interrupt_rate(NULL, queue, rate);
        return;
    }

    r = e10k_set_interrupt_rate__tx(binding, NOP_CONT, queue, rate);
    // TODO: handle busy
    assert(err_is_ok(r));

}

/** Tell card driver to stop this queue. */
static void idc_terminate_queue(void)
{
    errval_t r;
    INITDEBUG("idc_terminate_queue()\n");

    if (!standalone) {
        USER_PANIC("Terminating monolithic driver is not a good idea");
    }

    r = e10k_terminate_queue__tx(binding, NOP_CONT, qi);
    // TODO: handle busy
    assert(err_is_ok(r));
}

// Callback from device manager
void qd_queue_init_data(struct e10k_binding *b, struct capref registers,
        uint64_t macaddr)
{
    struct frame_identity frameid = { .base = 0, .bits = 0 };
    errval_t err;
    void *virt;

    INITDEBUG("idc_queue_init_data\n");

    mac_address = macaddr;

    // Map device registers
    invoke_frame_identify(registers, &frameid);
    err = vspace_map_one_frame_attr(&virt, 1 << frameid.bits, registers,
            VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    assert(err_is_ok(err));

    // Initialize mackerel device
    d = malloc(sizeof(*d));
#ifndef VF
    e10k_initialize(d, virt);
#else
    e10k_vf_initialize(d, virt);
#endif

    // Initialize queue
    setup_queue();
}

// Callback from device manager
void qd_queue_memory_registered(struct e10k_binding *b)
{
    initialized = 1;

    hwqueue_initialized();

    // Register queue with queue_mgr library
#ifndef LIBRARY
    ethersrv_init((char*) service_name, qi, get_mac_addr_fn, terminate_queue_fn,
        transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
        handle_free_tx_slot_fn, RXBUFSZ, register_rx_buffer_fn,
        find_rx_free_slot_count_fn);
#else
    ethernetif_backend_init((char*) service_name, qi, get_mac_addr_fn, terminate_queue_fn,
        transmit_pbuf_list_fn, find_tx_free_slot_count_fn,
        handle_free_tx_slot_fn, RXBUFSZ, register_rx_buffer_fn,
        find_rx_free_slot_count_fn);
#endif
}

// Callback from device manager
void qd_write_queue_tails(struct e10k_binding *b)
{
    INITDEBUG("idc_write_queue_tails()\n");

    e10k_queue_bump_rxtail(q);
    e10k_queue_bump_txtail(q);
}

#ifndef LIBRARY
// Callback from device manager
static void idc_queue_terminated(struct e10k_binding *b)
{
    errval_t err;

    INITDEBUG("idc_queue_terminated()\n");

    // Free memory for hardware ring buffers
    err = vspace_unmap(q->tx_ring);
    assert(err_is_ok(err));
    err = vspace_unmap(q->rx_ring);
    assert(err_is_ok(err));
    err = cap_delete(tx_frame);
    assert(err_is_ok(err));
    err = cap_delete(rx_frame);
    assert(err_is_ok(err));

    if (!capref_is_null(txhwb_frame)) {
        err = vspace_unmap(q->tx_hwb);
        assert(err_is_ok(err));
        err = cap_delete(txhwb_frame);
        assert(err_is_ok(err));
    }

    exit(0);
}

static struct e10k_rx_vtbl rx_vtbl = {
    .queue_init_data = qd_queue_init_data,
    .queue_memory_registered = qd_queue_memory_registered,
    .write_queue_tails = qd_write_queue_tails,
    .queue_terminated = idc_queue_terminated,
};

static void bind_cb(void *st, errval_t err, struct e10k_binding *b)
{
    assert(err_is_ok(err));

    INITDEBUG("Sucessfully connected to management interface\n");

    b->rx_vtbl = rx_vtbl;
    binding = b;

    idc_request_device_info();
}

/** Connect to the management interface */
static void connect_to_mngif(void)
{
    errval_t r;
    iref_t iref;
    const char *suffix = "_e10kmng";
    char name[strlen(service_name) + strlen(suffix) + 1];

    // Build label for interal management service
    sprintf(name, "%s%s", service_name, suffix);

    // Connect to service
    INITDEBUG("Looking up management interface (%s)\n", name);
    r = nameservice_blocking_lookup(name, &iref);
    assert(err_is_ok(r));

    INITDEBUG("Binding to management interface\n");
    r = e10k_bind(iref, bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(r));
}

#else

void ethersrv_argument(const char* arg)
{
    if (strncmp(arg, "queue=", strlen("queue=") - 1) == 0) {
        printf("Arrakis: Not passing '%s' argument to ethersrv\n", arg);
    } else {
        printf("Warning: ethersrv_argument '%s' not supported in Arrakis!\n", arg);
    }
}
#endif

void qd_argument(const char *arg)
{
    if (strncmp(arg, "cardname=", strlen("cardname=") - 1) == 0) {
        service_name = arg + strlen("cardname=");
        ethersrv_argument(arg);
    } else if (strncmp(arg, "queue=", strlen("queue=") - 1) == 0) {
        qi = atol(arg + strlen("queue="));
        ethersrv_argument(arg);
    } else if (strncmp(arg, "cache_coherence=",
                       strlen("cache_coherence=") - 1) == 0) {
        cache_coherence = !!atol(arg + strlen("cache_coherence="));
    } else if (strncmp(arg, "head_idx_wb=",
                       strlen("head_idx_wb=") - 1) == 0) {
        use_txhwb = !!atol(arg + strlen("head_idx_wb="));
    } else if (strncmp(arg, "interrupts=",
                       strlen("interrupts=") - 1) == 0) {
        use_interrupts = !!atol(arg + strlen("interrupts="));
    } else if (strncmp(arg, "rsc=",
                       strlen("rsc=") - 1) == 0) {
        use_rsc = !!atol(arg + strlen("rsc="));
    } else if (strncmp(arg, "int_delay=",
                       strlen("int_delay=") - 1) == 0) {
        long i = atol(arg + strlen("int_delay="));
        uint16_t max_delay = 1023;
        if (i < 0 || i > max_delay) {
            printf("Invalid int_delay value, must be between 0 and %u\n",
                max_delay);
        } else {
            interrupt_delay = i;
        }
    } else if (strncmp(arg, "msix=", strlen("msix=") - 1) == 0) {
        use_msix = !!atol(arg + strlen("msix="));
    } else if (strncmp(arg, "use_vtd=", strlen("use_vtd")-1) == 0) {
        use_vtd = !!atol(arg + strlen("use_vtd="));
    } else {
        ethersrv_argument(arg);
    }
}

#ifndef LIBRARY
static void parse_cmdline(int argc, char **argv)
{
    int i;
    for (i = 1; i < argc; i++) {
        qd_argument(argv[i]);
    }
}

static void eventloop(void)
#else
extern struct waitset *lwip_waitset;

#ifdef LIBRARY
void arranet_polling_loop(void)
{
    /* errval_t err; */
    /* err = event_dispatch_non_block(barrelfish_interrupt_waitset); */
    check_for_free_txbufs();
    if(!use_interrupts) {
        check_for_new_packets(1);
    } else {
        assert(!"NYI?");
    }
}
#endif

void e1000n_polling_loop(struct waitset *ws)
#endif
{
#ifndef LIBRARY
    struct waitset *ws = get_default_waitset();
#endif
    errval_t err;

    /* INITDEBUG("eventloop()\n"); */

    while (1) {
        err = event_dispatch_non_block(ws);
#ifdef LIBRARY
        check_for_free_txbufs();
#else
        do_pending_work_for_all();
#endif
        if(!use_interrupts) {
            check_for_new_packets(0);
            /* check_for_new_packets(1); */
            check_for_free_txbufs();
        }

#ifdef LIBRARY
        break;
#endif
    }
}

#ifndef LIBRARY
static void eventloop_ints(void)
{
    struct waitset *ws;
    INITDEBUG("eventloop_ints()\n");

    ws = get_default_waitset();
    while (1) {
        event_dispatch(ws);
        do_pending_work_for_all();
    }
}
#endif

static void interrupt_handler(void *data)
{
    qd_interrupt(true, false);
}

void qd_interrupt(bool is_rx, bool is_tx)
{
    size_t count;

#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_NI_I, 0);
#endif // TRACE_ETHERSRV_MODE

    if (is_rx) {
        count = check_for_new_packets(0);
        if (count == 0) {
            //printf("No RX\n");
        }
    }
    check_for_free_txbufs();
}

void qd_main(void)
{
    // Validate some settings
    if (qi == -1) {
        USER_PANIC("For queue driver the queue= parameter has to be specified "
                   "on the command line!");
    }

    if (use_interrupts && standalone && !use_msix) {
        USER_PANIC("Interrupts with standalone queue driver only work if MSI-X "
                   "is enabled.");
    }

#ifndef LIBRARY
    if (standalone) {
        connect_to_mngif();
    } else {
#endif
        idc_request_device_info();
#ifndef LIBRARY
    }

    if (use_interrupts) {
        eventloop_ints();
    } else {
        eventloop();
    }
#endif
}

#ifndef LIBRARY
int main(int argc, char **argv)
{
    DEBUG("Started\n");
    standalone = true;
    parse_cmdline(argc, argv);
    qd_main();
}



void cd_request_device_info(struct e10k_binding *b)
{
    USER_PANIC("Should not be called");
}

void cd_register_queue_memory(struct e10k_binding *b,
                              uint8_t queue,
                              struct capref tx,
                              struct capref txhwb,
                              struct capref rx,
                              uint32_t rxbufsz,
                              int16_t msix_intvec,
                              uint8_t msix_intdest,
                              bool use_ints,
                              bool use_rsc_,
			      lvaddr_t tx_va,
			      lvaddr_t rx_va,
			      lvaddr_t txhwb_va)
{
    USER_PANIC("Should not be called");
}

void cd_set_interrupt_rate(struct e10k_binding *b,
                           uint8_t queue,
                           uint16_t rate)
{
    USER_PANIC("Should not be called");
}

#endif
