/*
 * Copyright (c) 2008, ETH Zurich. All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
/*
 * e1000.c
 *
 *  Created on: Feb 12, 2013
 *      Author: mao
 *
 * NOTES:
 *      General:
 *          The driver uses kaluga to probe for supported PCI/e devices. At boot, It might happen
 *          that kaluga has not yet finished probing PCI devices and your card doesn't get detected.
 *          If you want the driver automaticaly at boot, try passing device id as a parameter in grub.
 *          Edit menu.lst:
 *          module /x86_64/sbin/e1000 deviceid=xxxx
 *
 *          If you don't know your device id, use lshw -pci to list available PCI devices.
 *          Try looking up all devices_id with vendor 0x8086 in the PCI database:
 *              http://www.pcidatabase.com/vendor_details.php?id=1302
 *          Your network card should be called some thing with e1000, Intel Pro/1000 or e1000.
 *
 *          If you use Simics, also read the Simics note.
 *
 *      Simics:
 *          Currently Simics doesn't provide an EEPROM for the tested network cards and the mac_address
 *          argument doesn't seem to set the MAC address properly. You will have to specify it manually:
 *
 *          e1000 mac=54:10:10:53:00:30
 *
 *
 * This part of the code builds on the original e1000 Barrelfish driver.
 *
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <octopus/octopus.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>
#include <trace/trace.h>
#ifdef LIBRARY
#       include <netif/e1000.h>
#endif

#include "e1000n.h"

#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

#define MAX_ALLOWED_PKT_PER_ITERATION   (0xff)  // working value
/* Transmit and receive buffers must be multiples of 8 */
#define DRIVER_RECEIVE_BUFFERS      (1024 * 8)
#define DRIVER_TRANSMIT_BUFFERS     (1024 * 8)

/* MTU is 1500 bytes, plus Ethernet header plus CRC. */
#define RX_PACKET_MAX_LEN       (1500 + 14 + 4)

#define PACKET_SIZE_LIMIT       1073741824      /* 1 Gigabyte */

/*****************************************************************
 * Local states:
 *****************************************************************/
static uint64_t minbase = -1;
static uint64_t maxbase = -1;

/*****************************************************************
 * External declarations for net_queue_manager
 *
 ****************************************************************/
extern uint64_t interrupt_counter;
extern uint64_t total_rx_p_count;
extern struct client_closure *g_cl;
extern uint64_t total_rx_datasize;

/*****************************************************************
 * Receive and transmit
 *****************************************************************/
static e1000_rx_bsize_t receive_buffer_size = bsize_16384;
static volatile struct tx_desc *transmit_ring;

// Data-structure to map sent buffer slots back to application slots
struct pbuf_desc {
    void *opaque;
};

static struct pbuf_desc pbuf_list_tx[DRIVER_TRANSMIT_BUFFERS];

//remember the tx pbufs in use

//receive
static volatile union rx_desc *receive_ring;

static uint32_t receive_bufptr = 0;
static uint32_t receive_index = 0;
static uint32_t receive_free = 0;
static void **receive_opaque = NULL;
/*****************************************************************

 * e1000 states:
 *****************************************************************/
static e1000_t e1000;
static e1000_device_t e1000_device;
static uint8_t mac_address[MAC_ADDRESS_LEN]; /* buffers the card's MAC address upon card reset */

/*****************************************************************
 * argument states:
 *****************************************************************/
static bool user_mac_address; /* True if the user specified the MAC address */
static bool use_interrupt = true; /* don't use card polling mode */
//static bool use_interrupt = false; /* don't use card polling mode */
static bool use_force = false; /* don't attempt to find card force load */

/*****************************************************************
 * Local states:
 *****************************************************************/
static uint32_t class = PCI_CLASS_ETHERNET;
static uint32_t subclass = PCI_DONT_CARE;
static uint32_t bus = PCI_DONT_CARE;
static uint32_t device = PCI_DONT_CARE;
static uint32_t function = PCI_DONT_CARE;
static uint32_t deviceid = PCI_DONT_CARE;
static uint32_t vendor = PCI_VENDOR_INTEL;
static uint32_t program_interface = PCI_DONT_CARE;
static e1000_mac_type_t mac_type = e1000_undefined;

/*****************************************************************
 * For use with the net_queue_manager
 *
 *****************************************************************/
static char *global_service_name = 0;
static uint64_t assumed_queue_id = 0;   /* what net queue to bind to */
static uint32_t ether_transmit_index = 0;
static uint32_t ether_transmit_bufptr = 0;

/*****************************************************************
 * Print physical link status.
 *
 ****************************************************************/
static void e1000_print_link_status(e1000_device_t *dev)
{
    const char *media_type = NULL;
    e1000_status_t status;

    status = e1000_status_rd(dev->device);
    e1000_ledctl_rd(dev->device);

    switch (dev->media_type) {
    case e1000_media_type_copper:
        media_type = "copper";
        break;
    case e1000_media_type_fiber:
        media_type = "fiber";
        break;
    case e1000_media_type_serdes:
        media_type = "SerDes";
        break;
    default:
        media_type = "Unknown";
        break;
    }

    if (e1000_check_link_up(dev)) {
        const char *duplex;

        if (e1000_status_fd_extract(status)) {
            duplex = "Full";
        } else {
            duplex = "Half";
        }

        switch (e1000_status_speed_extract(status)) {
        case 0x0:
            E1000_PRINT("Media type: %s, Link speed: 10 Mb in %s duplex.\n",
                        media_type, duplex);
            break;
        case 0x1:
            E1000_PRINT("Media type: %s, Link speed: 100 Mb in %s duplex.\n",
                        media_type, duplex);
            break;
        default:
            E1000_PRINT("Media type: %s, Link speed: 1 Gb in %s duplex.\n",
                        media_type, duplex);
            break;
        }
    } else {
        E1000_PRINT("Media type: %s, Link down.\n", media_type);
    }
}


/*****************************************************************
 * get_mac_address_fn for net_queue_manager
 *
 * NOTE: This function gets called in ethersrv.c.
 ****************************************************************/
static void get_mac_address_fn(uint8_t *mac)
{
    memcpy(mac, mac_address, sizeof(mac_address));
}

/*****************************************************************
 * find_tx_free_slot for net_queue_manager
 *
 *****************************************************************/
static uint64_t find_tx_free_slot_count_fn(void)
{
    uint64_t free_slots;

    if (ether_transmit_index >= ether_transmit_bufptr) {
        free_slots = DRIVER_TRANSMIT_BUFFERS
                     - ((ether_transmit_index - ether_transmit_bufptr)
                        % DRIVER_TRANSMIT_BUFFERS);
    } else {
        free_slots = (ether_transmit_bufptr - ether_transmit_index)
                     % DRIVER_TRANSMIT_BUFFERS;
    }

    return free_slots;
}

#ifdef LIBRARY
bool e1000n_queue_empty(void)
{
    uint16_t tail, head;

    tail = e1000_tdt_val_rdf(&(e1000), 0);
    head = e1000_tdh_val_rdf(&(e1000), 0);

    return (head == tail);
}
#endif

/*****************************************************************
 * Transmit logic
 *
 * Check if there are enough free buffers with driver,
 * so that packet can be sent
 ****************************************************************/
static bool can_transmit(uint64_t numbufs)
{
    uint64_t free_slots;

    assert(numbufs < DRIVER_TRANSMIT_BUFFERS);

    free_slots = find_tx_free_slot_count_fn();

    return (free_slots > numbufs);
}

/*****************************************************************
 * handle_free_TX_slot_fn for net_queue_manager
 *
 *****************************************************************/
static bool handle_free_TX_slot_fn(void)
{
    uint64_t ts = rdtsc();
    bool sent = false;
    volatile struct tx_desc *txd;

    if (ether_transmit_bufptr == ether_transmit_index) {
        return false;
    }

    txd = &transmit_ring[ether_transmit_bufptr];
    if (txd->ctrl.legacy.stat_rsv.d.dd != 1) {
        return false;

    }

#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_TXDRVSEE, 0);
#endif

    sent = handle_tx_done(pbuf_list_tx[ether_transmit_bufptr].opaque);

    ether_transmit_bufptr = (ether_transmit_bufptr + 1)
                                  % DRIVER_TRANSMIT_BUFFERS;
    netbench_record_event_simple(bm, RE_TX_DONE, ts);

    return sent;
}

/*****************************************************************
 * Setup transmit descriptor for packet transmission.
 *
 *****************************************************************/
 static uint64_t transmit_pbuf(uint64_t buffer_address,
                              size_t packet_len, bool last, void *opaque)
{
    e1000_dqval_t dqval = 0;
    struct tx_desc tdesc;

    tdesc.buffer_address = buffer_address;
    tdesc.ctrl.raw = 0;
    tdesc.ctrl.legacy.data_len = packet_len;
    tdesc.ctrl.legacy.cmd.d.rs = 1;
    tdesc.ctrl.legacy.cmd.d.ifcs = 1;
    tdesc.ctrl.legacy.cmd.d.eop = (last ? 1 : 0);

    /* FIXME: the packet should be copied into separate location, so that
     * application can't temper with it. */
    transmit_ring[ether_transmit_index] = tdesc;
    pbuf_list_tx[ether_transmit_index].opaque = opaque;

    ether_transmit_index = (ether_transmit_index + 1) % DRIVER_TRANSMIT_BUFFERS;
    dqval = e1000_dqval_val_insert(dqval, ether_transmit_index);
    e1000_tdt_wr(&(e1000), 0, dqval);

    E1000_DEBUG("ether_transmit_index %"PRIu32"\n", ether_transmit_index);

    /* Actual place where packet is sent.  Adding trace_event here */
#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_NO_S,
                (uint32_t)ether_transmit_index);
#endif

    return 0;
}

/*****************************************************************
 * transmit_pbuf_list_fn for net_queue_manager
 *
 * Send the buffer to device driver TX ring.
 *
 * NOTE: This function get called from ethersrv.c
 *****************************************************************/
static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers,
                                      size_t                count)

{
    E1000_DEBUG("transmit_pbuf_list_fn(count=%"PRIu64")\n", count);
    if (!can_transmit(count)){
        while(handle_free_TX_slot_fn());
        if (!can_transmit(count)){
            return ETHERSRV_ERR_CANT_TRANSMIT;
        }
    }

    if (count > 1) {
        E1000_DEBUG("Sending %zx chunks\n", count);
    }
    for (int i = 0; i < count; i++) {
        errval_t r = transmit_pbuf(buffers[i].pa, buffers[i].len,
                    i == (count - 1), //last?
                    buffers[i].opaque);
        if(err_is_fail(r)) {
            //E1000_DEBUG("ERROR:transmit_pbuf failed\n");
            printf("ERROR:transmit_pbuf failed\n");
            return r;
        }
        E1000_DEBUG("transmit_pbuf done for pbuf 0x%p, index %i\n",
            buffers[i].opaque, i);
    } // end for: for each pbuf
#if TRACE_ONLY_SUB_NNET
    trace_event(TRACE_SUBSYS_NNET,  TRACE_EVENT_NNET_TXDRVADD,
        (uint32_t)0);
#endif // TRACE_ONLY_SUB_NNET

    return SYS_ERR_OK;
} // end function: transmit_pbuf_list_fn


static bool handle_next_received_packet(void)
{
    volatile union rx_desc *rxd;
    size_t len = 0;
    bool new_packet = false;
    struct driver_rx_buffer rxb;

    if (receive_bufptr == receive_index) { //no packets received
        return false;
    }

//    E1000_DEBUG("Inside handle next packet 2\n");
    rxd = &receive_ring[receive_bufptr];

    if ((rxd->rx_read_format.info.status.dd) &&
            (rxd->rx_read_format.info.status.eop)
//            && (!local_pbuf[receive_bufptr].event_sent)
            ) {

//      valid packet received
      E1000_DEBUG ("Potential packet receive [%"PRIu32"]!\n",
            receive_bufptr);
        new_packet = true;
        len = rxd->rx_read_format.info.length;
        total_rx_datasize += len;

#if TRACE_ONLY_SUB_NNET
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_RXDRVSEE,
                    (uint32_t) len);
#endif // TRACE_ONLY_SUB_NNET

        rxb.opaque = receive_opaque[receive_bufptr];
        rxb.len = len;
        process_received_packet(&rxb, 1, 0);
    } // end if: valid packet received
    else {
    	// false alarm. Something else happened, not packet arrival
    	return false;
    }
    receive_bufptr = (receive_bufptr + 1) % DRIVER_RECEIVE_BUFFERS;
    --receive_free;
    return new_packet;
} // end function: handle_next_received_packet


/*****************************************************************
 * print receive benchmarking stats.
 *
 ****************************************************************/
static void print_rx_bm_stats(bool stop_trace)
{
    uint64_t running_time;
    uint64_t cts;

    if (g_cl == NULL) {
        return;
    }

    if (g_cl->debug_state != 4) {
        return;
    }

    cts = rdtsc();

#if TRACE_ETHERSRV_MODE
    if (stop_trace) {
        /* stopping the tracing */
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_STOP, 0);
    }
#endif // TRACE_ETHERSRV_MODE

    running_time = cts - g_cl->start_ts;
    E1000_PRINT("D:I:%u: RX speed = [%"PRIu64"] packets "
                "data(%"PRIu64") / time(%"PU") = [%f] MB/s ([%f]Mbps) = "
                " [%f]mpps, INT [%"PRIu64"].\n", disp_get_core_id(), total_rx_p_count,
                total_rx_datasize, in_seconds(running_time),
                ((total_rx_datasize / in_seconds(running_time)) / (1024 * 1024)),
                (((total_rx_datasize * 8) / in_seconds(running_time))
                 / (1024 * 1024)),
                ((total_rx_p_count / in_seconds(running_time)) / (double) (1000000)),
                interrupt_counter);

    netbench_print_event_stat(bm, RE_COPY, "D: RX CP T", 1);
    netbench_print_event_stat(bm, RE_PROCESSING_ALL, "D: RX processing T", 1);
}

/*****************************************************************
 * handle multiple packets for interrupt_handler.
 *
 ****************************************************************/
static uint64_t handle_multiple_packets(uint64_t upper_limit)
{
    static bool benchmark_complete = false;

    uint64_t ts = rdtsc();
    uint8_t local_pkt_count = 0;

    while (handle_next_received_packet()) {
        ++total_rx_p_count;

#if TRACE_ETHERSRV_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_DRV_SEE,
                    total_rx_p_count);
#endif // TRACE_ETHERSRV_MODE

        if (total_rx_datasize > PACKET_SIZE_LIMIT) {
            if (!benchmark_complete) {
                netbench_record_event_simple(bm, RE_PROCESSING_ALL, ts);
                benchmark_complete = true;
                print_rx_bm_stats(true);

                ts = rdtsc();
            }
        }

        ++local_pkt_count;
        if (local_pkt_count == upper_limit) {
            break;
        }
    }

    netbench_record_event_simple(bm, RE_PROCESSING_ALL, ts);

    return local_pkt_count;
}

/*****************************************************************
 * Polls all the client's channels as well as the transmit and
 * receive descriptor rings.
 *
 * This function should never exit.
 ****************************************************************/
#ifndef LIBRARY
static void polling_loop(void)
#else
extern struct waitset *lwip_waitset;

void arranet_polling_loop(void)
{
    errval_t err = event_dispatch_non_block(barrelfish_interrupt_waitset); // nonblocking
    if (err != LIB_ERR_NO_EVENT && err_is_fail(err)) {
        E1000_DEBUG("Error in event_dispatch_non_block, returned %d\n",
                    (unsigned int)err);
    }
    // Give it a manual poll if there were no interrupts
    if(err_no(err) == LIB_ERR_NO_EVENT) {
        while(handle_free_TX_slot_fn());
        handle_multiple_packets(1);
    }
}

void e1000n_polling_loop(struct waitset *ws)
#endif
{
    uint64_t poll_count = 0;
    uint64_t ts;
    uint8_t jobless_iterations = 0;
    errval_t err;
    bool no_work = true;

    while (1) {
        no_work = true;
        ++poll_count;

        ts = rdtsc();
#ifndef LIBRARY
        do_pending_work_for_all();
#endif
        netbench_record_event_simple(bm, RE_PENDING_WORK, ts);

#ifndef LIBRARY
        struct waitset *ws = get_default_waitset();
#endif

        if (use_interrupt) {
            err = event_dispatch_debug(ws); // blocking // doesn't work correctly
        } else {
            err = event_dispatch_non_block(ws); // nonblocking for polling mode
        }
//        err = event_dispatch_non_block(ws); // nonblocking for polling mode
//        err = event_dispatch(ws); // nonblocking for polling mode
        if (err != LIB_ERR_NO_EVENT && err_is_fail(err)) {
            E1000_DEBUG("Error in event_dispatch_non_block, returned %d\n",
                        (unsigned int)err);
            break;
        } else {
            no_work = true;
        }

        while(handle_free_TX_slot_fn());

#if TRACE_ETHERSRV_MODE
        trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_DRV_POLL, poll_count);
#endif // TRACE_ETHERSRV_MODE

        if (handle_multiple_packets(MAX_ALLOWED_PKT_PER_ITERATION) > 0) {
            no_work = false;
        }

/*
        err = event_dispatch_debug(ws); // blocking // doesn't work correctly
        if (err_is_fail(err)) {
            E1000_DEBUG("Error in event_dispatch_non_block, returned %d\n",
                        (unsigned int)err);
            break;
        }
*/

        if (no_work) {
            ++jobless_iterations;
            if (jobless_iterations == 10) {
#ifndef LIBRARY
                /* if (use_interrupt) { */
                /*     E1000_DEBUG("no work available, yielding thread\n"); */
                /*     thread_yield(); */
                /* } */
#else
                /* E1000_DEBUG("no work available, returning\n"); */
                return;
#endif
            }
        }
    } // end while
}

/*****************************************************************
 * Parse MAC address to see if it has a valid format.
 *
 ****************************************************************/
static bool parse_mac(uint8_t *mac, const char *str)
{
    for (int i = 0; i < 6; i++) {
        char *next = NULL;
        unsigned long val = strtoul(str, &next, 16);
        if (val > UINT8_MAX || next == NULL || (i == 5 && *next != '\0')
                || (i < 5 && (*next != ':' && *next != '-'))) {
            return false; /* parse error */
        }
        mac[i] = val;
        str = next + 1;
    }

    return true;
}


static int add_desc(uint64_t paddr, void *opaque)
{
    e1000_dqval_t dqval = 0;
    union rx_desc desc;

    desc.raw[0] = desc.raw[1] = 0;
    desc.rx_read_format.buffer_address = paddr;


    if(receive_free == DRIVER_RECEIVE_BUFFERS) {
        // This is serious error condition.
        // Printing debug information to help user!
    	//E1000_DEBUG("no space to add a new receive pbuf\n");
    	printf("no space to add a new receive pbuf [%"PRIu32"], [%"PRIu32"]\n",
                receive_free, receive_index);
        printf("%p\n%p\n%p\n", __builtin_return_address(0),
                __builtin_return_address(1), __builtin_return_address(2));
        abort();
    	/* FIXME: how can you return -1 as error here
    	 * when return type is unsigned?? */
    	return -1;
    }

    receive_ring[receive_index] = desc;
    receive_opaque[receive_index] = opaque;

    receive_index = (receive_index + 1) % DRIVER_RECEIVE_BUFFERS;
    dqval = e1000_dqval_val_insert(dqval, receive_index);
    e1000_rdt_wr(&e1000, 0, dqval);

    receive_free++;
    return 0;
}

static void setup_internal_memory(void)
{
    receive_opaque = calloc(sizeof(void *), DRIVER_RECEIVE_BUFFERS);
    assert(receive_opaque != NULL );
}


static errval_t rx_register_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{
    return add_desc(paddr, opaque);
}

static uint64_t rx_find_free_slot_count_fn(void)
{
    return DRIVER_RECEIVE_BUFFERS - receive_free;
}



/*****************************************************************
 * PCI init callback.
 *
 * Setup device, create receive ring and connect to Ethernet server.
 *
 ****************************************************************/
static void e1000_init_fn(struct device_mem *bar_info, int nr_allocated_bars)
{
    E1000_DEBUG("Starting hardware initialization.\n");
    e1000_hwinit(&e1000_device, bar_info, nr_allocated_bars, &transmit_ring,
                 &receive_ring, DRIVER_RECEIVE_BUFFERS, DRIVER_TRANSMIT_BUFFERS,
                 mac_address, user_mac_address, use_interrupt);
    E1000_DEBUG("Hardware initialization complete.\n");

    setup_internal_memory();

#ifndef LIBRARY
    ethersrv_init(global_service_name, assumed_queue_id, get_mac_address_fn,
		  NULL,
                  transmit_pbuf_list_fn,
                  find_tx_free_slot_count_fn,
                  handle_free_TX_slot_fn,
                  receive_buffer_size,
                  rx_register_buffer_fn,
                  rx_find_free_slot_count_fn);
#else
    ethernetif_backend_init(global_service_name, assumed_queue_id, get_mac_address_fn,
		  NULL,
                  transmit_pbuf_list_fn,
                  find_tx_free_slot_count_fn,
                  handle_free_TX_slot_fn,
                  receive_buffer_size,
                  rx_register_buffer_fn,
                  rx_find_free_slot_count_fn);
    
#endif

#if TRACE_ETHERSRV_MODE
    set_cond_termination(trace_conditional_termination);
#endif
}


/*****************************************************************
 * e1000 interrupt handler
 *
 ****************************************************************/
static void e1000_interrupt_handler_fn(void *arg)
{
    /* Read interrupt cause, this also acknowledges the interrupt */
    e1000_intreg_t icr = e1000_icr_rd(e1000_device.device);

#if TRACE_ETHERSRV_MODE
    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_NI_I, interrupt_counter);
#endif

//    printf("#### interrupt handler called: %"PRIu64"\n", interrupt_counter);
    ++interrupt_counter;

    if (e1000_intreg_lsc_extract(icr) != 0) {
        if (e1000_check_link_up(&e1000_device)) {
            e1000_auto_negotiate_link(&e1000_device);
        } else {
            E1000_DEBUG("Link status change to down.\n");
        }
    }

    if (e1000_intreg_rxt0_extract(icr) == 0) {
        return;
    }

#ifndef LIBRARY
    E1000_DEBUG("e1000 interrupt came in\n");
    handle_multiple_packets(MAX_ALLOWED_PKT_PER_ITERATION);
#else
    handle_multiple_packets(1);
#endif
}


/*****************************************************************
 * Print help and exit.
 *
 *
 ****************************************************************/
static void exit_help(const char *program)
{
    fprintf(stderr, "Usage: %s [options]\n", program);
    fprintf(stderr, "\taffinitymin=  Set RAM min affinity. When using this option it's mandatory to also set affinitymax.\n");
    fprintf(stderr, "\taffinitymax=  Set RAM max affinity. When using this option it's mandatory to also set affinitymin.\n");
    fprintf(stderr, "\tserivcename=  Set device driver service name.\n");
    fprintf(stderr, "\tbus=          Connect driver to device using this PCI bus.\n");
    fprintf(stderr, "\tfunction=     Connect driver to PCI device with function id.\n");
    fprintf(stderr, "\tdevice=       Connect driver to PCI device with type id.\n");
    fprintf(stderr, "\tdeviceid=     Connect driver to PCI device with device id.\n");
    fprintf(stderr, "\tmac=          Set device MAC address using MAC address format.\n");
    fprintf(stderr, "\tnoirq         Do not enable PCI bus IRQ, use device polling instead.\n");
    fprintf(stderr, "\t-h, --help    Print this help.\n");

    exit(2);
}


static errval_t trigger_existing_and_watch(const char *query,
        trigger_handler_fn event_handler, void *state,
        octopus_trigger_id_t *tid)
{
    errval_t error_code;
    char **names = NULL;
    char *output = NULL;
    char *record = NULL; // freed by cpu_change_event
    size_t len = 0;
    octopus_trigger_t t = oct_mktrigger(0, octopus_BINDING_EVENT,
                                        OCT_PERSIST | OCT_ON_SET | OCT_ON_DEL | OCT_ALWAYS_SET,
                                        event_handler, state);

    // Get current cores registered in system
    struct octopus_thc_client_binding_t *rpc = oct_get_thc_client();
    errval_t err = rpc->call_seq.get_names(rpc, query,
                                           t, &output, tid, &error_code);
    if (err_is_fail(err)) {
        goto out;
    }
    err = error_code;

    switch(err_no(err)) {
    case SYS_ERR_OK:
        err = oct_parse_names(output, &names, &len);
        if (err_is_fail(err)) {
            goto out;
        }

        for (size_t i=0; i < len; i++) {
            err = oct_get(&record, names[i]);

            switch (err_no(err)) {
            case SYS_ERR_OK:
                event_handler(OCT_ON_SET, record, state);
                break;

            case OCT_ERR_NO_RECORD:
                assert(record == NULL);
                break;

            default:
                DEBUG_ERR(err, "Unable to retrieve record for %s", names[i]);
                assert(record == NULL);
                break;
            }
        }
        break;
    case OCT_ERR_NO_RECORD:
        err = SYS_ERR_OK; // Overwrite (trigger is set)
        break;

    default:
        // Do nothing (wait for trigger)
        break;
    }

out:
    oct_free_names(names, len);
    free(output);

    return err;
}


static void check_possible_e1000_card(octopus_mode_t mode, char *record, void *st)
{
    errval_t err;
    if (mode & OCT_ON_SET) {

        int64_t pcibus, cls, subcls, dev, devid, fun, ven, prog_if;
        static char *format = "_ { bus: %d, class: %d, device: %d, "
                              "device_id: %d, function: %d, prog_if: %d, "
                              "subclass: %d, vendor: %d }";
        err = oct_read(record, format, &pcibus, &cls, &dev, &devid, &fun,
                       &prog_if, &subcls, &ven);

        if (err_is_ok(err)) {
            e1000_mac_type_t check_mac_type = e1000_get_mac_type(ven, devid);

            if (check_mac_type != e1000_undefined) {
                E1000_DEBUG("Using device. vendor: 0x%"PRIx64", device id: 0%"PRIx64".\n", ven, devid);
                mac_type = check_mac_type;
                bus = pcibus;
                class = cls;
                device = dev;
                deviceid = devid;
                function = fun;
                subclass = subcls;
                vendor = ven;
                program_interface = prog_if;
            } else {
                E1000_DEBUG("Unsupported device. vendor: 0x%"PRIx64", device id: 0x%"PRIx64".\n", ven, devid);
            }
        } else {
            E1000_DEBUG("Device record, %s.", record);
            DEBUG_ERR(err, "Unable to read it.\n");
        }

    }

    free(record);
}

/*****************************************************************
 * main.
 *
 *
 ****************************************************************/
#ifndef LIBRARY
int main(int argc, char **argv)
#else
int e1000n_driver_init(int argc, char **argv)
#endif
{
    char *service_name = 0;
    errval_t err;

    /** Parse command line arguments. */
    E1000_DEBUG("e1000 standalone driver started.\n");

    E1000_DEBUG("argc = %d\n", argc);
    for (int i = 1; i < argc; i++) {
        E1000_DEBUG("arg %d = %s\n", i, argv[i]);
        if (strncmp(argv[i], "affinitymin=", strlen("affinitymin=")) == 0) {
            minbase = atol(argv[i] + strlen("affinitymin="));
            E1000_DEBUG("minbase = %lu\n", minbase);
        } else if (strncmp(argv[i], "affinitymax=", strlen("affinitymax="))
                 == 0) {
            maxbase = atol(argv[i] + strlen("affinitymax="));
            E1000_DEBUG("maxbase = %lu\n", maxbase);
        } else if (strncmp(argv[i], "servicename=", strlen("servicename="))
                 == 0) {
            service_name = argv[i] + strlen("servicename=");
            E1000_DEBUG("service name = %s\n", service_name);
        } else if(strncmp(argv[i],"bus=",strlen("bus=")-1)==0) {
            bus = atol(argv[i] + strlen("bus="));
            E1000_DEBUG("bus = %ul\n", bus);
            use_force = true;
        } else if (strncmp(argv[i], "device=", strlen("device=")) == 0) {
            device = atol(argv[i] + strlen("device="));
            E1000_DEBUG("device = %ul\n", device);
            use_force = true;
        } else if (strncmp(argv[i], "function=", strlen("function=")) == 0) {
            function = atol(argv[i] + strlen("function="));
            E1000_DEBUG("function = %u\n", function);
            use_force = true;
        } else if (strncmp(argv[i], "deviceid=", strlen("deviceid=")) == 0) {
            deviceid = strtoul(argv[i] + strlen("deviceid="), NULL, 0);
            E1000_DEBUG("deviceid = %u\n", deviceid);
            use_force = true;
        } else if (strncmp(argv[i], "mac=", strlen("mac=")) == 0) {

            if (parse_mac(mac_address, argv[i] + strlen("mac="))) {
                user_mac_address = true;
                E1000_DEBUG("MAC= %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                            mac_address[0], mac_address[1], mac_address[2], mac_address[3], mac_address[4], mac_address[5]);
            } else {
                E1000_PRINT_ERROR("Error: Failed parsing MAC address '%s'.\n", argv[i]);
                exit(1);
            }
        } else if(strcmp(argv[i],"noirq")==0) {
            E1000_DEBUG("Driver working in polling mode.\n");
            use_interrupt = false;
        } else if (strcmp(argv[i], "-h") == 0 ||
                 strcmp(argv[i], "--help") == 0) {
            exit_help(argv[0]);
        } else {
            E1000_PRINT_ERROR("Error: unknown argument: %s.\n", argv[i]);
            //exit_help(argv[0]);
        }
    } // end for :

    if ((minbase != -1) && (maxbase != -1)) {
        E1000_DEBUG("set memory affinity [%lx, %lx]\n", minbase, maxbase);
        ram_set_affinity(minbase, maxbase);
    }

    if (service_name == 0) {
        service_name = (char *) malloc(sizeof("e1000") + 1);
        strncpy(service_name, "e1000", sizeof("e1000") + 1);
        E1000_DEBUG("Setting service name to %s\n", service_name);
    }

#ifndef CONFIG_QEMU_NETWORK
    // There is a bug which breaks the interrupt handling if driver runs
    // on core zero.  So, trying to avoid that situation
    if(use_interrupt) {
        if(disp_get_core_id() == 0) {
            USER_PANIC("ERROR: Can't run [%s] on core-0 with interrupt enabled, please choose different core\n",
                    disp_name());
            abort();
        }
    }
#endif

    E1000_DEBUG("Starting standalone driver.\n");

    /*
     * Scan for an supported, unclaimed PCI/PCIe card.
     */
    if (use_force == false) {
        err = oct_init();
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Unable to initialize octopus.");
        }

        // TODO(gz): We can remove this in case we make sure e1000 is
        // always started by kaluga, need to think of a proper way
        // to start device drivers
        octopus_trigger_id_t tid;
        trigger_existing_and_watch("r'hw\\.pci.device.*' {}",
                                   check_possible_e1000_card, NULL, &tid);
        if (mac_type == e1000_undefined) {
            E1000_DEBUG("No supported e1000 card found yet, wait for one to appear.\n");
        }
        while (mac_type == e1000_undefined) {
            messages_wait_and_handle_next();
        }
        oct_remove_trigger(tid);
        // end XXX
    } else {
        /* Check if forced device id and vendor is known to be supported. */
        mac_type = e1000_get_mac_type(vendor, deviceid);
    }

    /* Setup known device info */
    e1000_device.device = &e1000;
    e1000_device.mac_type = mac_type;
    e1000_device.device_id = deviceid;
    if (e1000_device.mac_type == e1000_82575 || e1000_device.mac_type == e1000_82576) {
        // These cards do not have a bsex reg entry
        // therefore, we can't use 16384 buffer size.
        // If we use smaller buffers than 2048 bytes the
        // eop bit on received packets might not be set in case the package
        // is biger than the receive buffer size and we don't handle these
        // cases currently.
        e1000_device.rx_bsize = bsize_2048;
    } else {
        e1000_device.rx_bsize = receive_buffer_size;
    }
    e1000_device.media_type = e1000_media_type_undefined;

    global_service_name = service_name;

    E1000_DEBUG("Connecting to PCI.\n");

    err = pci_client_connect();
    assert(err_is_ok(err));

    if (use_interrupt) {

        err = pci_register_driver_irq(e1000_init_fn, class, subclass, program_interface,
                                      vendor, deviceid, bus, device, function,
                                      e1000_interrupt_handler_fn, NULL);
        printf("########### Driver with interrupts ###########\n");
    } else {
        err = pci_register_driver_noirq(e1000_init_fn, class, subclass, program_interface,
                                        vendor, deviceid, bus, device, function);
        printf("########### Driver without interrupts ###########\n");
    }

    if (err_is_fail(err)) {
        E1000_PRINT_ERROR("Error: %u, pci_register_driver failed\n", (unsigned int)err);
        exit(err);
    }

    assert(err_is_ok(err));

    E1000_DEBUG("Registered driver.\n");

    e1000_print_link_status(&e1000_device);

#ifndef LIBRARY
    E1000_DEBUG("#### starting polling.\n");
    // FIXME: hack to force the driver in polling mode, as interrupts are
    // not reliably working
    use_interrupt = false;
    polling_loop();
#endif

    return 1;
}

