/**
 * \file
 * Barrelfish standard ethernet interface
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

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * This file is a skeleton for developing Ethernet network interface
 * drivers for lwIP. Add code to the low_level functions and do a
 * search-and-replace for the word "ethernetif" to replace it with
 * something that better describes your network interface.
 */

#include "lwip/opt.h"
#include "lwip/def.h"
#include "lwip/init.h"
#include "lwip/mem.h"
#include "lwip/pbuf.h"
#include "lwip/sys.h"
#include <lwip/stats.h>
#include <lwip/snmp.h>
#include "netif/etharp.h"
#include <assert.h>

#include <netif/bfeth.h>

#include <barrelfish/barrelfish.h>
#include <contmng/netbench.h>
#include <idc_barrelfish.h>
#include <mem_barrelfish.h>

// 10MBit interface
#define BFETH_NETSPEED  10000000

/* Define those to better describe your network interface. */
#define IFNAME0 'e'
#define IFNAME1 'n'


/**
 * Helper struct to hold private data used to operate your ethernet interface.
 * Keeping the ethernet address of the MAC in this struct is not necessary
 * as it is already kept in the struct netif.
 * But this is only an example, anyway...
 */
struct bfeth {
    struct eth_addr *ethaddr;
    /* Add whatever per-interface state that is needed here. */
};

/**
 * In this function, the hardware should be initialized.
 * Called from bfeth_init().
 *
 * @param netif the already initialized lwip network interface structure
 *        for this bfeth
 */
static void low_level_init(struct netif *netif)
{
    /* set MAC hardware address length */
    netif->hwaddr_len = ETHARP_HWADDR_LEN;

    /* set MAC hardware address */
    idc_get_mac_address(netif->hwaddr);

    /* maximum transfer unit */
    netif->mtu = 1500;

    /* device capabilities */
    netif->flags =
      NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_LINK_UP;
}

/**
 * This function should do the actual transmission of the packet. The packet is
 * contained in the pbuf that is passed to the function. This pbuf
 * might be chained.
 *
 * @param netif the lwip network interface structure for this bfeth
 * @param p the MAC packet to send (e.g. IP packet including MAC addresses and type)
 * @return ERR_OK if the packet could be sent
 *         an err_t value if the packet couldn't be sent
 *
 * @note Returning ERR_MEM here if a DMA queue of your MAC is full can lead to
 *       strange results. You might consider waiting for space in the DMA queue
 *       to become availale since the stack doesn't retry to send a packet
 *       dropped because of memory failure (except for the TCP timers).
 */

static err_t low_level_output(struct netif *netif, struct pbuf *p)
{
    uint8_t numpbuf = 0;
    //avoid that lwip frees this buffer before it has been sent by the network card.
    for (struct pbuf * tmpp = p; tmpp != 0; tmpp = tmpp->next) {
        pbuf_ref(tmpp);
        ++numpbuf;
    }
#if ETH_PAD_SIZE
    pbuf_header(p, -ETH_PAD_SIZE);      /* drop the padding word */
#endif
    //tell the network driver from which buffer and which offset to send the
    //new data.
    uint64_t ret = idc_send_packet_to_network_driver(p);

#if ETH_PAD_SIZE
    pbuf_header(p, ETH_PAD_SIZE);       /* reclaim the padding word */
#endif

    LINK_STATS_INC(link.xmit);

    if (ret == numpbuf) {
        return ERR_OK;
    }
    return ERR_IF;
}

uint64_t pbuf_free_tx_done_counter = 0;
static void bfeth_freeing_handler(struct pbuf *p)
{
    assert(p != 0);
    uint8_t freed_pbufs = pbuf_free(p);
    pbuf_free_tx_done_counter += freed_pbufs;
}

typedef void (*packetfilter_func_t) (struct pbuf *, struct netif *, uint64_t);
static packetfilter_func_t packetfilter = NULL;
void bfeth_register_packetfilter(packetfilter_func_t filter);

void bfeth_register_packetfilter(packetfilter_func_t filter)
{
    packetfilter = filter;
}



uint64_t pbuf_free_incoming_counter = 0;
/**
 * This function should be called when a packet is ready to be read
 * from the interface. It uses the function low_level_input() that
 * should handle the actual reception of bytes from the network
 * interface. Then the type of the received packet is determined and
 * the appropriate input function is called.
 *
 * @param netif the lwip network interface structure for this bfeth
 */
void
bfeth_input(struct netif *netif, uint64_t pbuf_id, uint64_t paddr, uint64_t len,
            uint64_t packet_len, struct pbuf *pp)
{
    struct bfeth *bfeth;
    struct eth_hdr *ethhdr;
    struct pbuf *p;

    bfeth = netif->state;

    //asq: low_level_input is not needed anymore, because p was preallocated
    //and filled with an arrived packet by the network card driver.
    //We only need to find the original vaddr of p according to the received
    //index.
    //We have to adjust the len and tot_len fields. The packet is
    //most probably shorter than pbuf's size.
    //LWIP is freeing the memory by looking at the type, not by the len or
    //tot_len fields, so that should be fine.

    //get vaddr of p and adjust the length according to the packet length.
    p = mem_barrelfish_get_pbuf(pbuf_id);
    //* Buffer has to be found
    assert(p != 0);

    assert(packet_len != 0);
    p->len = packet_len;
    p->tot_len = packet_len;
    ethhdr = p->payload;

    struct pbuf *replaced_pbuf = get_pbuf_for_packet();
    if (replaced_pbuf == NULL) {
       printf("%s:No free pbufs for replacement.  Assuming that packet is dropped\n", disp_name());
        USER_PANIC("ERROR: No more free pbufs, aborting\n");
        abort();
        replaced_pbuf = p;
//        printf("pbuf stats: total len = %"PRIu16", len = %"PRIu16", buf len = %"PRIu16", ref count = %"PRIu16", \n",
//                p->tot_len, p->len, p->buff_len, p->ref);
        replaced_pbuf->tot_len =  replaced_pbuf->buff_len;
        replaced_pbuf->len =  replaced_pbuf->buff_len;
        // Maybe I need to reset some pointers here!!
    } else { // Now doing  packet processing

        /* points to packet payload, which starts with an Ethernet header */

        switch (htons(ethhdr->type)) {
            /* IP or ARP packet? */
            case ETHTYPE_IP:
            case ETHTYPE_ARP:
#if PPPOE_SUPPORT
                /* PPPoE packet? */
            case ETHTYPE_PPPOEDISC:
            case ETHTYPE_PPPOE:
#endif                          /* PPPOE_SUPPORT */
                LWIP_DEBUGF(NETIF_DEBUG, ("bfeth_input: consuming the packet\n"));
                if (packetfilter != NULL) {
                    packetfilter(p, netif, pbuf_id);
                    return;
                } else {
                    /* full packet send to tcpip_thread to process */
                    assert(netif->input != NULL);
                    if (netif->input(p, netif) != ERR_OK) {
                        LWIP_DEBUGF(NETIF_DEBUG, ("bfeth_input: IP input error\n"));
                        ++pbuf_free_incoming_counter;
                        pbuf_free(p);
                        p = NULL;
                    }
                }
                break;

            default:
                LWIP_DEBUGF(NETIF_DEBUG,
                        ("unknown type %x!!!!!\n", htons(ethhdr->type)));
                ++pbuf_free_incoming_counter;
                pbuf_free(p);

                p = NULL;
                break;
        }

    }

    // Check if there is anything else that we should before sending back the
    // ack that we consumed packet.

    perform_lwip_work();

    //now we have consumed the preregistered pbuf containing a received packet
    //which was processed in this function. Therefore we have to register a new
    //free buffer for receiving packets. We can reuse the odl buffer's index
    //and the corresponding data structures (i.e. array entries)
    uint64_t ts = rdtsc();
    errval_t err = mem_barrelfish_replace_pbuf(replaced_pbuf);
    if (err != SYS_ERR_OK) {
        printf("Can't replace received pbuf in RX ring\n");
        pbuf_free(replaced_pbuf);
        USER_PANIC("Can't replace received pbuf in RX ring\n");
    }

    netbench_record_event_simple(nb, RE_PBUF_REPLACE, ts);
}

static void bfeth_input_handler(void *data, uint64_t pbuf_id, uint64_t paddr,
                                uint64_t len, uint64_t packet_len,
                                struct pbuf *p)
{
    bfeth_input((struct netif *) data, pbuf_id, paddr, len, packet_len, p);
}


/**
 * Should be called at the beginning of the program to set up the
 * network interface. It calls the function low_level_init() to do the
 * actual setup of the hardware.
 *
 * This function should be passed as a parameter to netif_add().
 *
 * @param netif the lwip network interface structure for this bfeth
 * @return ERR_OK if the loopif is initialized
 *         ERR_MEM if private data couldn't be allocated
 *         any other err_t on error
 */
err_t bfeth_init(struct netif *netif)
{
    struct bfeth *bfeth;

    LWIP_ASSERT("netif != NULL", (netif != NULL));

    bfeth = mem_malloc(sizeof(struct bfeth));
    if (bfeth == NULL) {
        LWIP_DEBUGF(NETIF_DEBUG, ("bfeth_init: out of memory\n"));
        return ERR_MEM;
    }
#if LWIP_NETIF_HOSTNAME
    /* Initialize interface hostname */
    netif->hostname = "lwip";
#endif                          /* LWIP_NETIF_HOSTNAME */

    /*
     * Initialize the snmp variables and counters inside the struct netif.
     * The last argument should be replaced with your link speed, in units
     * of bits per second.
     */
    NETIF_INIT_SNMP(netif, snmp_ifType_ethernet_csmacd, BFETH_NETSPEED);

    netif->state = bfeth;
    netif->name[0] = IFNAME0;
    netif->name[1] = IFNAME1;
    /* We directly use etharp_output() here to save a function call.
     * You can instead declare your own function an call etharp_output()
     * from it if you have to do some checks before sending (e.g. if link
     * is available...) */
    netif->output = etharp_output;
    netif->linkoutput = low_level_output;

    bfeth->ethaddr = (struct eth_addr *) &(netif->hwaddr[0]);

    /* initialize the hardware */
    low_level_init(netif);

    // register a callback to get notified of arrived packets
    idc_register_receive_callback(bfeth_input_handler, (void *) netif);

    //register a function which is called if a transmit descriptor can be freed
    //(which means the packet has been sent out of the network card and the buffer
    //is free now)
    idc_register_freeing_callback(bfeth_freeing_handler);

    return ERR_OK;
}
