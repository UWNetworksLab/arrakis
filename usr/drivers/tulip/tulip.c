/** \file
 * \brief DEC Tulip ethernet driver
 *
 * This file is a driver for the Tulip Ethernet controller
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <dev/tulip_dev.h>
#include <pci/pci.h>
#include <net_queue_manager/net_queue_manager.h>
#include <ipv4/lwip/inet.h>

#include "tulip.h"

#define BAR_OFFSET 4
#define PCI_CONFIG_HDR_CFIT     0x3c

#define REGISTER_SIZE uint32_t
REGISTER_SIZE * volatile pciconfig;
// XXX Should use Mackerel PCI:  static struct pci_hdr0_t config;

static struct tulip_t csrs;
static uint8_t mac_address[6];

#define BYTES_PER_FRAME      1536

// XXX PBAR FIXME!
// XXX PBAR Buffer allocation code doesn't deal with >1 page properly!
#define RX_FRAGMENTS  1
#define TX_FRAGMENTS  1

#define RX_BUFSIZE 4096 // (RX_FRAGMENTS * (tulip_TDES_size+BYTES_PER_FRAME))
#define TX_BUFSIZE 4096 // (TX_FRAGMENTS * (tulip_TDES_size+BYTES_PER_FRAME))

#define DESCRIPTOR_OFFSET(d) ((d)*tulip_TDES_size)
#define BUFFER_OFFSET(b,n) (((n)*tulip_TDES_size + (b)*BYTES_PER_FRAME))

struct capref rxcap, txcap;
static uint8_t *volatile rxbufs;
static uint8_t *volatile txbufs;

#define RXDESC_AT(_o) ((tulip_RDES_t)(rxbufs + ((_o)*tulip_RDES_size)))
#define TXDESC_AT(_o) ((tulip_TDES_t)(txbufs + ((_o)*tulip_TDES_size)))

#ifdef TULIP_TU_DEBUG
static void print_csrs(void)
{
    char buf[4096];
    if (tulip_pr(buf, 4095, &csrs) < 4095) {
	printf("Not enough buffer to print registers: edit %s line %d", 
	       __FILE__, __LINE__ -2);
    } else {
	printf("%s\n", buf);
    }
}
static void dump_pkt(uint8_t *pkt, uint32_t len)
{
    for (int i = 0; i < len; i++) {
        printf("%02x", pkt[i]);
    }
    printf("\n");
}
#endif

//
// SROM related methods
//
static void delay(int kernel_ticks)
{
    // kernel ticks in units of 100ns
    // spin until delay elapsed
}

static uint8_t srom_read_preamble[] = {
    0x01, 0x31, 0x57, 0x57, 0x51, 0x31
};

static uint16_t srom_read16(uint32_t addr, uint32_t addrBits)
{
    int i;
    tulip_CSR9_t csr9 = tulip_CSR9_initial;
    csr9 = tulip_CSR9_DATA_insert(csr9, 0);
    csr9 = tulip_CSR9_SR_insert(csr9, 1);
    csr9 = tulip_CSR9_RD_insert(csr9, 1);

    // This is taken from section 7-8 in 21140A reference manual. We
    // deliberately make all delays as 3 ticks since we don't have
    // sufficient resolution and don't care about 300ns vs 150ns for
    // the SROM accesses.

    //uint32_t srom_base_cmd = 0x4800;

    // Starting byte address to starting word address
    addr >>= 1;

    for (i = 0; i < sizeof(srom_read_preamble); i++)
    {
        uint8_t b = srom_read_preamble[i];
	// srom_base_cmd | (uint32_t)(b >> 4));    delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, b >> 4);
        tulip_CSR9_wr(&csrs, csr9); 
	// srom_base_cmd | (uint32_t)(b & 0x0f));  delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, b & 0x0f);
        tulip_CSR9_wr(&csrs, csr9); 
    }

    // Write address to be read
    for (i = (int)addrBits - 1; i >= 0; --i)
    {
        uint32_t bit = (addr >> i) & 0x01;
        bit <<= 2;
        
	csr9 = tulip_CSR9_DATA_insert(csr9, bit | 0x01);
        tulip_CSR9_wr(&csrs, csr9); // srom_base_cmd | bit | 0x01);        
        delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, bit | 0x03);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | bit | 0x03);        
        delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, bit | 0x01);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | bit | 0x01);        
        delay(3);
    }

    // Get lsb
    uint32_t r = 0;
    for (i = 7; i >= 0; --i)
    {
	csr9 = tulip_CSR9_DATA_insert(csr9, 0x03);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x03);              
        delay(3);
        r |= ((tulip_CSR9_DATA_rdf(&csrs) & 0x08) >> 3) << i;         
        delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, 0x01);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x01);              
        delay(3);
    }

    // Get msb
    for (i = 15; i >= 8; --i)
    {
	csr9 = tulip_CSR9_DATA_insert(csr9, 0x03);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x03);              
        delay(3);
        r |= ((tulip_CSR9_DATA_rdf(&csrs) & 0x08) >> 3) << i;         
        delay(3);
	csr9 = tulip_CSR9_DATA_insert(csr9, 0x01);
        tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd | 0x01);              
        delay(3);
    }

    // Finish up
    csr9 = tulip_CSR9_DATA_insert(csr9, 0x00);
    tulip_CSR9_wr(&csrs, csr9); //srom_base_cmd);                         
    delay(3);

    return (uint16_t)r;
}

static int get_srom_width(void)
{
    for (uint32_t i = 6; i < 13; i++)
    {
        uint16_t w = srom_read16(18u, i);
        if (w == 0)
        {
            return i - 1;
        }
    }
    return 6;
}


static uint8_t *read_srom(void)
{
    int srom_width = get_srom_width();
    TU_DEBUG("srom_width=%d\n", srom_width);
    
    int len = 2 << (int)srom_width;
    TU_DEBUG("malloc %d\n", len);
    //breakpoint();
    uint8_t *b = malloc(len);
    TU_DEBUG("done %p\n", b);
    for (int i = 0; i < len; i += 2)
    {
        uint16_t w = srom_read16(i, srom_width);
        b[i]     = (uint8_t) (w >> 8);
        b[i + 1] = (uint8_t) (w & 0xff);
        if (0) TU_DEBUG("%02x%02x", b[i], b[i+1]);
    }
    TU_DEBUG("\n");
    return b;
}

static void *contig_alloc(size_t bufsize, lpaddr_t *pa)
{
    struct capref frame;
    errval_t r;
    void *va;

    // Allocate
    r = frame_alloc(&frame, bufsize, &bufsize);
    if (err_is_fail(r)) {
        TU_DEBUG(r, "frame_alloc");
        return NULL;
    }
    
    r = vspace_map_one_frame_attr(&va, bufsize, frame, 
				  VREGION_FLAGS_READ_WRITE, NULL, NULL);
    if (err_is_fail(r)) {
        TU_DEBUG(r, "vspace_map_one_frame_attr");
        return NULL;
    }
    return va;
}


static void init_chains(int rxFragments, int txFragments)
{
    // struct capref frame_ci;
    /// int rc;
    int i;
    lpaddr_t pa = 0;

    TU_DEBUG("\nXXXXXXXXXXXXXXXXXXXXXXXX\n");
    TU_DEBUG("InitChains(rx = %d, tx = %d)\n",
                     rxFragments, txFragments);

    // Turn off receive and transmit
    tulip_CSR6_t mode = tulip_CSR6_rd(&csrs);
    mode = tulip_CSR6_SR_insert(mode, 0);
    mode = tulip_CSR6_ST_insert(mode, 0);
    //mode &= ~(CSR6.SR | CSR6.ST);
    tulip_CSR6_wr(&csrs, mode);

    // XXX PBAR This allocation code doesn't deal with >1 page properly!
    TU_DEBUG("RX_BUFSIZE %x\n", RX_BUFSIZE);
    rxbufs = contig_alloc(BASE_PAGE_SIZE /*RX_BUFSIZE*/, &pa);
    TU_DEBUG("RXBUFS @ %p\n", rxbufs);

    TU_DEBUG("PA=%lx\n", pa);

    for( i=0; i < RX_FRAGMENTS; i++) {
	tulip_RDES_t des = RXDESC_AT(i);
	memset(des, 0, tulip_RDES_size);
	tulip_RDES_OWN_insert(des, 0);
	tulip_RDES_RCH_insert(des, 1);
	tulip_RDES_RBS1_insert(des, BYTES_PER_FRAME);
	tulip_RDES_RBA1_insert(des, pa + BUFFER_OFFSET(i, RX_FRAGMENTS));
	tulip_RDES_RBA2_insert(des, pa + DESCRIPTOR_OFFSET((i + 1) % RX_FRAGMENTS));
    }
    
    tulip_CSR3_wr(&csrs, (uint32_t)pa);
    tulip_CSR2_wr(&csrs, 1);   // Receive poll demand

    txbufs = contig_alloc(BASE_PAGE_SIZE /*TX_BUFSIZE*/, &pa);
    TU_DEBUG("TXBUFS @ %p\n", txbufs);

    TU_DEBUG("PA=%lx\n", pa);

    for( i=0; i < TX_FRAGMENTS; i++) {
	tulip_TDES_t des = TXDESC_AT(i);
	memset(des, 0, tulip_TDES_size);
	tulip_TDES_OWN_insert(des, 0);
	tulip_TDES_FS_insert(des, 1);
	tulip_TDES_LS_insert(des, 1);
	tulip_TDES_TCH_insert(des, 1);
	tulip_TDES_TBS1_insert(des, BYTES_PER_FRAME);
	tulip_TDES_TBA1_insert(des, pa + BUFFER_OFFSET(i, TX_FRAGMENTS));
	tulip_TDES_TBA2_insert(des, pa + DESCRIPTOR_OFFSET((i + 1) % TX_FRAGMENTS));
    }
    tulip_CSR4_wr(&csrs, (uint32_t)pa);
    tulip_CSR1_wr(&csrs, 1);    // Transmit poll demand

}

static void start_io(void)
{
    tulip_CSR6_t csr6 = tulip_CSR6_initial;
    csr6 = tulip_CSR6_HBD_insert(csr6, 1);
    csr6 = tulip_CSR6_PS_insert(csr6, 1);
    csr6 = tulip_CSR6_TR_insert(csr6, 3);
    csr6 = tulip_CSR6_ST_insert(csr6, 1);
    csr6 = tulip_CSR6_SR_insert(csr6, 1);

    TU_DEBUG("start_io\n");
    
    //csr6.Write32(CSR6.MBO | CSR6.HBD | CSR6.PS | (3u << CSR6.TR_ROLL) |
    //             CSR6.ST);
    
    //WriteStandardSetupFrame();

    // Write CSR6 to start receive and transmit processes
    //
    // This is taken from the state remaining after pxeboot.
    // == 100Mb/s MII/SYM (table 3-43 entry 1000)
    //csr6.Write32(CSR6.MBO | CSR6.HBD | CSR6.PS | (3u << CSR6.TR_ROLL) |
    //             CSR6.ST | CSR6.SR);
    tulip_CSR6_wr(&csrs, csr6);
    
    //TU_DEBUG("Programmed CSR values...\n");
    //print_csrs();

}


static void enable_irq(void)
{
    // Enable interrupts
    TU_DEBUG("enable_irq\n");
    tulip_CSR7_t csr7 = tulip_CSR7_initial;
    csr7 = tulip_CSR7_RI_insert(csr7, 1);
    csr7 = tulip_CSR7_TI_insert(csr7, 1);
    csr7 = tulip_CSR7_AI_insert(csr7, 1);
    tulip_CSR7_wr(&csrs, csr7);

}

/**
 * \brief Return MAC address.
 *
 * \param mac   Pointer to 6 byte array to hold MAC address.
 */
static void ethernet_get_mac_address(uint8_t *mac)
{
    TU_DEBUG("tulip: get_mac_address\n");
    memcpy(mac, mac_address, 6);
}

/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 * This is currently a very basic polled-mode transmit which waits until
 * the packet has been sent on the wire.
 *
 * \param packet        Pointer to packet buffer.
 * \param size          Size in bytes of packet.
 */
static errval_t ethernet_send_packets(struct client_closure* cl)
{
    assert("ethernet_send_packet for tulip is not yet (re)implemented");
#if 0
    // What is all this?  The next three lines pull information out of
    // the client closure, and are part of the *new* networking design
    // (as of 3/2013). 
    struct shared_pool_private *spp = cl->spp_ptr;
    struct slot_data *sld = &spp->sp->slot_list[cl->tx_index].d;
    uint64_t rtpbuf = sld->no_pbufs;

    // This is the legacy code, which dealt directly with pbufs.  This
    // needs to be converted. 
    TU_DEBUG("tulip: send_packet %d called\n", p->tot_len);
    tulip_TDES_t volatile tdes = TXDESC_AT(0);
    uint8_t * volatile txpkt = txbufs + BUFFER_OFFSET(0, 1);
    for (; p != NULL; p = p->next) {
        memcpy(txpkt, p->payload, p->len);
        txpkt += p->len;
    }
    //tulip_TDES_IC_insert(tdes, 1);
    tulip_TDES_LS_insert(tdes, 1);
    tulip_TDES_TCH_insert(tdes, 1);
    tulip_TDES_TBS1_insert(tdes, p->tot_len);
    tulip_TDES_OWN_insert(tdes, 1);
    tulip_CSR1_wr(&csrs, 1);    // Transmit poll demand
    while (tulip_TDES_OWN_extract(tdes)) {
#ifdef TULIP_TU_DEBUG
	char buf[1024];
	tulip_TDES_prtval(buf, 1023, tdes);
	TU_DEBUG("Transmit: %s\n", buf);
#endif // TULIP_TU_DEBUG
        thread_yield();
    }
    TU_DEBUG("tulip: send complete\n");
#endif 
    return SYS_ERR_OK;
}

/**
 * \brief Receive Ethernet packet.
 *
 */
static uint16_t ethernet_get_next_packet(uint8_t *packet)
{
    tulip_RDES_t volatile rdes0 = RXDESC_AT(0);
    uint32_t len = tulip_RDES_FL_extract(rdes0);
    uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
    TU_DEBUG("tulip: get_next_packet %d\n", len);
    //dump_pkt(rxpkt, len);
    memcpy(packet, rxpkt, len);
    // Give the buffer back to the NIC
    tulip_RDES_OWN_insert(rdes0, 1);
    //tulip_CSR2_wr(&csrs, 1);    // receive poll demand (just in case)
    return len;
}

/**
 * \brief Return the number of free TX slots
 *
 */
static uint64_t ethernet_tx_slots_count(void)
{
    return 1; // Probably not what you want for a real application!
}

static bool ethernet_handle_free_tx_slot(void)
{
    TU_DEBUG("tulip: handle_free_tx_slot not implemented.\n");
    return false; // FIXME!
}

static void tulip_handle_interrupt(void *arg)
{
    tulip_CSR5_t csr5 = tulip_CSR5_rd(&csrs);
    TU_DEBUG("tulip: interrupt\n");
    do {
        // Clear interrupts on card (interrupt bits are "write 1 to clear")
        tulip_CSR5_wr(&csrs, csr5);
        //print_csrs();
	
        if (tulip_CSR5_RI_extract(csr5)) {
            TU_DEBUG("RI\n");
	    tulip_RDES_t volatile rdes0 = RXDESC_AT(0);
            if (tulip_RDES_OWN_extract(rdes0) == 0) {

                uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
#if 0
                dump_pkt(rxpkt, tulip_RDES_FL_extract(rdes0));
#endif            
        	/* Ensures that netd is up and running */
        	if(waiting_for_netd()){
		    TU_DEBUG("still waiting for netd to reg buf\n");
		    return;
        	}
		ethernet_get_next_packet(rxpkt);
	    }
	}
	if (tulip_CSR5_TI_extract(csr5)) {
	    TU_DEBUG("TI\n");
        }
        
        csr5 = tulip_CSR5_rd(&csrs);
    } while(tulip_CSR5_NIS_extract(csr5));    
    
    //print_csrs();
}


/**
 * \brief Initialize network interface.
 *
 */
static void tulip_init(void)
{
    uint8_t * srom;
    
    TU_DEBUG("tulip: Initialize() called\n");
    
#ifdef TULIP_TU_DEBUG
    // Dump config registers
    TU_DEBUG("Initial CSR values...\n");
    print_csrs(void)();
#endif

    TU_DEBUG("Reset...\n");
    tulip_CSR0_SWR_wrf(&csrs, 1);

#ifdef TULIP_TU_DEBUG
    TU_DEBUG("New CSR values...\n");
    print_csrs();
#endif

    TU_DEBUG("Read SROM...\n");
    srom = read_srom();
    memcpy(mac_address, srom+20, 6);
    TU_DEBUG("MAC Address: %02x:%02x:%02x:%02x:%02x:%02x\n",
        mac_address[0], mac_address[1], mac_address[2], 
        mac_address[3], mac_address[4], mac_address[5]);

    // Build the transmit and receive descriptors
    init_chains(1, 1);

    // Enable IRQs
    enable_irq();

    // Clear interrupts
    tulip_CSR5_wr(&csrs, tulip_CSR5_rd(&csrs));

    // Start RX and TX DMA engines
    start_io();

#if 0
    // Poll for receive packets
    tulip_RDES_t volatile rdes0 = RXDESC_AT(0);
    uint8_t * volatile rxpkt = rxbufs + BUFFER_OFFSET(0, 1);
    memset(rxpkt, 0xaa, BYTES_PER_FRAME);

    while(1) {
        if (tulip_RDES_OWN_extract(rdes0) == 0) {
            uint32_t len = tulip_RDES_FL_extract(rdes0);
            TU_DEBUG("Got a packet len %d!\n", len);
            dump_pkt(rxpkt, len);
            print_csrs();
            // Give the buffer back to the NIC
            tulip_RDES_OWN_insert(rdes0, 1);

            // Clear pending interrupts
            tulip_CSR5_wr(&csrs, tulip_CSR5_rd(&csrs));
            TU_DEBUG("CSR5=%x\n", tulip_CSR5_rd(&csrs));
        }
        thread_yield();
        
    }
#endif
    ethersrv_init("tulip", 
		  0, // assumed queue id
		  ethernet_get_mac_address,
		  ethernet_send_packets,
		  ethernet_tx_slots_count, 
		  ethernet_handle_free_tx_slot);
}


/**
 * \brief Initialize tulip as a legacy driver.
 *
 *
 */
static errval_t legacy_tulip_driver_init(void)
{
	/* FIXME: pci_client_connect returns int and not errval_t.
	 * So, change the pci_client_connect() */
    errval_t err = pci_client_connect();
    if (err_is_fail(err)) {
	return err;
    }
    TU_DEBUG("connected to pci\n");

    return pci_register_legacy_driver_irq(tulip_init, 
					  TULIP_PORTBASE,
					  TULIP_PORTEND, 
					  TULIP_IRQ,
					  tulip_handle_interrupt, 
					  NULL);
}


int main(int argc, char *argv[])
{
    struct waitset *ws = get_default_waitset();
    errval_t err;

    TU_DEBUG("Starting tulip standalone driver.....\n");
    // Initialize driver
    err = legacy_tulip_driver_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "legacy_tulip_driver_init\n");
    }
    TU_DEBUG("registered driver\n");

    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }
}

