/**
 * \file
 * \brief Realtek RTL8029(AS) driver.
 */

/*
 * Copyright (c) 2007, 2008, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Accessing register pages
 */
#include <stdio.h>
#include <string.h>
#include <inttypes.h>

/*
 *  Code to provide page address space access for Mackerel definitions
 */
static void page_select(uint8_t page);

#define PAGE_READ(_d,_a,_s,_p) (page_select(_p), mackerel_read_io_##_s(_d->base,_a))
#define PAGE_WRITE(_d,_a,_s,_p,_v) (page_select(_p), mackerel_write_io_##_s(_d->base,_a,_v))

#define rtl8029as_page0_read_8(d,a)     PAGE_READ(d,a,8,rtl8029as_ne2000p0)
#define rtl8029as_page0_write_8(d,a,v)  PAGE_WRITE(d,a,8,rtl8029as_ne2000p0,v)
#define rtl8029as_page0_read_16(d,a)    PAGE_READ(d,a,16,rtl8029as_ne2000p0)
#define rtl8029as_page0_write_16(d,a,v) PAGE_WRITE(d,a,16,rtl8029as_ne2000p0,v)

#define rtl8029as_page1_read_8(d,a)     PAGE_READ(d,a,8,rtl8029as_ne2000p1)
#define rtl8029as_page1_write_8(d,a,v)  PAGE_WRITE(d,a,8,rtl8029as_ne2000p1,v)

#define rtl8029as_p2p0_read_8(d,a)      PAGE_READ(d,a,8,rtl8029as_ne2000p2)
#define rtl8029as_p2p0_write_8(d,a,v)   PAGE_WRITE(d,a,8,rtl8029as_ne2000p0,v)

#define rtl8029as_rtlp_read_8(d,a)      PAGE_READ(d,a,8,rtl8029as_rtl8029as)
#define rtl8029as_rtlp_write_8(d,a,v)   PAGE_WRITE(d,a,8,rtl8029as_rtl8029as,v)
#define rtl8029as_rtlp_read_16(d,a)     PAGE_READ(d,a,16,rtl8029as_rtl8029as)
#define rtl8029as_rtlp_write_16(d,a,v)  PAGE_WRITE(d,a,16,rtl8029as_rtl8029as,v)
#define rtl8029as_rtlp_read_32(d,a)     PAGE_READ(d,a,32,rtl8029as_rtl8029as)
#define rtl8029as_rtlp_write_32(d,a,v)  PAGE_WRITE(d,a,32,rtl8029as_rtl8029as,v)

#include <net_queue_manager/net_queue_manager.h>
#include "rtl8029.h"
#include <dev/rtl8029as_dev.h>


/// The only instance of the RTL8029AS we're handling
static rtl8029as_t      rtl;

/// This buffers the card's MAC address upon card reset
static uint8_t          rtl8029_mac[6];

// Buffer registered by net_queue_mgr library
static uint8_t *packetbuf = NULL;
static void *packetbuf_opaque = NULL;

// the length of packet copied into packetbuf
static uint16_t packet_length;

static uint64_t assumed_queue_id = 0; // queue_id that will be initialized

/**
 * \brief Select an RTL8029(AS) register page.
 *
 * This selects one of the 4 RTL8029(AS) register pages.
 *
 * \param page  Page to select.
 */
static void page_select(uint8_t page)
{
    /// The currently selected register page
    static uint8_t current_page = -1;
    if (current_page != page) {
	rtl8029as_cr_t cr = rtl8029as_cr_default;
	cr = rtl8029as_cr_sta_insert(cr, 1);
	cr = rtl8029as_cr_rd_insert(cr, rtl8029as_acrdma);
	cr = rtl8029as_cr_ps_insert(cr, page);
	rtl8029as_cr_wr(&rtl, cr);
	current_page = page;
    }
}

/*
 * The RTL8029(AS) has 32K of memory, starting at address 0x4000.
 * That is page 0x40, as each page is 256 bytes in size. Memory is
 * going up to address 0xc000.
 */


static uint8_t curr_page = READ_START_PAGE;


/**
 * \brief Yield ASIC memory address from page number.
 *
 * \param page  Page number.
 *
 * \return Corresponding memory address.
 */
static inline uint16_t page_to_mem(uint8_t page)
{
    return page << 8;
}

/**
 * \brief Read from ASIC memory.
 *
 * \param dst           Pointer to buffer to copy data to. If NULL, the data is
 *                      thrown away.
 * \param src           Source address in ASIC memory to read from.
 * \param amount        Number of bytes to transfer.
 */
static void read_mem(uint8_t *dst, int src, int amount)
{
    int remain = amount % 4;
    uint32_t *d = (uint32_t *)dst;
    uint32_t val;
    int i;

    rtl8029as_rbcr_wr(&rtl, amount);    // Amount of bytes to transfer
    rtl8029as_rsar_wr(&rtl, src);       // Source in NIC mem

    // Start read
    rtl8029as_cr_t cr = rtl8029as_cr_default;
    cr = rtl8029as_cr_sta_insert(cr, 1);
    cr = rtl8029as_cr_rd_insert(cr, rtl8029as_rrd);
    rtl8029as_cr_wr(&rtl, cr);

    // Read PIO 32-bit
    for(i = 0; i < amount - remain; i += 4, d++) {
        *d = rtl8029as_rdma32_rd(&rtl);
    }

    // Read remaining bytes
    for(; i < amount; i++) {
        val = rtl8029as_rdma8_rd(&rtl);
        if (dst != NULL) {
            dst[i] = val;
        }
    }

    // Stop read
    cr = rtl8029as_cr_rd_insert(cr, rtl8029as_acrdma);
    rtl8029as_cr_wr(&rtl, cr);
}

/**
 * \brief Write packet to memory at a particular page.
 *
 * \param page          Destination start page in ASIC memory.
 * \param buffers       Descriptors for buffer chain
 * \param count         Number of buffers in chain
 * \param pkt_len       Length of packet to be sent next.
 */
static inline void write_page(uint8_t page, struct driver_buffer *buffers,
        size_t count, uint64_t pkt_len)
{
    uint64_t pbuf_len = 0;
    uint16_t dst = page_to_mem(page);

    RTL8029_DEBUG("write page\n");
    rtl8029as_rbcr_wr(&rtl, pkt_len);// Number of bytes to transfer
    rtl8029as_rsar_wr(&rtl, dst);       // Destination in NIC mem

    // Start write
    rtl8029as_cr_t cr = rtl8029as_cr_default;
    cr = rtl8029as_cr_sta_insert(cr, 1);
    cr = rtl8029as_cr_rd_insert(cr, rtl8029as_rwr);
    rtl8029as_cr_wr(&rtl, cr);

    for (int idx = 0; idx < count; idx++) {
        uint8_t *src = buffers[idx].va;
        pbuf_len = buffers[idx].len;

        uint32_t i = 0;

        // write bytes until we reach word alignment in the card's memory
        for (i = 0; dst % sizeof(uint32_t) != 0; i++, dst++) {
	    // RTL8029_DEBUG("sending byte %d\n", i);
            rtl8029as_rdma8_wr(&rtl, src[i]);
        }
	// RTL8029_DEBUG("sending %d %u for len %lu\n", i, dst, pbuf_len);
        // write 32-bit words until we don't have any whole words left
        for (; pbuf_len - i > sizeof(uint32_t);
             i += sizeof(uint32_t), dst += sizeof(uint32_t)) {
	    // RTL8029_DEBUG("sending word %d %u loc %p, data%u\n",
	    //                     i, dst, &src[i], src[i]);
            // RTL8029_DEBUG("base is 0x%x\n", rtl.base);
	    rtl8029as_rdma32_wr(&rtl, *(uint32_t*)&src[i]);
        }
	// RTL8029_DEBUG("done with loop 1\n");
        // Write remaining bytes
        for(; i < pbuf_len; i++, dst++) {
            rtl8029as_rdma8_wr(&rtl, src[i]);
        }
    } // end for : for each pbuf in the list (for single packet)

    // RTL8029_DEBUG("stopping the write\n");

    // Stop write
    cr = rtl8029as_cr_rd_insert(cr, rtl8029as_acrdma);
    rtl8029as_cr_wr(&rtl, cr);
    //RTL8029_DEBUG("finished writing page!\n");
}

/**
 * \brief Same as read_mem(), but with page granularity.
 *
 * \param dst           Pointer to buffer to copy data to.
 * \param src           Source start page in ASIC memory.
 * \param amount        Number of bytes to transfer.
 */
static inline void read_page(uint8_t *dst, uint8_t page, int amount)
{
    read_mem(dst, page_to_mem(page), amount);
}

/**
 * \brief Reset RTL8029(AS).
 */
static inline void rtl8029_reset(void)
{
    rtl8029as_reset_rd(&rtl);
}


// FIXME: This is just a placeholder function.  Should be removed
static uint64_t rtl_tx_slots_count_fn(void)
{
    return 1000; // RTL_TX_RING_SIZE;
}

/**
 * \brief Send Ethernet packet.
 *
 * The packet should be a complete Ethernet frame. Nothing is added
 * by the card or the driver.
 *
 */
static errval_t rtl8029_send_ethernet_packet_fn(struct driver_buffer *buffers,
                                                size_t                count)
{
    // Find the length of entire packet
    uint64_t pkt_len = 0;
    for (int idx = 0; idx < count; idx++) {
        pkt_len += buffers[idx].len;
    }

    // RTL8029_DEBUG("sending ethernet packet\n");
    assert(pkt_len <= WRITE_BUF_SIZE);

    // Write packet to ASIC memory
    write_page(WRITE_PAGE, buffers, count, pkt_len);
    // RTL8029_DEBUG("page written\n");

    // Set address & size
    rtl8029as_tpsr_wr(&rtl, WRITE_PAGE);
    rtl8029as_tbcr_wr(&rtl, pkt_len);
    // RTL8029_DEBUG("address set\n");


    // Initiate send
    rtl8029as_cr_t cr = rtl8029as_cr_default;
    cr = rtl8029as_cr_sta_insert(cr, 1);
    cr = rtl8029as_cr_txp_insert(cr, 1);
    cr = rtl8029as_cr_rd_insert(cr, rtl8029as_sp);
    rtl8029as_cr_wr(&rtl, cr);

    // Wait until done...
    while(rtl8029as_tsr_ptx_rdf(&rtl) == 0);

    // Tell the client we sent them!!!
    for (int idx = 0; idx < count; idx++) {
        handle_tx_done(buffers[idx].opaque);
    }

    return SYS_ERR_OK;
}


static void read_ring_buffer(uint8_t *dst, int src, int amount)
{
    int stopaddr = READ_STOP_PAGE << 8;

    if(src + amount < stopaddr) {
        // No ring-buffer wrap-around
        read_mem(dst, src, amount);
    } else {
        int size = stopaddr - src;

        // Read everything up to end of buffer
        read_mem(dst, src, size);
        // Read rest from (wrapped-around) start of buffer
        if (dst != NULL) {
            dst += size;
        }
        read_mem(dst, page_to_mem(READ_START_PAGE), amount - size);
    }
}

/**
 * \brief Receive Ethernet packet.
 *
 * Reads latest new packet from ASIC packet ring-buffer and calls
 * higher-level receive function to process packet now in main
 * memory.
 *
 * Assumes card is at register page 0.
 */
static void rtl8029_receive_packet(void)
{
    struct {
        rtl8029as_rsr_t rsr;
        uint8_t         next_page;
        uint16_t        length;
    } __attribute__ ((packed)) status;

    // Read packet status (first 4 bytes before Ethernet header)
    read_page((uint8_t *)&status, curr_page, sizeof(status));

    assert(rtl8029as_rsr_prx_extract(status.rsr) == 1);
    assert(status.length <= PACKET_SIZE);
    int pos = curr_page + ((status.length + 4 + 255) >> 8);
    assert(status.next_page == (pos >= READ_STOP_PAGE ? pos -
                (READ_BUF_SIZE >> 8) : pos));

    // Read packet
    packet_length = status.length - sizeof(status);
    // FIXME: Can we skip reading from the ring buffer if packetbuf == NULL? Or
    // does the card require us to read the data to continue working properly?
    // At the moment I chose the safe path and read the data.
    read_ring_buffer(packetbuf, page_to_mem(curr_page) + sizeof(status),
                     packet_length);

     RTL8029_DEBUG("................... Packet received (length = %d)\n",
                  status.length);

    // Update boundary
    curr_page = status.next_page;
    rtl8029as_bnry_wr(&rtl, curr_page);
}

/**
 * \brief rtl8029as IRQ handler
 *
 * This handler assumes the card is at page 0.
 *
 * The order of actions in this function is important. An interrupt
 * needs to be acknowledged to the card first, before reading packets
 * from the card. Otherwise a race between reading packets and newly
 * received packets arises and packet reception can be delayed
 * arbitrarily.
 */
static void rtl8029_handle_interrupt(void *arg)
{
//    thread_mutex_lock(&driver_lock);

    rtl8029as_irq_t isr = rtl8029as_isr_rd(&rtl);
    RTL8029_DEBUG("interrupt came in.\n");
    // 1. Acknowledge all interrupt causes
    /* rtl8029as_irq_t nisr = {
        .prx = 1,
        .ptx = 1,
        .rxe = 1,
        .txe = 1,
        .ovw = 1,
        .cnt = 1,
        .rdc = 1,
        .rst = 1
	}; */
    rtl8029as_isr_wr(&rtl, 0xff);

    // 2. Get card's current packet pointer
    uint8_t curr = rtl8029as_curr_rd(&rtl);

    // 3. Process current interrupt causes
    if(rtl8029as_irq_prx_extract(isr)) {
        // Read as many packets as possible
        while(curr_page != curr) {
            rtl8029_receive_packet();

        	assert(packet_length <= 1522);
        	assert(packet_length > 0);

            if (packetbuf != NULL) {
                struct driver_rx_buffer buf = { .opaque = packetbuf_opaque,
                    .len = packet_length };
                packetbuf = NULL;
                process_received_packet(&buf, 1, 0);
            }
        }
    }

//    thread_mutex_unlock(&driver_lock);
}


/**
 * \brief Initialize RTL8029(AS).
 *
 * \param net_card_address      Pointer to card's PCI configuration.
 *
 * \return 0 on success. Failure code otherwise.
 */
static int rtl8029_initialize_card(void)
{
//    thread_mutex_lock(&driver_lock);
    printf("Initializing RTL8029(AS)...\n");

    uint32_t cbio = RTL8029_IOBASE;
    uint16_t portbase = cbio & ~0x1;
    rtl8029as_initialize(&rtl, portbase);

    // Reset card
    rtl8029as_reset_rd(&rtl);

    // Identify rtl8029as
    if( rtl8029as_chipid_rd(&rtl,0) == 'P'
	&& rtl8029as_chipid_rd(&rtl,1) == 'C') {
        printf("rtl8029as identified\n");
    } else {
        printf("This is not a recognized rtl8029as: ID is %02x:%02x!\n",
	       rtl8029as_chipid_rd(&rtl,0),
	       rtl8029as_chipid_rd(&rtl,1) );
        return -1;
    }
    printf("RTL base is %d\n",rtl.base);

    // Read my MAC address
    for(int i=0; i < 6; i++) {
	rtl8029_mac[i] = rtl8029as_par_rd(&rtl,i);
    }
    printf("My MAC address: %02x:%02x:%02x:%02x:%02x:%02x\n",
           rtl8029_mac[0], rtl8029_mac[1], rtl8029_mac[2], rtl8029_mac[3],
           rtl8029_mac[4], rtl8029_mac[5]);

    // Start the card
    page_select(rtl8029as_ne2000p0);
    rtl8029as_cr_t cr = rtl8029as_cr_rd(&rtl);
    cr = rtl8029as_cr_stp_insert(cr, 0);
    rtl8029as_cr_wr(&rtl, cr);

    // Clear interrupt status register
    rtl8029as_irq_t isr = 0xff;
	/* Which implies:
        .prx = 1,
        .ptx = 1,
        .rxe = 1,
        .txe = 1,
        .ovw = 1,
        .cnt = 1,
        .rdc = 1,
        .rst = 1
	*/
    rtl8029as_isr_wr(&rtl, isr);

    /*
    // Register interrupt handler
    uint64_t badge = idc_handler_register(rtl8029_handle_interrupt);

    struct capref ep;
    if (endpoint_create(badge, &ep) != SYS_ERR_OK) {
        assert(!"endpoint_create failed");
        return -1;
    }
    if (irq_handle(cfit & 0xf, ep) != SYS_ERR_OK) {
        assert(!"Registering IRQ failed");
        return -1;
    }
*/
    // Set byte-wide PIO transfer
    rtl8029as_dcr_t dcr = rtl8029as_dcr_default;
    dcr = rtl8029as_dcr_wts_insert(dcr,0);
    rtl8029as_dcr_wr(&rtl, dcr);

    // Setup on-card receive ring-buffer
    rtl8029as_curr_wr(  &rtl, READ_START_PAGE);
    rtl8029as_pstart_wr(&rtl, READ_START_PAGE);
    rtl8029as_pstop_wr( &rtl, READ_STOP_PAGE);
    rtl8029as_bnry_wr(  &rtl, READ_START_PAGE);

    // Enable interrupts (IRQ handler assumes we're at page 0!)
    rtl8029as_irq_t imr = rtl8029as_irq_default;
    imr = rtl8029as_irq_prx_insert(imr,1);
    /*         .ptx = 1, */
    /*         .rdc = 1 */
    rtl8029as_imr_wr(&rtl, imr);

//    thread_mutex_unlock(&driver_lock);
    return 0;
}

static void get_mac_address_fn(uint8_t *mac)
{
    memcpy(mac, rtl8029_mac, 6);
}

static bool handle_free_TX_slot_fn(void)
{
    return false;
}

/**
 * Callback for net_queue_mgr library. Since we do PIO anyways, we only ever use
 * one buffer.
 */
static uint64_t find_rx_free_slot_count_fn(void)
{
    return (packetbuf == NULL);
}

/** Callback for net_queue_mgr library. */
static errval_t register_rx_buffer_fn(uint64_t paddr, void *vaddr, void *opaque)
{
    if (packetbuf != NULL) {
        return ETHERSRV_ERR_TOO_MANY_BUFFERS;
    }

    packetbuf = vaddr;
    packetbuf_opaque = opaque;
    return SYS_ERR_OK;
}

static void rtl8029_init(void)
{
	/* FIXME: use correct name, and make apps and netd
	 * work with multiple service names for driver. */
	char *service_name = "rtl8029";
	RTL8029_DEBUG("starting hardware init\n");
	rtl8029_initialize_card();
	/* FIXME: do hardware init*/
	RTL8029_DEBUG("Done with hardware init\n");

	ethersrv_init(service_name, assumed_queue_id, get_mac_address_fn, NULL,
            rtl8029_send_ethernet_packet_fn,
            rtl_tx_slots_count_fn, handle_free_TX_slot_fn,
            PACKET_SIZE, register_rx_buffer_fn, find_rx_free_slot_count_fn);
}

/**
 * \brief Initialize rtl8029 driver as legacy driver.
 *
 *
 */
static errval_t legacy_rtl8029_driver_init(void)
{
	/* FIXME: pci_client_connect returns int and not errval_t.
	 * So, change the pci_client_connect() */
    errval_t err = pci_client_connect();
    if (err_is_fail(err)) {
    	return err;
    }
    RTL8029_DEBUG("connected to pci\n");

    return pci_register_legacy_driver_irq(rtl8029_init, RTL8029_IOBASE,
						RTL8029_IOEND, RTL8029_IRQ,
						rtl8029_handle_interrupt, NULL);
}


//this functions polls all the client's channels as well as the transmit and
//receive descriptor rings
static void polling_loop(void)
{
    errval_t err;
    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
//        RTL8029_DEBUG("inside event dispatch\n");
/*        notify_client_next_free_tx();
*/
    }
}


int main(int argc, char *argv[])
{
	errval_t err;
    int i;
    RTL8029_DEBUG("Starting rtl8029 standalone driver.....\n");

    // Process commandline arguments
    for (i = 1; i < argc; i++) {
        ethersrv_argument(argv[i]);
    }
#ifdef CONFIG_QEMU_NETWORK
    printf("Starting RTL8029 for QEMU\n");
#else // CONFIG_QEMU_NETWORK
    printf("Starting RTL8029 for hardware\n");
#endif // CONFIG_QEMU_NETWORK
    // Initialize driver
    err = legacy_rtl8029_driver_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "legacy_rtl8029_driver_init\n");
    }
    RTL8029_DEBUG("registered driver\n");

    polling_loop(); //loop myself
}

