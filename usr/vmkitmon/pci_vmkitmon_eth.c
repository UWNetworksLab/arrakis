#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_vmkitmon_eth.h"
#include "guest.h"
#include "string.h"
#include "benchmark.h"
#include <pci/devids.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>

#define PCI_ETHERNET_IRQ 11
#define INVALID         0xffffffff
#define PCI_HEADER_MEM_ROM_BASE_REGISTER 0xc

#define DRIVER_RECEIVE_BUFFERS 256
#define DRIVER_TRANSMIT_BUFFER 256

int global_packet_in_count = 0;

static uint8_t guest_mac[] = { 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF}; //The mac address presented to virt. linux
static uint8_t host_mac[] = { 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xBF}; //The mac address presented to barrelfish
static uint64_t assumed_queue_id = 0;
static struct pci_device *the_pci_vmkitmon_eth;

// Data-structure to map sent buffer slots back to application slots
struct pbuf_desc {
    void *opaque;
};
static struct pbuf_desc pbuf_list_tx[DRIVER_TRANSMIT_BUFFER];

// tx_index = head, tx_bufptr = tail
static uint32_t ether_transmit_index = 0, ether_transmit_bufptr = 0;

// rx_index = head, rx_bufptr = tail
static uint32_t receive_index = 0, receive_bufptr = 0;
static uint32_t receive_free = 0;

struct rx_buffer {
    uint64_t paddr;
    void *vaddr;
    void *opaque;
};
static struct rx_buffer rx_buffer_ring[DRIVER_RECEIVE_BUFFERS];

static void generate_interrupt(struct pci_device *dev){
	struct pci_vmkitmon_eth * h = dev->state;
	h->mmio_register[PCI_VMKITMON_ETH_STATUS] |= PCI_VMKITMON_ETH_IRQST;
	lpc_pic_assert_irq(dev->lpc, dev->irq);
}

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
	VMKITMON_ETH_DEBUG("pci_vmkitmon_eth confspace_write addr: 0x%x, val: 0x%x\n", addr.d.doubleword, val);
	struct pci_vmkitmon_eth *h = dev->state;
	if(4 <= addr.d.doubleword && addr.d.doubleword <= 9 && val == INVALID){
		//caller wants to figure out bar size
		val = ~h->pci_device->bars[addr.d.doubleword - 4].bytes + 1; //~0x100000 + 1
		VMKITMON_ETH_DEBUG("pci_vmkitmon_eth writing bar %d size. 0x%x\n", addr.d.doubleword-4, val);
	}
	if(addr.d.doubleword == 1){
		//status register is clear by write 1
		val = (val & 0xffff) | (h->pci_header[addr.d.doubleword] & ~val)<<16;
	}
	h->pci_header[addr.d.doubleword] = val;
}

static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
	VMKITMON_ETH_DEBUG("confspace_read addr: 0x%x, ",addr.d.doubleword);
    struct pci_vmkitmon_eth *h = dev->state;

    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
    } else {
    	if(addr.d.doubleword == PCI_HEADER_MEM_ROM_BASE_REGISTER) {
    		//we dont support a rom, return always 0
    		*val = 0;
    	} else if(addr.d.doubleword < 0x40) {
			*val = h->pci_header[addr.d.doubleword];
		} else {
			*val = INVALID;
		}
    }

    VMKITMON_ETH_DEBUG(" val: 0x%x\n", *val);
}

static void get_mac_address_fn(uint8_t* mac)
{
    memcpy(mac, &host_mac, 6);
}

#if defined(VMKITMON_ETH_DEBUG_SWITCH)
static void dumpRegion(uint8_t *start){
	printf("-- dump starting from 0x%lx --\n", (uint64_t)start);
	for(int i=0; i<64;i++){
		printf("0x%04x: ", i*16);
		for(int j=0; j<16; j++){
			printf("%02x ", *( (start) + (i*16 + j)));
		}
		printf("\n");
	}
	printf("-- dump finished --\n");
}
#endif

static errval_t transmit_pbuf_list_fn(struct driver_buffer *buffers, size_t size) {
	struct pci_vmkitmon_eth *h = the_pci_vmkitmon_eth->state;
	int i;
	uint64_t paddr;

	record_packet_receive_from_bf();

    VMKITMON_ETH_DEBUG("transmit_pbuf_list_fn, no_pbufs: 0x%lx\n", size);

	struct pci_vmkitmon_eth_rxdesc * first_rx = (struct pci_vmkitmon_eth_rxdesc *) guest_to_host( h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR] );
	uint32_t rxdesc_len = h->mmio_register[PCI_VMKITMON_ETH_RXDESC_ADR];
    
	int transmitted = 0;
	for (i = 0; i < size; i++) {
        struct driver_buffer *buffer = &buffers[i];
        
		paddr = buffer->pa;
		VMKITMON_ETH_DEBUG("paddr: 0x%lx, len: 0x%lx\n", paddr, buffer->len);
#if defined(VMKITMON_ETH_DEBUG_SWITCH)
		dumpRegion(buffer->va);
#endif

		for(int j = 0; j <= rxdesc_len/sizeof(struct pci_vmkitmon_eth_rxdesc); j++){
			struct pci_vmkitmon_eth_rxdesc * cur_rx =first_rx + j;
			if(cur_rx->len == 0 && cur_rx->addr != 0){
				void *hv_addr = (void *)guest_to_host(cur_rx->addr);
				memcpy(hv_addr, buffer->va, buffer->len);
				cur_rx->len = buffer->len;
				VMKITMON_ETH_DEBUG("Used rxdesc %d to transmit\n", j);
				transmitted = 1;
                
                pbuf_list_tx[ether_transmit_index].opaque = buffer->opaque;
                ether_transmit_index = (ether_transmit_index + 1) % DRIVER_TRANSMIT_BUFFER;
                
				break;
			}
		}
	}

	if(transmitted){
		generate_interrupt(the_pci_vmkitmon_eth);
	}

	return SYS_ERR_OK;
}

static uint64_t find_tx_free_slot_count_fn(void) {
	//only called once at beginning & looks fine (returns 256)
    uint64_t nr_free;
    if (ether_transmit_index >= ether_transmit_bufptr) {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_index - ether_transmit_bufptr) %
                DRIVER_TRANSMIT_BUFFER);
    } else {
        nr_free = DRIVER_TRANSMIT_BUFFER -
            ((ether_transmit_bufptr - ether_transmit_index) %
            DRIVER_TRANSMIT_BUFFER);
    }
    //printf("find_tx_free_slot_count_fn: %lu, case1?: %d\n", nr_free, ether_transmit_index >= ether_transmit_bufptr);
    return nr_free;
}

static bool handle_free_TX_slot_fn(void) {
	VMKITMON_ETH_DEBUG("handle_free_TX_slot_fn\n");
    
    if(ether_transmit_bufptr == ether_transmit_index) {
        return false;
    }
    
    handle_tx_done(pbuf_list_tx[ether_transmit_bufptr].opaque);
    
    ether_transmit_bufptr = (ether_transmit_bufptr + 1) % DRIVER_TRANSMIT_BUFFER;
    //netbench_record_event_simple(bm, RE_TX_DONE, rdtsc());
	return true;
}

static void transmit_pending_packets(struct pci_vmkitmon_eth * h){
	VMKITMON_ETH_DEBUG("transmit_pending_packets\n");
	uint32_t rxdesc_len = h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN];
	struct pci_vmkitmon_eth_txdesc * first_tx = (struct pci_vmkitmon_eth_txdesc *) guest_to_host( h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR] );
	for(int i=0; i <= rxdesc_len/sizeof(struct pci_vmkitmon_eth_rxdesc); i++){
		struct pci_vmkitmon_eth_txdesc * cur_tx =first_tx + i;
		if(cur_tx->len != 0 && cur_tx->addr != 0){
			void *hv_addr = (void *)guest_to_host(cur_tx->addr);
			//VMKITMON_ETH_DEBUG("Sending packet at txdesc %d, addr: 0x%x, len: 0x%x\n", i, cur_tx->addr, cur_tx->len);
			//printf("Sending packet at txdesc %d, addr: 0x%x, len: 0x%x\n", i, cur_tx->addr, cur_tx->len);
            
            if(receive_free == 0) {
                VMKITMON_ETH_DEBUG("Could not deliver packet, no receive buffer available. Drop packet.\n");
                printf("Could not deliver packet, no receive buffer available. Drop packet.\n");
            } else {
                struct driver_rx_buffer buf;
                memcpy(rx_buffer_ring[receive_bufptr].vaddr, hv_addr, cur_tx->len);
                //printf("got packet with len: %d\n",cur_tx->len);
                if(cur_tx->len == 185){
                	print_bench_stats();
                } else {
					record_packet_transmit_to_bf();
                }
                buf.opaque = rx_buffer_ring[receive_bufptr].opaque;
                buf.len = cur_tx->len;
                process_received_packet(&buf, 1, 0);
                /*
                if(*(unsigned char *)hv_addr == 0xaa) {
                    //printf("packet %d delivered to barrelfish\n", ++global_packet_in_count);
                    if(0) dumpRegion(hv_addr);
                    unsigned char *xid = hv_addr + 42;
                    if(*xid == 0 && *(xid + 1) == 0){
						printf("XID: 0x%02x%02x%02x%02x\n", *xid, *(xid + 1), *(xid + 2), *(xid + 3));
                    	//dumpRegion(hv_addr);
                    }
                } */
                receive_bufptr = (receive_bufptr + 1) % DRIVER_RECEIVE_BUFFERS;
                --receive_free;
            }
			memset(hv_addr, 0xBF, cur_tx->len);
            cur_tx->len = 0;
		}
	}
}

static errval_t rx_register_buffer_fn(uint64_t paddr, void *vaddr, void *opaque) {
    VMKITMON_ETH_DEBUG("rx_register_buffer_fn called\n");
    
    rx_buffer_ring[receive_index].paddr = paddr;
    rx_buffer_ring[receive_index].vaddr = vaddr;
    rx_buffer_ring[receive_index].opaque = opaque;
    
    receive_index = (receive_index + 1) % DRIVER_RECEIVE_BUFFERS;
    receive_free++;
    return SYS_ERR_OK;
}

static uint64_t rx_find_free_slot_count_fn(void) {
	uint64_t nr_free;
	if (receive_index >= receive_bufptr) {
		nr_free = DRIVER_RECEIVE_BUFFERS -
			((receive_index - receive_bufptr) %
				DRIVER_RECEIVE_BUFFERS);
	} else {
		nr_free = DRIVER_RECEIVE_BUFFERS -
			((receive_bufptr - receive_index) %
			  DRIVER_RECEIVE_BUFFERS);
	}
	VMKITMON_ETH_DEBUG("rx_find_free_slot_count_fn called, returning %lu\n", nr_free);
    return nr_free;
}

static void mem_write(struct pci_device *dev, uint32_t addr, int bar, uint32_t val){
	struct pci_vmkitmon_eth *h = dev->state;
	VMKITMON_ETH_DEBUG("mem_write addr: 0x%x,  bar: %d, val: 0x%x, irq: %d\n",addr, bar, val, dev->irq );
	switch(addr) {
	case PCI_VMKITMON_ETH_STATUS:
		break;
	case PCI_VMKITMON_ETH_CONTROL:
		if( val & PCI_VMKITMON_ETH_RSTIRQ )
			h->mmio_register[PCI_VMKITMON_ETH_STATUS] &= ~PCI_VMKITMON_ETH_IRQST;
		if( val & PCI_VMKITMON_ETH_TXMIT ) {
			VMKITMON_ETH_DEBUG("Transmitting packet. guest-phys packet base address: 0x%x, packet-len: 0x%x\n",h->mmio_register[PCI_VMKITMON_ETH_TXDESC_ADR], h->mmio_register[PCI_VMKITMON_ETH_TXDESC_LEN]);
			transmit_pending_packets(h);
        }
		if( val & PCI_VMKITMON_ETH_IFUP) {
			VMKITMON_ETH_DEBUG("Interface up, registering\n");
			// register to queue_manager
			ethersrv_init("vmkitmon_eth", 
                          assumed_queue_id, 
                          get_mac_address_fn,
                          NULL,
                          transmit_pbuf_list_fn,
                          find_tx_free_slot_count_fn,
                          handle_free_TX_slot_fn,
                          2048, //                      rx_buffer_size,
                          rx_register_buffer_fn,
                          rx_find_free_slot_count_fn
                          );

		}
		break;
	default:
		h->mmio_register[addr] = val;
		break;
	}
}

static void mem_read(struct pci_device *dev, uint32_t addr, int bar, uint32_t *val){
	struct pci_vmkitmon_eth *h = (struct pci_vmkitmon_eth *) dev->state;
	if(addr != 0) VMKITMON_ETH_DEBUG("mem_read addr: 0x%x,  bar: %d, asserting irq: %d\n",addr, bar, dev->irq);
	switch(addr){
	case PCI_VMKITMON_ETH_MAC_LOW:
		memcpy(val, guest_mac, 4);
		break;
	case PCI_VMKITMON_ETH_MAC_HIGH:
		memcpy(val, guest_mac+4, 2);
		break;
	default:
		*val = h->mmio_register[addr];
	}
}


struct pci_device *pci_vmkitmon_eth_new(struct lpc *lpc, struct guest *g) {
	struct pci_device *dev = calloc(1, sizeof(struct pci_device));
	struct pci_vmkitmon_eth *host = calloc(1, sizeof(struct pci_vmkitmon_eth));
	host->pci_device = dev;

	//initialize device
	dev->confspace_write = confspace_write;
	dev->confspace_read = confspace_read;
	dev->mem_read = mem_read;
	dev->mem_write = mem_write;
	dev->state = host;
	dev->irq = PCI_ETHERNET_IRQ;
	dev->lpc = lpc;

	pci_hdr0_mem_t *ph = &host->ph;
	pci_hdr0_mem_initialize(ph, (mackerel_addr_t) host->pci_header);

	pci_hdr0_mem_vendor_id_wr(ph, PCI_VENDOR_FISH);
	pci_hdr0_mem_device_id_wr(ph, PCI_VMKITMON_ETH_DEVID);
	pci_hdr0_mem_class_code_clss_wrf(ph, PCI_CLASS_ETHERNET);

	//The next line could be: pci_hdr0_mem_int_line_wr(ph, PCI_ETHERNET_IRQ);
	//but the register is defined read only...
	host->pci_header[0xf] = 1<<8 | PCI_ETHERNET_IRQ;


	//Figure out a nice address, for the moment, make sure you dont go over 0xce000000
	// and stay close beyond (thats the point where the ixgbe is mapped).
	host->mem_guest_paddr = 0xcb000000;
	dev->bars[0].paddr = host->mem_guest_paddr;
	dev->bars[0].bytes = 0x100000; //1 MB

	//Write BAR0 into pci header
	pci_hdr0_mem_bars_wr(ph, 0, host->mem_guest_paddr);

	the_pci_vmkitmon_eth = dev;
	return dev;
}
