/**
 * \file Pass through device which connects a physical intel ixgbe pci network adapter to the virtual machine
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>

#include "vmkitmon.h"
#include "pci.h"
#include "pci_devices.h"
#include "pci_ethernet.h"
#include "benchmark.h"
#include <pci/pci.h>
#include <arch/x86/barrelfish/iocap_arch.h>
#include <dev/e10k_dev.h>
#include <dev/e10k_q_dev.h>

#define INVALID         0xffffffff
#define PCI_CONFIG_ADDRESS_PORT 0x0cf8
#define PCI_CONFIG_DATA_PORT    0x0cfc
#define PCI_ETHERNET_IRQ 11

struct pci_address {
    uint8_t bus;
    uint8_t device;
    uint8_t function;
};

static struct guest *guest_info;
static struct pci_ethernet *pci_ethernet_unique;
static genpaddr_t eth_base_paddr = 0;
static uint32_t function = PCI_DONT_CARE;

static e10k_t *d = NULL;

#define EICR1_OFFSET 0x00800 //Interrupt Cause Read
#define EICR2_OFFSET 0x00804
#define EICS1_OFFSET 0x00808 //Interrupt Cause Set
#define EICS2_OFFSET 0x00812
#define TDT_OFFSET 0x6018
#define TDH_OFFSET 0x6010
#define TDBAL0_OFFSET 0x6000
#define TDBAH0_OFFSET 0x6004
#define RDBAL0_OFFSET 0x1000
#define RDBAH0_OFFSET 0x1004
#define RDH0_OFFSET 0x1010
#define RDT0_OFFSET 0x1018
#define RXDCTL0_OFFSET 0x1028
#define RDLEN_OFFSET 0x01008

static int register_needs_translation(uint64_t addr){
	return (
		(0x1000 <= addr && addr <= 0x1fc0 && (addr & 0x3f) == 0) ||  //RDBAL 1
		(0xd000 <= addr && addr <= 0xdfc0 && (addr & 0x3f) == 0) ||  //RDBAL 2
		(0x6000 <= addr && addr <= 0x7fc0 && (addr & 0x3f) == 0)     //TDBAL
	);
}

static uint32_t deviceid = 0x10fb; //intel ixgbe

static void confspace_write(struct pci_device *dev,
                            union pci_config_address_word addr,
                            enum opsize size, uint32_t val)
{
    if(addr.d.fnct_nr != 0) {
        return;
    }
    
    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_write_conf_header(addr.d.doubleword, val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r, "Writing conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Written to conf header at %u: %x\n", addr.d.doubleword, val);
        }
    }
}



static void confspace_read(struct pci_device *dev,
                           union pci_config_address_word addr,
                           enum opsize size, uint32_t *val)
{
    if(addr.d.fnct_nr != 0) {
        *val = INVALID;
        return;
    }
    if(addr.d.doubleword < 0x40) {
        errval_t r = pci_read_conf_header(addr.d.doubleword,val);
        if(err_is_fail(r)) {
            DEBUG_ERR(r,"Reading conf header failed\n");
        } else {
            VMKIT_PCI_DEBUG("Read from conf header at %u: %x\n",addr.d.doubleword, *val);
        }
    } else {
        *val = INVALID;
    }
}

static void pci_ethernet_init(void *bar_info, int nr_allocated_bars)
{
	VMKIT_PCI_DEBUG("pci_ethernet_init. nr_allocated_bars: %d!\n", nr_allocated_bars);
    struct device_mem *bar = (struct device_mem *)bar_info;
    
    eth_base_paddr = bar[0].paddr;

    struct pci_ethernet * eth = pci_ethernet_unique;
    eth->phys_base_addr = bar[0].paddr;
    eth->bytes = bar[0].bytes;

    eth->pci_device->bars[0].bytes = bar[0].bytes;
    eth->pci_device->bars[0].vaddr = NULL; //Not mapped
    eth->pci_device->bars[0].paddr = bar[0].paddr;

    errval_t err = map_device(&bar[0]);
    if(err_is_fail(err)){
		printf("guest_vspace_map_wrapper failed\n");
	}
    printf("pci_ethernet_init: map_device successful. vaddr: 0x%lx, bytes: %d...\n", (uint64_t)bar[0].vaddr, (int)bar[0].bytes);
    eth->virt_base_addr = bar[0].vaddr;

    d = malloc(sizeof(*d));
    e10k_initialize(d, (void*) bar[0].vaddr);

    //Enable memory map
    /* err = guest_vspace_map_wrapper(&guest_info->vspace, bar[0].paddr, bar[0].frame_cap[0], bar[0].bytes);
    if(err_is_fail(err)){
    	printf("guest_vspace_map_wrapper failed\n");
    } */

}

static uint64_t vaddr_to_paddr(uint64_t vaddr){
	uint64_t res = 0x100000000 + vaddr;
	VMKIT_PCI_DEBUG("Returning: 0x%lx\n", res);
	return res;
}

#if defined(VMKIT_PCI_ETHERNET_DUMPS_DEBUG_SWITCH)
static void dumpRegion(uint8_t *start){
	printf("-- dump starting from 0x%lx --\n", (uint64_t)start);
	for(int i=0; i<64;i++){
		printf("0x%4x: ", i*16);
		for(int j=0; j<16; j++){
			printf("%2x ", *( (start) + (i*16 + j)));
		}
		printf("\n");
	}
	printf("-- dump finished --\n");
}
#endif


static void mem_write(struct pci_device *dev, uint32_t addr, int bar, uint32_t val){
	struct pci_ethernet * eth = (struct pci_ethernet *)dev->state;
	if(register_needs_translation(addr)){
		//VMKIT_PCI_DEBUG("Write access to Pointer register 0x%"PRIx32" value 0x%"PRIx32"\n", addr & ETH_MMIO_MASK(eth), val);
		if(val){
			val = vaddr_to_paddr(val);
		}
		VMKIT_PCI_DEBUG("Translated to value 0x%"PRIx32"\n", val);
	}
	if(TDBAH0_OFFSET == addr) {
		//!!HACK: OUR TRANSLATED ADDRESS GOES OVER 32BIT SPACE....
		VMKIT_PCI_DEBUG("TDBAH0 write detected. writing 1.\n");
		val = 1;
	}
	if(RDBAH0_OFFSET == addr) {
		//!!HACK: OUR TRANSLATED ADDRESS GOES OVER 32BIT SPACE....
		VMKIT_PCI_DEBUG("RDBAH0 write detected. writing 1.\n");
		val = 1;
	}
	if(TDT_OFFSET == addr){
		uint32_t tdt = val;
		uint32_t tdt_old = e10k_tdt_rd(d,0);
		uint32_t tdbal = e10k_tdbal_rd(d,0);
		uint32_t tdlen = e10k_tdlen_rd(d,0);
		uint32_t tdslots = tdlen/16;

		e10k_q_tdesc_legacy_array_t * tdbal_monvirt = (e10k_q_tdesc_legacy_array_t *)guest_to_host((lvaddr_t)tdbal);
		if(tdbal != 0){
			//Patch region. RDLEN is in bytes. each descriptor needs 16 bytes
			for(int j = tdt_old; j != tdt; j = j+1 == tdslots ? 0 : j+1 ){
				e10k_q_tdesc_legacy_t cur = (e10k_q_tdesc_legacy_t) (tdbal_monvirt + j);
				uint64_t new_buf = e10k_q_tdesc_legacy_buffer_extract(cur);
				e10k_q_tdesc_legacy_buffer_insert(cur, vaddr_to_paddr(new_buf));
			}
		}
		record_packet_transmit_to_net();
	}
	if(addr == RDT0_OFFSET){
		//Inspect the contents of the RECEIVE descriptors
		uint32_t rdbal = e10k_rdbal_1_rd(d,0);
		uint32_t rdlen = e10k_rdlen_1_rd(d,0);
		uint32_t rdslots = rdlen / 16; //receive desc size is 16bytes and rdlen is in bytes.
		uint32_t rdt_old = e10k_rdt_1_rd(d, 0);
		uint32_t rdt = val;

		VMKIT_PCI_DEBUG("RDT write detected!  rdt: %"PRIu32", old_rdt: %"PRIu32", rdslots: %"PRIu32"\n", rdt, rdt_old, rdslots);

#if defined(VMKIT_PCI_ETHERNET_DUMPS_DEBUG_SWITCH)
		uint32_t rdxctl = e10k_rxdctl_1_rd(d,0); // mackerel calls untested
		uint32_t rdbah  = e10k_rdbah_1_rd(d,0);
		uint32_t rdh = e10k_rdh_1_rd(d,0);

		uint32_t rdt = e10k_rdt_1_rd(d,0);
		VMKIT_PCI_DEBUG("Inspecting ReceiveDescriptor. RDBAL: 0x%08x, RDBAH: 0x%08x, RDH: %d, RDT: %d, RDLEN: %d, RDXCTL: 0x%x\n",
				rdbal, rdbah, rdh, rdt, rdlen, rdxctl);
#endif

		e10k_q_rdesc_adv_rd_array_t * rdbal_monvirt = (e10k_q_rdesc_adv_rd_array_t *)guest_to_host((lvaddr_t)rdbal);
		if(rdbal != 0){
#if defined(VMKIT_PCI_ETHERNET_DUMPS_DEBUG_SWITCH)
			dumpRegion((uint8_t*)rdbal_monvirt);
#endif
			//Patch region. RDLEN is in bytes. each descriptor needs 16 bytes
			for(int j = rdt_old; j != rdt; j = j+1 == rdslots ? 0 : j+1 ){
				e10k_q_rdesc_adv_rd_t cur_rd = (e10k_q_rdesc_adv_rd_t)(rdbal_monvirt + j);
				uint64_t new_buf = e10k_q_rdesc_adv_rd_buffer_extract(cur_rd);
				e10k_q_rdesc_adv_rd_buffer_insert(cur_rd, vaddr_to_paddr(new_buf));

				uint64_t new_hdr = e10k_q_rdesc_adv_rd_hdr_buffer_extract(cur_rd);
				e10k_q_rdesc_adv_rd_hdr_buffer_insert(cur_rd, vaddr_to_paddr(new_hdr));
			}
#if defined(VMKIT_PCI_ETHERNET_DUMPS_DEBUG_SWITCH)
			dumpRegion((uint8_t*)rdbal_monvirt);
#endif
		}
	}

	*((uint32_t *)(((uint64_t)(eth->virt_base_addr)) + addr)) = val;
}

static void mem_read(struct pci_device *dev, uint32_t addr, int bar, uint32_t *val){
	struct pci_ethernet * eth = (struct pci_ethernet *)dev->state;
	*val = *((uint32_t *)((uint64_t)(eth->virt_base_addr) + addr));
	if( register_needs_translation(addr) ){
		VMKIT_PCI_DEBUG("Read  access to Pointer register 0x%"PRIu32" value 0x%"PRIx32"\n", addr, *val);
		//dont translate null pointers
		if(*val) {
			*val = host_to_guest((lvaddr_t)val);
		}
		VMKIT_PCI_DEBUG("Translated to value 0x%"PRIx32"\n", *val);
	}
}

static void pci_ethernet_interrupt_handler(void *arg)
{
	record_packet_receive_from_net();
    struct pci_device *dev = (struct pci_device *)arg;
    lpc_pic_assert_irq(dev->lpc, dev->irq);
}

struct pci_device *pci_ethernet_new(struct lpc *lpc, struct guest *g)
{
    struct pci_device *dev = calloc(1, sizeof(struct pci_device));
    struct pci_ethernet *host = calloc(1, sizeof(struct pci_ethernet));
    
    pci_ethernet_unique = host;
    host->pci_device = dev;
    guest_info = g;

    //initialize device
    dev->confspace_write = confspace_write;
    dev->confspace_read = confspace_read;
    dev->mem_read = mem_read;
    dev->mem_write = mem_write;
    dev->state = host;
    dev->irq = PCI_ETHERNET_IRQ;
    dev->lpc = lpc;
    
    //Connect to pci server
	errval_t r = pci_client_connect();
	assert(err_is_ok(r));
	VMKIT_PCI_DEBUG("connected to pci\n");
    
    //Register as driver
	r = pci_register_driver_irq((pci_driver_init_fn)pci_ethernet_init,
                                PCI_CLASS_ETHERNET,
                                PCI_DONT_CARE, PCI_DONT_CARE,
								PCI_VENDOR_INTEL, deviceid,
								PCI_DONT_CARE, PCI_DONT_CARE, function,
								pci_ethernet_interrupt_handler, dev);

	if(err_is_fail(r)) {
		DEBUG_ERR(r, "ERROR: vmkitmon pci_ethernet: pci_register_driver");
	}
	assert(err_is_ok(r));
	VMKIT_PCI_DEBUG("registered ethernet driver, waiting for init...\n");

    return dev;
}
