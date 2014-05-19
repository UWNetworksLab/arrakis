/*
 * pci_vmio.h
 *
 * Virtual Network device for Host/Guest communication
 *
 *  Created on: May 13, 2012
 *      Author: luki
 */

#ifndef PCI_VMKITMON_ETH_H_
#define PCI_VMKITMON_ETH_H_

#include "pci.h"
#include "pci_hdr0_mem_dev.h"

// control register
#define PCI_VMKITMON_ETH_RSTIRQ (1<<0) //guest writes 1 if he has handled interrupt
#define PCI_VMKITMON_ETH_TXMIT (1<<1)  //guest writes 1 if it wants send packets
#define PCI_VMKITMON_ETH_IFUP (1<<2)   //guest writes 1 if interface is up

// status register
#define PCI_VMKITMON_ETH_IRQST 1

// PCI device id
#define PCI_VMKITMON_ETH_DEVID 0x1000

// Memory mapped registers
enum pci_vmkitmon_registers {
	PCI_VMKITMON_ETH_STATUS,
	PCI_VMKITMON_ETH_CONTROL,
	PCI_VMKITMON_ETH_MAC_LOW,
	PCI_VMKITMON_ETH_MAC_HIGH,
	PCI_VMKITMON_ETH_TXDESC_ADR,	//Guest Physical ptr to array of rxdescs
	PCI_VMKITMON_ETH_TXDESC_LEN,	//Packet length
	PCI_VMKITMON_ETH_RXDESC_ADR, 	//Guest Physical ptr to receive buffer
	PCI_VMKITMON_ETH_RXDESC_LEN		//Size of the receive buffer
};

struct pci_vmkitmon_eth_rxdesc {
	uint32_t addr;
	uint32_t len;
};

struct pci_vmkitmon_eth_txdesc {
	uint32_t addr;
	uint32_t len;
};

struct pci_vmkitmon_eth {
	pci_hdr0_mem_t      ph;
    uint32_t            pci_header[0x40];
    uint32_t			mmio_register[8];
    uint32_t            mem_guest_paddr; //guest physical base address of memory register
	struct pci_device *pci_device;
};

//#define VMKITMON_ETH_DEBUG_SWITCH 1
#if defined(VMKITMON_ETH_DEBUG_SWITCH)
#define VMKITMON_ETH_DEBUG(x...) printf("VMKITMON_ETH: " x)
#else
#define VMKITMON_ETH_DEBUG(x...) ((void)0)
#endif

#endif
