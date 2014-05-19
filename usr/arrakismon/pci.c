/**
 * \file PCI bus
 *
 * Virtual PCI bus implementation.
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

#define INVALID         0xffffffff

int pci_handle_pio_write(struct pci *pci, uint16_t port, enum opsize size,
                         uint32_t val)
{
    assert(pci != NULL);

    switch(port) {
    case 0xcf8:         // PCI config address port
        VMKIT_PCI_DEBUG("wrote %x to 0xcf8\n", val);
        if(size == OPSIZE_32) {
            pci->address.raw = val;
        } else {
            VMKIT_PCI_DEBUG("ignoring write (not 32bit opsize)\n");
        }
        break;

    case 0xcfc:         // PCI config data port
    case 0xcfd:
    case 0xcfe:
    case 0xcff:

        if(port != 0xcfc) {
            printf("!!!!!!!!!!!!!!!!! Unaligned write !!!!!!!!!!!!!!!\n");
        }

        VMKIT_PCI_DEBUG("wrote %x to 0x%x\n", val, port);
        {
            int busnr = pci->address.d.bus_nr;
            int device = pci->address.d.dev_nr;
            struct pci_bus *bus = pci->bus[busnr];

            if(bus == NULL) {
                break;
            }

            struct pci_device *dev = bus->device[device];

            if(dev != NULL) {
                dev->confspace_write(dev, pci->address, size, val);
            }
        }
        break;

    default:
        VMKIT_PCI_DEBUG("write to invalid port\n");
        break;
    }

    return 0;
}

int pci_handle_pio_read(struct pci *pci, uint16_t port, enum opsize size,
                        uint32_t *val)
{
    assert(pci != NULL);

    switch(port) {
    case 0xcf8:         // PCI config address port
        *val = pci->address.raw;
        VMKIT_PCI_DEBUG("read from 0xcf8: %x\n", *val);
        break;

    case 0xcfc:         // PCI config data port
    case 0xcfd:
    case 0xcfe:
    case 0xcff:
        {
            int busnr = pci->address.d.bus_nr;
            int device = pci->address.d.dev_nr;
            struct pci_bus *bus = pci->bus[busnr];

            if(bus == NULL) {
                *val = INVALID;
                break;
            }

            struct pci_device *dev = bus->device[device];

            if(dev != NULL) {
                dev->confspace_read(dev, pci->address, size, val);
            } else {
                *val = INVALID;
            }
        }

        // Shift on unaligned read (masking is done later)
        *val >>= (port - 0xcfc) * 8;

        VMKIT_PCI_DEBUG("read %x from 0x%x\n", *val, port);
        break;

    default:
        VMKIT_PCI_DEBUG("read from invalid port\n");
        break;
    }

    return 0;
}

static struct pci_bus *pci_new_bus(void)
{
    struct pci_bus *bus = calloc(1, sizeof(struct pci_bus));
    return bus;
}

struct pci *pci_new(void)
{
    struct pci *pci = calloc(1, sizeof(struct pci));

    pci->bus[0] = pci_new_bus();

    // Put a host-bridge on the bus (Linux expects to find one)
    struct pci_device *bridge = pci_hostbridge_new();
    pci_attach_device(pci, 0, 0, bridge);

    return pci;
}

int pci_attach_device(struct pci *pci, uint8_t busnr, uint8_t devnr,
                      struct pci_device *device)
{
    struct pci_bus *bus = pci->bus[busnr];

    if(bus == NULL) {
        return -1;
    }

    if(bus->device[devnr] != NULL) {
        return -2;
    }

    bus->device[devnr] = device;
    return 0;
}
