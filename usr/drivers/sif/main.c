/**
 * \file
 * \brief SIF driver service handling.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
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
#include <barrelfish/nameservice_client.h>

#include "sif.h"

#define PCI_CLASS_MEMORY        0x5
#define PCI_DEVICE_ROCKYLAKE    0xc148

int main(int argc, char *argv[])
{
    // Register our device driver
    int r = pci_client_connect();
    assert(r == 0);

    r = pci_register_driver_irq(sif_init, PCI_CLASS_MEMORY, PCI_DONT_CARE,
                                PCI_DONT_CARE, PCI_VENDOR_INTEL,
                                PCI_DEVICE_ROCKYLAKE,
                                PCI_DONT_CARE, PCI_DONT_CARE, PCI_DONT_CARE,
                                sif_interrupt_handler, NULL);
    assert(r == 0);

    messages_handler_loop();
}
