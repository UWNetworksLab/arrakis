/**
 * \file
 * \brief PCI vendor/device IDs
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PCI_DEVIDS_H
#define PCI_DEVIDS_H

#define PCI_DONT_CARE           0x10000 // has to be > 0xffff
#define PCI_VENDOR_INTEL        0x8086
#define PCI_VENDOR_REALTEK      0x10ec
#define PCI_VENDOR_AMD          0x1022
#define PCI_VENDOR_ATI          0x1002
#define PCI_VENDOR_LSI		0x1000
#define PCI_VENDOR_FISH         0xdada

#define PCI_CLASS_MASS_STORAGE  0x1
#define PCI_SUB_RAID		0x4
#define PCI_SUB_SATA            0x6

#define PCI_CLASS_ETHERNET      0x2
#define PCI_CLASS_DISPLAY       0x3
#define PCI_CLASS_SYSTEMPERIPHERAL 0x8

#define PCI_CLASS_HOST_BRIDGE   0x6
#define PCI_SUB_PCI_ISA_BRIDGE  0x1

#define PCI_CLASS_SERIAL        0x0c
#define PCI_SUB_USB             0x03
#define PCI_IF_USB_UHCI         0x00
#define PCI_IF_USB_OHCI         0x10
#define PCI_IF_USB_EHCI         0x20
#define PCI_IF_USB_GENERIC      0x80
#define PCI_IF_USB_DEVICE       0xfe

#define PCI_SUB_IOMMU			0x6
#define PCI_IF_IOMMU			0x0

#endif
