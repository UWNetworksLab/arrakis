/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_OHCI_DEVICE_H_
#define _USB_OHCI_DEVICE_H_

#if defined(__x86_64__) || defined(__i386__)
#include <dev/ohci_dev.h>
#else
#include <dev/omap/ohci_dev.h>
#endif

#endif /* _USB_OHCI_DEVICE_H_ /*/
