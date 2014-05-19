/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef EHCI_DEVICE_H_
#define EHCI_DEVICE_H_

/*
 * Note: This file is responsible for including the correct Mackerel file for
 *       accessing the the hardware, depending on the platform
 */

#if __arm___
    #include <dev/omap/ehci_dev.h>
#else
    #include <dev/ehci_dev.h>
#endif


#endif /* EHCI_DEVICE_H_ */
