/**
 * \file
 * \brief Intel eMAC driver: Debug functionality
 *
 * This file is a driver for the PCI Express e1000 card
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include "eMAC_driver.h"
#include <net_queue_manager/net_queue_manager.h>
//static eMAC_t *d;
/*
 * Handy-dandy shorthand for printing registers: make sure you have a
 * suitable buffer declared.
 */
#define PR_REG(t) eMAC_##t##_pr(pb, PRTBUF_SZ, &d); printf("%s\n",pb)
#define PR_REGZ(t) eMAC_##t##_pri(pb, PRTBUF_SZ, &d, 0); printf("%s\n",pb)
#define PRTBUF_SZ 4000


void print_statistics(void)
{

    // Transmit registers

    // Receive registers

    // Statistics registers

}

