/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <barrelfish/barrelfish.h>

#include "tulip.h"

void lwip_demo(void);
void tcp_server(void);

int main(int argc, char *argv[])
{
    int r;

    TU_DEBUG("tulip: entered\n");

    // Initialize hardware
    struct pci_address ad = {
        .bus = 0,
        .device = 10, // XXX NASTY HACK
        .function = 0,
    };

    // XXX: Hack to get driver to work without pci domain
    TU_DEBUG("tulip: writing conf header\n");
    pci_write_conf_header(&ad, BAR_OFFSET, PORTBASE | 0x1);

    r = tulip_initialize_card(&ad);
    assert(r == 0);

    // XXX: Do some fancy lwip stuff
    // lwip_demo();
    tcp_server();

    thread_exit();
    assert(!"thread_exit returned");
    return EXIT_FAILURE;
}
