/**
 * \file
 * \brief Serial port driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include "serial.h"
#include "pc16550d_dev.h"

static struct pc16550d_t uart;
static uint16_t portbase;

static void serial_poll(void)
{
    // Read as many characters as possible from FIFO
    while(pc16550d_lsr_dr_rdf(&uart)) {
        char c = pc16550d_rbr_rd(&uart);
        serial_input(&c, 1);
    }
}

static void serial_interrupt(void *arg)
{
    pc16550d_iir_t iir = pc16550d_iir_rd(&uart);

    // Assert no error happened
    assert(pc16550d_iir_iid_extract(iir) != pc16550d_rls
           && pc16550d_iir_iid_extract(iir) != pc16550d_ms);

    // Read serial port just like with polling
    serial_poll();
}

static void real_init(void)
{
    // Initialize Mackerel with base port
    pc16550d_initialize(&uart, portbase);

    // enable interrupt
    pc16550d_ier_t ier = pc16550d_ier_default;
    ier = pc16550d_ier_erbfi_insert(ier, 1);
    pc16550d_ier_wr(&uart, ier);

    // enable FIFOs
    pc16550d_fcr_t fcr = pc16550d_fcr_default;
    fcr = pc16550d_fcr_fifoe_insert(fcr, 1);
    // FIFOs hold 14 bytes
    fcr = pc16550d_fcr_rtrigger_insert(fcr, pc16550d_bytes14);
    pc16550d_fcr_wr(&uart, fcr);

    pc16550d_lcr_t lcr = pc16550d_lcr_default;
    lcr = pc16550d_lcr_wls_insert(lcr, pc16550d_bits8); // 8 data bits
    lcr = pc16550d_lcr_stb_insert(lcr, 1); // 1 stop bit
    lcr = pc16550d_lcr_pen_insert(lcr, 0); // no parity
    pc16550d_lcr_wr(&uart, lcr);

    // set data terminal ready
    pc16550d_mcr_t mcr = pc16550d_mcr_default;
    mcr = pc16550d_mcr_dtr_insert(mcr, 1);
    mcr = pc16550d_mcr_out_insert(mcr, 2);
    pc16550d_mcr_wr(&uart, mcr);

    // Set baudrate (XXX: hard-coded to 115200)
    pc16550d_lcr_dlab_wrf(&uart, 1);
    pc16550d_dl_wr(&uart, pc16550d_baud115200);
    pc16550d_lcr_dlab_wrf(&uart, 0);

    // offer service now we're up
    start_service();
}

errval_t serial_init(uint16_t portbase_arg, uint8_t irq)
{
    errval_t err;

    err = pci_client_connect();
    if (err_is_fail(err)) {
        return err;
    }

    portbase = portbase_arg;

    return pci_register_legacy_driver_irq(real_init, portbase, portbase+8,
                                          irq, serial_interrupt, NULL);
}

static void serial_putc(char c)
{
    // Wait until FIFO can hold more characters
    while(!pc16550d_lsr_thre_rdf(&uart));
    // Write character
    pc16550d_thr_wr(&uart, c);
}

void serial_write(char *c, size_t len)
{
    for (int i = 0; i < len; i++) {
        serial_putc(c[i]);
    }
}
