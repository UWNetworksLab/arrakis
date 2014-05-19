/**
 * \file
 * \brief PC16550 low-level kernel UART driver
 *
 * AB's quick-and-nasty serial driver.
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <x86.h>
#include <serial.h>
#include "pc16550d_dev.h"

int serial_portbase = 0x3f8; // COM1 default, can be changed via command-line arg

#define NIBBLE 4
#define HEX2ASCII 0x30
#define HEXLETTER 0x3a
#define HEXCORRECTION 0x7

#define NUM_PORTS 2
unsigned serial_console_port = 0;
unsigned serial_debug_port = 0;
const unsigned serial_num_physical_ports = NUM_PORTS;

// Note: hardwired for PC hardware
static const uint32_t portbases[NUM_PORTS] = { 0x3f8, 0x2f8 };
static pc16550d_t ports[NUM_PORTS];

/** \brief Initialise the serial driver. */
errval_t serial_init(unsigned port)
{
    if (port >= NUM_PORTS) {
	return SYS_ERR_SERIAL_PORT_INVALID;
    };

    // XXX Backwards compatibility!
    if (serial_portbase != 0x3f8 && port == 0) {
	// We're trying to initialize the console port on a machine
	// which uses the second UART, but the CPU driver doesn't
	// understand the new serial interface yet.  Kluge this into
	// using the second UART, and set the console and debug ports
	// accordingly. 
	serial_console_port = 1;
	serial_debug_port = 1;
	port = 1;
    }

    pc16550d_t *uart = &ports[port];
    pc16550d_initialize(uart, portbases[port]);
    

    // XXX: if non-BSP core, assume HW is already initialised
    if (!arch_core_is_bsp()) {
        return SYS_ERR_OK;
    }

    // Initialize UART
    // disable interrupt
    pc16550d_ier_t ier = pc16550d_ier_default;
    ier = pc16550d_ier_erbfi_insert(ier, 0);
    pc16550d_ier_wr(uart, ier);

    // enable FIFOs
    pc16550d_fcr_t fcr = pc16550d_fcr_default;
    fcr = pc16550d_fcr_fifoe_insert(fcr, 1);
    // FIFOs hold 14 bytes
    fcr = pc16550d_fcr_rtrigger_insert(fcr, pc16550d_bytes14);
    pc16550d_fcr_wr(uart, fcr);

    pc16550d_lcr_t lcr = pc16550d_lcr_default;
    lcr = pc16550d_lcr_wls_insert(lcr, pc16550d_bits8); // 8 data bits
    lcr = pc16550d_lcr_stb_insert(lcr, 1); // 1 stop bit
    lcr = pc16550d_lcr_pen_insert(lcr, 0); // no parity
    pc16550d_lcr_wr(uart, lcr);

    // set data terminal ready
    pc16550d_mcr_t mcr = pc16550d_mcr_default;
    mcr = pc16550d_mcr_dtr_insert(mcr, 1);
    mcr = pc16550d_mcr_out_insert(mcr, 2);
    pc16550d_mcr_wr(uart, mcr);

    // Set baudrate (XXX: hard-coded to 115200)
    if (!CPU_IS_M5_SIMULATOR) {
        pc16550d_lcr_dlab_wrf(uart, 1);
        pc16550d_dl_wr(uart, pc16550d_baud115200);
        pc16550d_lcr_dlab_wrf(uart, 0);
    }

    return SYS_ERR_OK;
}

errval_t serial_early_init(unsigned port)
{
    return SYS_ERR_OK;
}

/** \brief Prints a single character to the default serial port. */
void serial_putchar(unsigned port, char c)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    // Wait until FIFO can hold more characters
    while(!pc16550d_lsr_thre_rdf(&ports[port]));
    // Write character
    pc16550d_thr_wr(&ports[port], c);
}

/** \brief Reads a single character from the default serial port.
 * This function spins waiting for a character to arrive.
 */
char serial_getchar(unsigned port)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);

    // Read a character from FIFO
    while( !pc16550d_lsr_dr_rdf(&ports[port]));
    return pc16550d_rbr_rd(&ports[port]);
}
