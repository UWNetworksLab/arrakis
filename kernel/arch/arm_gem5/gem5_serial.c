/**
 * \file
 * \brief ARM GEM5 kernel-level serial driver.  Implements the
 * interface in /kernel/include/serial.h
 */
/*
 * Copyright (c) 2007-2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <kernel.h>
#include <paging_kernel_arch.h>
#include <serial.h>
#include <dev/pl011_uart_dev.h>
#include <pl011_uart.h>

#define NUM_PORTS 2
unsigned serial_console_port = 0;
unsigned serial_debug_port = 0;
const unsigned serial_num_physical_ports = NUM_PORTS;


#define UART0_VBASE            0x1c090000
#define UART0_SECTION_OFFSET   0x90000

#define UART_DEVICE_BYTES	0x4c
#define UART_MAPPING_DIFF	0x1000

static pl011_uart_t ports[NUM_PORTS];

/*
 * Initialize a serial port 
 */
errval_t serial_init(unsigned port)
{
    if (port < NUM_PORTS) {
        lvaddr_t base = paging_map_device(UART0_VBASE + port*UART_MAPPING_DIFF,
                                          UART_DEVICE_BYTES);
        pl011_uart_init(&ports[port], 
			base + UART0_SECTION_OFFSET + port*UART_MAPPING_DIFF);
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_SERIAL_PORT_INVALID;
    }
}

errval_t serial_early_init(unsigned port)
{
    if (port < NUM_PORTS) {
	assert(ports[port].base == 0);
	pl011_uart_init(&ports[port], UART0_VBASE + port*UART_MAPPING_DIFF);
	return SYS_ERR_OK;
    } else {
	return SYS_ERR_SERIAL_PORT_INVALID;
    }
}

void serial_putchar(unsigned port, char c) 
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    pl011_putchar(&ports[port], c);
};

char serial_getchar(unsigned port)
{
    assert(port < NUM_PORTS);
    assert(ports[port].base != 0);
    return pl011_getchar(&ports[port]);
};
