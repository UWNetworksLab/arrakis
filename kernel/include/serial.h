/**
 * \file
 * \brief Architecture-independent interface to the kernel serial port
 * subsystem.  
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

#ifndef __SERIAL_H
#define __SERIAL_H

/* Need to include this for errval_t */
#include <errors/errno.h>

/*
 * What kind of serial ports do we have?
 */
extern const unsigned serial_num_physical_ports;

/*
 * Initialize a physical serial port
 */
extern errval_t serial_init(unsigned port);
extern errval_t serial_early_init(unsigned port);

/*
 * Polled, blocking input/output.  No buffering.
 */
extern void serial_putchar(unsigned port, char c);
extern char serial_getchar(unsigned port);

/*
 * Console logical port.  Putchar will replace LF with CRLF, unlike
 * the above calls.
 */
extern unsigned serial_console_port;

static inline errval_t serial_console_init(void)
{
    return serial_init(serial_console_port);
}
static inline void serial_console_putchar(char c)
{
    if (c == '\n') {
        serial_putchar(serial_console_port,'\r');
    }
    serial_putchar(serial_console_port, c);
}
static inline char serial_console_getchar(void)
{
    return serial_getchar(serial_console_port);
}

/*
 * Debug logical port.  Putchar will replace LF with CRLF, unlike
 * the above calls.
 */
extern unsigned serial_debug_port;

static inline errval_t serial_debug_init(void)
{
    return serial_init(serial_debug_port);
}
static inline void serial_debug_putchar(char c)
{
    if (c == '\n') {
        serial_putchar(serial_debug_port,'\r');
    }
    serial_putchar(serial_debug_port, c);
}
static inline char serial_debug_getchar(void)
{
    return serial_getchar(serial_debug_port);
}

#endif //__SERIAL_H
