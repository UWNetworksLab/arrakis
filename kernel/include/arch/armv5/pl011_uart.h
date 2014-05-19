/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __PL011_UART_H__
#define __PL011_UART_H__

void pl011_uart_init(pl011_uart_t *uart, lvaddr_t base);
void pl011_putchar(pl011_uart_t *uart, char c);
char pl011_getchar(pl011_uart_t *uart);

#endif // __PL011_UART_H__
