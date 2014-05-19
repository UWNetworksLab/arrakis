/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __IXP2800_UART_H__
#define __IXP2800_UART_H__

void ixp2800_uart_init(ixp2800_uart_t *uart, lvaddr_t base);
void ixp2800_putchar(ixp2800_uart_t *uart, char c);
char ixp2800_getchar(ixp2800_uart_t *uart);

#endif // __IXP2800_UART_H__
