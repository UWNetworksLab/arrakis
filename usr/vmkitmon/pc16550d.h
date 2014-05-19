/**
 * \file
 * \brief Virtual 16550 UART controller.
 */

/*
 * Copyright (c) 2009, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef PC16550D_H
#define PC16550D_H

#include <barrelfish/waitset.h>
#include <if/serial_defs.h>

#include <stdbool.h>
#include <stdint.h>

#include "lpc.h"
#include "pc16550d_mem_dev.h"

#define PC16550D_FIFO_BITS      4
#define PC16550D_FIFO_SIZE      (1 << PC16550D_FIFO_BITS)
#define PC16550D_FIFO_MASK      (PC16550D_FIFO_SIZE - 1)

enum pc16550d_forward {
    PC16550d_FORWARD_NONE, ///< do not forward data (disconnected)
    PC16550d_FORWARD_UART, ///< forward data to host uart
    PC16550d_FORWARD_FILE, ///< NYI
};

struct pc16550d_forward_uart {
    bool connected;
    struct waitset *ws;
    struct serial_binding *binding;
};

struct pc16550d {
    uint16_t        base_port;
    uint8_t         irq;
    struct lpc      *lpc;
    pc16550d_mem_t  dev;
    uint8_t         regs[12];
    char            fifo_in[PC16550D_FIFO_SIZE];
    unsigned int    fifo_in_produced;
    unsigned int    fifo_in_consumed;
    enum pc16550d_forward forward_state;
    struct pc16550d_forward_uart *forward_uart_state;
};

struct pc16550d *pc16550d_new (uint16_t base_port, uint8_t irq, struct lpc *lpc);
int pc16550d_handle_pio_read (struct pc16550d *u, uint16_t port,
                              enum opsize size, uint32_t *val);
int pc16550d_handle_pio_write (struct pc16550d *u, uint16_t port,
                               enum opsize size, uint32_t val);
void pc16550d_attach_to_host_uart (struct pc16550d *user_data,
                                   const char *host_uart);

#endif //PC16550D_H
