/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PC16550D_H
#define PC16550D_H

#include <stdint.h>
#include <stdbool.h>
#include "lpc.h"
#include "pc16550d_mem_dev.h"

#define PC16550D_FIFO_BITS      4
#define PC16550D_FIFO_SIZE      (1 << PC16550D_FIFO_BITS)
#define PC16550D_FIFO_MASK      (PC16550D_FIFO_SIZE - 1)

struct pc16550d {
    uint16_t        base_port;
    uint8_t         irq;
    struct lpc      *lpc;
    pc16550d_mem_t  dev;
    uint8_t         regs[12];
    char            fifo_in[PC16550D_FIFO_SIZE];
    unsigned int    fifo_in_produced;
    unsigned int    fifo_in_consumed;
};

struct pc16550d *pc16550d_new (uint16_t base_port, uint8_t irq, struct lpc *lpc);
int pc16550d_handle_pio_read (struct pc16550d *u, uint16_t port,
                              enum opsize size, uint32_t *val);
int pc16550d_handle_pio_write (struct pc16550d *u, uint16_t port,
                               enum opsize size, uint32_t val);
void pc16550d_attach_to_console (struct pc16550d *u);

#endif //PC16550D_H
