/**
 * \file
 * \brief x86 legacy timer driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TIMER_H
#define TIMER_H

#define TIMER_IRQ       0

typedef void (*timer_handler_fn)(void);

void timer_init_complete(void);
errval_t lpc_timer_init(void);
void lpc_timer_register_handler(timer_handler_fn handler);
void lpc_timer_set(uint64_t us);
uint64_t lpc_timer_read(void);

#endif
