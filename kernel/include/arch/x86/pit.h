/**
 * \file
 * \brief x86 legacy timer driver (PIT).
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PIT_H
#define PIT_H

/// Period of LPC timer 0 counter, in nanoseconds
#define PIT_TIMER0_PERIOD_NS    838

void pit_init(void);
void pit_timer0_set(uint16_t count, bool periodic);
uint16_t pit_timer0_read(void);

#endif
