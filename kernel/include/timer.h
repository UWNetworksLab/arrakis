/**
 * \file
 * \brief Header for one-shot timer support
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __TIMER_H
#define __TIMER_H

#include <kernel.h> /* systime_t */

/**
 * This needs to be defined by the architecture
 */
void arch_set_timer(systime_t t);

/**
 * this value is used, when the scheduler or the wakeup subsystem do not want
 * to be woken up in the foresable future.
 */
#define TIMER_INF ((systime_t)(-1))

void update_wakeup_timer(systime_t wakeup_timer);
void update_sched_timer(systime_t sched_timer);

#endif // __TIMER_H
