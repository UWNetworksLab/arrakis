/**
 * \file
 * \brief Timer calibration and setting functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef TIMING_H
#define TIMING_H

void timing_calibrate(void);
void timing_apic_timer_set_ms(unsigned int ms);
uint64_t timing_get_tsc_per_ms(void);
uint32_t timing_get_apic_ticks_per_sec(void);

#endif
