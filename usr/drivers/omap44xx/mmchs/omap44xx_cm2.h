/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef __OMAP44XX_CM2_H__
#define __OMAP44XX_CM2_H__

#include <barrelfish/barrelfish.h>

void cm2_enable_i2c(size_t i2c_index);
int  cm2_get_hsmmc1_base_clock(void);
void cm2_debug_print(void);
void cm2_print_standby_state(void);

void cm2_enable_hsmmc1(void);
void cm2_init(void);

#endif /* __OMAP44XX_CM2_H__ */
