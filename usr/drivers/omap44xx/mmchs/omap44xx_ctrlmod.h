/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef __OMAP44XX_CTRLMOD_H__
#define __OMAP44XX_CTRLMOD_H__

#include <barrelfish/barrelfish.h>
#include <dev/omap/omap44xx_sysctrl_padconf_core_dev.h>

// afaict, the power enabling procedure is only possible in a
// interrupt-driven way and we need the PBIAS interrupt -SG
#define PBIAS_IRQ (32+75)

// initialize registers
void ctrlmod_init(void);
// turn on sdmmc1 power
void sdmmc1_enable_power(void);

void pbias_handle_irq(void *args);

#endif /* __OMAP44XX_CTRLMOD_H__ */
