/**
 * \file
 * \brief Hardware Abstraction Layer interface for ARMv7-M boards.
 *
 * This file defines the hardware abstraction layer for ARMv7-M targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2007, 2009, 2012, 2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARM_HAL_H__
#define __ARM_HAL_H__

#include <barrelfish_kpi/types.h>
#include <dev/omap/omap44xx_cortex_m3_nvic_dev.h>

/**
 * @return Unique 32-bit identifier associated with current board.
 */
uint32_t hal_get_board_id(void);

/**
 * @return Current processor ordinal. Value has range 0 to n_cpus - 1.
 */
uint8_t  hal_get_cpu_id(void);

/**
 * @return true if current processor is bootstrap processor.
 */
bool     hal_cpu_is_bsp(void);

/*
 * nested vectored interrupt controller functionality 
 * (note that in this model, all interrupts are to local cpu and do not need to be acknowledged)
 */

omap44xx_cortex_m3_nvic_t nvic;

void     nvic_init(void);
void     nvic_enable_interrupt(uint32_t int_id, uint16_t prio);
void     nvic_disable_all_irqs(void);
uint32_t nvic_get_active_irq(void);
void     enable_all_system_interrupts(void);
//void     nvic_raise_softirq(uint8_t irq);//not used in code yet -> since we can not interrupt other cores yet this is a bit pointless

//TODO: heteropanda: not sure what the systick functions should be exactly
//the pandaboard does not provide a reference to calculate hz, so we count actual cycles
void     systick_init(uint32_t tick_cycles);
void     systick_start(void);
void     systick_stop(void);


//XXX: does this even exist for the cortex-m3 ??
void write_sysflags_reg(uint32_t regval);

//pure dummy functions - just so I do not have to do case distinctions in pure debug code
uint32_t tsc_read(void);
uint32_t tsc_get_hz(void);


/* [2009-11-17 orion] TODO: device enumeration */

#endif // __ARM_HAL_H__
