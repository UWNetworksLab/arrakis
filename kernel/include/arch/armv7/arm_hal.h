/**
 * \file
 * \brief Hardware Abstraction Layer interface for ARM boards.
 *
 * This file defines the hardware abstraction layer for ARM targets. Each
 * board is expected to have an implementation that corresponds to this
 * interface.
 *
 * This interface is expected to change as new boards are added.
 */

/*
 * Copyright (c) 2007, 2009, 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __ARM_HAL_H__
#define __ARM_HAL_H__

#include <barrelfish_kpi/types.h>

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
 * generic interrupt controller functionality
 */
void     gic_init(void);
void     gic_distributor_init(void);
void     gic_cpu_interface_init(void);
void     gic_cpu_interface_enable(void);
void     gic_cpu_interface_disable(void);
void     gic_enable_interrupt(uint32_t int_id, uint8_t cpu_targets, uint16_t prio,
                              bool edge_triggered, bool one_to_n);
void     gic_disable_all_irqs(void);
uint32_t gic_get_active_irq(void);
void     gic_ack_irq(uint32_t irq);
void     gic_raise_softirq(uint8_t cpumask, uint8_t irq);

/*
 * Timer
 */
void     pit_init(uint32_t tick_hz, uint8_t pit_id);
void     pit_start(uint8_t pit_id);
bool     pit_handle_irq(uint32_t irq);
void     pit_mask_irq(bool masked, uint8_t pit_id);

/*
 * Time-stamp counter
 */
void     tsc_init(void);
uint32_t tsc_read(void);
uint32_t tsc_get_hz(void);

/*
 * system control unit
 * only for multi-core
 */
void scu_initialize(void);
void scu_enable(void);
int  scu_get_core_count(void);

void write_sysflags_reg(uint32_t regval);

/* [2009-11-17 orion] TODO: device enumeration */

#endif // __ARM_HAL_H__
