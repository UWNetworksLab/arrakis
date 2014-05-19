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

#ifndef APIC_H
#define APIC_H

#include <stdint.h>

// Source material from AMD64 Manual, System Programming, Chapter 16: APIC

struct apic {
    uint64_t mmio_base_va;

    // registers
    uint32_t spurious_intr_vec_reg;
    uint32_t lvt_timer_reg;
    uint32_t err_stat_reg;
    uint32_t lvt_thermal_monitor_reg;
    uint32_t lvt_perf_counter_reg;
    uint32_t lvt_lint0_reg;
    uint32_t lvt_lint1_reg;
    uint32_t lvt_err_reg;
};

struct apic * apic_new (uint64_t mmio_base_va);
int apic_handle_mmio_read (struct apic *a, uint64_t addr, enum opsize size,
                           uint64_t *val);
int apic_handle_mmio_write (struct apic *a, uint64_t addr, enum opsize size,
                            uint64_t val);
void apic_assert_irq (struct apic *a, uint8_t irq);
void apic_assert_pic_irq (struct apic *a, uint8_t irq);

#endif // APIC_H
