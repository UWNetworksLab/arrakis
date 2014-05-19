/**
 * \file
 * \brief Header file for the APIC implementation
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef APIC_H_
#define APIC_H_

#include <xapic_dev.h>

#define APIC_INTER_CORE_VECTOR                  249
#define APIC_TIMER_INTERRUPT_VECTOR             250
#define APIC_THERMAL_INTERRUPT_VECTOR           251
#define APIC_PERFORMANCE_INTERRUPT_VECTOR       252
#define APIC_ERROR_INTERRUPT_VECTOR             253
#define APIC_SPURIOUS_INTERRUPT_VECTOR          254

void apic_init(void);
void apic_send_init_assert(uint8_t destination, uint8_t destination_shorthand);
void apic_send_init_deassert(void);
void apic_send_start_up(uint8_t destination,
                        uint8_t destination_shorthand,
                        uint8_t realmode_startpage);
extern uint8_t apic_id;
extern bool apic_bsp;

static inline bool apic_is_bsp(void)
{
    return apic_bsp;
}

void apic_send_std_ipi(uint8_t destination, uint8_t destination_shorthand, uint8_t vector);
void apic_eoi(void);
void apic_seoi(uint8_t int_nr);
uint8_t apic_get_id(void);

void apic_timer_init(bool masked, bool periodic);
void apic_perfcnt_init(void);
void apic_perfcnt_stop(void);
void apic_timer_set_count(uint32_t count);
uint32_t apic_timer_get_count(void);
void apic_timer_set_divide(xapic_divide_t divide);
void apic_mask_timer(void);
void apic_unmask_timer(void);
xapic_esr_t apic_get_esr(void);

#endif // APIC_H_
