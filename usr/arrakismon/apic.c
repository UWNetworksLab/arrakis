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

#include "vmkitmon.h"
#include "apic.h"
#include <stdlib.h>

struct apic *
apic_new (uint64_t mmio_base_va)
{
    struct apic *ret = calloc(1, sizeof(struct apic));

    ret->mmio_base_va = mmio_base_va;
    ret->spurious_intr_vec_reg = 0xff;
    ret->lvt_timer_reg = 0x10000;
    ret->lvt_thermal_monitor_reg = 0x10000;
    ret->lvt_perf_counter_reg = 0x10000;
    ret->lvt_lint0_reg = 0x10000;
    ret->lvt_lint1_reg = 0x10000;
    ret->lvt_err_reg = 0x10000;

    return ret;
}

int apic_handle_mmio_read (struct apic *a, uint64_t addr, enum opsize size,
                           uint64_t *val)
{
    uint32_t reg = addr - a->mmio_base_va;

    switch (reg) {
    // APIC Version Register
    case 0x30:
        *val = 0x80050010;
        return HANDLER_ERR_OK;
    // Spurious Interrupt Vector Register
    case 0xf0:
        *val = a->spurious_intr_vec_reg;
        return HANDLER_ERR_OK;
    // Error Status Register
    case 0x280:
        *val = a->err_stat_reg;
        return HANDLER_ERR_OK;
    // LVT Timer Register
    case 0x320:
        *val = a->lvt_timer_reg;
        return HANDLER_ERR_OK;
    // LVT Thermal Monitor Register
    case 0x330:
        *val = a->lvt_thermal_monitor_reg;
        return HANDLER_ERR_OK;
    // LVT Performance Counter RegisterNode
    case 0x340:
        *val = a->lvt_perf_counter_reg;
        return HANDLER_ERR_OK;
    // LVT LINT0 Register
    case 0x350:
        *val = a->lvt_lint0_reg;
        return HANDLER_ERR_OK;
    // LVT LINT1 Register
    case 0x360:
        *val = a->lvt_lint1_reg;
        return HANDLER_ERR_OK;
    // LVT Error Register
    case 0x370:
        *val = a->lvt_err_reg;
        return HANDLER_ERR_OK;
    }

    printf("APIC: unhandeled read access to APIC reg %x\n", reg);
    return -1;
}

int
apic_handle_mmio_write (struct apic *a, uint64_t addr, enum opsize size,
                        uint64_t val)
{
    uint32_t reg = addr - a->mmio_base_va;

    switch (reg) {
    // Spurious Interrupt Vector Register
    case 0xf0:
        a->spurious_intr_vec_reg = val;
        // the apic needs to be enabled for now
        assert(val & 0x100);
        return HANDLER_ERR_OK;
    // Error Status Register
    case 0x280:
        // ignore the incoming value
        // according to the specification the ESR should now be loaded with the
        // current errors, we keep it loaded all the time
        return HANDLER_ERR_OK;
    // LVT Timer Register
    case 0x320:
        a->lvt_timer_reg = val;
        // This assert is here to detect if the OS enables the APIC timer
        // as long as it is not implemented
        assert(val & 0x10000);
        return HANDLER_ERR_OK;
    // LVT Thermal Monitor Register
    case 0x330:
        a->lvt_thermal_monitor_reg = val;
        return HANDLER_ERR_OK;
    // LVT Performance Counter RegisterNode
    case 0x340:
        a->lvt_perf_counter_reg = val;
        return HANDLER_ERR_OK;
    // LVT LINT0 Register
    case 0x350:
        a->lvt_lint0_reg = val;
        return HANDLER_ERR_OK;
    // LVT LINT1 Register
    case 0x360:
        a->lvt_lint1_reg = val;
        return HANDLER_ERR_OK;
    // LVT Error Register
    case 0x370:
        a->lvt_err_reg = val;
        return HANDLER_ERR_OK;
    }

    printf("APIC: unhandeled write access to APIC reg %x, val %lx\n", reg, val);
    return -1;
}

void
apic_assert_irq (struct apic *a, uint8_t irq)
{
}

void
apic_assert_pic_irq (struct apic *a, uint8_t irq)
{
}
