/**
 * \file
 * \brief AMD performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <capabilities.h>
#include <arch/x86/perfmon_amd.h>
#include <arch/x86/perfmon_intel.h>
#include <arch/x86/perfmon.h>
#include <arch/x86/apic.h>
#include <arch/x86/timing.h>

static bool perfmon_amd = false;
bool perfmon_intel = false;
/* static bool perfmon_measurement_running = false; */
/* static bool perfmon_apic_activated = false; */
static uint64_t perfmon_cntr_init = 0;
struct capability perfmon_callback_ep = {
    .type = ObjType_Null,
};

void perfmon_init(void) 
{
    // Return if init already done .. 
    if(perfmon_amd || perfmon_intel) {
        return;
    }

    // Try to initialize on intel.
    if(err_is_fail(perfmon_intel_init())) {
        // Try amd
        perfmon_amd_init();
        // Check amd
        perfmon_amd = perfmon_amd_supported();
    } else {
        perfmon_intel = true;
    }

    // Debug output
    if(perfmon_intel) {
        printf("Activated perfmon for Intel!\n");
    } else if(perfmon_amd) {
        printf("Activated perfmon for AMD!\n");
    } else {
        printf("Perfmon activation failed. "
               "Neither Intel nor AMD support detected\n");
    }
}

uint8_t perfmon_event = 0;
static uint8_t perfmon_umask, perfmon_counter_id;
static bool perfmon_kernel;
static uint64_t perfmon_ctr;
extern int mycnt_incr;

/*
 * \brief Initialize measuring for performance analysis.
 *
 * An overflow will be registered and the counter will set such that this 
 * overflow occures every ctr counter steps.
 */
void perfmon_measure_start(uint8_t event, uint8_t umask, 
                           uint8_t counter_id, bool kernel, uint64_t ctr)
{
    // Activate APIC interrupts for overflow if init successful
    /* if(ctr!=0) { */
    /*     if(perfmon_amd || perfmon_intel) { */
    /*         printf("Starting APIC perf interrupt\n"); */
    /*         apic_perfcnt_init(); */
    /*     } */
    /* } */

    if(perfmon_amd) {
        perfmon_amd_measure_write(ctr*-1, 0);
        perfmon_amd_measure_start(event, umask, kernel, counter_id, ctr!=0);
    }

    // Activate performance measurement for Intel
    if(perfmon_intel) {
        printf("Starting perfmon on Intel...\n");
        perfmon_intel_measure_write(ctr*-1);
        perfmon_intel_measure_start(event, umask, kernel, counter_id, ctr!=0);
    }

    perfmon_cntr_init = ctr;
    perfmon_event = event;
    perfmon_umask = umask;
    perfmon_kernel = kernel;
    perfmon_counter_id = counter_id;
    perfmon_ctr = ctr;

    timing_apic_timer_set_ms(1);
    mycnt_incr = 1;
}

/*
 * Re-Initialize counter after overflow.
 * This function is called from the interrupt processing.
 */
void perfmon_measure_reset(void)
{
    if(perfmon_amd) {
        perfmon_amd_measure_write(perfmon_cntr_init*-1, 0);
    }
    if(perfmon_intel) {
        perfmon_intel_reset();
        perfmon_intel_measure_write(perfmon_cntr_init*-1);
        perfmon_intel_measure_start(perfmon_event, perfmon_umask, perfmon_kernel, perfmon_counter_id, perfmon_ctr!=0);
    }
}

uint64_t perfmon_measure_read(void)
{
    if(perfmon_amd) {
        return perfmon_amd_measure_read(0);
    }

    if(perfmon_intel) {
        return perfmon_intel_measure_read();
    }

    return 0;

}

void perfmon_measure_write(uint8_t counter_id, uint64_t val)
{

    if(perfmon_amd) {
        perfmon_amd_measure_write(val, counter_id);
    }

    if(perfmon_intel) {
        perfmon_intel_measure_write(val);
    }

}

/*
 * \brief Deactivate performance measuring
 */
void perfmon_measure_stop(void)
{
    if(perfmon_amd) {
        perfmon_amd_measure_stop(0);
    } else if(perfmon_intel) {
        perfmon_event = 0;
        /* panic("Intel performance monitoring not supported yet."); */
    }

    // Mask out performance counter overflow interrupts on APIC
    apic_perfcnt_stop();
}
