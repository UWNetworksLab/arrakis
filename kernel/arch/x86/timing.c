/**
 * \file
 * \brief Timer calibration and setting functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <kernel.h>
#include <arch/x86/apic.h>
#ifndef __scc__
#       include <arch/x86/rtc.h>
#       include <arch/x86/pit.h>
#endif
#include <arch/x86/global.h>
#include <arch/x86/timing.h>

static uint32_t tickspersec = 0;
static uint64_t tscperms = 0;

#ifndef __scc__
/**
 * \brief Calibrates local APIC timer against RTC.
 * \return Local APIC timer ticks per RTC second.
 */
static uint32_t calibrate_apic_timer_rtc(void)
{
    // Set APIC timer to one-shot mode
    apic_timer_init(true, false);
    apic_timer_set_divide(xapic_by1);

    // Wait until start of new second
    uint8_t start = rtc_read_secs(), now;
    do {
        now = rtc_read_secs();
    } while(now == start);

    apic_timer_set_count(UINT32_MAX);

    // Wait a second
    //uint32_t oldcount = UINT32_MAX;
    int reads = 0;
    start = now;
    do {
        now = rtc_read_secs();

#if 0
        // Assert timer never underflows
        // XXX: this fires on QEMU, because the APIC timer is driven directly by
        // gettimeofday() and may go backwards if ntpd is adjusting the clock
        uint32_t curc = apic_timer_get_count();
        assert(curc <= oldcount);
        oldcount = curc;
#endif
        reads++;
    } while(start == now);

    // Get new count
    uint32_t curcount = apic_timer_get_count();
    assert(curcount != 0);

    uint32_t tps = UINT32_MAX - curcount;
    printk(LOG_NOTE, "Measured %"PRIu32" APIC timer counts in one RTC second, "
           "%d data points.\n", tps, reads);

    return tps;
}
#endif

#if 0
/**
 * \brief Calibrates PIT against RTC.
 * \return PIT ticks per RTC second.
 */
static uint32_t calibrate_pit_rtc(void)
{
    // Calibrate against RTC
    rtc_init();
    pit_init();

    // Wait until start of new second
    rtc_wait_next_second();

    pit_timer0_set(0xffff, false);

    // Wait a second
    uint16_t oldcnt = pit_timer0_read();
    uint32_t ticks = 0;
    int reads = 0;
    do {
        uint16_t cnt = pit_timer0_read();
        if(cnt <= oldcnt) {
            ticks += oldcnt - cnt;
        } else {
            ticks += oldcnt + (0xffff - cnt);
        }
        oldcnt = cnt;
        reads++;
    } while((rtc_read_cmos(0xa) & 0x80) == 0);

    printf("Measured %d PIT counts in one RTC second, reads = %d.\n", ticks, reads);
    return ticks;
}

/**
 * \brief Calibrates local APIC timer against PIT.
 * \return Local APIC timer ticks per PIT second.
 */
static uint32_t calibrate_apic_timer_pit(void)
{
    // Set APIC timer to one-shot mode
    xapic_lvt_timer_wr(&apic, (xapic_lvt_timer_t) {
            .vector = APIC_TIMER_INTERRUPT_VECTOR,
            .mask = xapic_masked,
            .mode = xapic_one_shot }
        );

    apic_timer_set_divide(xapic_by1);

    // Calibrate against PIT
    pit_init();
    pit_timer0_set(0xffff, false);

    // Start both timers
    apic_timer_set_count(UINT32_MAX);

    // Wait a second (1,193,180 Ticks)
    uint16_t oldcnt = pit_timer0_read();
    uint32_t ticks = 0;
    do {
        uint16_t cnt = pit_timer0_read();
        if(cnt <= oldcnt) {
            ticks += oldcnt - cnt;
        } else {
            ticks += oldcnt + (0xffff - cnt);
        }
        oldcnt = cnt;
    } while(ticks < 1193180);

    // Get new count
    uint32_t curcount = xapic_cur_count_rd(&apic);
    assert(curcount != 0);

    uint32_t tps = UINT32_MAX - curcount;
    printf("Measured %d APIC timer counts in one PIT second.\n",
           tps);

    return tps;
}
#endif

#ifndef __scc__
/// Number of measurement iterations
#define MAX_ITERATIONS  100

/**
 * \brief Calibrates TSC against local APIC timer.
 * \return TSC ticks per local APIC timer tick.
 */
static uint64_t calibrate_tsc_apic_timer(void)
{
    // Must tick with higher granularity than a millisecond
    assert(tickspersec > 1000);
    uint32_t ticksperms = tickspersec / 1000;

    // XXX: Let's hope this fits on the stack (reserve half the stack)
    /* assert(sizeof(uint64_t) * MAX_ITERATIONS < KERNEL_STACK_SIZE / 2); */
    uint64_t timestamps[MAX_ITERATIONS];
    memset(timestamps, 0, sizeof(timestamps));

    // Set APIC timer to periodic mode
    apic_timer_init(true, true);
    apic_timer_set_divide(xapic_by1);
    apic_timer_set_count(UINT32_MAX);

    // Do all measurement iterations
    uint32_t oldcnt = apic_timer_get_count();
    for(int i = 0; i < MAX_ITERATIONS; i++) {
        // Wait a millisecond
        uint32_t ticks = 0;
        do {
            uint32_t cnt = apic_timer_get_count();
            if(cnt <= oldcnt) {
                ticks += oldcnt - cnt;
            } else {
                ticks += oldcnt + (UINT32_MAX - cnt);
            }
            oldcnt = cnt;
        } while(ticks < ticksperms);

        timestamps[i] = rdtsc();
    }

    // Calculate average
    uint64_t tpms = 0;
    for(int i = 1; i < MAX_ITERATIONS; i++) {
        assert(timestamps[i - 1] < timestamps[i]);
        tpms += timestamps[i] - timestamps[i - 1];
    }
    tpms /= MAX_ITERATIONS - 1;

    // Check that jitter is not too high.
    // No more than 1% of values should deviate more than 1%
    // from the average.
    unsigned int outliers = 0;
    uint64_t avgdistance = 0;
    for(int i = 1; i < MAX_ITERATIONS; i++) {
        uint64_t diff = timestamps[i] - timestamps[i - 1],
            distance = diff > tpms ? diff - tpms : tpms - diff;

        if(distance > tpms / 100) {
            outliers++;
        }

        avgdistance += distance;
    }
    avgdistance /= MAX_ITERATIONS - 1;

    // Always round up
    if(outliers > ((MAX_ITERATIONS - 1) / 100 + 1)) {
        printk(LOG_WARN, "Considerable TSC jitter detected! %" PRIu64 " ticks "
               "on average.\n", avgdistance);
    }

    printk(LOG_NOTE, "Measured %" PRIu64 " TSC counts per ms, "
           "%d data points. Average jitter %" PRIu64 " TSC ticks.\n",
           tpms, MAX_ITERATIONS - 1, avgdistance);
    return tpms;
}
#endif

void timing_apic_timer_set_ms(unsigned int ms)
{
    // Must tick with higher granularity than a millisecond
    assert(tickspersec > 1000);
    assert(ms < UINT32_MAX / (tickspersec / 1000));

    apic_timer_set_divide(xapic_by1);
    apic_timer_set_count(ms * (tickspersec / 1000));
}

void arch_set_timer(systime_t t);
void arch_set_timer(systime_t t)
{
    // systime_t is absolute time in ms,
    // and the APIC time count registers are 32 bit
    assert(t > kernel_now);
    uint32_t ms = (t - kernel_now);
    apic_timer_set_count(ms * (tickspersec / 1000));
}

uint32_t timing_get_apic_ticks_per_sec(void)
{
    return tickspersec;
}

uint64_t timing_get_tsc_per_ms(void)
{
    // Has to tick with at least ms granularity
    assert(tscperms > 0);
    return tscperms;
}

void timing_calibrate(void)
{
    if (CPU_IS_M5_SIMULATOR) {
        // Guess -- avoid delay of calibration
        printk(LOG_WARN, "Warning: using hard-coded timer calibration on M5\n");
        tickspersec = 31250000; 
        tscperms = tickspersec/1000;
    } else {
#ifndef __scc__
        if(apic_is_bsp()) {
            tickspersec = calibrate_apic_timer_rtc();
            global->tickspersec = tickspersec;
        } else {
            tickspersec = global->tickspersec;
        }

        tscperms = calibrate_tsc_apic_timer();
#else
        // SCC timer rate (we just know it)
        tickspersec = 400000000;     // XXX: APIC timer ticks faster than fits in a 32bit value
        tscperms = 533000;
#endif
    }
}
