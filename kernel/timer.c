/**
 * \file
 * \brief Support of one-shot timers
 *
 * To simplify things we maintain one timer for the scheduler, and one for the
 * wakeup infrastructure. Each of these subsystems is responsible for updating
 * their timer value. When an update happens, we update the hardware timer if
 * the previous (global) timer has changed.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <timer.h>
#include <kernel.h>

/* these are systime_t i.e., absolute time values in ms */
static systime_t next_sched_timer = TIMER_INF;   //< timer for scheduler
static systime_t next_wakeup_timer = TIMER_INF;  //< timer for wakeups
static systime_t last_timer;                     //< last set timer

#define MIN(x,y) ((x<y) ? (x) : (y))

static void update_timer(void)
{
    systime_t timer = MIN(next_sched_timer, next_wakeup_timer);
    assert(timer != 0);
    if (last_timer != timer) {
        #if 0 /* does not seem to create a problem */
        if (timer == TIMER_INF) {
            printk(LOG_WARN, "********* %s:%s() timer == TIMER_INF\n", __FILE__, __FUNCTION__);
            //timer = kernel_now + kernel_timeslice;
        }
        #endif
        arch_set_timer(last_timer = timer);
    }
}

/**
 * \brief update the wakeup timer
 * \param t absolute time in ms for the next interrupt
 */
void update_wakeup_timer(systime_t t)
{
    next_wakeup_timer = t;
    update_timer();
}

/**
 * \brief update the sched timer
 * \param t absolute time in ms for the next interrupt
 */
void update_sched_timer(systime_t t)
{
    next_sched_timer = t;
    update_timer();
}

