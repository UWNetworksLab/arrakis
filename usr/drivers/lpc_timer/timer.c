/**
 * \file
 * \brief x86 legacy timer driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <pci/pci.h>
#include "lpc_timer_dev.h"
#include "timer.h"

#include "lpc_timer_debug.h"

#define TIMER_IOBASE    0x40

/// Period of LPC timer 0 counter, in nanoseconds
#define TIMER0_PERIOD_NS    838

/// Maximum value of (16-bit) count
#define TIMER_MAX_COUNT     ((1 << 16) - 1)


/*************************************************************//**
 * \defGroup LocalStates Local states
 *
 * @{
 * 
 ****************************************************************/

static struct lpc_timer_t timer;        ///< Mackerel state for timer registers
static timer_handler_fn timer_handler;  ///< Expiry handler

/// Remaining value of current timeout, in timer ticks
static uint64_t timer_remainder;

/*
 * @} 
 */


/*************************************************************//**
 * \defGroup Main Main
 *
 * @{
 * 
 ****************************************************************/


/**
 * \brief Set hardware timer mode and count value
 *
 * Sets timer 0 as either a rate generator (mode 2) or one-shot timer (mode 0)
 * and sets its value. This function currently does not deal with any other
 * timers.
 *
 * \param count  Count for oneshot timer, rate for ticker
 * \param periodic True for a periodic timer, false for oneshot
 */
static void timer0_set(uint16_t count, bool periodic)
{
	/*
    LPC_DEBUG("timer0_set: programming %s timer for %u ticks\n",
              periodic ? "periodic" : "one-shot", count);
*/
    struct lpc_timer_tcw_t tcw = {
        .bcd = 0,                       // Binary mode (no BCD)
        .mode = periodic ? lpc_timer_rtgen : lpc_timer_oseoc, // Operating mode
        .rwsel = lpc_timer_lmsb,        // First MSB, then LSB
        .select = lpc_timer_c0          // Select counter 0
    };

    // Prepare timer 0 to set its count
    lpc_timer_tcw_wr(&timer, tcw);

    if (count > 0) {
        // Set the count/rate (LSB, then MSB)
        lpc_timer_cntacc0_wr(&timer, count & 0xff);
        lpc_timer_cntacc0_wr(&timer, count >> 8);
    }
}


/**
 * \brief Read current value of timer
 *
 * \returns the current value of timer 0
 */
static uint16_t timer0_read(void)
{
    uint16_t val;
    lpc_timer_sbyte_fmt_t status;

    do {
        // 1. Issue read back command to read the status and count of the counter
        struct lpc_timer_rdbk_cmd_t cmd = {
            .c0 = 1, .c1 = 0, .c2 = 0,  // select counter 0 only
            .stat = 0, .count = 0       // latch both status and count
        };
        lpc_timer_rdbk_cmd_wr(&timer, cmd);

        // 2. Read status
        status = lpc_timer_sbyte_fmt0_rd(&timer);

        // 3. Read value latched value (LSB, then MSB)
        // (we must do this even if the status shows an invalid count)
        val = lpc_timer_cntacc0_rd(&timer) << 8;
        val |= lpc_timer_cntacc0_rd(&timer);

        LPC_DEBUG("timer0_read:%s %u ticks remaining\n", 
                  status.cnt_stat ? " null count read," : "", val);

    // if we got unlucky, and read the counter before it had finished loading,
    // the count may be invalid ("null count"), so we repeat the whole rigmarole
    } while (status.cnt_stat);

    return val;
}


/**
 * \brief message handler for timer interrupt
 *
 * 
 *
 */
static void lpc_timer_interrupt(void *arg)
{
//    LPC_DEBUG("interrupt\n");

    // reprogram timer if remainder is set
    if (timer_remainder != 0) {
        if (timer_remainder > TIMER_MAX_COUNT) {
            timer0_set(TIMER_MAX_COUNT, false);
            timer_remainder -= TIMER_MAX_COUNT;
        } else {
            timer0_set(timer_remainder, false);
            timer_remainder = 0;
        }
    // otherwise we have a timeout: run the handler
    } else if (timer_handler != NULL) {
        timer_handler();
    } else {
        LPC_DEBUG("timer_interrupt: no handler\n");
    }
}


/**
 * \brief
 *
 * 
 *
 */
void lpc_timer_register_handler(timer_handler_fn handler)
{
    LPC_DEBUG("timer_register_handler: called\n");

    timer_handler = handler;

    LPC_DEBUG("timer_register_handler: terminated\n");
}

static void timer_init(void)
{
    LPC_DEBUG("timer_init: called\n");

    lpc_timer_initialize(&timer, TIMER_IOBASE);
    timer0_set(0, false);

    timer_init_complete();

    LPC_DEBUG("timer_init: terminated\n");
}

/**
 * \brief Initialize timer driver.
 *
 * Initializes the timer (and driver) to a known state.
 */
errval_t lpc_timer_init(void)
{
    int r = pci_client_connect();
    assert(r == 0); // XXX

    return pci_register_legacy_driver_irq(timer_init, TIMER_IOBASE,
                                          TIMER_IOBASE + 4, TIMER_IRQ,
                                          lpc_timer_interrupt, NULL);
}


/**
 * \brief Set the timeout.
 *
 * \param us Timeout in microseconds
 */
void lpc_timer_set(uint64_t us)
{
    // the requested time must not exceed the maximum possible interval
    assert (us < UINT64_MAX / (1000 / TIMER0_PERIOD_NS));

    // convert to timer count value
    uint64_t count = us * 1000 / TIMER0_PERIOD_NS;

    // program hardware timer for max(TIMER_MAX_COUNT, count)
    if (count > TIMER_MAX_COUNT) {
        timer_remainder = count - TIMER_MAX_COUNT;
        count = TIMER_MAX_COUNT;
    } else {
        timer_remainder = 0;
    }

    timer0_set(count, false);

    LPC_DEBUG("lpc_timer_set: %lu us -> %lu + %lu ticks\n",
              us, count, timer_remainder);
}


/**
 * \brief Read the current value of the timer
 *
 * \returns Remaining time of pending timer, in microseconds
 */
uint64_t lpc_timer_read(void)
{
    uint16_t val = timer0_read();
    uint64_t us = (val + timer_remainder) * TIMER0_PERIOD_NS / 1000;

    LPC_DEBUG("lpc_timer_read: %u ticks + %lu remaining = %lu us\n",
              val, timer_remainder, us);

    return us;
}

/*
 * @}
 */
