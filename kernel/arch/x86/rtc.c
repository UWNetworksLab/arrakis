/**
 * \file
 * \brief Simple RTC hardware clock access.
 *
 * This file implements a simple driver for the hardware real time clock.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zürich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zürich D-INFK, Haldeneggsteig 4, CH-8092 Zürich. Attn: NetOS Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <arch/x86/rtc.h>
#include <dev/lpc_rtc_dev.h>

static lpc_rtc_t rtc;

/** \brief This function reads the hardware clock.
    This function reads the hardware real time clock and fills the
    passed struct with the read values.
    \param t pointer to a rtc_time struct
*/

void rtc_write_cmos(int addr, uint8_t b)
{
    lpc_rtc_ndx_wr(&rtc,addr);
    lpc_rtc_target_wr(&rtc,b);
}

void rtc_write_extended(int addr, uint8_t b)
{
    lpc_rtc_endx_wr(&rtc,addr);
    lpc_rtc_etarget_wr(&rtc,b);
}

uint8_t rtc_read_cmos(int addr)
{
    lpc_rtc_ndx_wr(&rtc,addr);
    return lpc_rtc_target_rd(&rtc);
}

uint8_t rtc_read_extended(int addr, uint8_t b)
{
    lpc_rtc_endx_wr(&rtc,addr);
    return lpc_rtc_etarget_rd(&rtc);
}

static inline uint8_t _rtc_read( lpc_rtc_t *rt, uint8_t _r) 
{
    lpc_rtc_ndx_wr(rt,_r);
    return lpc_rtc_target_rd(rt);
}


void rtc_read(struct rtc_time *t)
{
    uint8_t sec, min, hr;

    // read hour
    hr = _rtc_read(&rtc, lpc_rtc_hours );

    // read minutes
    min = _rtc_read(&rtc, lpc_rtc_minutes );

    // read seconds
    sec = _rtc_read(&rtc, lpc_rtc_seconds );

    // Convert in the case of BCD hours
    lpc_rtc_ndx_wr(&rtc, lpc_rtc_regb);
    if ( lpc_rtc_regb_dm_rdf(&rtc) ) {
        t->hr = hr;
        t->min = min;
        t->sec = sec;
    } else {
        t->hr = (hr / 16) * 10 + hr % 16;
        t->min = (min / 16) * 10 + min % 16;
        t->sec = (sec / 16) * 10 + sec % 16;
    }
}

uint8_t rtc_read_secs(void)
{
    while(_rtc_read(&rtc, lpc_rtc_rega) & 128);
    return _rtc_read(&rtc, lpc_rtc_seconds);
}

void rtc_init(void)
{
    lpc_rtc_initialize(&rtc, 0x00);

    // Set RTC to binary mode (not BCD), no interrupts
    /* rtc_write_cmos(0xb, (1 << 1) | (1 << 2)); */
}

/** \brief Print current time.
    This function prints the given time
    \param t pointer to a rtc_time struct
*/
void rtc_print(struct rtc_time *t)
{
    printf("%02hhu:%02hhu:%02hhu\n", t->hr, t->min, t->sec);
}
