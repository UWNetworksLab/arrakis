/**
 * \file
 * \brief Real Time Clock Header File
 *
 * This file contains the defines, the struct and the procedure
 * signatures used to access the real time clock (the hardware
 * clock).
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RTC_H_
#define RTC_H_

#include <stdint.h>

/** \struct rtc_time rtc.h <rtc.h>
    \brief Time structure.
    This structure is used to get the current RTC time
*/

struct rtc_time {
    uint8_t hr; /**< current hour */
    uint8_t min; /**< current minute */
    uint8_t sec; /**< current second */
};

extern void rtc_write_cmos(int addr, uint8_t b);
extern void rtc_write_extended(int addr, uint8_t b);
extern uint8_t rtc_read_cmos(int addr);
extern uint8_t rtc_read_extended(int addr, uint8_t b); 

extern void rtc_read(struct rtc_time *t);
uint8_t rtc_read_secs(void);
extern void rtc_print(struct rtc_time *t);

#endif // RTC_H_
