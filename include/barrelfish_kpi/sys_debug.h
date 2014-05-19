/**
 * \file
 * \brief Essential capability definitions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_SYS_DEBUG_H
#define BARRELFISH_KPI_SYS_DEBUG_H

enum debug_message {
    DEBUG_CONTEXT_COUNTER_RESET,
    DEBUG_CONTEXT_COUNTER_READ,
    DEBUG_TIMESLICE_COUNTER_READ,
    DEBUG_FLUSH_CACHE,
    DEBUG_SEND_IPI,
    DEBUG_SET_BREAKPOINT,
    DEBUG_SEND_NOTIFY,
    DEBUG_SLEEP,
    DEBUG_HARDWARE_TIMER_READ,
    DEBUG_HARDWARE_TIMER_HERTZ_READ,
    DEBUG_GET_TSC_PER_MS,
    DEBUG_GET_APIC_TIMER,
    DEBUG_GET_APIC_TICKS_PER_SEC,
    DEBUG_FEIGN_FRAME_CAP,
    DEBUG_GET_APIC_ID
};

#endif //BARRELFISH_KPI_SYS_DEBUG_H
