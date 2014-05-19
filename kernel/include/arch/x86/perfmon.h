/**
 * \file
 * \brief Performace monitoring support for x86\
 * This should be working on Intel and AMD platforms.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef PERFMON_H
#define PERFMON_H

#include <dispatch.h> // For the length of the task name

void perfmon_init(void);

void perfmon_measure_start(uint8_t event, uint8_t umask, 
                           uint8_t counter_id, bool kernel, 
                           uint64_t ctr);
void perfmon_measure_stop(void);

uint64_t perfmon_measure_read(void);
void perfmon_measure_write(uint8_t counter_id, uint64_t val);
void perfmon_measure_reset(void);

#endif
