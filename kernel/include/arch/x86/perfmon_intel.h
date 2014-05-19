/**
 * \file
 * \brief Intel 64 performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <errors/errno.h>

#ifndef PERFMON_INTEL_H
#define PERFMON_INTEL_H

errval_t perfmon_intel_init(void);

void perfmon_intel_measure_start(uint8_t event, uint8_t umask, bool os, 
                                 uint8_t idx, bool intr);
uint64_t perfmon_intel_measure_read(void);
void perfmon_intel_measure_write(uint64_t val);
void perfmon_intel_reset(void);

#endif
