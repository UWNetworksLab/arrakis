/**
 * \file
 * \brief AMD performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PERFMON_AMD_H
#define PERFMON_AMD_H

void perfmon_amd_init(void);
bool perfmon_amd_supported(void);

void perfmon_amd_measure_stop(uint8_t idx);
void perfmon_amd_measure_start(uint16_t event, uint8_t umask, bool os, 
                               uint8_t idx, bool intr);
uint64_t perfmon_amd_measure_read(uint8_t idx);
void perfmon_amd_measure_write(uint64_t val, uint8_t idx);


#endif
