/**
 * \file
 * \brief CMOS memory interface
 *
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CMOS_H_
#define CMOS_H_

#include <stdint.h>

#define CMOS_RAM_SHUTDOWN_ADDR 0x0f
#define CMOS_RAM_WARM_SHUTDOWN 0x0a
#define CMOS_RAM_BIOS_WARM_START_INIT_VECTOR 0x467

void cmos_write(int addr, uint8_t b);
void cmos_write_extended(int addr, uint8_t b);
uint8_t cmos_read(int addr);
uint8_t cmos_read_extended(int addr, uint8_t b); 

#endif // CMOS_H_
