/**
 * \file
 * \brief VGA console I/O.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CONIO_H
#define CONIO_H

#include <stdint.h>
#include <barrelfish_kpi/types.h>

#define VIDEO_MEM   0xb8000             ///< Video memory physical base address

void conio_cls(void);
void conio_putchar(char c);
void conio_relocate_vidmem(lvaddr_t newaddr);

#ifdef __scc__
void klog_init(void);
#endif

#endif
