/** \file
 *  \brief Simple sleep call
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SLEEP_H__
#define __SLEEP_H_

void sleep_init(void);
void cycle_sleep(uint64_t cycles);
void milli_sleep(uint64_t ms);

#endif
