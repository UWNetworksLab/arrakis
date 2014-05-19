/**
 * \file
 * \brief x86 debug registers
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DEBUGREGS_H
#define DEBUGREGS_H

void debugregs_set_breakpoint(uintptr_t addr, uint8_t mode, uint8_t len);

#endif
