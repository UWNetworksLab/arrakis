/**
 * \file
 * \brief Header for ARMv5-specific GDB stub code.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <arm.h>

extern uintptr_t *gdb_arch_registers;

/** Address of saved registers as void * */
#define GDB_ARCH_REGADDR    ((void*)gdb_arch_registers)

/** Number of bytes saved in GDB frame */
#define GDB_ARCH_REGBYTES   (sizeof(uintptr_t) * ARCH_NUMREGS)
