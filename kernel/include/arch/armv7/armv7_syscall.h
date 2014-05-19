/**
 * \file
 * \brief armv7-specific system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARMV7_SYSCALL_H
#define ARMV7_SYSCALL_H

#include <capabilities.h>

struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry);

#endif // ARMV7_SYSCALL_H
