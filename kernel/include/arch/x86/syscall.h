/**
 * \file
 * \brief x86-specific system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef X86_SYSCALL_H
#define X86_SYSCALL_H

#include <capabilities.h>

struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry);
struct sysret sys_io(struct capability *to, enum io_cmd cmd,
                     uint16_t port, uint32_t data);
struct sysret sys_monitor_handle_sync_timer(uint64_t synctime);

extern uint64_t tsc_lasttime;

#endif
