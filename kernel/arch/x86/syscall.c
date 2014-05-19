/**
 * \file
 * \brief Arch-specific system calls implementation.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arch/x86/syscall.h>
#include <barrelfish_kpi/syscalls.h>
#include <arch/x86/start_aps.h>
#include <arch/x86/timing.h>
#include <x86.h>

/**
 * \brief TSC value at beginning of the last timeslice.
 */
uint64_t tsc_lasttime = 0;

#ifndef __scc__
/**
 * \brief Spawn a new core
 */
struct sysret sys_monitor_spawn_core(coreid_t core_id, enum cpu_type cpu_type,
                                     genvaddr_t entry)
{
    int r;
    switch(cpu_type) {
    case CPU_X86_64:
        r = start_aps_x86_64_start(core_id, entry);
        if (r != 0) {
            return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        }
        break;
    case CPU_X86_32:
        r = start_aps_x86_32_start(core_id, entry);
        if (r != 0) {
            return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        }
        break;
    default:
        assert(!"Architecture not supported");
        return SYSRET(SYS_ERR_CORE_NOT_FOUND);
        break;
    }

    return SYSRET(SYS_ERR_OK);
}
#endif

struct sysret sys_io(struct capability *to, enum io_cmd cmd,
                     uint16_t port, uint32_t data)
{
    // Check whether IO operation is allowed
    if(port < to->u.io.start || port > to->u.io.end) {
        printk(LOG_ERR,
               "handle_io: illegal operation: port %" PRIx16 " out of range\n", port);
        return SYSRET(SYS_ERR_IO_PORT_INVALID);
    }

    debug(SUBSYS_IO, "handle_io: IO cmd 0x%x to port 0x%" PRIx16 "\n", cmd, port);

    switch(cmd) {
    case IOCmd_Outb:
        outb(port, data);
        break;

    case IOCmd_Outw:
        outw(port, data);
        break;

    case IOCmd_Outd:
        outd(port, data);
        break;

    case IOCmd_Inb:
        data = inb(port);
        break;

    case IOCmd_Inw:
        data = inw(port);
        break;

    case IOCmd_Ind:
        data = ind(port);
        break;

    default:
        panic("invalid IO command");
    }

    return (struct sysret){ .error = SYS_ERR_OK, .value = data };
}

struct sysret sys_monitor_handle_sync_timer(uint64_t synctime)
{
    if(rdtsc() > synctime) {
        return SYSRET(SYS_ERR_SYNC_MISS);
    }

    while(rdtsc() < synctime);
    timing_apic_timer_set_ms(kernel_timeslice);
    tsc_lasttime = rdtsc();
    scheduler_reset_time();

    return SYSRET(SYS_ERR_OK);
}
