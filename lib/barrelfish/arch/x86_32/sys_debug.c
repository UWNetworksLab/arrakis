/**
 * \file
 * \brief Debug system calls, user-side
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/syscall_arch.h>
#include <barrelfish_kpi/sys_debug.h>
#define ENABLE_FEIGN_FRAME_CAP
#include <barrelfish/sys_debug.h>
#include <stdio.h>
#include <inttypes.h>

errval_t sys_nop(void)
{
    return syscall1(SYSCALL_NOP).error;
}

errval_t sys_reboot(void)
{
    return syscall1(SYSCALL_REBOOT).error;
}

errval_t sys_debug_context_counter_reset(void)
{
    return syscall2(SYSCALL_DEBUG, DEBUG_CONTEXT_COUNTER_RESET).error;
}

errval_t sys_debug_context_counter_read(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_CONTEXT_COUNTER_READ);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_print_context_counter(void)
{
    uint64_t val;
    errval_t err = sys_debug_context_counter_read(&val);
    if (err_is_ok(err)) {
        printf("core %d: csc = %" PRIu64 "\n", disp_get_core_id(), val);
    }
    return err;
}

errval_t sys_debug_timeslice_counter_read(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_TIMESLICE_COUNTER_READ);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_get_tsc_per_ms(uint64_t *ret)
{
    struct sysret sr = syscall2(SYSCALL_DEBUG, DEBUG_GET_TSC_PER_MS);
    *ret = sr.value;
    return sr.error;
}

errval_t sys_debug_print_timeslice(void)
{
    uint64_t val;
    errval_t err = sys_debug_timeslice_counter_read(&val);
    if (err_is_ok(err)) {
        printf("core %d: kernel_now = %" PRIu64 "\n", disp_get_core_id(), val);
    }
    return err;
}

errval_t sys_debug_flush_cache(void)
{
    return syscall2(SYSCALL_DEBUG, DEBUG_FLUSH_CACHE).error;
}

errval_t sys_debug_send_ipi(uint8_t destination, uint8_t shorthand, uint8_t vector)
{
    return syscall5(SYSCALL_DEBUG,
                   DEBUG_SEND_IPI, destination, shorthand, vector).error;
}

errval_t sys_debug_set_breakpoint(uintptr_t addr, uint8_t mode, uint8_t length)
{
    return syscall5(SYSCALL_DEBUG,
                    DEBUG_SET_BREAKPOINT, addr, mode, length).error;
}

errval_t sys_debug_feign_frame_cap(struct capref slot, lpaddr_t base,
                                   uint8_t bits)
{
    uint8_t cap_bits = get_cnode_valid_bits(slot);
    capaddr_t addr = get_cnode_addr(slot);

    return syscall5(SYSCALL_DEBUG,
                    DEBUG_FEIGN_FRAME_CAP, addr, base, bits | (cap_bits << 8) | (slot.slot << 16)).error;
}
