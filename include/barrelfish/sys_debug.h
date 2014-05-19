/**
 * \file
 * \brief Essential capability definitions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_SYS_DEBUG_H
#define BARRELFISH_SYS_DEBUG_H

#include <sys/cdefs.h>

__BEGIN_DECLS

// XXX: arguments to sys_debug_set_breakpoint, which are clearly x86-specific!!
#define X86_DEBUG_MODE_EXECONLY     (0) ///< Break on instruction execution only.
#define X86_DEBUG_MODE_WRITEONLY    (1) ///< Break on data writes only.
#define X86_DEBUG_MODE_READWRITE    (3) ///< Break on data reads or writes but not instruction fetches.
#define X86_DEBUG_LENGTH_1BYTE      (0)
#define X86_DEBUG_LENGTH_2BYTE      (1)
#define X86_DEBUG_LENGTH_8BYTE      (2) ///< Undefined?
#define X86_DEBUG_LENGTH_4BYTE      (3)

errval_t sys_debug_context_counter_reset(void);
errval_t sys_debug_context_counter_read(uint64_t *ret);
errval_t sys_debug_timeslice_counter_read(uint64_t *ret);
errval_t sys_debug_get_tsc_per_ms(uint64_t *ret);
errval_t sys_debug_get_apic_id(uint8_t *ret);
errval_t sys_debug_get_apic_timer(uint32_t *ret);
errval_t sys_debug_print_context_counter(void);
errval_t sys_debug_print_timeslice(void);
errval_t sys_debug_flush_cache(void);
errval_t sys_debug_send_ipi(uint8_t destination, uint8_t shorthand, uint8_t vector);
errval_t sys_debug_set_breakpoint(uintptr_t addr, uint8_t mode, uint8_t length);
errval_t sys_debug_hardware_timer_read(uintptr_t* ret);
errval_t sys_debug_hardware_timer_hertz_read(uintptr_t* ret);
errval_t sys_debug_get_apic_ticks_per_sec(uint32_t *ret);

#ifdef ENABLE_FEIGN_FRAME_CAP
errval_t sys_debug_feign_frame_cap(struct capref slot, lpaddr_t base,
                                   uint8_t bits);
#endif

__END_DECLS

#endif //BARRELFISH_SYS_DEBUG_H
