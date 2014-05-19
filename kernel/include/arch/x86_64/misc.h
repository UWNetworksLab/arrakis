/**
 * \file
 * \brief Miscellaneous architecture-specific functions
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_MISC_H
#define ARCH_MISC_H

#include <x86.h>
#include <irq.h>

void maybe_reload_ldt(struct dcb *dcb, bool force_reload);

/**
 * \brief Set thread-local-storage register.
 */
static inline void arch_set_thread_register(uintptr_t val)
{
    panic("shouldn't be called for x64 -AB");
#if 0
    curdisp->d.lo_base = val & ((1 << 24) - 1);
    curdisp->d.hi_base = val >> 24;
    __asm volatile("mov %[fs], %%fs" :: [fs] "r" (GSEL(DISP_SEL, SEL_UPL)));
#else
    wrmsr(MSR_IA32_FSBASE, val);
#endif
}

#define arch_get_cycle_count() rdtscp()

#endif
