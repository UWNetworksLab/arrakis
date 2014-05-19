/**
 * \file
 * \brief Startup prototypes.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __STARTUP_X86_H
#define __STARTUP_X86_H

#include <startup.h>

#define BOOTINFO_BASE           ((lvaddr_t)0x200000)
#define ARGS_BASE               (BOOTINFO_BASE + BOOTINFO_SIZE)
#define DISPATCHER_BASE         (ARGS_BASE + ARGS_SIZE)
#define MON_URPC_BASE           (DISPATCHER_BASE + DISPATCHER_SIZE)

#ifdef __scc__
extern int kernel_scckernel;
#endif

errval_t startup_map_init(lvaddr_t vbase, lpaddr_t base, size_t size,
                          uint32_t flags);
errval_t startup_alloc_init(void *state, genvaddr_t gvbase, size_t size,
                            uint32_t flags, void **ret);
void create_module_caps(struct spawn_state *st);

struct dcb *spawn_bsp_init(const char *name, alloc_phys_func alloc_phys);
struct dcb *spawn_app_init(struct x86_core_data *core_data,
                           const char *name, alloc_phys_func alloc_phys);

extern struct x86_core_data *glbl_core_data; // XXX: Arch specific

#endif // __STARTUP_X86_H
