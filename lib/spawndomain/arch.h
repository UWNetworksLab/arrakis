/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SPAWN_ARCH_H
#define SPAWN_ARCH_H

errval_t spawn_arch_load(struct spawninfo *si,
                         lvaddr_t binary, size_t binary_size,
                         genvaddr_t *entry, void** arch_load_info);

void spawn_arch_set_registers(void *arch_load_info,
                              dispatcher_handle_t handle,
                              arch_registers_state_t *enabled_area,
                              arch_registers_state_t *disabled_area);

#endif
