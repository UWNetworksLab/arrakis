/**
 * \file
 * \brief Local descriptor table (LDT) management
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_ARCH_LDT_H
#define LIBBARRELFISH_ARCH_LDT_H

#include <barrelfish/ldt.h>

void ldt_init_disabled(dispatcher_handle_t handle);
errval_t ldt_alloc_segment_disabled(dispatcher_handle_t handle, void *segbase,
                                    uint16_t *ret_selector);
errval_t ldt_free_segment_ondisp(dispatcher_handle_t handle, uint16_t selector);

#endif // LIBBARRELFISH_ARCH_LDT_H
