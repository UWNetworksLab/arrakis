/**
 * \file
 * \brief Dispatcher architecture-specific code
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_ARM_BARRELFISH_CURDISPATCHER_H
#define ARCH_ARM_BARRELFISH_CURDISPATCHER_H

#include <barrelfish_kpi/dispatcher_handle.h>

//
// Helpers for pasting #defined values into inline assembler.
//
#define STR(x) #x
#define XTR(x) STR(x)

/**
 * \brief Returns pointer to current dispatcher, using thread register
 */
static inline dispatcher_handle_t curdispatcher(void)
{
    dispatcher_handle_t ret = 0;
    __asm (
        "mov %[ret]," XTR(THREAD_REGISTER) :  [ret] "=r" (ret)
          );
    return ret;
}

#endif // ARCH_ARM_BARRELFISH_CURDISPATCHER_H
