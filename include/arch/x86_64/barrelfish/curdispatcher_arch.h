/**
 * \file
 * \brief Implementation of curdispatcher
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_CURDISPATCHER_H
#define ARCH_X86_64_BARRELFISH_CURDISPATCHER_H

/**
 * \brief Returns pointer to current dispatcher, using thread register
 */
static inline dispatcher_handle_t curdispatcher(void)
{
    dispatcher_handle_t ret;

    __asm("movq %%fs:8, %[ret]" // XXX: assume offsetof(struct thread, disp) == 8
          : [ret] "=r" (ret));

    return ret;
}

#endif // ARCH_X86_64_BARRELFISH_CURDISPATCHER_H
