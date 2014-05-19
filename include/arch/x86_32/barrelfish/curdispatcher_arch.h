/**
 * \file
 * \brief Implementation of curdispatcher
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_CURDISPATCHER_H
#define ARCH_X86_32_BARRELFISH_CURDISPATCHER_H

/**
 * \brief Returns pointer to current dispatcher, using thread register
 */
static inline dispatcher_handle_t curdispatcher(void)
{
    dispatcher_handle_t ret;

    /* %gs refers to the dispatcher, but as a user we can't access it. However,
     * the udisp field of the dispatcher conveniently contains its
     * address, so we load that (as an offset from %gs).
     */
    // FIXME: this should be using TLS (and have %gs == thread ptr)
    __asm("movl %%gs:(%c[off]), %[ret]"
          : [ret] "=r" (ret)
          : [off] "i" (offsetof(struct dispatcher_shared_generic, udisp)));

    return ret;
}

#endif // ARCH_X86_32_BARRELFISH_CURDISPATCHER_H
