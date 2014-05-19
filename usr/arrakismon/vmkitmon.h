/**
 * \file
 */

/*
 * Copyright (c) 2009, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VMKITMON_H
#define VMKITMON_H

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/vmkit.h>
#include "guest.h"

// Handler return values - this does probably not belong here
#define HANDLER_ERR_OK              (0)
#define HANDLER_ERR_FATAL           (-1)
#define HANDLER_ERR_UNHANDELED      (-2)

#define assert_err(e,m)     \
do {                        \
    if (err_is_fail(e)) {   \
        DEBUG_ERR(e,m);     \
        abort();            \
    }                       \
} while (0)

#define LIKELY(x)       __builtin_expect((x),1)
#define UNLIKELY(x)     __builtin_expect((x),0)

#if defined(VMKIT_PCI_DEBUG_SWITCH)
#define VMKIT_PCI_DEBUG(x...) printf("VMKit PCI: " x)
#else
#define VMKIT_PCI_DEBUG(x...) ((void)0)
#endif

#endif // VMKITMON_H
