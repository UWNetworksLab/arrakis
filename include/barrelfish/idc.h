/**
 * \file
 * \brief IDC declarations common to all transports
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_IDC_H
#define BARRELFISH_IDC_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/// Generic control operations on an established IDC binding
typedef enum idc_control {
    IDC_CONTROL_TEARDOWN,   ///< Initiate connection teardown
    IDC_CONTROL_SET_SYNC,   ///< Enable synchronous optimisations
    IDC_CONTROL_CLEAR_SYNC, ///< Disable synchronous optimisations
} idc_control_t;

/// Flags on an IDC export/service
typedef enum idc_export_flags {
    // Do not block on receipt notifications even if transport supports it
    IDC_EXPORT_FLAG_NO_NOTIFY = 1 << 0,
} idc_export_flags_t;

#define IDC_EXPORT_FLAGS_DEFAULT 0

/// Flags on an IDC bind
typedef enum idc_bind_flags {
    /// signals that binding will use RPCs involving capability transfer
    /// in practice, this results in a new monitor binding for non-LMP transports
    IDC_BIND_FLAG_RPC_CAP_TRANSFER = 1 << 0,
    // Do not block on receipt notifications even if transport supports it
    IDC_BIND_FLAG_NO_NOTIFY = 1 << 1,

    // request a multi-hop channel
    IDC_BIND_FLAG_MULTIHOP = 1 << 2,
} idc_bind_flags_t;

#define IDC_BIND_FLAGS_DEFAULT 0

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
#include <barrelfish/lmp_chan.h>
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
#include <barrelfish/ump_chan.h>
#endif

#if defined(CONFIG_FLOUNDER_BACKEND_UMP_IPI)
#include <arch/x86/barrelfish/ipi_notify.h>
#endif

#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
#include <barrelfish/multihop_chan.h>
#endif

void idc_init(void);

__END_DECLS

#endif // BARRELFISH_IDC_H
