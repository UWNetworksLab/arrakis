/**
 * \file
 * \brief IDC export declarations
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_IDC_EXPORT_H
#define LIBBARRELFISH_IDC_EXPORT_H

#include <sys/cdefs.h>
#include <barrelfish/idc.h>

__BEGIN_DECLS

typedef void idc_export_callback_fn(void *st, errval_t err, iref_t iref);

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
typedef errval_t lmp_connect_callback_fn(void *st, size_t buflen_words,
                                         struct capref endpoint,
                                         struct lmp_chan **retlc);
#endif // CONFIG_INTERCONNECT_DRIVER_LMP

#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
typedef errval_t ump_connect_callback_fn(void *st, struct monitor_binding *mb,
                                         uintptr_t mon_id, struct capref frame,
                                         size_t inchanlength,
                                         size_t outchanlength,
                                         struct capref notify_cap);
#endif // CONFIG_INTERCONNECT_DRIVER_UMP

#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
typedef errval_t multihop_connect_callback_fn(void *st, uint64_t vci);
#endif // CONFIG_INTERCONNECT_DRIVER_MULTIHOP

struct idc_export {
    iref_t iref;
    idc_export_flags_t flags;
    idc_export_callback_fn *export_callback;
    void *export_cb_st; // state passed to export_callback
    void *connect_cb_st; // state passed to (driver-specific) connect callback

    /* for each configured channel type, we need a binding-specific
     * connect callback */
#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
    lmp_connect_callback_fn *lmp_connect_callback;
#endif
#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    ump_connect_callback_fn *ump_connect_callback;
#endif
#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
  multihop_connect_callback_fn *multihop_connect_callback;
#endif
};

errval_t idc_export_service(struct idc_export *e);
void idc_export_init(void);

__END_DECLS

#endif
