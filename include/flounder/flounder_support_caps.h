/**
 * \file
 * \brief Flounder support code for interconnects with indirect (via monitor)
 *      cap transfer
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_CAPS_H
#define __FLOUNDER_SUPPORT_CAPS_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/// State for indirect (via monitor) cap tx/rx machinery
struct flounder_cap_state {
    bool tx_cap_ack;    ///< Waiting to send a cap ack (for current rx message)
    bool rx_cap_ack;    ///< Have seen a cap ack (for current tx message)
    bool monitor_mutex_held; ///< We hold the monitor binding mutex (for current tx message)
    int tx_capnum;      ///< Number of capabilities transmitted
    int rx_capnum;      ///< Number of capabilities received

    /// Continuation for cap transmission
    void (*cap_send_continuation)(void *binding);
    void *binding;
};

errval_t flounder_stub_send_cap(struct flounder_cap_state *s,
                                struct monitor_binding *mb,
                                uintptr_t monitor_id,
                                struct capref cap, bool give_away,
                                void (*cont)(void *st));

__END_DECLS

#endif // __FLOUNDER_SUPPORT_CAPS_H
