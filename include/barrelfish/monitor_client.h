/**
 * \file
 * \brief Client for interacting with the monitor
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_MONITOR_CLIENT_H
#define BARRELFISH_MONITOR_CLIENT_H

#include <sys/cdefs.h>

__BEGIN_DECLS

struct monitor_lmp_binding;

/// Handlers for incoming/outgoing capabilities on the monitor binding
struct monitor_cap_handlers {
    void *st; ///< state pointer passed to handler functions
    void (*cap_receive_handler)(void *st, errval_t success, struct capref cap,
                                uint32_t capid);
};

/* XXX: duplicate of monitor_bind_continuation_fn in generated code */
typedef void monitor_bind_cont_fn(void *st, errval_t err,
                                  struct monitor_binding *_binding);

errval_t monitor_client_lmp_bind(struct monitor_lmp_binding *mcb,
                                 monitor_bind_cont_fn *cont, void *st,
                                 struct waitset *ws, size_t lmp_buflen_words);

errval_t monitor_client_lmp_accept(struct monitor_lmp_binding *mcb,
                                   struct waitset *ws, size_t lmp_buflen_words);

errval_t monitor_client_new_binding(monitor_bind_cont_fn *cont, void *st,
                                    struct waitset *ws, size_t lmp_buflen_words);


errval_t monitor_client_blocking_rpc_init(void);

errval_t monitor_cap_set_remote(struct capref cap, bool remote);

__END_DECLS

#endif // BARRELFISH_MONITOR_CLIENT_H
