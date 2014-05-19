/**
 * \file
 * \brief LMP endpoints declarations
 */

/*
 * Copyright (c) 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_LMP_ENDPOINTS_H
#define LIBBARRELFISH_LMP_ENDPOINTS_H

#include <sys/cdefs.h>

#include <barrelfish/waitset.h>
#include <barrelfish_kpi/lmp.h>

__BEGIN_DECLS

/// In-endpoint size of a maximum-sized LMP message plus header
#define LMP_RECV_LENGTH         (LMP_MSG_LENGTH + LMP_RECV_HEADER_LENGTH)

/// Default size of LMP endpoint buffer (in words), must be >= LMP_RECV_LENGTH
#define DEFAULT_LMP_BUF_WORDS           (LMP_RECV_LENGTH * 2)

/// LMP endpoint structure (including data accessed only by user code)
struct lmp_endpoint {
    struct lmp_endpoint *next, *prev; ///< Next/prev endpoint in poll list
    struct capref recv_slot;///< Receive slot
    struct waitset_chanstate waitset_state; ///< Waitset per-channel state
    uint32_t buflen;        ///< Length of endpoint buffer (in words)
    uint32_t seen;          ///< Position in buffer processed by poll loop, but
                            ///< not necessarily consumed by the user
    struct lmp_endpoint_kern k; ///< Public part (shared with kernel)
    /* buffer beyond end of struct */
};


/**
 * \brief Message layout in user's buffer.
 *
 * Note that the kernel never delivers a message like this.
 */
struct lmp_recv_buf {
    size_t msglen;      ///< Length of message payload (in words)
    size_t buflen;      ///< Length of entire buffer (in words)
    uintptr_t words[0]; ///< Payload (variable length)
};

/// Compute the size needed for an lmp_recv_buf buffer
#define LMP_RECV_BUF_SIZE(n) (sizeof(struct lmp_recv_buf) + ((n)*sizeof(uintptr_t)))

/// Fixed-length version of #lmp_recv_buf
struct lmp_recv_msg {
    struct lmp_recv_buf buf;
    uintptr_t words[LMP_MSG_LENGTH]; ///< Payload (fixed length)
};

/// Static initialiser for lmp_recv_msg
#define LMP_RECV_MSG_INIT { .buf.buflen = LMP_MSG_LENGTH };

errval_t lmp_endpoint_alloc(size_t buflen, struct lmp_endpoint **retep);
void lmp_endpoint_free(struct lmp_endpoint *ep);
errval_t lmp_endpoint_create_in_slot(size_t buflen, struct capref dest,
                                     struct lmp_endpoint **retep);
void lmp_endpoint_set_recv_slot(struct lmp_endpoint *ep, struct capref slot);
bool lmp_endpoint_can_recv(struct lmp_endpoint *ep);
void lmp_endpoints_poll_disabled(dispatcher_handle_t handle);
errval_t lmp_endpoint_recv(struct lmp_endpoint *ep, struct lmp_recv_buf *buf,
                           struct capref *cap);
errval_t lmp_endpoint_register(struct lmp_endpoint *ep, struct waitset *ws,
                               struct event_closure closure);
errval_t lmp_endpoint_deregister(struct lmp_endpoint *ep);
void lmp_endpoint_migrate(struct lmp_endpoint *ep, struct waitset *ws);
void lmp_endpoint_store_lrpc_disabled(struct lmp_endpoint *ep, uint32_t bufpos,
                                      uintptr_t arg1, uintptr_t arg2,
                                      uintptr_t arg3, uintptr_t arg4);
void lmp_endpoint_init(void);

__END_DECLS

#endif // LIBBARRELFISH_LMP_ENDPOINTS_H
