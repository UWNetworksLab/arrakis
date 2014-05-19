/**
 * \file
 * \brief RPC definitions
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _RPC_H
#define _RPC_H

#include <barrelfish/deferred.h>

/**
 * A reply to a call message can take on two forms: The message was
 * either accepted or rejected.
 */
enum rpc_reply_stat {
    RPC_MSG_ACCEPTED = 0,
    RPC_MSG_DENIED   = 1
};

/**
 * Given that a call message was accepted, the following is the status
 * of an attempt to call a remote procedure.
 */
enum rpc_accept_stat {
    RPC_SUCCESS       = 0, ///< RPC executed successfully
    RPC_PROG_UNAVAIL  = 1, ///< remote hasn't exported program
    RPC_PROG_MISMATCH = 2, ///< remote can't support version #
    RPC_PROC_UNAVAIL  = 3, ///< program can't support procedure
    RPC_GARBAGE_ARGS  = 4  ///< procedure can't decode params
};

/// Reasons why a call message was rejected
enum rpc_reject_stat {
    RPC_RPC_MISMATCH    = 0, ///< RPC version number != 2
    RPC_AUTH_ERROR      = 1  ///< remote can't authenticate caller
};

/// Why authentication failed
enum rpc_auth_stat {
    RPC_AUTH_BADCRED      = 1,  ///< bad credentials (seal broken)
    RPC_AUTH_REJECTEDCRED = 2,  ///< client must begin new session
    RPC_AUTH_BADVERF      = 3,  ///< bad verifier (seal broken)
    RPC_AUTH_REJECTEDVERF = 4,  ///< verifier expired or replayed
    RPC_AUTH_TOOWEAK      = 5   ///< rejected for security reasons
};

#define RPC_HTABLE_SIZE 128

/// RPC client instance data
struct rpc_client {
    struct udp_pcb *pcb;    ///< UDP connection data in LWIP
    struct ip_addr server;  ///< Server IP
    struct rpc_call *call_hash[RPC_HTABLE_SIZE];

    uint32_t nextxid;       ///< Next transaction ID
    struct periodic_event timer;    ///< Retransmit timer
};

/**
 * \brief Callback function for RPC reply handlers
 *
 * \param rpc_client RPC client instance pointer
 * \param arg1,arg2 Opaque argument values provided to rpc_call()
 * \param replystat Reply status (RPC_MSG_ACCEPTED on success)
 * \param acceptstat Accept status (RPC_SUCCESS on success)
 * \param reply_xdr XDR pointer for deserialising any return values
 *
 * \note Two opaque argument values are provided to the callback, as this
 * allows the NFS code to use them to store the callback pointer and argument
 * for its own callbacks without further memory allocation.
 */
typedef void (*rpc_callback_t)(struct rpc_client *rpc_client, void *arg1,
                               void *arg2, uint32_t replystat,
                               uint32_t acceptstat, XDR *reply_xdr);

err_t rpc_init(struct rpc_client *client, struct ip_addr server);
void rpc_destroy(struct rpc_client *client);
err_t rpc_call(struct rpc_client *client, uint16_t port, uint32_t prog,
               uint32_t vers, uint32_t proc, xdrproc_t args_xdrproc, void *args,
               size_t args_size, rpc_callback_t callback, void *cbarg1,
               void *cbarg2);

#endif // _RPC_H
