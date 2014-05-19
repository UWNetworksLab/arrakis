/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_LMP_CHAN_H
#define ARCH_X86_64_BARRELFISH_LMP_CHAN_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/lmp.h>

/**
 * \brief Send a message on the given LMP endpoint, if possible
 *
 * Non-blocking, may fail if there is no space in the receiver's endpoint.
 *
 * \param ep Remote endpoint
 * \param flags LMP send flags
 * \param send_cap (Optional) capability to send with the message
 * \param length_words Length of the message in words; payload beyond this
 *                      size will not be delivered
 * \param arg1..N Message payload
 */
static inline errval_t lmp_ep_send(struct capref ep, lmp_send_flags_t flags,
                                   struct capref send_cap, uint8_t length_words,
                                   uint64_t arg1, uint64_t arg2, uint64_t arg3,
                                   uint64_t arg4, uint64_t arg5, uint64_t arg6,
                                   uint64_t arg7, uint64_t arg8, uint64_t arg9,
                                   uint64_t arg10)
{
    uint8_t send_bits = get_cap_valid_bits(send_cap);
    capaddr_t send_cptr = get_cap_addr(send_cap) >> (CPTR_BITS - send_bits);

    if(debug_notify_syscall) {
        printf("memcached: lmp_ep_send while forbidden from %p, %p, %p\n",
               __builtin_return_address(0),
               __builtin_return_address(1),
               __builtin_return_address(2));
    }

#ifndef TRACE_DISABLE_LRPC
    // Do an LRPC if possible
    if (send_cptr == 0 && send_bits == 0          // Not sending a cap
        && ep.cnode.address == CPTR_ROOTCN        // EP in rootcn
        && (flags & LMP_FLAG_SYNC) != 0           // sync option
        && length_words <= LRPC_MSG_LENGTH) {     // Check length

        assert(LRPC_MSG_LENGTH == 4);
        return syscall6(SYSCALL_LRPC, ep.slot, arg1, arg2, arg3, arg4).error;
    }
#endif

    uint8_t invoke_bits = get_cap_valid_bits(ep);
    capaddr_t invoke_cptr = get_cap_addr(ep) >> (CPTR_BITS - invoke_bits);

    return syscall(SYSCALL_INVOKE,
                   (uint64_t)invoke_cptr << 32 | (uint64_t)send_bits << 24 |
                   (uint64_t)invoke_bits << 16 | (uint64_t)length_words << 8 |
                   flags, send_cptr,
                   arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
                   arg10).error;
}

#define lmp_ep_send10(ep, flags, send_cap, a, b, c, d, e, f, g, h, i, j) \
  lmp_ep_send((ep), (flags), (send_cap), 10, (a), (b), (c), (d), (e), (f), (g), (h), (i), (j))
#define lmp_ep_send9(ep, flags, send_cap, a, b, c, d, e, f, g, h, i) \
  lmp_ep_send((ep), (flags), (send_cap), 9, (a), (b), (c), (d), (e), (f), (g), (h), (i), 0)
#define lmp_ep_send8(ep, flags, send_cap, a, b, c, d, e, f, g, h) \
  lmp_ep_send((ep), (flags), (send_cap), 8, (a), (b), (c), (d), (e), (f), (g), (h), 0, 0)
#define lmp_ep_send7(ep, flags, send_cap, a, b, c, d, e, f, g) \
  lmp_ep_send((ep), (flags), (send_cap), 7, (a), (b), (c), (d), (e), (f), (g), 0, 0, 0)
#define lmp_ep_send6(ep, flags, send_cap, a, b, c, d, e, f) \
  lmp_ep_send((ep), (flags), (send_cap), 6, (a), (b), (c), (d), (e), (f), 0, 0, 0, 0)
#define lmp_ep_send5(ep, flags, send_cap, a, b, c, d, e) \
  lmp_ep_send((ep), (flags), (send_cap), 5, (a), (b), (c), (d), (e), 0, 0, 0, 0, 0)
#define lmp_ep_send4(ep, flags, send_cap, a, b, c, d) \
  lmp_ep_send((ep), (flags), (send_cap), 4, (a), (b), (c), (d), 0, 0, 0, 0, 0, 0)
#define lmp_ep_send3(ep, flags, send_cap, a, b, c) \
  lmp_ep_send((ep), (flags), (send_cap), 3, (a), (b), (c), 0, 0, 0, 0, 0, 0, 0)
#define lmp_ep_send2(ep, flags, send_cap, a, b) \
  lmp_ep_send((ep), (flags), (send_cap), 2, (a), (b), 0, 0, 0, 0, 0, 0, 0, 0)
#define lmp_ep_send1(ep, flags, send_cap, a) \
  lmp_ep_send((ep), (flags), (send_cap), 1, (a), 0, 0, 0, 0, 0, 0, 0, 0, 0)
#define lmp_ep_send0(ep, flags, send_cap) \
  lmp_ep_send((ep), (flags), (send_cap), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

#define lmp_chan_send(lc, flags, send_cap, len, a, b, c, d, e, f, g, h, i, j) \
  lmp_ep_send((lc)->remote_cap, (flags), (send_cap), (len), (a), (b), (c), (d), (e), (f), (g), (h), (i), (j))

#define lmp_chan_send10(lc, flags, send_cap, a, b, c, d, e, f, g, h, i, j) \
  lmp_ep_send10((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e), (f), (g), (h), (i), (j))
#define lmp_chan_send9(lc, flags, send_cap, a, b, c, d, e, f, g, h, i) \
  lmp_ep_send9((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e), (f), (g), (h), (i))
#define lmp_chan_send8(lc, flags, send_cap, a, b, c, d, e, f, g, h) \
  lmp_ep_send8((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e), (f), (g), (h))
#define lmp_chan_send7(lc, flags, send_cap, a, b, c, d, e, f, g) \
  lmp_ep_send7((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e), (f), (g))
#define lmp_chan_send6(lc, flags, send_cap, a, b, c, d, e, f) \
  lmp_ep_send6((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e), (f))
#define lmp_chan_send5(lc, flags, send_cap, a, b, c, d, e) \
  lmp_ep_send5((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d), (e))
#define lmp_chan_send4(lc, flags, send_cap, a, b, c, d) \
  lmp_ep_send4((lc)->remote_cap, (flags), (send_cap), (a), (b), (c), (d))
#define lmp_chan_send3(lc, flags, send_cap, a, b, c) \
  lmp_ep_send3((lc)->remote_cap, (flags), (send_cap), (a), (b), (c))
#define lmp_chan_send2(lc, flags, send_cap, a, b) \
  lmp_ep_send2((lc)->remote_cap, (flags), (send_cap), (a), (b))
#define lmp_chan_send1(lc, flags, send_cap, a) \
  lmp_ep_send1((lc)->remote_cap, (flags), (send_cap), (a))
#define lmp_chan_send0(lc, flags, send_cap) \
  lmp_ep_send0((lc)->remote_cap, (flags), (send_cap))

#endif // ARCH_X86_64_BARRELFISH_LMP_CHAN_H
