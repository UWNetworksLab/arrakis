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

#ifndef ARCH_X86_32_BARRELFISH_LMP_CHAN_H
#define ARCH_X86_32_BARRELFISH_LMP_CHAN_H

#include <barrelfish/syscall_arch.h>
#include <barrelfish/caddr.h>
#include <barrelfish_kpi/lmp.h>

/**
 * \brief Send a message on the given LMP channel, if possible
 *
 * Non-blocking, may fail if there is no space in the receiver's endpoint.
 *
 * \param ep Remote endpoint cap
 * \param flags LMP send flags
 * \param send_cap (Optional) capability to send with the message
 * \param length_words Length of the message in words; payload beyond this
 *                      size will not be delivered
 * \param arg1..N Message payload
 */
static inline errval_t lmp_ep_send(struct capref ep, lmp_send_flags_t flags,
                                   struct capref send_cap, uint8_t length_words,
                                   uintptr_t arg1, uintptr_t arg2, uintptr_t arg3,
                                   uintptr_t arg4)
{
    uint8_t invoke_bits = get_cap_valid_bits(ep);
    capaddr_t invoke_cptr = get_cap_addr(ep) >> (CPTR_BITS - invoke_bits);

    uint8_t send_bits = get_cap_valid_bits(send_cap);
    capaddr_t send_cptr = get_cap_addr(send_cap) >> (CPTR_BITS - send_bits);

    assert(length_words <= LMP_MSG_LENGTH);

    return syscall7((length_words << 28) | ((flags & 0xf) << 24)
                    | (invoke_bits << 16) | (send_bits << 8) | SYSCALL_INVOKE,
                    invoke_cptr, send_cptr, arg1, arg2, arg3, arg4).error;
}

#define lmp_ep_send4(ep, flags, send_cap, a, b, c, d) \
  lmp_ep_send((ep), (flags), (send_cap), 4, (a), (b), (c), (d))
#define lmp_ep_send3(ep, flags, send_cap, a, b, c) \
  lmp_ep_send((ep), (flags), (send_cap), 3, (a), (b), (c), 0)
#define lmp_ep_send2(ep, flags, send_cap, a, b) \
  lmp_ep_send((ep), (flags), (send_cap), 2, (a), (b), 0, 0)
#define lmp_ep_send1(ep, flags, send_cap, a) \
  lmp_ep_send((ep), (flags), (send_cap), 1, (a), 0, 0, 0)
#define lmp_ep_send0(ep, flags, send_cap) \
  lmp_ep_send((ep), (flags), (send_cap), 0, 0, 0, 0, 0)

#define lmp_chan_send(lc, flags, send_cap, len, a, b, c, d) \
  lmp_ep_send((lc)->remote_cap, (flags), (send_cap), (len), (a), (b), (c), (d))

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

#endif
