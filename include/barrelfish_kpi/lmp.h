/**
 * \file
 * \brief LMP declarations
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LMP_H
#define LMP_H

#include <barrelfish_kpi/lmp_arch.h>

#define LMP_RECV_HEADER_LENGTH  1 /* word */

#ifndef __ASSEMBLER__

/// Incoming LMP endpoint message buffer
struct lmp_endpoint_kern {
    capaddr_t     recv_cptr;  ///< CSpace address of CNode to receive caps
    capaddr_t     recv_slot;  ///< Slot number in #recv_cptr
    uint8_t     recv_bits;  ///< Valid bits in #recv_cptr
    uint32_t    delivered;  ///< Position in buffer (words delivered by kernel)
    uint32_t    consumed;   ///< Position in buffer (words consumed by user)
    uintptr_t   buf[];      ///< Buffer for async LMP messages
};

/// LMP send flags
typedef enum lmp_send_flag {
    LMP_FLAG_SYNC       = 1 << 0,
    LMP_FLAG_YIELD      = 1 << 1
} lmp_send_flags_t;

#define LMP_SEND_FLAGS_DEFAULT (LMP_FLAG_SYNC | LMP_FLAG_YIELD)

/**
 * \brief LMP receiver-side header.
 */
union lmp_recv_header {
    uintptr_t raw;
    struct {
        struct __attribute__ ((__packed__)) {
            uint8_t captransfer :1;         ///< A cap was transferred
        } flags;
        uint8_t     length;                 ///< Length of payload in words
    } x;
};
STATIC_ASSERT_SIZEOF(union lmp_recv_header, sizeof(uintptr_t));

#endif // __ASSEMBLER__
#endif // LMP_H
