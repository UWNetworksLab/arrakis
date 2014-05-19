/**
 * \file
 * \brief Legacy IDC buffer format. PLEASE DO NOT USE FOR ANY NEW CODE!
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_LEGACY_IDC_BUFFER_H
#define BARRELFISH_LEGACY_IDC_BUFFER_H

/**
 * \brief Maximum total length of IDC message (header + payload)
 *
 * Determined by number of registers available to transfer messages.
 */
// XXX Doesnt that sound ever so slightly machine dependent to you ???
// XXX See also related magic in syscalls.c
#define IDC_SEND_LENGTH         12

/// Length of IDC message headers
// XXX Is there a good reason that this isnt just done with sizeof() ???
#if defined(__SIZEOF_POINTER__)
#if __SIZEOF_POINTER__ == 4
#define IDC_SEND_HEADER_LENGTH 3
#elif __SIZEOF_POINTER__ == 8
#define IDC_SEND_HEADER_LENGTH 2
#else
#error "Unknown pointer size"
#endif
#else
#error "Unknown pointer size"
#endif

/// Length of IDC message payload
#define IDC_MSG_LENGTH          (IDC_SEND_LENGTH - IDC_SEND_HEADER_LENGTH)

#include <assert.h>
#include <string.h>
#include <stdlib.h> // for abort

#if __x86_64__
typedef unsigned int idc_flag_t;
#elif __i386__
typedef unsigned int idc_flag_t;
#elif __arm__
typedef uint8_t idc_flag_t;
#else
#error "Unknown architecture."
#endif

#ifdef IN_KERNEL
#define idc_fatal_error(x, ...) panic(x, __VA_ARGS__)
#else // IN_KERNEL
#define idc_fatal_error(x, ...) abort();
#endif // IN_KERNEL

/**
 * \brief IDC sender-side header.
 */
union idc_send_header {
    uintptr_t raw[IDC_SEND_HEADER_LENGTH];
    struct {
        struct __attribute__ ((__packed__)) {
            idc_flag_t sync   :1; ///< Synchronous call (yields caller's timeslice)
            idc_flag_t yield  :1; ///< Yield to receiver if disabled/unable to transfer
        } flags;
        uint8_t     length;         ///< Length of payload in words
        uint8_t     invoke_bits;    ///< Valid bits in invoke_cptr
        uint8_t     send_bits;      ///< Valid bits in send_cptr
        capaddr_t     invoke_cptr;    ///< Cap to invoke
        capaddr_t     send_cptr;      ///< Cap to send or #CPTR_NULL
    } x;
};

/**
 * \brief IDC receiver-side header.
 */
union idc_recv_header {
    uintptr_t raw;
    struct {
        struct __attribute__ ((__packed__)) {
            idc_flag_t captransfer :1;    ///< A cap was transferred
        } flags;
        uint8_t     length;                 ///< Length of payload in words
    } x;
};

/**
 * \brief Message layout on sender side.
 */
union idc_send_msg_body {
    uintptr_t       raw[IDC_SEND_LENGTH]; ///< Raw message contents

    struct {
        union idc_send_header header;
        uintptr_t words[IDC_MSG_LENGTH];  ///< Message payload
    } x;
};

/**
 * \brief Message layout on receiver side.
 */
struct idc_recv_msg_body {
    union idc_recv_header header;     ///< Header
    uintptr_t words[IDC_MSG_LENGTH];  ///< Message payload
};

/// IDC send message, with position indicator for marshalling
struct idc_send_msg {
    int                         pos;    ///< Index in message
    union idc_send_msg_body     u;      ///< Actual transferred message content
};

/// IDC receive message, with position indicator for demarshalling
struct idc_recv_msg {
    int                         pos;    ///< Index in message
    struct idc_recv_msg_body    msg;    ///< Actual transferred message content
};

static inline uintptr_t *idc_get_raw(struct idc_send_msg *msg)
{
    return msg->u.raw;
}

static inline errval_t idc_msg_decode_word(struct idc_recv_msg *msg,
                                           uintptr_t *ret)
{
    assert(ret != NULL);
    if(msg->pos < msg->msg.header.x.length) {
        *ret = msg->msg.words[msg->pos++];
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_IDC_MSG_BOUNDS;
    }
}

/**
 * \brief decode a word from the message
 * \returns the word, or 0 if out of bounds
 * \bug XXX: do not use this for new code!
 */
static inline uintptr_t idc_msg_decode_word_or_zero(struct idc_recv_msg *msg)
{
    uintptr_t w;
    if (err_is_ok(idc_msg_decode_word(msg, &w))) {
        return w;
    } else {
        return 0;
    }
}

static inline void idc_msg_encode_word(struct idc_send_msg *msg, uintptr_t word)
{
    if(msg->pos < IDC_MSG_LENGTH) {
        msg->u.x.words[msg->pos++] = word;
        msg->u.x.header.x.length = msg->pos;
    }
    else {
        idc_fatal_error("%s: msg %p pos %d",
                        __FUNCTION__, msg, (int)msg->pos);
    }
}

static inline void idc_msg_init(struct idc_send_msg *msg)
{
    int i;
    msg->pos = 0;
    for (i = 0; i < IDC_SEND_HEADER_LENGTH; i++) {
        msg->u.raw[i] = 0;
    }

    // Sync, yielding IDC is the default
    msg->u.x.header.x.flags.sync = 1;
    msg->u.x.header.x.flags.yield = 1;
}

// Set the msg flag to be async
static inline void idc_msg_set_async(struct idc_send_msg *msg)
{
    msg->u.x.header.x.flags.sync = 0;
}

// Macro cleanup
#undef idc_fatal_error

#endif // BARRELFISH_LEGACY_IDC_BUFFER_H
