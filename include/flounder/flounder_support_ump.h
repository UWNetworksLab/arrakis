/**
 * \file
 * \brief Prototypes for use by flounder-generated stubs
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_UMP_H
#define __FLOUNDER_SUPPORT_UMP_H

#include <sys/cdefs.h>

#include <barrelfish/ump_chan.h>
#include <flounder/flounder_support_caps.h>
#include <trace/trace.h>

__BEGIN_DECLS

/// Number of bits available for the message type in the header
#define FL_UMP_MSGTYPE_BITS (UMP_HEADER_BITS - UMP_INDEX_BITS)

/// Special message types
enum flounder_ump_msgtype {
    FL_UMP_ACK = 0,
    FL_UMP_CAP_ACK = (1 << FL_UMP_MSGTYPE_BITS) - 1,
};

struct flounder_ump_state {
    struct ump_chan chan;

    ump_index_t next_id;   ///< Sequence number of next message to be sent
    ump_index_t seq_id;    ///< Last sequence number received from remote
    ump_index_t ack_id;    ///< Last sequence number acknowledged by remote
    ump_index_t last_ack;  ///< Last acknowledgement we sent to remote

    struct flounder_cap_state capst; ///< State for indirect cap tx/rx machinery
};

void flounder_stub_ump_state_init(struct flounder_ump_state *s, void *binding);

errval_t flounder_stub_ump_send_string(struct flounder_ump_state *s,
                                       int msgnum, const char *str,
                                       size_t *pos, size_t *len);

errval_t flounder_stub_ump_recv_string(volatile struct ump_message *msg,
                                       char **str, size_t *pos, size_t *len);

errval_t flounder_stub_ump_send_buf(struct flounder_ump_state *s,
                                       int msgnum, const void *buf,
                                       size_t len, size_t *pos);

errval_t flounder_stub_ump_recv_buf(volatile struct ump_message *msg,
                                    void **buf, size_t *len, size_t *pos);

/// Computes (from seq/ack numbers) whether we can currently send on the channel
static inline bool flounder_stub_ump_can_send(struct flounder_ump_state *s) {
    return (ump_index_t)(s->next_id - s->ack_id) <= s->chan.max_send_msgs;
}

#define ENABLE_MESSAGE_PASSING_TRACE 1
/// Prepare a "control" word (header for each UMP message fragment)
static inline void flounder_stub_ump_control_fill(struct flounder_ump_state *s,
                                                  struct ump_control *ctrl,
                                                  int msgtype)
{
#if ENABLE_MESSAGE_PASSING_TRACE
    trace_event_raw((((uint64_t)0xEA)<<56) | 
                    ((uint64_t)s->chan.sendid << 12) | 
                    (s->next_id & 0xffff));
#endif // ENABLE_MESSAGE_PASSING_TRACE
    assert(s->chan.sendid != 0);
    assert(msgtype < (1 << FL_UMP_MSGTYPE_BITS)); // check for overflow
    ctrl->header = ((uintptr_t)msgtype << UMP_INDEX_BITS) | (uintptr_t)s->seq_id;
    s->last_ack = s->seq_id;
    s->next_id++;
}

/// Process a "control" word
static inline int flounder_stub_ump_control_process(struct flounder_ump_state *s,
                                                    struct ump_control ctrl)
{
#if ENABLE_MESSAGE_PASSING_TRACE
    trace_event_raw( (((uint64_t)0xEB)<<56) | 
                     ((uint64_t)s->chan.recvid << 12) | 
                     (s->seq_id & 0xffff));
#endif // ENABLE_MESSAGE_PASSING_TRACE
    s->ack_id = ctrl.header & UMP_INDEX_MASK;
    s->seq_id++;
    return ctrl.header >> UMP_INDEX_BITS;
}

/// Emit memory barrier needed between writing UMP payload and header
static inline void flounder_stub_ump_barrier(void)
{
#if defined(__i386__) || defined(__x86_64__) || defined(__scc__)
    /* the x86 memory model ensures ordering of stores, so all we need to do
     * is prevent the compiler from reordering the instructions */
    __asm volatile ("" : : : "memory");
#else
    /* use conservative GCC intrinsic */
    __sync_synchronize();
#endif
}

/// Should we send an ACK?
static inline bool flounder_stub_ump_needs_ack(struct flounder_ump_state *s)
{
    // send a forced ACK if the channel is full
    // FIXME: should probably send it only when "nearly" full
    return (ump_index_t)(s->seq_id - s->last_ack) >=
        (ump_index_t)(s->chan.max_recv_msgs - 1);
}
/// Send an explicit ACK
static inline void flounder_stub_ump_send_ack(struct flounder_ump_state *s)
{
    assert(flounder_stub_ump_can_send(s));
    struct ump_control ctrl;
    volatile struct ump_message *msg = ump_chan_get_next(&s->chan, &ctrl);
    flounder_stub_ump_control_fill(s, &ctrl, FL_UMP_ACK);
    msg->header.control = ctrl;
}

/// Send a cap ACK (message that we are ready to receive caps)
static inline void flounder_stub_ump_send_cap_ack(struct flounder_ump_state *s)
{
    assert(flounder_stub_ump_can_send(s));
    struct ump_control ctrl;
    volatile struct ump_message *msg = ump_chan_get_next(&s->chan, &ctrl);
    flounder_stub_ump_control_fill(s, &ctrl, FL_UMP_CAP_ACK);
    msg->header.control = ctrl;
}

__END_DECLS

#endif // __FLOUNDER_SUPPORT_UMP_H
