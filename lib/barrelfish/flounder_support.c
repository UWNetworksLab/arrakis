/**
 * \file
 * \brief Support code for Flounder-generated stubs
 */

/*
 * Copyright (c) 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/monitor_client.h>
#include <barrelfish/waitset_chan.h>
#include <flounder/flounder_support.h>
#include <flounder/flounder_support_caps.h>
#include <if/monitor_defs.h>

/*
 * NB: many of these functions are trivial, but exist so that we don't need to
 * expose private libbarrelfish headers or generated flounder headers to every
 * generated stub
 */

static void dummy_event_handler(void *arg)
{
}

struct event_closure dummy_event_closure = {
    .handler = dummy_event_handler,
    .arg = NULL,
};

void flounder_support_trigger_chan(struct waitset_chanstate *wc)
{
    if (waitset_chan_is_registered(wc)) {
        errval_t err = waitset_chan_trigger(wc);
        assert(err_is_ok(err)); // shouldn't fail if registered
    }
}

void flounder_support_deregister_chan(struct waitset_chanstate *wc)
{
    if (waitset_chan_is_registered(wc)) {
        errval_t err = waitset_chan_deregister(wc);
        assert(err_is_ok(err)); // shouldn't fail if registered
    }
}

errval_t flounder_support_register(struct waitset *ws,
                                   struct waitset_chanstate *wc,
                                   struct event_closure ec,
                                   bool trigger_now)
{
    if (trigger_now) {
        return waitset_chan_trigger_closure(ws, wc, ec);
    } else {
        return waitset_chan_register(ws, wc, ec);
    }
}

void flounder_support_waitset_chanstate_init(struct waitset_chanstate *wc)
{
    waitset_chanstate_init(wc, CHANTYPE_FLOUNDER);
}

void flounder_support_waitset_chanstate_destroy(struct waitset_chanstate *wc)
{
    waitset_chanstate_destroy(wc);
}

errval_t flounder_support_change_monitor_waitset(struct monitor_binding *mb,
                                                 struct waitset *ws)
{
    return mb->change_waitset(mb, ws);
}

void flounder_support_monitor_mutex_enqueue(struct monitor_binding *mb,
                                            struct event_queue_node *qn,
                                            struct event_closure cl)
{
    event_mutex_enqueue_lock(&mb->mutex, qn, cl);
}

void flounder_support_monitor_mutex_unlock(struct monitor_binding *mb)
{
    event_mutex_unlock(&mb->mutex);
}

void flounder_support_migrate_notify(struct waitset_chanstate *chan,
                                     struct waitset *new_ws)
{
    waitset_chan_migrate(chan, new_ws);
}

static void cap_send_cont(void *arg)
{
    struct flounder_cap_state *s = arg;
    s->cap_send_continuation(s->binding);
}

errval_t flounder_stub_send_cap(struct flounder_cap_state *s,
                                struct monitor_binding *mb,
                                uintptr_t monitor_id,
                                struct capref cap, bool give_away,
                                void (*cont)(void *st))
{
    errval_t err;

    s->cap_send_continuation = cont;

    err = mb->tx_vtbl.cap_send_request(mb, MKCONT(cap_send_cont, s), monitor_id,
                                       cap, s->tx_capnum, give_away);
    if (err_is_ok(err)) {
        s->tx_capnum++;
        return err;
    } else if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
        // register to retry
        return mb->register_send(mb, mb->waitset, MKCONT(cap_send_cont, s));
    } else {
        return err_push(err, LIB_ERR_MONITOR_CAP_SEND);
    }
}

#if defined(CONFIG_INTERCONNECT_DRIVER_UMP)
static void flounder_stub_cap_state_init(struct flounder_cap_state *s, void *binding)
{
    s->tx_cap_ack = false;
    s->rx_cap_ack = false;
    s->monitor_mutex_held = false;
    s->tx_capnum = 0;
    s->rx_capnum = 0;
    s->binding = binding;
}
#endif // UMP

static uintptr_t getword(const uint8_t *buf, size_t *pos, size_t len)
{
    uintptr_t word = 0;

    for (int i = 0; *pos < len && i < sizeof(uintptr_t); i++) {
        // read and shift in next byte
        word <<= NBBY;
        word |= buf[(*pos)++];
    }

    return word;
}

static void putword(uintptr_t word, uint8_t *buf, size_t *pos, size_t len)
{
    const size_t shift_bits = (sizeof(uintptr_t) - 1) * NBBY;

    // throw away leading zeros if this is the end of the message
    if (len - *pos < sizeof(uintptr_t)) {
        word <<= NBBY * (sizeof(uintptr_t) - (len - *pos));
    }

    for (int i = 0; *pos < len && i < sizeof(uintptr_t); i++) {
        buf[(*pos)++] = (word & ((uintptr_t)0xff << shift_bits)) >> shift_bits;
        word <<= NBBY;
    }
}

#ifdef CONFIG_INTERCONNECT_DRIVER_LMP

#include <flounder/flounder_support_lmp.h>

errval_t flounder_stub_lmp_send_buf(struct lmp_chan *chan,
                                    lmp_send_flags_t flags, const void *bufp,
                                    size_t len, size_t *pos)
{
    errval_t err;
    const uint8_t *buf = bufp;

    do {
        // compute number of words for this message
        size_t msg_words = DIVIDE_ROUND_UP(len - *pos, sizeof(uintptr_t));
        if (*pos == 0) { // space for header
            msg_words += 1;
        }
        if (msg_words > LMP_MSG_LENGTH)
            msg_words = LMP_MSG_LENGTH;

        // store initial position for retry
        size_t restartpos = *pos;

        // is this the start of the string?
        uintptr_t w1;
        if (*pos == 0) {
            // if so, send the length in the first word
            w1 = len;
        } else {
            // otherwise use it for payload
            w1 = getword(buf, pos, len);
        }

        // get the rest of the message, painfully
#if LMP_MSG_LENGTH > 1
        uintptr_t w2 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 2
        uintptr_t w3 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 3
        uintptr_t w4 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 4
        uintptr_t w5 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 5
        uintptr_t w6 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 6
        uintptr_t w7 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 7
        uintptr_t w8 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 8
        uintptr_t w9 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 9
        uintptr_t w10 = getword(buf, pos, len);
#endif
#if LMP_MSG_LENGTH > 10
#error Need to unroll message send loop further
#endif

        // only set the sync flag if this is the last fragment
        lmp_send_flags_t f = flags;
        if (*pos < len) {
            f &= ~LMP_FLAG_SYNC;
        }

        // try to send
        err = lmp_chan_send(chan, f, NULL_CAP, msg_words, w1
#if LMP_MSG_LENGTH > 1
                            , w2
#endif
#if LMP_MSG_LENGTH > 2
                            , w3
#endif
#if LMP_MSG_LENGTH > 3
                            , w4
#endif
#if LMP_MSG_LENGTH > 4
                            , w5
#endif
#if LMP_MSG_LENGTH > 5
                            , w6
#endif
#if LMP_MSG_LENGTH > 6
                            , w7
#endif
#if LMP_MSG_LENGTH > 7
                            , w8
#endif
#if LMP_MSG_LENGTH > 8
                            , w9
#endif
#if LMP_MSG_LENGTH > 9
                            , w10
#endif
                            );

        if (err_is_fail(err)) {
            *pos = restartpos;
        }
    } while (err_is_ok(err) && *pos < len);

    // do we need to send more? if not, zero out our state for the next send
    if (*pos >= len) {
        *pos = 0;
    }

    return err;
}

errval_t flounder_stub_lmp_recv_buf(struct lmp_recv_msg *msg, void **bufp,
                                    size_t *len, size_t *pos)
{
    int msgpos;

    // is this the first fragment?
    // if so, unmarshall the length and allocate a buffer
    if (*pos == 0) {
        if (msg->buf.msglen == 0) {
            return FLOUNDER_ERR_RX_INVALID_LENGTH;
        }

        *len = msg->words[0];
        if (*len == 0) {
            *bufp = NULL;
        } else {
            *bufp = malloc(*len);
            if (*bufp == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
        }
        msgpos = 1;
    } else {
        msgpos = 0;
    }

    uint8_t *buf = *bufp;

    // copy remainder of fragment to buffer
    for (; msgpos < msg->buf.msglen && *pos < *len; msgpos++) {
        putword(msg->words[msgpos], buf, pos, *len);
    }

    // are we done?
    if (*pos < *len) {
        return FLOUNDER_ERR_BUF_RECV_MORE;
    } else {
        // reset state for next buffer
        *pos = 0;
        return SYS_ERR_OK;
    }
}

errval_t flounder_stub_lmp_send_string(struct lmp_chan *chan,
                                       lmp_send_flags_t flags,
                                       const char *str,
                                       size_t *pos, size_t *len)
{
    // compute length, if this is the first call
    if (*pos == 0) {
        if (str == NULL) {
            *len = 0;
        } else {
            // send the '\0', making it easy to reuse the buffer code
            *len = strlen(str) + 1;
        }
    }

    return flounder_stub_lmp_send_buf(chan, flags, str, *len, pos);
}

errval_t flounder_stub_lmp_recv_string(struct lmp_recv_msg *msg, char **strp,
                                       size_t *pos, size_t *len)
{
    return flounder_stub_lmp_recv_buf(msg, (void **)strp, len, pos);
}
#endif // CONFIG_INTERCONNECT_DRIVER_LMP


#ifdef CONFIG_INTERCONNECT_DRIVER_UMP

#include <flounder/flounder_support_ump.h>

void flounder_stub_ump_state_init(struct flounder_ump_state *s, void *binding)
{
    s->next_id = 1;
    s->seq_id = 0;
    s->ack_id = 0;
    s->last_ack = 0;
    flounder_stub_cap_state_init(&s->capst, binding);
}

errval_t flounder_stub_ump_send_buf(struct flounder_ump_state *s,
                                       int msgnum, const void *bufp,
                                       size_t len, size_t *pos)
{
    volatile struct ump_message *msg;
    const uint8_t *buf = bufp;
    struct ump_control ctrl;
    int msgpos;

    do {
        if (!flounder_stub_ump_can_send(s)) {
            return FLOUNDER_ERR_BUF_SEND_MORE;
        }

        msg = ump_chan_get_next(&s->chan, &ctrl);
        flounder_stub_ump_control_fill(s, &ctrl, msgnum);

        // is this the start of the buffer?
        if (*pos == 0) {
            // if so, send the length in the first word
            msg->data[0] = len;
            // XXX: skip as many words as the largest word size
            msgpos = (sizeof(uint64_t) / sizeof(uintptr_t));
        } else {
            // otherwise use it for payload
            msgpos = 0;
        }

        for (; msgpos < UMP_PAYLOAD_WORDS && *pos < len; msgpos++) {
            msg->data[msgpos] = getword(buf, pos, len);
        }

        flounder_stub_ump_barrier();
        msg->header.control = ctrl;
    } while (*pos < len);

    // we're done. zero out our state for the next buffer
    assert(*pos >= len);
    *pos = 0;

    return SYS_ERR_OK;
}

errval_t flounder_stub_ump_recv_buf(volatile struct ump_message *msg,
                                    void **bufp, size_t *len, size_t *pos)
{
    int msgpos;

    // is this the first fragment?
    // if so, unmarshall the length and allocate a buffer
    if (*pos == 0) {
        *len = msg->data[0];
        if (*len == 0) {
            *bufp = NULL;
        } else {
            *bufp = malloc(*len);
            if (*bufp == NULL) {
                return LIB_ERR_MALLOC_FAIL;
            }
        }

        // XXX: skip as many words as the largest word size
        msgpos = (sizeof(uint64_t) / sizeof(uintptr_t));
    } else {
        msgpos = 0;
    }

    uint8_t *buf = *bufp;

    // copy remainder of fragment to buffer
    for (; msgpos < UMP_PAYLOAD_WORDS && *pos < *len; msgpos++) {
        putword(msg->data[msgpos], buf, pos, *len);
    }

    // are we done?
    if (*pos < *len) {
        return FLOUNDER_ERR_BUF_RECV_MORE;
    } else {
        // reset state for next buffer
        *pos = 0;
        return SYS_ERR_OK;
    }
}

errval_t flounder_stub_ump_send_string(struct flounder_ump_state *s,
                                       int msgnum, const char *str,
                                       size_t *pos, size_t *len)
{
    // compute length, if this is the first call
    if (*pos == 0) {
        if (str == NULL) {
            *len = 0;
        } else {
            // send the '\0', making it easy to reuse the buffer code
            *len = strlen(str) + 1;
        }
    }

    return flounder_stub_ump_send_buf(s, msgnum, str, *len, pos);
}

errval_t flounder_stub_ump_recv_string(volatile struct ump_message *msg,
                                       char **strp, size_t *pos, size_t *len)
{
    return flounder_stub_ump_recv_buf(msg, (void **)strp, len, pos);
}

#endif // CONFIG_INTERCONNECT_DRIVER_UMP
