/**
 * \file
 * \brief Management of incoming LMP endpoints
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/dispatcher_arch.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/lmp_endpoints.h>
#include <barrelfish/caddr.h>
#include <barrelfish/waitset_chan.h>
#include "waitset_chan_priv.h"

static void endpoint_init(struct lmp_endpoint *ep)
{
    ep->k.delivered = ep->k.consumed = 0;
    ep->k.recv_cptr = 0;
    ep->k.recv_bits = 0;
    ep->seen = 0;
    waitset_chanstate_init(&ep->waitset_state, CHANTYPE_LMP_IN);
}

/**
 * \brief Allocate an LMP endpoint buffer on the current dispatcher
 *
 * In order to accomodate for the in-kernel sentinel word, the
 * allocated size of the buffer will be one larger than buflen.
 *
 * \param buflen  Length of incoming LMP buffer, in words
 * \param retep   Double pointer to LMP endpoint, filled-in with allocated EP
 */
errval_t lmp_endpoint_alloc(size_t buflen, struct lmp_endpoint **retep)
{
    // sanity-check buflen
    if (buflen <= LMP_RECV_LENGTH) {
        return LIB_ERR_LMP_BUFLEN_INVALID;
    }

    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dg = get_dispatcher_generic(handle);
    size_t epsize = sizeof(struct lmp_endpoint) + buflen * sizeof(uintptr_t);
    struct lmp_endpoint *ep = heap_alloc(&dg->lmp_endpoint_heap, epsize);
    if(ep == NULL) {
        disp_enable(handle);
        return LIB_ERR_NO_ENDPOINT_SPACE;
    }

    endpoint_init(ep);
    ep->buflen = buflen;

    disp_enable(handle);

    assert(retep != NULL);
    *retep = ep;
    return SYS_ERR_OK;
}

/**
 * \brief Free an LMP endpoint buffer on the current dispatcher
 *
 * Does not delete the endpoint capability nor free any receive slot.
 *
 * \param ep LMP endpoint
 */
void lmp_endpoint_free(struct lmp_endpoint *ep)
{
    assert(ep != NULL);
    waitset_chanstate_destroy(&ep->waitset_state);
    dispatcher_handle_t dhandle = disp_disable();
    struct dispatcher_generic *dg = get_dispatcher_generic(dhandle);
    heap_free(&dg->lmp_endpoint_heap, ep);
    disp_enable(dhandle);
}

/**
 * \brief Create endpoint to caller on current dispatcher in a specified slot.
 *
 * \param buflen  Length of incoming LMP buffer, in words
 * \param dest    Location of empty slot in which to create endpoint
 * \param retep   Double pointer to LMP endpoint, filled-in with allocated EP
 *
 * This function mints into the given slot an endpoint capability to the 
 * current dispatcher.
 */
errval_t lmp_endpoint_create_in_slot(size_t buflen, struct capref dest,
                                     struct lmp_endpoint **retep)
{
    struct lmp_endpoint *ep = NULL;
    errval_t err;

    // We increase buflen by 1 here to accomodate for in-kernel sentinel word
    buflen++;

    err = lmp_endpoint_alloc(buflen, &ep);
    if (err_is_fail(err)) {
        return err;
    }

    assert(ep != NULL);
    if (retep != NULL) {
        *retep = ep;
    }

    uintptr_t epoffset = (uintptr_t)&ep->k - (uintptr_t)curdispatcher();

    // mint new badged cap from our existing reply endpoint
    return cap_mint(dest, cap_selfep, epoffset, buflen);
}

/**
 * \brief Set the receive capability slot for a given endpoint
 *
 * \param ep Endpoint returned from messages_lmp_alloc_endpoint()
 * \param slot Receive slot
 */
void lmp_endpoint_set_recv_slot(struct lmp_endpoint *ep, struct capref slot)
{
    ep->k.recv_cptr = get_cnode_addr(slot);
    ep->k.recv_bits = get_cnode_valid_bits(slot);
    ep->k.recv_slot = slot.slot;
    ep->recv_slot = slot;
}

/**
 * \brief Returns true iff there are messages in the given endpoint buffer
 *
 * May be called enabled or disabled. As a result, when enabled, the result may
 * be incorrect as soon as the function returns.
 */
inline bool lmp_endpoint_can_recv(struct lmp_endpoint *ep)
{
    return ep->k.delivered != ep->k.consumed;
}

/// Returns number of words, including headers, previously unseen in endpoint
static uint32_t lmp_endpoint_words_unseen(struct lmp_endpoint *ep)
{
    uint32_t delivered = ep->k.delivered;
    uint32_t ret;
    if (delivered >= ep->seen) {
        ret = delivered - ep->seen;
    } else {
        ret = ep->buflen - ep->seen + delivered;
    }
    ep->seen = delivered;
    return ret;
}

/**
 * \brief Check incoming LMP endpoints for messages and notify waitsets
 *
 * \param disp_priv Dispatcher's private data
 *
 * Must be called while disabled.
 */
void lmp_endpoints_poll_disabled(dispatcher_handle_t handle)
{
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);
    struct lmp_endpoint *ep, *nextep, *firstep;
    errval_t err;

    // get LMP delivered word count at time of entry
    uint32_t lmp_delivered = disp->lmp_delivered;

    // try the hint EP first if set
    if (disp->lmp_hint != 0) {
        assert_disabled(disp->lmp_hint < (1UL << DISPATCHER_FRAME_BITS));

        /* compute endpoint location */
        ep = (struct lmp_endpoint *)
            ((char *)handle + disp->lmp_hint - offsetof(struct lmp_endpoint, k));

        // clear hint now we're about to look at it
        disp->lmp_hint = 0;

        // if channel has a message, is registered, and isn't already pending
        if (lmp_endpoint_can_recv(ep)
            && waitset_chan_is_registered(&ep->waitset_state)
            && ep->waitset_state.state == CHAN_IDLE) {

            // update seen count
            disp->lmp_seen += lmp_endpoint_words_unseen(ep);

            // should have been in the poll list if it was registered
            assert_disabled(ep->next != NULL && ep->prev != NULL);

            err = waitset_chan_trigger_disabled(&ep->waitset_state, handle);
            assert_disabled(err_is_ok(err));

            /* remove from poll list */
            if (ep->next == ep) {
                assert(ep->prev == ep);
                assert(dp->lmp_poll_list == ep);
                dp->lmp_poll_list = NULL;
            } else {
                ep->prev->next = ep->next;
                ep->next->prev = ep->prev;
                if (dp->lmp_poll_list == ep) {
                    dp->lmp_poll_list = ep->next;
                }
            }
        }
    }

    // if there is now nothing outstanding, we can skip the full poll
    // it's possible that another message arrived while we were polling, but
    // we'll stay runnable and get that next time
    if (disp->lmp_seen == lmp_delivered) {
        return;
    }

    // there are other polled endpoints with unseen messages: search for them
    for (ep = dp->lmp_poll_list; ep != NULL; ep = nextep) {
        nextep = ep->next;
        firstep = dp->lmp_poll_list;

        if (lmp_endpoint_can_recv(ep)) {
            err = waitset_chan_trigger_disabled(&ep->waitset_state, handle);
            assert_disabled(err_is_ok(err)); // can't fail

            // update seen count
            disp->lmp_seen += lmp_endpoint_words_unseen(ep);

            /* remove from poll list */
            if (ep->next == ep) {
                assert(ep->prev == ep);
                assert(dp->lmp_poll_list == ep);
                dp->lmp_poll_list = NULL;
                break;
            } else {
                ep->prev->next = ep->next;
                ep->next->prev = ep->prev;
                if (dp->lmp_poll_list == ep) {
                    dp->lmp_poll_list = ep->next;
                }
            }
        }

        if (nextep == firstep) {
            break; // looped
        }
    }

    // if there are now any outstanding unseen messages, they must be in
    // endpoints which we aren't currently polling / aren't currently
    // registered, so we can ignore them here, as long as we update
    // ep->seen = ep->delivered when inserting an endpoint into the poll list.
    disp->lmp_seen = lmp_delivered;
}

/**
 * \brief Register an event handler to be notified when messages can be received
 *
 * In the future, call the closure on the given waitset when it is likely that
 * a message can be received on the endpoint. An endpoint may only be registered
 * with a single event handler on a single waitset at any one time.
 *
 * \param ep LMP endpoint
 * \param ws Waitset
 * \param closure Event handler
 */
errval_t lmp_endpoint_register(struct lmp_endpoint *ep, struct waitset *ws,
                               struct event_closure closure)
{
    errval_t err;

    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);

    // update seen count before checking for any new messages
    ep->seen = ep->k.delivered;

    if (lmp_endpoint_can_recv(ep)) { // trigger immediately
        err = waitset_chan_trigger_closure_disabled(ws, &ep->waitset_state,
                                                    closure, handle);
    } else {
        err = waitset_chan_register_disabled(ws, &ep->waitset_state, closure);
        if (err_is_ok(err)) {
            /* enqueue on poll list */
            if (dp->lmp_poll_list == NULL) {
                ep->prev = ep->next = ep;
            } else {
                ep->next = dp->lmp_poll_list;
                ep->prev = ep->next->prev;
                ep->next->prev = ep;
                ep->prev->next = ep;
            }
            dp->lmp_poll_list = ep;
        }
    }

    disp_enable(handle);

    return err;
}

/**
 * \brief Cancel an event registration made with lmp_endpoint_register()
 *
 * \param ep LMP Endpoint
 */
errval_t lmp_endpoint_deregister(struct lmp_endpoint *ep)
{
    assert(ep != NULL);
    dispatcher_handle_t handle = disp_disable();
    struct dispatcher_generic *dp = get_dispatcher_generic(handle);

    errval_t err = waitset_chan_deregister_disabled(&ep->waitset_state);
    if (err_is_ok(err)) {
        /* dequeue from poll list */
        if (ep->next == ep) {
            assert(ep->prev == ep);
            assert(dp->lmp_poll_list == ep);
            dp->lmp_poll_list = NULL;
        } else {
            ep->next->prev = ep->prev;
            ep->prev->next = ep->next;
            if (dp->lmp_poll_list == ep) {
                dp->lmp_poll_list = ep->next;
            }
        }
    }

    disp_enable(handle);

    return err;
}

/**
 * \brief Migrate an event registration made with lmp_endpoint_register() to a new waitset.
 *
 * \param ep LMP Endpoint
 * \param ws New waitset
 */
void lmp_endpoint_migrate(struct lmp_endpoint *ep, struct waitset *ws)
{
    waitset_chan_migrate(&ep->waitset_state, ws);
}

/**
 * \brief Retrieve an LMP message from an endpoint, if possible
 *
 * \param ep  Endpoint
 * \param buf LMP message buffer, to be filled-in
 * \param cap If non-NULL, filled-in with location of received capability, if any
 *
 * \return LIB_ERR_NO_LMP_MSG if no message is available
 * \return LIB_ERR_LMP_RECV_BUF_OVERFLOW if user-provided receive buffer is too small
 *                                       to store the entire message
 */
errval_t lmp_endpoint_recv(struct lmp_endpoint *ep, struct lmp_recv_buf *buf,
                           struct capref *cap)
{
    assert(buf != NULL);

    dispatcher_handle_t handle = disp_disable();

    if (!lmp_endpoint_can_recv(ep)) {
        disp_enable(handle);
        return LIB_ERR_NO_LMP_MSG;
    }

    uint32_t pos = ep->k.consumed;
    assert(pos < ep->buflen);

    /* look at the header first */
    union lmp_recv_header header;
    header.raw = ep->k.buf[pos];
    buf->msglen = header.x.length;

    if (header.x.length >= ep->buflen) {
        USER_PANIC("lmp_endpoint_recv: insane message (%u words @ %"PRIu32")."
                   " delivered=%"PRIu32" consumed=%"PRIu32" len=%"PRIu32"\n",
                   header.x.length, pos, ep->k.delivered, ep->k.consumed,
                   ep->buflen);
    }

    /* check for space in the user's buffer */
    if (header.x.length > buf->buflen) {
        disp_enable(handle);
        debug_printf("lmp_endpoint_recv: recv buf (%zu words @ %p) overflow"
                     " by pending message (%u words @ %"PRIu32")."
                     " delivered=%"PRIu32" consumed=%"PRIu32" len=%"PRIu32"\n",
                     buf->buflen, &buf, header.x.length, pos,
                     ep->k.delivered, ep->k.consumed, ep->buflen);
        return LIB_ERR_LMP_RECV_BUF_OVERFLOW;
    }

    /* consume the header */
    if (++pos == ep->buflen) {
        pos = 0;
    }

    /* copy the rest out one word at a time... */
    for (int i = 0; i < header.x.length; i++) {
        buf->words[i] = ep->k.buf[pos];
        if (++pos == ep->buflen) {
            pos = 0;
        }
    }

    /* did we get a cap? */
    if (header.x.flags.captransfer) {
        assert_disabled(ep->k.recv_cptr != 0);
        if (cap != NULL) {
            *cap = ep->recv_slot;
        }
        ep->k.recv_cptr = ep->k.recv_bits = ep->k.recv_slot = 0;
    } else if (cap != NULL) {
        *cap = NULL_CAP;
    }

    /* update dispatcher */
    ep->k.consumed = pos;

    disp_enable(handle);

    return SYS_ERR_OK;
}

/**
 * \brief Store a newly-received LRPC message into an endpoint buffer
 *
 * Must be called while disabled.
 *
 * \param ep   Endpoint
 * \param bufpos Reserved position in endpoint message buffer
 * \param arg1 Message payload
 * \param arg2 Message payload
 * \param arg3 Message payload
 * \param arg4 Message payload
 */
void lmp_endpoint_store_lrpc_disabled(struct lmp_endpoint *ep, uint32_t bufpos,
                                      uintptr_t arg1, uintptr_t arg2,
                                      uintptr_t arg3, uintptr_t arg4)
{
    /* prefabricated LMP header */
    static union lmp_recv_header header = {
        .x.length = LRPC_MSG_LENGTH,
    };

    uint32_t buflen = ep->buflen;
    assert_disabled(buflen != 0);
    assert_disabled(bufpos < buflen);

    /* deposit message into buffer, starting from bufpos */
    ep->k.buf[bufpos] = header.raw;
    if (++bufpos == buflen) {
        bufpos = 0;
    }
    ep->k.buf[bufpos] = arg1;
    if (++bufpos == buflen) {
        bufpos = 0;
    }
    ep->k.buf[bufpos] = arg2;
    if (++bufpos == buflen) {
        bufpos = 0;
    }
    ep->k.buf[bufpos] = arg3;
    if (++bufpos == buflen) {
        bufpos = 0;
    }
    ep->k.buf[bufpos] = arg4;
}

/**
 * \brief Initialize LMP endpoint subsystem.
 */
void lmp_endpoint_init(void)
{
    dispatcher_handle_t handle = disp_disable();
    size_t dispsize = get_dispatcher_size();
    void *buf = (char *)get_dispatcher_vaddr(handle) + dispsize;
    size_t buflen = (1UL << DISPATCHER_FRAME_BITS) - dispsize;
    struct dispatcher_generic *d = get_dispatcher_generic(handle);

    heap_init(&d->lmp_endpoint_heap, buf, buflen, NULL);
    disp_enable(handle);
}
