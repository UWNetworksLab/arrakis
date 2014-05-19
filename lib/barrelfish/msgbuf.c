/**
 * \file
 * \brief Generic bag-o-bytes message format
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/msgbuf.h>

void msgbuf_init_static(struct msgbuf *msgbuf, void *buf, size_t buflen,
                        struct capref *capbuf, size_t capbuflen)
{
    assert(msgbuf != NULL);
    msgbuf->pos = 0;
    msgbuf->len = buflen;
    msgbuf->buf = buf;
    msgbuf->cap_pos = 0;
    msgbuf->ncaps = capbuflen;
    msgbuf->caps = capbuf;
    msgbuf->dynamic = false;
}

void msgbuf_init_dynamic(struct msgbuf *msgbuf, size_t hintlen, size_t hintcaps)
{
    void *buf;
    if (hintlen > 0) {
        buf = malloc(hintlen);
        if (buf == NULL) { // XXX: malloc failed, but it will be reported later
            hintlen = 0;
        }
    } else {
        buf = NULL;
    }

    struct capref *capbuf;
    if (hintcaps > 0) {
        capbuf = malloc(sizeof(struct capref) * hintcaps);
        if (capbuf == NULL) {
            hintcaps = 0;
        }
    } else {
        capbuf = NULL;
    }

    msgbuf_init_static(msgbuf, buf, hintlen, capbuf, hintcaps);
    msgbuf->dynamic = true;
}

void msgbuf_destroy(struct msgbuf *msgbuf)
{
    if (msgbuf->dynamic) {
        if (msgbuf->buf != NULL) {
            free(msgbuf->buf);
            msgbuf->buf = NULL;
        }
        if (msgbuf->caps != NULL) {
            free(msgbuf->caps);
            msgbuf->caps = NULL;
        }
        msgbuf->dynamic = false;
    }
}

static void *msgbuf_ensure(struct msgbuf *msgbuf, size_t extralen)
{
    void *ret = &msgbuf->buf[msgbuf->pos];
    size_t newpos = msgbuf->pos + extralen;
    if (newpos <= msgbuf->len) {
        msgbuf->pos = newpos;
        return ret;
    } else {
        return NULL;
    }
}

static void *msgbuf_grow(struct msgbuf *msgbuf, size_t extralen)
{
    void *ret = &msgbuf->buf[msgbuf->pos];
    size_t newpos = msgbuf->pos + extralen ;
    if (newpos <= msgbuf->len) {
        msgbuf->pos = newpos;
        return ret;
    } else if (msgbuf->dynamic) {
        // try to grow buffer using realloc
        void *newbuf = realloc(msgbuf->buf, msgbuf->pos + extralen);
        if (newbuf == NULL) {
            return NULL;
        } else {
            msgbuf->buf = newbuf;
            msgbuf->pos = newpos;
            return ret;
        }
    } else {
        return NULL; // can't grow a static buffer
    }
}

errval_t msgbuf_marshall_buffer(struct msgbuf *msgbuf, const void *src_buf,
                                size_t len)
{
    // copy the argument into the memory region
    // the size of the buffer
    errval_t err = msgbuf_marshall_size(msgbuf, len);
    if (err_is_fail(err)) {
        return err;
    }

    // the buffer itself
    void *dest = msgbuf_grow(msgbuf, len);
    if (dest == NULL) {
        // XXX: reach around grow/marshall API and remove size from buffer
        msgbuf->pos -= sizeof(size_t);
        return LIB_ERR_MSGBUF_CANNOT_GROW;
    }

    memcpy(dest, src_buf, len);
    return SYS_ERR_OK;
}

errval_t msgbuf_unmarshall_buffer(struct msgbuf *msgbuf, size_t *retlen,
                                  void **retbuf)
{
    errval_t err;

    // the size of the buffer
    size_t len;
    err = msgbuf_unmarshall_size(msgbuf, &len);
    if (err_is_fail(err)) {
        return err;
    }

    // the buffer payload
    void *src = msgbuf_ensure(msgbuf, len);
    if (src == NULL) {
        return LIB_ERR_MSGBUF_OVERFLOW;
    }

    assert(retbuf != NULL);
    *retbuf = malloc(len);
    assert(*retbuf != NULL);

    memcpy(*retbuf, src, len);

    if (retlen != NULL) {
        *retlen = len;
    }

    return SYS_ERR_OK;
}

errval_t msgbuf_unmarshall_buffer_to_buf(struct msgbuf *msgbuf, void *dest_buf,
                                         size_t buflen, size_t *retlen)
{
    errval_t err;

    // the size of the buffer
    size_t len;
    err = msgbuf_unmarshall_size(msgbuf, &len);
    if (err_is_fail(err)) {
        return err;
    }

    // the buffer payload
    void *src = msgbuf_ensure(msgbuf, len);
    if (src == NULL) {
        return LIB_ERR_MSGBUF_OVERFLOW;
    }

    memcpy(dest_buf, src, len < buflen ? len : buflen);

    if (retlen != NULL) {
        *retlen = len;
    }

    return SYS_ERR_OK;
}

errval_t msgbuf_marshall_string(struct msgbuf *msgbuf, const char *string)
{
    return msgbuf_marshall_buffer(msgbuf, string, strlen(string) + 1);
}

errval_t msgbuf_unmarshall_string(struct msgbuf *msgbuf, char **ret)
{
    return msgbuf_unmarshall_buffer(msgbuf, NULL, (void **)ret);
}

errval_t msgbuf_unmarshall_string_to_buf(struct msgbuf *msgbuf, char *buf,
                                          size_t buflen, size_t *retlen)
{
    // unpack to provided buffer
    size_t len;
    errval_t err = msgbuf_unmarshall_buffer_to_buf(msgbuf, buf, buflen, &len);

    if (err_is_ok(err)) {
        // ensure string termination
        if (len >= buflen) {
            buf[buflen - 1] = '\0';
        }

        if (retlen != NULL) {
            *retlen = len;
        }
    }

    return err;
}

errval_t msgbuf_marshall_cap(struct msgbuf *msgbuf, struct capref cap)
{
    if (msgbuf->cap_pos >= msgbuf->ncaps) {
        if (!msgbuf->dynamic) {
            return LIB_ERR_MSGBUF_OVERFLOW;
        }

        void *newbuf = realloc(msgbuf->caps,
                               (msgbuf->ncaps + 1) * sizeof(struct capref));
        if (newbuf == NULL) {
            return LIB_ERR_MSGBUF_CANNOT_GROW;
        } else {
            msgbuf->caps = newbuf;
            msgbuf->ncaps++;
        }
    }

    msgbuf->caps[msgbuf->cap_pos++] = cap;
    return SYS_ERR_OK;
}

errval_t msgbuf_unmarshall_cap(struct msgbuf *msgbuf, struct capref *retcap)
{
    if (msgbuf->cap_pos < msgbuf->ncaps) {
        *retcap = msgbuf->caps[msgbuf->cap_pos++];
        return SYS_ERR_OK;
    } else {
        return LIB_ERR_MSGBUF_OVERFLOW;
    }
}

#define DEFINE_PRIM_MARSHALL(NAME, TYPE) \
errval_t msgbuf_marshall_##NAME(struct msgbuf *msgbuf, TYPE val) \
{ \
    void *buf = msgbuf_grow(msgbuf, sizeof(TYPE)); \
    if (buf == NULL) { \
        return LIB_ERR_MSGBUF_CANNOT_GROW; \
    } \
    TYPE *dest = buf; \
    *dest = val; \
    return SYS_ERR_OK; \
}

#define DEFINE_PRIM_UNMARSHALL(NAME, TYPE) \
errval_t msgbuf_unmarshall_##NAME(struct msgbuf *msgbuf, TYPE *ret) \
{ \
    void *buf = msgbuf_ensure(msgbuf, sizeof(TYPE)); \
    if (buf == NULL) { \
        return LIB_ERR_MSGBUF_OVERFLOW; \
    } \
    TYPE *src = buf; \
    *ret = *src; \
    return SYS_ERR_OK; \
}

#define DEFINE_PRIM_MARSHALLING(NAME, TYPE) \
    DEFINE_PRIM_MARSHALL(NAME, TYPE) \
    DEFINE_PRIM_UNMARSHALL(NAME, TYPE)

DEFINE_PRIM_MARSHALLING(uintptr, uintptr_t)
DEFINE_PRIM_MARSHALLING(intptr, intptr_t)
DEFINE_PRIM_MARSHALLING(uint64, uint64_t)
DEFINE_PRIM_MARSHALLING(int64, int64_t)
DEFINE_PRIM_MARSHALLING(uint32, uint32_t)
DEFINE_PRIM_MARSHALLING(int32, int32_t)
DEFINE_PRIM_MARSHALLING(uint16, uint16_t)
DEFINE_PRIM_MARSHALLING(int16, int16_t)
DEFINE_PRIM_MARSHALLING(uint8, uint8_t)
DEFINE_PRIM_MARSHALLING(int8, int8_t)
DEFINE_PRIM_MARSHALLING(int, int)
DEFINE_PRIM_MARSHALLING(float, float)
DEFINE_PRIM_MARSHALLING(double, double)
DEFINE_PRIM_MARSHALLING(size, size_t)
DEFINE_PRIM_MARSHALLING(iref, iref_t)

#undef DEFINE_PRIM_MARSHALLING
#undef DEFINE_PRIM_MARSHALL
#undef DEFINE_PRIM_UNMARSHALL
