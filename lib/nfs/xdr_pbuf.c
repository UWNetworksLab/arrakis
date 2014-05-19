/**
 * \file
 * \brief XDR implementation using LWIP PBuf structures
 *
 * Uses standard XDR structure. Private fields in XDR are used as follows:
 *  * x_private points to the first struct pbuf in a pbuf chain
 *  * x_base points to the current struct pbuf in a pbuf chain
 *  * x_handy is the position (offset) _within the current pbuf_
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <lwip/pbuf.h>
#include <assert.h>
#include <nfs/xdr.h>
#include "xdr_pbuf.h"

/* move to the next pbuf in the chain */
static bool nextbuf(XDR *xdr)
{
    // shouldn't leave anything behind in previous pbuf
    assert(xdr->x_handy == ((struct pbuf *)xdr->x_base)->len);

    struct pbuf *nextpbuf;
    if ((nextpbuf = ((struct pbuf *)xdr->x_base)->next) != NULL) {
        assert(nextpbuf->len % BYTES_PER_XDR_UNIT == 0);
        xdr->x_base = nextpbuf;
        xdr->x_handy = 0;
        return true;
    } else {
        return false;
    }
}

/* make space within the buffer, returns NULL if it won't fit */
static inline int32_t *make_space(XDR *xdr, size_t size)
{
    if (((struct pbuf *)xdr->x_base)->len == xdr->x_handy) {
        if (!nextbuf(xdr)) {
            return NULL;
        }
    }
    if (xdr->x_handy + size > ((struct pbuf *)xdr->x_base)->len) {
        fprintf(stderr, "xdr_pbuf: make_space(%zu) failing (%zu available)\n",
                size, ((size_t)((struct pbuf *)xdr->x_base)->len) - (size_t)xdr->x_handy);
        return NULL;
    } else {
        int32_t *ret = (int32_t *)((char *)((struct pbuf *)xdr->x_base)->payload + xdr->x_handy);
        xdr->x_handy += size;
        return ret;
    }
}

/* get a word from underlying stream */
static bool xdr_pbuf_getint32(XDR *xdr, int32_t *ret)
{
    int32_t *buf = make_space(xdr, sizeof(int32_t));
    if (buf) {
        *ret = ntohl((uint32_t)*buf);
        return true;
    } else {
        return false;
    }
}

/* put a word to underlying stream */
static bool xdr_pbuf_putint32(XDR *xdr, const int32_t *val)
{
    int32_t *buf = make_space(xdr, sizeof(int32_t));
    if (buf) {
        *buf = htonl((uint32_t)(*val));
        return true;
    } else {
        return false;
    }
}

/* common implementation of getbytes and putbytes */
static bool movebytes(bool copyin, XDR *xdr, char *callerbuf, size_t nbytes)
{
    while (nbytes > 0) {
        if (xdr->x_handy == ((struct pbuf *)xdr->x_base)->len) {
            if (!nextbuf(xdr)) {
                return false;
            }
        }

        size_t space = ((struct pbuf *)xdr->x_base)->len - xdr->x_handy;
        if (space > nbytes) {
            space = nbytes;
        }
        int32_t *buf = make_space(xdr, space);
        assert(buf != NULL);
        if (copyin) {
            memcpy(buf, callerbuf, space);
        } else {
            memcpy(callerbuf, buf, space);
        }
        nbytes -= space;
        callerbuf += space;
    }
    return true;
}

/* get some bytes from underlying stream */
static bool xdr_pbuf_getbytes(XDR *xdr, char *retbuf, size_t nbytes)
{
    return movebytes(false, xdr, retbuf, nbytes);
}

/* put some bytes to underlying stream */
static bool xdr_pbuf_putbytes(XDR *xdr, const char *inbuf, size_t nbytes)
{
    return movebytes(true, xdr, (char *)inbuf, nbytes);
}

/* returns bytes off from beginning */
static size_t xdr_pbuf_getpostn(XDR *xdr)
{
    struct pbuf *pbuf = xdr->x_private;
    size_t len = 0;
    while (xdr->x_base != pbuf) {
        len += pbuf->len;
        pbuf = pbuf->next;
    }
    return len + xdr->x_handy;
}

/* lets you reposition the stream */
static bool xdr_pbuf_setpostn(XDR *xdr, size_t pos)
{
    struct pbuf *pbuf = xdr->x_private;
    if (pos > pbuf->tot_len) {
        return false;
    } else {
        for (; pos > pbuf->len; pbuf=pbuf->next) {
            assert(pbuf != NULL); // or tot_len is wrong!
            pos -= pbuf->len;
        }
        xdr->x_base = pbuf;
        xdr->x_handy = pos;
        return true;
    }
}

/* buf quick ptr to buffered data */
static int32_t *xdr_pbuf_inline(XDR *xdr, size_t nbytes)
{
    assert(nbytes % BYTES_PER_XDR_UNIT == 0);
    return make_space(xdr, nbytes);
}

/* free privates of this xdr_stream */
static void xdr_pbuf_destroy(XDR *xdr)
{
    pbuf_free((struct pbuf *)xdr->x_private);
}

/// XDR operations table
static struct xdr_ops xdr_pbuf_ops = {
    .x_getint32 = xdr_pbuf_getint32,
    .x_putint32 = xdr_pbuf_putint32,
    .x_getbytes = xdr_pbuf_getbytes,
    .x_putbytes = xdr_pbuf_putbytes,
    .x_getpostn = xdr_pbuf_getpostn,
    .x_setpostn = xdr_pbuf_setpostn,
    .x_inline = xdr_pbuf_inline,
    .x_destroy = xdr_pbuf_destroy,
};

/**
 * \brief Create XDR and allocate PBUF for serialising data
 *
 * \param xdr Memory for XDR struct, to be initialised
 * \param size Size of pbuf buffers to allocate
 *
 * \returns True on success, false on error
 */
bool xdr_pbuf_create_send(XDR *xdr, size_t size)
{
    assert(xdr != NULL);
    struct pbuf *pbuf = pbuf_alloc(PBUF_TRANSPORT, size, PBUF_RAM);
    if (pbuf == NULL) {
        return false;
    }
    assert(size % BYTES_PER_XDR_UNIT == 0);
    assert(pbuf->len % BYTES_PER_XDR_UNIT == 0);
    xdr->x_base = xdr->x_private = pbuf;
    xdr->x_op = XDR_ENCODE;
    xdr->x_ops = &xdr_pbuf_ops;
    xdr->x_handy = 0;
    return true;
}

/**
 * \brief Create XDR for deserialising data in given PBUF
 *
 * \param xdr Memory for XDR struct, to be initialised
 * \param pbuf LWIP packet buffer pointer
 *
 * \returns True on success, false on error
 */
void xdr_pbuf_create_recv(XDR *xdr, struct pbuf *pbuf)
{
    assert(xdr != NULL);
    assert(pbuf->len % BYTES_PER_XDR_UNIT == 0);
    assert(pbuf->tot_len % BYTES_PER_XDR_UNIT == 0);
    xdr->x_private = xdr->x_base = pbuf;
    xdr->x_op = XDR_DECODE;
    xdr->x_ops = &xdr_pbuf_ops;
    xdr->x_handy = 0;
}
