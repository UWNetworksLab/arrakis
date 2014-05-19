/**
 * \file
 * \brief Generic bag-o-bytes message format
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_MSGBUF_H
#define BARRELFISH_MSGBUF_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/// A generic message buffer into which things can be marshalled
struct msgbuf {
    char *buf;      ///< Pointer to start of message buffer
    size_t pos;     ///< Current position (byte-offset) in buffer
    size_t len;     ///< Currently-allocated length (in bytes) of buffer
    struct capref *caps;
    size_t cap_pos;
    size_t ncaps;
    bool dynamic; ///< Buffer is dynamically allocated (from malloc/realloc)
};

void msgbuf_init_dynamic(struct msgbuf *msgbuf, size_t hintlen, size_t hintcaps);
void msgbuf_init_static(struct msgbuf *msgbuf, void *buf, size_t buflen,
                        struct capref *capbuf, size_t capbuflen);
void msgbuf_destroy(struct msgbuf *msgbuf);

errval_t msgbuf_marshall_buffer(struct msgbuf *msgbuf, const void *src_buf,
                                size_t len);
errval_t msgbuf_unmarshall_buffer(struct msgbuf *msgbuf, size_t *retlen,
                                  void **retbuf);
errval_t msgbuf_unmarshall_buffer_to_buf(struct msgbuf *msgbuf, void *dest_buf,
                                         size_t buflen, size_t *retlen);
errval_t msgbuf_marshall_string(struct msgbuf *msgbuf, const char *string);
errval_t msgbuf_unmarshall_string(struct msgbuf *msgbuf, char **ret);
errval_t msgbuf_unmarshall_string_to_buf(struct msgbuf *msgbuf, char *buf,
                                         size_t buflen, size_t *retlen);
errval_t msgbuf_marshall_cap(struct msgbuf *msgbuf, struct capref cap);
errval_t msgbuf_unmarshall_cap(struct msgbuf *msgbuf, struct capref *retcap);

#define DECLARE_PRIM_MARSHALLING(NAME, TYPE) \
    errval_t msgbuf_marshall_##NAME(struct msgbuf *msgbuf, TYPE val);   \
    errval_t msgbuf_unmarshall_##NAME(struct msgbuf *msgbuf, TYPE *ret);

DECLARE_PRIM_MARSHALLING(uintptr, uintptr_t)
DECLARE_PRIM_MARSHALLING(intptr, intptr_t)
DECLARE_PRIM_MARSHALLING(uint64, uint64_t)
DECLARE_PRIM_MARSHALLING(int64, int64_t)
DECLARE_PRIM_MARSHALLING(uint32, uint32_t)
DECLARE_PRIM_MARSHALLING(int32, int32_t)
DECLARE_PRIM_MARSHALLING(uint16, uint16_t)
DECLARE_PRIM_MARSHALLING(int16, int16_t)
DECLARE_PRIM_MARSHALLING(uint8, uint8_t)
DECLARE_PRIM_MARSHALLING(int8, int8_t)
DECLARE_PRIM_MARSHALLING(int, int)
DECLARE_PRIM_MARSHALLING(float, float)
DECLARE_PRIM_MARSHALLING(double, double)
DECLARE_PRIM_MARSHALLING(size, size_t)
DECLARE_PRIM_MARSHALLING(iref, iref_t)

#undef DECLARE_PRIM_MARSHALLING

__END_DECLS

#endif // BARRELFISH_MSGBUF_H
