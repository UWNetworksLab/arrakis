/**
 * \file
 * \brief Barrelfish collections library - flip buffer
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _COLLECTIONS_FLIPBUFFER_H_
#define _COLLECTIONS_FLIPBUFFER_H_

#include <stdbool.h>
#include <stdlib.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

struct collections_fbuf {
    void *front;        /// < front buffer (currently active buffer)
    size_t frontlen;    /// < length (in bytes) of front buffer
    void *back;         /// < back buffer
    size_t backlen;     /// < length (in bytes) of back buffer
};

void   collections_fbuf_append(struct collections_fbuf *fbuf, const void *data,
                               size_t length);

void   collections_fbuf_create(struct collections_fbuf **fbuf);

void   collections_fbuf_flip(struct collections_fbuf *fbuf);

void   collections_fbuf_free(struct collections_fbuf *fbuf);

void  *collections_fbuf_get_data(struct collections_fbuf *fbuf);

size_t collections_fbuf_get_length(struct collections_fbuf *fbuf);

bool   collections_fbuf_is_empty(struct collections_fbuf *fbuf);

void   collections_fbuf_other_free(struct collections_fbuf *fbuf);

bool   collections_fbuf_other_is_empty(struct collections_fbuf *fbuf);

void   collections_fbuf_release(struct collections_fbuf *fbuf);

__END_DECLS

#endif // _COLLECTIONS_FLIPBUFFER_H_
