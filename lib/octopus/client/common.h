/**
 * \file
 * \brief Contains common functions/macros/defines used throughout
 * the octopus client library.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_COMMON_H_
#define OCTOPUS_COMMON_H_

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>

#include <barrelfish/event_mutex.h>

#include <octopus/definitions.h>

// TODO saw some TODOs in event_mutex_* so this will probably not work
// as expected right now
// TODO: Barrier/Lock code calls this a number of times because
// we currently use the existing getset API there
#define OCT_LOCK_BINDING(cl) event_mutex_threaded_lock(&(cl)->b->mutex)
#define OCT_UNLOCK_BINDING(cl) event_mutex_unlock(&(cl)->b->mutex)


// Make sure args come right after query
#define FORMAT_QUERY(query, args, buf) do {                         \
    size_t length = 0;                                              \
    va_start(args, query);                                          \
    err = allocate_string(query, args, &length, &buf);              \
    va_end(args);                                                   \
    if(err_is_fail(err)) {                                          \
        return err;                                                 \
    }                                                               \
    va_start(args, query);                                          \
    size_t bytes_written = vsnprintf(buf, length+1, query, args);   \
    va_end(args);                                                   \
    assert(bytes_written == length);                                \
} while (0)

static inline errval_t allocate_string(const char *fmt, va_list args,
        size_t *length, char **buf)
{
    *length = vsnprintf(NULL, 0, fmt, args);

    if (*length >= MAX_QUERY_LENGTH) {
        return OCT_ERR_QUERY_SIZE;
    }

    *buf = malloc((*length) + 1); // include \0
    if (buf == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    return SYS_ERR_OK;
}

#endif /* OCTOPUS_COMMON_H_ */
