/**
 * \file
 * \brief Libc stdio (debug streams).
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdio_file.h>
#include <barrelfish/barrelfish.h> /* for THREAD_MUTEX_INITIALIZER */

size_t (*_libc_terminal_read_func)(char *, size_t);
size_t (*_libc_terminal_write_func)(const char *, size_t);

static size_t stdin_read(void *data, long int position, size_t count,
                         void *handle /*unused*/)
{
    if (_libc_terminal_read_func) {
        return _libc_terminal_read_func((char *)data, count);
    } else {
        return -1;
    }
}

static size_t stdout_write(void *data, long int position, size_t count,
                           void *handle /*unused*/)
{
    if (_libc_terminal_write_func) {
        return _libc_terminal_write_func((char *)data, count);
    } else {
        return -1;
    }
}

struct __file __stdin = {
    .read_fn = stdin_read,
    .buffering_mode = _IONBF,
    .mutex = THREAD_MUTEX_INITIALIZER,
    .buf_size = BUFSIZ
};


struct __file __stdout = {
    .write_fn = stdout_write,
    .buffering_mode = _IOLBF,
    .mutex = THREAD_MUTEX_INITIALIZER,
    .buf_size = BUFSIZ
};

struct __file __stderr = {
    .write_fn = stdout_write,
    .buffering_mode = _IONBF,
    .mutex = THREAD_MUTEX_INITIALIZER,
    .buf_size = BUFSIZ
};

FILE *stdin = &__stdin;
FILE *stdout = &__stdout;
FILE *stderr = &__stderr;
