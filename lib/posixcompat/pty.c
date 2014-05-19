/**
 * \file
 * \brief posix pseudo terminal implementation
 *
 * This implementation provides a blocking API to pseudo-terminals with buffered
 * input and unbuffered output.
 *
 * \bug Need to dispatch monitor waitset in certain cases, otherwise bind
 *      requests from pseudo-terminal slave never complete.
 */

/*
 * Copyright (c) 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#define _USE_XOPEN
#include <barrelfish/barrelfish.h>
#include <barrelfish/threads.h>
#include <barrelfish/waitset.h>
#include <collections/list.h>
#include <if/octopus_defs.h>
#include <if/octopus_rpcclient_defs.h>
#include <octopus/octopus.h>
#include <posixcompat.h>
#include <term/server/server.h>
#include <vfs/fdtab.h>
#include <vfs/vfs.h>

#include "pty.h"

#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>

/* Internal functions and bind code */
static char    *build_pathname(const char *prefix, int number);
static void     chars_cb(void *st, char *buffer, size_t length);
static void     conf_cb(void *st, terminal_config_option_t opt, char *arguments);
static size_t   copy_to_user(struct _pty *state, char *ubuf, size_t ucount);
static void     free__pty_struct(struct _pty *state);
static errval_t init__pty_struct(struct _pty *state);
static errval_t posix_openpt_init(void);
static int      search_master_fd(const char *ptspath);
static void     session_cb(void *st, struct capref session_id);
static int      session_loop(void *arg);

/**
 * \brief Open a pseudo-terminal device.
 *
 * Exports a terminal interface and creates the file '/dev/pts/0' for the slave
 * side.
 *
 * \note The vfs must be initialized before this function is called.
 */
int posix_openpt(int oflag)
{
    static bool initialized = false;

    errval_t err;
    struct fdtab_entry e;
    int fd = 0;
    struct _pty *state;
    vfs_handle_t handle;

    /* initialize directory structure */
    if (!initialized) {
        err = posix_openpt_init();
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to initialize posix_openpt.");
            errno = EAGAIN;
            return -1;
        }
        initialized = true;
    }

    /* init struct _pty */
    state = malloc(sizeof(struct _pty));
    if (state == NULL) {
        errno = EAGAIN;
        return -1;
    }
    err = init__pty_struct(state);
    if (err_is_fail(err)) {
        goto finish;
    }

    /* export terminal server */
    err = term_server_init(state->ts, &state->iref, &state->session_ws,
                           &state->in_ws, &state->out_ws, &state->conf_ws,
                           chars_cb, conf_cb, session_cb);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to export terminal server.");
        goto finish;
    }

    /* initialize state pointer passed to callback functions. */
    state->ts->st = state;

    /* Spin of a thread that dispatches the session waitset. */
    state->session_thread = thread_create(session_loop, state);
    assert(state->session_thread != NULL);

    /* create file for slave /dev/pts/X */
    err = vfs_create(state->ptsname, &handle);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Error creating %s", state->ptsname);
        goto finish;
    }

    /* fd entry settings */
    e.type = FDTAB_TYPE_PTM;
    e.handle = state;

    /* allocate file descriptor */
    fd = fdtab_alloc(&e);
    if (fd < 0) {
        errno = EMFILE;
        return -1;
    } else {
        return fd;
    }

finish:
    free__pty_struct(state);
    errno = EAGAIN;
    return -1;
}

/**
 * \brief Unlock a pseudo-terminal master/slave pair.
 */
int unlockpt(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    if (e->type != FDTAB_TYPE_PTM) {
        errno = EINVAL;
        return -1;
    }

    /* Nothing to be done */
    return 0;
}

/**
 * \brief Grant access to the slave pseudo-terminal device.
 */
int grantpt(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    if (e->type != FDTAB_TYPE_PTM) {
        errno = EINVAL;
        return -1;
    }

    /* Nothing to be done */
    return 0;
}

/**
 * \brief Get name of the slave pseudo-terminal device.
 */
char *ptsname(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    if (e->type != FDTAB_TYPE_PTM) {
        return NULL;
    }

    struct _pty *state = e->handle;

    return state->ptsname;
}

/*********** close(), read(), write() wrappers for master follow **************/

int ptm_close(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);
    struct _pty *state = e->handle;

    /* sanitiy checks */
    assert(e->type == FDTAB_TYPE_PTM);

    /*
     * Decrement reference count, free if 0.
     */
    if ((--state->opencount) == 0) {
        free__pty_struct(state);
    }

    fdtab_free(fd);

    return 0;
}

/**
 * \brief Read on pseudo-terminal master. (thread-safe)
 *
 * Called, when a read happens on a file-descriptor of type FDTAB_TYPE_PTM.
 * Should not be called directly by applications.
 * Implements POSIX read semantics.
 */
ssize_t ptm_read(int fd, void *buf, size_t count)
{
    errval_t err;
    size_t read = 0;

    struct fdtab_entry *e = fdtab_get(fd);
    struct _pty *state = e->handle;

    /* sanity checks */
    assert(e->type == FDTAB_TYPE_PTM);
    if (count == 0) {
        return 0;
    }
    if (buf == NULL) {
        errno = EFAULT;
        return -1;
    }

    thread_mutex_lock(&state->mmutex);

    /* Check if we have buffered data and copy it to user buffer. */
    if (state->mreadbuf_start != NULL) {
        read += copy_to_user(state, buf + read, count - read);
    }
    if (read == count) {
        thread_mutex_unlock(&state->mmutex);
        return read;
    }

    /*
     * Retrieve all available data without blocking and copy it to the user
     * buffer.
     */
    while (true) {
        err = event_dispatch_non_block(&state->in_ws);
        if (err == LIB_ERR_NO_EVENT) {
            break;
        } else if (err_is_fail(err)) {
            DEBUG_ERR(err, "Error in event_dispatch_non_block.");
            goto finish;
        }

        if (state->mreadbuf_start != NULL) {
            read += copy_to_user(state, buf + read, count - read);
        }
        if (read == count) {
            thread_mutex_unlock(&state->mmutex);
            return read;
        }
    }

    /* If O_NONBLOCK is set, bail out. */
    if (state->file_status_flags & O_NONBLOCK) {
        thread_mutex_unlock(&state->mmutex);
        if (read == 0) {
            errno = EAGAIN;
            return -1;
        } else {
            return read;
        }
    }

    /*
     * If no data is available, block until at least 1 byte is available.
     * We do not have to wait until the full amount the user requested is
     * availabe, we just return a short count.
     */
    while (read == 0) {
        err = event_dispatch(&state->in_ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Error in event_dispatch.");
            goto finish;
        }

        if (state->mreadbuf_start != NULL) {
            read += copy_to_user(state, buf + read, count - read);
        }
    }

    thread_mutex_unlock(&state->mmutex);
    return read;

finish:
    thread_mutex_unlock(&state->mmutex);
    return -1;
}

/**
 * \brief Write on pseudo-terminal master. (thread-safe)
 *
 * Called, when a write happens on a file-descriptor of type FDTAB_TYPE_PTM.
 * Should not be called directly by applications.
 * Implements POSIX write semantics.
 */
ssize_t ptm_write(int fd, const void *buf, size_t count)
{
    errval_t err;
    bool channel_busy = true;

    struct fdtab_entry *e = fdtab_get(fd);
    struct _pty *state = e->handle;

    /* sanity checks */
    assert(e->type == FDTAB_TYPE_PTM);
    if (count == 0) {
        return 0;
    }
    if (buf == NULL) {
        errno = EFAULT;
        return -1;
    }

    thread_mutex_lock(&state->mmutex);

    /* Send data that is ready without blocking. */
    while (true) {
        err = event_dispatch_non_block(&state->out_ws);
        if (err == LIB_ERR_NO_EVENT) {
            break;
        } else if (err_is_fail(err)) {
            DEBUG_ERR(err, "Error in event_dispatch_non_block.");
            goto finish;
        }
    }

    /*
     * If previous data is not yet sent and we have O_NONBLOCK, bail out.
     * While we could term_server_send() without blocking, we don't, in order to
     * create proper backpressure.
     */
    channel_busy = term_server_sending(state->ts);
    if (channel_busy && (state->file_status_flags & O_NONBLOCK)) {
        errno = EAGAIN;
        goto finish;
    }

    /* Enqueue data for sending. */
    term_server_send(state->ts, buf, count);

    /* Wait until the sending completes. */
    while (term_server_sending(state->ts)) {
        err = event_dispatch(&state->out_ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Error in event_dispatch.");
            goto finish;
        }
    }

    thread_mutex_unlock(&state->mmutex);
    return count;

finish:
    thread_mutex_unlock(&state->mmutex);
    return -1;
}

/******** close(), open(), read(), write() wrappers for slave follow **********/

int pts_close(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    /* sanitiy checks */
    assert(e->type == FDTAB_TYPE_PTS);

    struct _pty *state = e->handle;

    /*
     * Decrement reference count, free if 0.
     */
    if ((--state->opencount) == 0) {
        free__pty_struct(state);
    }

    fdtab_free(fd);
    return 0;
}

int pts_open(const char *path, int oflags)
{
    int fdm = 0;
    int fds = 0;
    struct fdtab_entry *em_p;
    struct fdtab_entry es;
    struct _pty *state;

    /* search file descriptor of master */
    fdm = search_master_fd(path);
    if (fdm < 0) {
        return -1;
    }
    em_p = fdtab_get(fdm);

    /* fd entry settings */
    es.type = FDTAB_TYPE_PTS;
    /* master and slave share the struct _pty */
    es.handle = em_p->handle;

    /* increment reference count */
    state = em_p->handle;
    state->opencount++;

    /* allocate file descriptor */
    fds = fdtab_alloc(&es);
    if (fds < 0) {
        errno = EMFILE;
        return -1;
    } else {
        return fds;
    }
}

ssize_t pts_read(int fd, void *buf, size_t count)
{
    assert(!"NYI");
    return -1;
}

ssize_t pts_write(int fd, const void *buf, size_t count)
{
    assert(!"NYI");
    return -1;
}

/**
 * \brief Retrieve interface reference of session interface associated with
 *        pseudo-terminal.
 *
 * \param fd File descriptor of slave side.
 *
 * \return Interface reference associated with fd or IREF_NULL on error.
 */
iref_t posixcompat_pts_get_iref(int fd)
{
    struct fdtab_entry *e = fdtab_get(fd);

    if (e->type == FDTAB_TYPE_PTS) {
        struct _pty *state = e->handle;
        assert(state != NULL);

        return state->iref;
    } else {
        return NULL_IREF;
    }
}

/******************* Interal functions and bind code follow *******************/

static errval_t allocate_unique_number(uint32_t *np)
{
    errval_t err;
    char *record;
    octopus_trigger_id_t tid;

    struct octopus_rpc_client *oc = get_octopus_rpc_client();

    /* request a system-wide unique number at octopus */
    char *query = PTY_PTS_OCTOPUS_PREFIX;
    oc->vtbl.set(oc, query, SET_SEQUENTIAL, NOP_TRIGGER, true, &record, &tid,
                 &err);
    if (err_is_fail(err)) {
        goto finish;
    }

    /*
     * Octpus returns the record in the form 'ptypts0 {}'. Extract unique
     * number.
     */
    int ret = sscanf(record, PTY_PTS_OCTOPUS_PREFIX "%" PRIu32, np);
    assert(ret == 1);

finish:
    free(record);
    return err;
}

static char *build_pathname(const char *prefix, int number)
{
    char *name = NULL;
    size_t len = 0;

    len = snprintf(NULL, 0, "%s%d", prefix, number);
    name = malloc(len + 1);
    if (name == NULL) {
        return NULL;
    } else {
        snprintf(name, len + 1, "%s%d", prefix, number);
        return name;
    }
}

static void chars_cb(void *st, char *buffer, size_t length)
{
    struct _pty *state = (struct _pty *) st;

    assert(buffer != NULL);
    assert(length > 0);
    assert(state != NULL);

    /*
     * Ideally, the function chars_cb(), which is indirectly invoked by
     * dispatching events on the in_ws, should only be called if
     * mreadbuf_start == NULL. Due to some dispatching of waitsets in some
     * unidentified place in the current codebase, it gets called multiple
     * times. In this cases we append to the current buffer which is correct
     * but non-optimal from a performance point of view.
     */
    //assert(state->mreadbuf_start == NULL);

    if (state->mreadbuf_start == NULL) {
        state->mreadbuf_start = buffer;
        state->mreadbuf_current = buffer;
        state->mreadbuf_end = buffer + length;
        state->mreadbuf_length = length;
    } else {
        // append
        size_t newlen = state->mreadbuf_length + length;
        void *newbuf = realloc(state->mreadbuf_start, newlen);
        assert(newbuf != NULL);
        state->mreadbuf_start = newbuf;
        memcpy(state->mreadbuf_end, buffer, length);
        state->mreadbuf_length = newlen;
        state->mreadbuf_end += length;
        free(buffer);
    }
}

static void conf_cb(void *st, terminal_config_option_t opt, char *arguments)
{
    assert(!"NYI");
}

static size_t copy_to_user(struct _pty *state, char *ubuf, size_t ucount)
{
    size_t amount;
    bool totally_read;
    size_t buffered = state->mreadbuf_end - state->mreadbuf_current;

    if (buffered > ucount) {
        amount = ucount;
        totally_read = false;
    } else {
        amount = buffered;
        totally_read = true;
    }

    memcpy(ubuf, state->mreadbuf_current, amount);

    if (totally_read) {
        free(state->mreadbuf_start);
        state->mreadbuf_start = NULL;
        state->mreadbuf_current = NULL;
        state->mreadbuf_end = NULL;
        state->mreadbuf_length = 0;
    } else {
        state->mreadbuf_current += amount;
    }

    return amount;
}

static errval_t init__pty_struct(struct _pty *state)
{
    errval_t err;

    state->ts = malloc(sizeof(struct term_server));
    if (state->ts == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    state->iref = NULL_IREF;
    waitset_init(&state->session_ws);
    waitset_init(&state->in_ws);
    waitset_init(&state->out_ws);
    waitset_init(&state->conf_ws);
    state->connected = false;
    thread_mutex_init(&state->mmutex);
    err = allocate_unique_number(&state->number);
    if (err_is_fail(err)) {
        return err;
    }
    state->ptsname = build_pathname(PTY_PTS_PATH_PREFIX, state->number);
    if (state->ptsname == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }
    state->mreadbuf_start = NULL;
    state->mreadbuf_current = NULL;
    state->mreadbuf_end = NULL;
    state->mreadbuf_length = 0;
    state->session_thread = NULL;
    state->opencount = 1;
    termios_fill_defaults(&state->termios);
    state->winsize.ws_row = 0;
    state->winsize.ws_col = 0;
    state->winsize.ws_xpixel = 0;
    state->winsize.ws_ypixel = 0;
    state->file_status_flags = O_RDWR;

    return SYS_ERR_OK;
}

static void free__pty_struct(struct _pty *state)
{
    errval_t err;

    if (state == NULL) {
        return;
    }

    if (state->ts != NULL) {
        free(state->ts);
    }
    waitset_destroy(&state->session_ws);
    waitset_destroy(&state->in_ws);
    waitset_destroy(&state->out_ws);
    waitset_destroy(&state->conf_ws);
    // TODO: dealocation unique number from octopus
    if (state->ptsname != NULL) {
        free(state->ptsname);
    }
    if (state->mreadbuf_start != NULL) {
        free(state->mreadbuf_start);
    }
    if (state->session_thread != NULL) {
        err = thread_detach(state->session_thread);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Couldn't free session thread of pseudo-terminal.\n");
        }
    }
    free(state);
}

static errval_t posix_openpt_init(void)
{
    errval_t err = SYS_ERR_OK;
    char *pts_path = PTY_PTS_PATH_PREFIX;
    size_t pts_pathlen = strlen(pts_path);
    char *next_sep = NULL;
    char *directory = NULL;

    /*
     * Create directories for PTY_PTS_PATH_PREFIX.
     * If the prefix is '/dev/pts/' for example. Create the directories
     * '/dev' and '/dev/pts'.
     */
    if (pts_pathlen > 0) {
        /* skip leading / */
        next_sep = pts_path + 1;
    } else {
        /* empty prefix, no need to create directories */
        return err;
    }

    /* recursively create directories */
    while ((next_sep = strchr(next_sep, '/')) != NULL) {
        directory = strndup(pts_path, next_sep - pts_path);

        /* create directroy */
        err = vfs_mkdir(directory);
        if (err_no(err) == FS_ERR_EXISTS) {
            /* directroy already exists, continue */
            err = SYS_ERR_OK;
        } else if (err_is_fail(err)) {
            free(directory);
            return err;
        }

        free(directory);
        next_sep++;
    }

    return err;
}

static int search_master_fd(const char *aptspath)
{
    struct fdtab_entry *e = NULL;
    char *ptspath = NULL;

    for (int fd = MIN_FD; fd < MAX_FD; fd++) {
        e = fdtab_get(fd);
        if (e->type == FDTAB_TYPE_PTM) {
            ptspath = ((struct _pty *) e->handle)->ptsname;
            if (strcmp(ptspath, aptspath) == 0) {
                return fd;
            }
        }
    }

    /* not found */
    return -1;
}

static void session_cb(void *st, struct capref session_id)
{
    struct _pty *state = (struct _pty *) st;
    assert(state != NULL);

    state->connected = true;
}

static int session_loop(void *arg)
{
    struct _pty *state = arg;
    errval_t err;

    while (true) {
        err = event_dispatch(&state->session_ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "Error in event_dispatch.");
        }
    }

    return EXIT_FAILURE;
}
