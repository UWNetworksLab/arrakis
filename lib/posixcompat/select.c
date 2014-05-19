/*
 * Copyright (c) 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/deferred.h>
#include <if/monitor_defs.h>
#include <lwip/sys.h>
#include <lwip/sockets.h>
#include <lwip/sock_chan_support.h>
#include <term/server/server.h>
#include <vfs/fdtab.h>

#include "posixcompat.h"
#include "pty.h"
#include "unixsock.h"

#include <assert.h>
#include <unistd.h>

#define	MAX(a,b) (((a)>(b))?(a):(b))
#define READ_SLOT 0
#define WRITE_SLOT 1

#if 0
# define SELECT_DEBUG(x...) debug_printf("select(): " x)
# define SELECT_DEBUG_ENABLED
#else
# define SELECT_DEBUG(x...) ((void)0)
#endif

/* Internal functions */
static int check_fds(int maxfdp1, fd_set *readfds, fd_set *writefds,
                     fd_set *exceptfds);

static void debug_print_fdsets(int maxfdp1, fd_set *readfds, fd_set *writefds,
                               fd_set *exceptfds);

static void debug_print_waitset(struct waitset *ws);

static int pack_on_waitset(int maxfdp1, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, fd_set *changed_ws[],
                           struct waitset *ws_store[][maxfdp1],
                           struct waitset *ws);

static void timeout_fired(void *arg);

static int update_waitset(int maxfdp1, fd_set *readfds, fd_set *writefds,
                          fd_set *exceptfds, fd_set *changed_ws[],
                          struct waitset *ws_store[][maxfdp1],
                          struct waitset *ws);

static inline void zero_fdsets(fd_set *readfds, fd_set *writefds,
                               fd_set *exceptfds);

struct timeout_event {
  bool fired;
};

/**
 * \brief select - synchronous I/O multiplexing
 *
 * In a nutshell, i.e. not considering corner cases, this functions checks if
 * any of the fds associated with the readfds, writefds and exceptfds is ready
 * for the corresponding operation. If this is not the case, we pack every
 * event we are interessted in on a waitset and dispatch on this waitset.
 *
 * FIXME: The exception set is currently ignored. Find out what to map them to.
 */
int select(int maxfdp1, fd_set *readfds, fd_set *writefds, fd_set *exceptfds,
           struct timeval *timeout)
{
    errval_t err;
    int ret;

    SELECT_DEBUG("called with the following fdsets:\n");
    debug_print_fdsets(maxfdp1, readfds, writefds, exceptfds);

    /* Waitset on which all events were are interessted in are packed. */
    struct waitset ws;

    /*
     * The monitor binding is needed in case we have to wait for a bind to
     * complete.
     */
    struct monitor_binding *mb = get_monitor_binding();

    /* Total number of file descriptors that are ready. */
    int retfds = 0;

    /* Indicates whether or not the timout expired. */
    struct timeout_event toe = {
      .fired = false
    };
    struct deferred_event timeout_event;

    /*
     * FD sets keep track of file descriptors, for which we changed the waitset
     * and therefore have to restore it before we return.
     *
     * FD_ISSET(changed_read_ws, maxfdp1) indicates whether or not we changed the
     * monitor binding.
     */
    fd_set changed_read_ws;
    fd_set changed_write_ws;
    fd_set *changed_ws[2];
    changed_ws[READ_SLOT] = &changed_read_ws;
    changed_ws[WRITE_SLOT] = &changed_write_ws;
    FD_ZERO(&changed_read_ws);
    FD_ZERO(&changed_write_ws);

    /* Backup of the waitsets that were changed. */
    struct waitset *ws_store[2][maxfdp1];

    /* check validity of maxfdp1 */
    if (maxfdp1 < MIN_FD || maxfdp1 > MAX_FD || maxfdp1 > FD_SETSIZE) {
        errno = EINVAL;
        return -1;
    }

    /* Initialize the waitset well use to block in the select */
    waitset_init(&ws);

    /*
     * If the user specified a timeout, setup a deferred event. Otherwise we
     * block indefinitely until an event happens on the specified FDs.
     *
     * The POSIX standard states that if timeout.tv_sec = 0 and
     * timeout.tv_usec = 0, select shall just poll. In this case we do not
     * setup a deferred event but set the timeout_event.fired to true. This way
     * we do not block at all but bail out as soon as we checked if any file
     * descriptors are already ready.
     */
    if (timeout != NULL) {
        if (timeout->tv_sec == 0 && timeout->tv_usec == 0) {
            toe.fired = true;
        } else {
            delayus_t delay = timeout->tv_sec * 1000000 + timeout->tv_usec;
            deferred_event_init(&timeout_event);
            err = deferred_event_register(&timeout_event, &ws, delay,
                                          MKCLOSURE(timeout_fired, &toe));
            if (err_is_fail(err)) {
                errno = EINVAL;
                return -1;
            }
        }
    }

    retfds = check_fds(maxfdp1, readfds, writefds, exceptfds);
    if (retfds < 0) {
        /* error occured */
        return -1;
    } else if (retfds > 0) {
        /* some file descriptors are ready */
        goto finish_no_ws_changed;
    }

    /* retfds == 0 */

    if (toe.fired) {
        /*
         * No file descriptors are ready but timeout fired, zero all FD sets and
         * return.
         */
        zero_fdsets(readfds, writefds, exceptfds);
        goto finish_no_ws_changed;
    }

    /*
     * Since no file descriptors were ready, we pack all relevant events on our
     * own waitset and dispatch on that waitset until some file descriptors
     * are ready or a timeout occurs.
     * Note that the occurence of an event on our waitset does not imply that at
     * least one file descriptor is ready, we need to check this condition in
     * a loop.
     */

    ret = pack_on_waitset(maxfdp1, readfds, writefds, exceptfds, changed_ws,
                          ws_store, &ws);
    if (ret < 0) {
        return -1;
    }

    /* Loop until something we're waiting for has happened */
    while (true) {
        SELECT_DEBUG("calling event_dispatch()\n");
        debug_print_waitset(&ws);
        err = event_dispatch(&ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Error in event_dispatch.");
        }
        SELECT_DEBUG("returned from event_dispatch()\n");

        retfds = check_fds(maxfdp1, readfds, writefds, exceptfds);
        if (retfds < 0) {
            /* error occured */
            return -1;
        } else if (retfds > 0) {
            /* some file descriptors are ready */
            goto finish;
        } else {
            ret = update_waitset(maxfdp1, readfds, writefds, exceptfds,
                                 changed_ws, ws_store, &ws);
            SELECT_DEBUG("update_waitset()\n");
            if (ret < 0) {
                return -1;
            }
        }

        /* retfds == 0 */
        if (toe.fired) {
            /*
             * No file descriptors are ready but timeout fired, zero all FD
             * sets and return.
             */
            zero_fdsets(readfds, writefds, exceptfds);
            goto finish;
        }
    }

finish:
    /* If we waited on monitor, restore its old waitset */
    if (FD_ISSET(maxfdp1, changed_ws[READ_SLOT])) {
        SELECT_DEBUG("restoring monitor ws\n");
        err = mb->change_waitset(mb, ws_store[READ_SLOT][maxfdp1]);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "monitor change_waitset");
        }
    }

    /* Restore all waitsets changed becase fd was in readset */
    for (int fd = 0; fd < maxfdp1; fd++) {
        if (FD_ISSET(fd, changed_ws[READ_SLOT])) {
            SELECT_DEBUG("fd %d: restore read waitset\n", fd);
            struct fdtab_entry *e = fdtab_get(fd);

            switch (e->type) {
            case FDTAB_TYPE_AVAILABLE:
                {
                    errno = EBADF;
                    return -1;
                }
                break;

            case FDTAB_TYPE_LWIP_SOCKET:
                {
                    lwip_mutex_lock();
                    /*
                     * In case the socket did not yet became ready, the
                     * previously registered channel is still on the waitset.
                     * We deregister it so that waitset_destroy() completes
                     * without error.
                     */
                    err = lwip_sock_waitset_deregister_read(e->fd);
                    if (err_is_fail(err) &&
                        err_no(err) != LIB_ERR_CHAN_NOT_REGISTERED) {
                        USER_PANIC_ERR(err, "error deregister read channel for "
                                       "lwip socket");
                    }
                    lwip_mutex_unlock();
                }
                break;

            case FDTAB_TYPE_UNIX_SOCKET:
                {
                    struct _unix_socket *us = e->handle;
                    err = us->u.active.binding->change_waitset
                        (us->u.active.binding, ws_store[READ_SLOT][fd]);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }
                }
                break;

            case FDTAB_TYPE_PTM:
                {
                    struct _pty *pty = e->handle;
                    err = term_server_change_in_ws
                        (pty->ts, ws_store[READ_SLOT][fd]);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }
                }
                break;

            default:
                fprintf(stderr, "Restore waitset on FD type %d NYI.\n",
                        e->type);
                assert(!"NYI");
                errno = EBADF;
                return -1;
            }
        }
    }

    /* Restore all waitsets changed becase fd was in writeset */
    for (int fd = 0; fd < maxfdp1; fd++) {
        if (FD_ISSET(fd, changed_ws[WRITE_SLOT])) {
            SELECT_DEBUG("fd %d: restore write waitset\n", fd);
            struct fdtab_entry *e = fdtab_get(fd);

            switch (e->type) {
            case FDTAB_TYPE_AVAILABLE:
                {
                    errno = EBADF;
                    return -1;
                }
                break;

            case FDTAB_TYPE_LWIP_SOCKET:
                {
                    lwip_mutex_lock();
                    err = lwip_sock_waitset_deregister_write(e->fd);
                    if (err_is_fail(err) &&
                        err_no(err) != LIB_ERR_CHAN_NOT_REGISTERED) {
                        USER_PANIC_ERR(err, "error deregister write channel for "
                                       "lwip socket");
                    }
                    lwip_mutex_unlock();
                }
                break;

            case FDTAB_TYPE_UNIX_SOCKET:
                {
                    struct _unix_socket *us = e->handle;
                    err = us->u.active.binding->change_waitset
                        (us->u.active.binding, ws_store[WRITE_SLOT][fd]);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }
                }
                break;

            case FDTAB_TYPE_PTM:
                {
                    struct _pty *pty = e->handle;
                    err = term_server_change_out_ws
                        (pty->ts, ws_store[WRITE_SLOT][fd]);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }
                }
                break;

            default:
                fprintf(stderr, "Restore waitset on FD type %d NYI.\n",
                        e->type);
                assert(!"NYI");
                errno = EBADF;
                return -1;
            }
        }
    }

finish_no_ws_changed:
    // Remove timeout from waitset if it was set
    if(timeout != NULL && !toe.fired) {
        deferred_event_cancel(&timeout_event);
    }

    err = waitset_destroy(&ws);
    if (err_is_fail(err)) {
        SELECT_DEBUG("Error destroying waitset.\n");
        debug_print_waitset(&ws);
        USER_PANIC_ERR(err, "waitset_destroy");
    }

    SELECT_DEBUG("returing %d fds:\n", retfds);
    debug_print_fdsets(maxfdp1, readfds, writefds, exceptfds);

    return retfds;
}

/*************************** internal functions *******************************/

/**
 * \brief Check all FD sets if a file descriptor is ready without blocking.
 *
 * \param maxfdp1   Maximum file descriptor in all sets + 1.
 * \param readfds   The set of file descriptors to be checked for being ready
 *                  to read.
 * \param writefds  The set of file descriptors to be checked for being ready
 *                  to write.
 * \param exceptfds The set of file descriptors to be checked for pending
 *                  error conditions.
 *
 * \return          The number of file descriptors in all sets being ready.
 *                  If no file descriptor is ready, 0 is returned and the FD
 *                  sets are left unchanged. Otherwise, the FD sets are
 *                  modified and indicate which FDs are ready.
 *                  If an error -1 is returned and errno is set, the FD sets
 *                  are left unchanged.
 */
static int check_fds(int maxfdp1, fd_set *readfds, fd_set *writefds,
                     fd_set *exceptfds)
{
    int retfds = 0;
    fd_set oreadfds, owritefds, oexceptfds;
    FD_ZERO(&oreadfds);
    FD_ZERO(&owritefds);
    FD_ZERO(&oexceptfds);

    /* Check list of readfds for events */
    if (readfds != NULL) {
        for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, readfds)) {
                SELECT_DEBUG("fd %d: check for read readiness\n", fd);
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        lwip_mutex_lock();
                        if (lwip_sock_ready_read(e->fd)) {
                            SELECT_DEBUG("fd %d: read ready\n", fd);
                            FD_SET(fd, &oreadfds);
                            retfds++;
                        }
                        lwip_mutex_unlock();
                    }
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;

                        if (us->passive) { /* passive side */
                            /* Check for pending connection requests. */
                            for (int i = 0; i < us->u.passive.max_backlog; i++)
                            {
                                if (us->u.passive.backlog[i] != NULL) {
                                    SELECT_DEBUG("fd %d: read ready\n", fd);
                                    FD_SET(fd, &oreadfds);
                                    retfds++;
                                    break;
                                }
                            }
                        } else { /* active side */
                            /* Check for incoming data. */
                            if (us->recv_buf_valid > 0) {
                                SELECT_DEBUG("fd %d: read ready\n", fd);
                                FD_SET(fd, &oreadfds);
                                retfds++;
                            }
                        }
                    }
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        /* check if data is available for read */
                        if (pty->mreadbuf_start != NULL) {
                            SELECT_DEBUG("fd %d: read ready\n", fd);
                            FD_SET(fd, &oreadfds);
                            retfds++;
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "select() on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }
        }
    }

    /* Check list of writefds for events */
    if (writefds != NULL) {
         for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, writefds)) {
                SELECT_DEBUG("fd %d: check for write readiness\n", fd);
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        lwip_mutex_lock();
                        if (lwip_sock_ready_write(e->fd)) {
                            SELECT_DEBUG("fd %d: write ready\n", fd);
                            FD_SET(fd, &owritefds);
                            retfds++;
                        }
                        lwip_mutex_unlock();
                    }
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;
                        assert(!us->passive);

                        switch (us->u.active.mode) {
                        case _UNIX_SOCKET_MODE_CONNECTING:
                            break;

                        case _UNIX_SOCKET_MODE_CONNECTED:
                            if (us->send_buf == NULL) {
                                SELECT_DEBUG("fd %d: write ready\n", fd);
                                FD_SET(fd, &owritefds);
                                retfds++;
                            }
                            break;
                        }
                    }
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        /* check if we can write to the pty master side */
                        if (!term_server_sending(pty->ts)) {
                            SELECT_DEBUG("fd %d: write ready\n", fd);
                            FD_SET(fd, &owritefds);
                            retfds++;
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "select() on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }
        }
    }

    /* Update FD sets if at least one FD is ready */
    if (retfds > 0) {
        if (readfds != NULL) {
            memcpy(readfds, &oreadfds, sizeof(fd_set));
        }
        if (writefds != NULL) {
            memcpy(writefds, &owritefds, sizeof(fd_set));
        }
        if (exceptfds != NULL) {
            memcpy(exceptfds, &oexceptfds, sizeof(fd_set));
        }
    }

    return retfds;
}

/**
 * \brief Pack all relevant channels on the waitset 'ws'.
 *
 * \param maxfdp1    Maximum file descriptor in all sets +1.
 * \param readfds    read FD set
 * \param writefds   write FD set
 * \param exceptfds  exceptions FD set
 * \param changed_ws Indicates for which file descriptors the waitset was
 *                   changed. Filled-in by function.
 * \param ws_store   Backup of the old waitsets for file descriptors for which
 *                   function changed waitset. Filled-in by function.
 * \param ws         Waitset on which to pack channels.
 *
 * \return           -1 if error, 0 if successful
 */
static int pack_on_waitset(int maxfdp1, fd_set *readfds, fd_set *writefds,
                           fd_set *exceptfds, fd_set *changed_ws[],
                           struct waitset *ws_store[][maxfdp1],
                           struct waitset *ws)
{
    errval_t err;
    struct monitor_binding *mb = get_monitor_binding();
    struct waitset *monitor_ws = mb->waitset;

    FD_ZERO(changed_ws[READ_SLOT]);
    FD_ZERO(changed_ws[WRITE_SLOT]);

    /* go through readfds and change waitsets */
    if (readfds != NULL) {
        for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, readfds)) {
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        int retval;

                        lwip_mutex_lock();
                        retval = lwip_sock_waitset_register_read(e->fd, ws);
                        assert(retval == 0);
                        lwip_mutex_unlock();
                        FD_SET(fd, changed_ws[READ_SLOT]);
                    }
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;

                        if (us->passive) { /* passive side */
                            int i;

                            /* Check for pending connection requests. */
                            for (i = 0; i < us->u.passive.max_backlog; i++)
                            {
                                if (us->u.passive.backlog[i] != NULL) {
                                    break;
                                }
                            }

                            /*
                             * If there are not pending connection request
                             * wait on monitor binding.
                             */
                            if (i == us->u.passive.max_backlog) {
                                /* wait on monitor */
                                FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                            }
                        } else { /* active side */
                            /* Check for incoming data */
                            if (us->recv_buf_valid <= 0) {
                                /* backup waitset */
                                FD_SET(fd, changed_ws[READ_SLOT]);
                                ws_store[READ_SLOT][fd] =
                                    us->u.active.binding->waitset;

                                /* change waitset */
                                err = us->u.active.binding->change_waitset
                                    (us->u.active.binding, ws);
                                if (err_is_fail(err)) {
                                    USER_PANIC_ERR(err, "change waitset");
                                }
                            }
                        }
                    }
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        if (!pty->connected) {
                            /* wait on monitor */
                            FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                        } else if (pty->mreadbuf_start == NULL) {
                            /* check if data is available for read */
                            SELECT_DEBUG("fd %d: change in ws\n", fd);

                            /* backup waitset */
                            FD_SET(fd, changed_ws[READ_SLOT]);
                            ws_store[READ_SLOT][fd] = &pty->in_ws;

                            /* change waitset */
                            err = term_server_change_in_ws(pty->ts, ws);
                            if (err_is_fail(err)) {
                                USER_PANIC_ERR(err, "change waitset");
                            }

                            /* Also wait on monitor to allow new pseudo-terminal
                               slaves to connect. */
                            FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "change waitset on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }
        }
    }

    /* go through writefds and change waitsets */
    if (writefds != NULL) {
         for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, writefds)) {
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        int retval;

                        lwip_mutex_lock();
                        retval = lwip_sock_waitset_register_write(e->fd, ws);
                        assert(retval == 0);
                        lwip_mutex_unlock();
                        FD_SET(fd, changed_ws[WRITE_SLOT]);
                    }
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;
                        assert(!us->passive);

                        switch (us->u.active.mode) {
                        case _UNIX_SOCKET_MODE_CONNECTING:
                            /* wait on monitor */
                            FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                            break;

                        case _UNIX_SOCKET_MODE_CONNECTED:
                            if (us->send_buf != NULL) {
                                /*
                                 * Only change waitset if we did not already
                                 * change it because the fd also was in the
                                 * read fdset.
                                 */
                                if (!FD_ISSET(fd, changed_ws[READ_SLOT])) {
                                    /* backup waitset */
                                    FD_SET(fd, changed_ws[WRITE_SLOT]);
                                    ws_store[WRITE_SLOT][fd] =
                                        us->u.active.binding->waitset;

                                    /* change binding */
                                    err = us->u.active.binding->change_waitset
                                        (us->u.active.binding, ws);
                                    if (err_is_fail(err)) {
                                        USER_PANIC_ERR(err, "change_waitset");
                                    }
                                }
                            }
                            break;
                        }
                    }
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        if (!pty->connected) {
                            /* wait on monitor */
                            FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                        } else if (term_server_sending(pty->ts)) {
                            /* check if we can write to the pty master side */
                            SELECT_DEBUG("fd %d: change out ws\n", fd);

                            /* backup waitset */
                            FD_SET(fd, changed_ws[WRITE_SLOT]);
                            ws_store[WRITE_SLOT][fd] = &pty->out_ws;

                            /* change waitset */
                            err = term_server_change_out_ws(pty->ts, ws);
                            if (err_is_fail(err)) {
                                USER_PANIC_ERR(err, "change_waitset");
                            }

                            /* Also wait on monitor to allow new pseudo-terminal
                               slaves to connect. */
                            FD_SET(maxfdp1, changed_ws[READ_SLOT]);
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "change waitset on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }
        }
    }

    /* Change monitor binding if we wait on the monitor. */
    if (FD_ISSET(maxfdp1, changed_ws[READ_SLOT])) {
        SELECT_DEBUG("changed monitor ws\n");
        ws_store[READ_SLOT][maxfdp1] = monitor_ws;
        err = mb->change_waitset(mb, ws);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "change_waitset");
        }
    }

    return 0;
}

/**
 * \brief Helper function called when deferred event happens.
 */
static void timeout_fired(void *arg)
{
  struct timeout_event *toe = arg;
  assert(toe != NULL);
  toe->fired = true;
}


/**
 * \brief Possibly update the internal waitset that select dispatches on.
 *
 * This is necessary for example if a pseudo-terminal master file descriptor is
 * in the read FD set but no slave has yet connected. In this particular case we
 * pack (in the function pack_on_waitset()) the monitor binding onto selects
 * internal waitset but since the binding for the slave side is not yet available
 * we can't put it on the internal waitset as well. After a slave connected we
 * need to update selects internal waitset to include the binding of the slave or
 * select blocks indefinetely.
 *
 * \param maxfdp1    Maximum file descriptor in all sets +1.
 * \param readfds    read FD set
 * \param writefds   write FD set
 * \param exceptfds  exceptions FD set
 * \param changed_ws Indicates for which file descriptors the waitset was
 *                   changed. Updated by function.
 * \param ws_store   Backup of the old waitsets for file descriptors for which
 *                   function changed waitset. Updated by function.
 * \param ws         Waitset on which to pack channels.
 *
 * \return           -1 if error, 0 if successful
 */
static int update_waitset(int maxfdp1, fd_set *readfds, fd_set *writefds,
                          fd_set *exceptfds, fd_set *changed_ws[],
                          struct waitset *ws_store[][maxfdp1],
                          struct waitset *ws)
{
    errval_t err;

    /* go through readfds and update waitsets if necessary. */
    if (readfds != NULL) {
        for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, readfds)) {
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    /* No update necessary. */
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    /* TODO: is update of internal ws necessary? */
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        /* If we're now connected but weren't before. */
                        if (pty->connected &&
                            !FD_ISSET(fd, changed_ws[READ_SLOT])) {
                            SELECT_DEBUG("fd %d: added in ws", fd);

                            /* backup waitset */
                            FD_SET(fd, changed_ws[READ_SLOT]);
                            ws_store[READ_SLOT][fd] = &pty->in_ws;

                            /* change waitset */
                            err = term_server_change_in_ws(pty->ts, ws);
                            if (err_is_fail(err)) {
                                USER_PANIC_ERR(err, "change waitset");
                            }
                        }
                        break;

                    default:
                        {
                            fprintf(stderr,
                                    "update waitset on FD type %d NYI.\n",
                                    e->type);
                            assert(!"NYI");
                            errno = EBADF;
                            return -1;
                        }
                    }
                }
            }
        }
    }

    /* go through writefds and update waitsets if necessary. */
    if (writefds != NULL) {
        for (int fd = 0; fd < maxfdp1; fd++) {
            if (FD_ISSET(fd, writefds)) {
                struct fdtab_entry *e = fdtab_get(fd);

                switch (e->type) {
                case FDTAB_TYPE_AVAILABLE:
                    {
                        errno = EBADF;
                        return -1;
                    }
                    break;

                case FDTAB_TYPE_LWIP_SOCKET:
                    /* No update necessary. */
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    /* TODO: is update of internal ws necessary? */
                    break;

                case FDTAB_TYPE_PTM:
                    {
                        struct _pty *pty = e->handle;

                        /* If we're now connected but weren't before. */
                        if (pty->connected &&
                            !FD_ISSET(fd, changed_ws[WRITE_SLOT])) {
                            SELECT_DEBUG("fd %d: added out ws", fd);

                            /* backup waitset */
                            FD_SET(fd, changed_ws[WRITE_SLOT]);
                            ws_store[WRITE_SLOT][fd] = &pty->out_ws;

                            /* change waitset */
                            err = term_server_change_out_ws(pty->ts, ws);
                            if (err_is_fail(err)) {
                                USER_PANIC_ERR(err, "change_waitset");
                            }

                        }
                        break;

                    default:
                        {
                            fprintf(stderr,
                                    "update waitset on FD type %d NYI.\n",
                                    e->type);
                            assert(!"NYI");
                            errno = EBADF;
                            return -1;
                        }
                    }
                }
            }
        }
    }

    return 0;
}

/**
 * \brief Zero passed waitsets.
 */
static inline void zero_fdsets(fd_set *readfds, fd_set *writefds,
                               fd_set *exceptfds)
{
    if (readfds != NULL) {
        FD_ZERO(readfds);
    }
    if (writefds != NULL) {
        FD_ZERO(writefds);
    }
    if (exceptfds != NULL) {
        FD_ZERO(exceptfds);
    }
}

/*************************** debugging functions *******************************/

#if defined(SELECT_DEBUG_ENABLED)
static void debug_fdset_to_string(char *str, size_t size, int maxfdp1,
                                  fd_set *fdset)
{
    assert(str != NULL);
    size_t strlength = 0;

    if (size < 1) {
        return;
    }
    str[0] = '\0';
    if (fdset == NULL) {
        strncat(str, "(null)", size - 1);
        return;
    }

    strncat(str, "{ ", size - 1);
    strlength += 2;

    for (int fd = 0; fd < maxfdp1; fd++) {
        int prt = 0;
        if (FD_ISSET(fd, fdset)) {
            struct fdtab_entry *e = fdtab_get(fd);
            switch (e->type) {
            case FDTAB_TYPE_AVAILABLE:
                prt = snprintf(str + strlength, size - strlength,
                               "%d (avail), ", fd);
                strlength += prt;
                break;

            case FDTAB_TYPE_LWIP_SOCKET:
                prt = snprintf(str + strlength, size - strlength, "%d (lwip), ",
                               fd);
                strlength += prt;
                break;

            case FDTAB_TYPE_UNIX_SOCKET:
                prt = snprintf(str + strlength, size - strlength,
                               "%d (unix sock), ", fd);
                strlength += prt;
                break;

            case FDTAB_TYPE_PTM:
                prt = snprintf(str + strlength, size - strlength, "%d (ptm), ",
                               fd);
                strlength += prt;
                break;

            default:
                prt = snprintf(str + strlength, size - strlength,
                               "%d (unknown), ", fd);
                strlength += prt;
                break;

            }
        }
    }

    if (str[strlength - 2] == ',') {
        str[strlength - 2] = ' ';
        str[strlength - 1] = '}';
    } else {
        strncat(str, " }", size - 1);
        strlength += 2;
    }
}
#else /* SELECT_DEBUG_ENABLED */
static inline void debug_fdset_to_string(char *str, size_t size, int maxfdp1,
                                         fd_set *fdset)
{
}
#endif /* SELECT_DEBUG_ENABLED */

#if defined(SELECT_DEBUG_ENABLED)
#define PRINT_BUFSZ 128
static void debug_print_fdsets(int maxfdp1, fd_set *readfds, fd_set *writefds,
                               fd_set *exceptfds)
{
    char rbuffer[PRINT_BUFSZ];
    char wbuffer[PRINT_BUFSZ];
    char ebuffer[PRINT_BUFSZ];
    debug_fdset_to_string(rbuffer, PRINT_BUFSZ, maxfdp1, readfds);
    debug_fdset_to_string(wbuffer, PRINT_BUFSZ, maxfdp1, writefds);
    debug_fdset_to_string(ebuffer, PRINT_BUFSZ, maxfdp1, exceptfds);
    SELECT_DEBUG("readfds %s | writefds %s | exceptfds %s\n", rbuffer, wbuffer,
                 ebuffer);
}
#else /* defined(SELECT_DEBUG_ENABLED) */
static inline void debug_print_fdsets(int maxfdp1, fd_set *readfds,
                                     fd_set *writefds, fd_set *exceptfds)
{
}
#endif /* defined(SELECT_DEBUG_ENABLED) */

#if defined(SELECT_DEBUG_ENABLED)
static void debug_print_chanstate(struct waitset_chanstate *start)
{
    struct waitset_chanstate *i = start;
    do {
        switch (i->chantype) {
        case CHANTYPE_LMP_IN:
        case CHANTYPE_LMP_OUT:
            SELECT_DEBUG("LMP\n");
            break;

        case CHANTYPE_UMP_IN:
            SELECT_DEBUG("UMP\n");
            break;

        case CHANTYPE_DEFERRED:
            SELECT_DEBUG("deferred\n");
            break;

        case CHANTYPE_LWIP_SOCKET:
            SELECT_DEBUG("lwip\n");
            break;

        default:
            SELECT_DEBUG("other\n");
        }
        i = i->next;
    } while (i != start && i != NULL);
}
#else /* defined(SELECT_DEBUG_ENABLED) */
static inline void debug_print_chanstate(struct waitset_chanstate *start)
{
}
#endif /* defined(SELECT_DEBUG_ENABLED) */

#if defined(SELECT_DEBUG_ENABLED)
static void debug_print_waitset(struct waitset *ws)
{
    if (ws->pending != NULL) {
        SELECT_DEBUG("Ws contains the follwoing channels on the pending "
                     "queue:\n");
        debug_print_chanstate(ws->pending);
    }

    if (ws->polled != NULL) {
        SELECT_DEBUG("Ws contains the following channels on the polled "
                     "queue:\n");
        debug_print_chanstate(ws->polled);
    }

    if (ws->idle != NULL) {
        SELECT_DEBUG("Ws contains the following channels on the polled "
                     "queue:\n");
        debug_print_chanstate(ws->idle);
    }

    if (ws->waiting_threads != NULL) {
        SELECT_DEBUG("Threads are blocked on this waitset.\n");
    }
}
#else /* defined(SELECT_DEBUG_ENABLED) */
static inline void debug_print_waitset(struct waitset *ws)
{
}
#endif /* defined(SELECT_DEBUG_ENABLED) */
