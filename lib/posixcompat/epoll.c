/*
 * Copyright (c) 2013, University of Washington.
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
#include <vfs/fdtab.h>

#include "posixcompat.h"
#include "unixsock.h"

#include <assert.h>
#include <sys/epoll.h>

#define MAX_EPOLL_EVENTS    16

struct _epoll_fd {
    struct waitset ws;
    struct _epoll_events_list *events;
};

int epoll_create(int size)
{
    // size is ignored these days, even on Linux
    return epoll_create1(0);
}

int epoll_create1(int flags)
{
    struct fdtab_entry e;
    struct _epoll_fd *efd = malloc(sizeof(struct _epoll_fd));
    assert(efd != NULL);

    memset(efd, 0, sizeof(struct _epoll_fd));
    waitset_init(&efd->ws);

    e.type = FDTAB_TYPE_EPOLL_INSTANCE;
    e.handle = efd;
    e.inherited = false;
    e.epoll_fd = -1;

    int fd = fdtab_alloc(&e);
    POSIXCOMPAT_DEBUG("epoll_create1(%d) as fd %d\n", flags, fd);
    if (fd < 0) {
        return -1;
    } else {
        return fd;
    }
}

int epoll_ctl(int epfd, int op, int fd, struct epoll_event *event)
{
    struct fdtab_entry *mye = fdtab_get(epfd);
    assert(mye->type == FDTAB_TYPE_EPOLL_INSTANCE);
    struct _epoll_fd *efd = mye->handle;
    int ret = 0;

    if(op != EPOLL_CTL_DEL) {
        assert(!(event->events & EPOLLRDHUP));
        assert(!(event->events & EPOLLPRI));
        assert(!(event->events & EPOLLERR));
        assert(!(event->events & EPOLLHUP));
        assert(!(event->events & EPOLLET));
        assert(!(event->events & EPOLLONESHOT));
    }

    switch(op) {
    case EPOLL_CTL_ADD:
        // Add event/FD to events/FDs list
        {
            // Check that it's not already in the list
            /* for(struct _epoll_events_list *i = efd->events; i != NULL; i = i->next) { */
            /*     assert(i->fd != fd); */
            /* } */

            struct fdtab_entry *e = fdtab_get(fd);
            if(e->epoll_fd == epfd) {
                errno = EEXIST;
                ret = -1;
                break;
            }
            assert(e->epoll_fd == -1);
            e->epoll_fd = epfd;
            struct _epoll_events_list *li = &e->epoll_events;
            li->prev = NULL;
            li->next = efd->events;
            if(li->next != NULL) {
                li->next->prev = li;
            }
            li->event = *event;
            li->fd = fd;
            efd->events = li;
        }
        break;

    case EPOLL_CTL_DEL:
        {
#if 0
            struct _epoll_events_list *lasti = NULL, *i;
            for(i = efd->events; i != NULL; i = i->next) {
                if(i->fd == fd) {
                    if(lasti == NULL) {
                        // First entry in list
                        efd->events = i->next;
                    } else {
                        // Anywhere else
                        lasti->next = i->next;
                    }

                    free(i);

                    struct fdtab_entry *e = fdtab_get(fd);
                    assert(e->epoll_fd != -1);
                    e->epoll_fd = -1;
                    break;
                }

                lasti = i;
            }

            if(i == NULL) {
                errno = ENOENT;
                ret = -1;
            }
#else
            struct fdtab_entry *e = fdtab_get(fd);
            assert(e->epoll_fd != -1);
            e->epoll_fd = -1;
            if(&e->epoll_events == efd->events) {
                // First entry in list -- update head
                efd->events = e->epoll_events.next;
            }
            if(e->epoll_events.next != NULL) {
                e->epoll_events.next->prev = e->epoll_events.prev;
            }
            if(e->epoll_events.prev != NULL) {
                e->epoll_events.prev->next = e->epoll_events.next;
            }
#endif
        }
        break;

    case EPOLL_CTL_MOD:
        {
            struct _epoll_events_list *i;
            for(i = efd->events; i != NULL; i = i->next) {
                if(i->fd == fd) {
                    // Found
                    i->event = *event;
                    break;
                }
            }

            if(i == NULL) {
                errno = ENOENT;
                ret = -1;
            }
        }
        break;

    default:
        errno = EINVAL;
        return -1;
    }

    return ret;
}

struct timeout_event {
  bool fired;
};

static void timeout_fired(void *arg)
{
  struct timeout_event *toe = arg;
  assert(toe != NULL);
  toe->fired = true;
}

int epoll_wait(int epfd, struct epoll_event *events,
               int maxevents, int timeout)
{
    struct fdtab_entry *mye = fdtab_get(epfd);
    assert(mye->type == FDTAB_TYPE_EPOLL_INSTANCE);
    struct _epoll_fd *efd = mye->handle;
    struct monitor_binding *mb = get_monitor_binding();
    errval_t err;

    /* waitset_init(&efd->ws); */
    assert(maxevents >= 1);

    for(struct _epoll_events_list *i = efd->events; i != NULL; i = i->next) {
        struct fdtab_entry *e = fdtab_get(i->fd);
        struct epoll_event *event = &i->event;

        switch (e->type) {
        case FDTAB_TYPE_LWIP_SOCKET:
            {
                int retval;

                lwip_mutex_lock();
                if(event->events & EPOLLIN) {
                    retval = lwip_sock_waitset_register_read(e->fd, &efd->ws);
                    assert(retval == 0);
                }
                if(event->events & EPOLLOUT) {
                    retval = lwip_sock_waitset_register_write(e->fd, &efd->ws);
                    assert(retval == 0);
                }
                lwip_mutex_unlock();
            }
            break;

        case FDTAB_TYPE_UNIX_SOCKET:
            {
                struct _unix_socket *us = e->handle;

                if(event->events & EPOLLIN) {
                    if (us->passive) { /* passive side */
                        int j;

                        /* Check for pending connection requests. */
                        for (j = 0; j < us->u.passive.max_backlog; j++)
                            {
                                if (us->u.passive.backlog[j] != NULL) {
                                    break;
                                }
                            }

                        /*
                         * If there are not pending connection request
                         * wait on monitor binding.
                         */
                        if (j == us->u.passive.max_backlog) {
                            /* wait on monitor */
                            err = mb->change_waitset(mb, &efd->ws);
                            if (err_is_fail(err)) {
                                USER_PANIC_ERR(err, "change_waitset");
                            }
                        }
                    }
                }

                if(event->events & EPOLLOUT) {
                    assert(!us->passive);

                    if(us->u.active.mode == _UNIX_SOCKET_MODE_CONNECTING) {
                        /* wait on monitor */
                        err = mb->change_waitset(mb, &efd->ws);
                        if (err_is_fail(err)) {
                            USER_PANIC_ERR(err, "change_waitset");
                        }
                    }
                }

                assert(event->events & (EPOLLIN | EPOLLOUT));

                // Change waitset
                err = us->u.active.binding->change_waitset
                    (us->u.active.binding, &efd->ws);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "change waitset");
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

    // Timeout handling
    struct timeout_event toe = {
      .fired = false
    };
    struct deferred_event timeout_event;
    if (timeout > 0) {
        deferred_event_init(&timeout_event);
        err = deferred_event_register(&timeout_event, &efd->ws, timeout,
                                      MKCLOSURE(timeout_fired, &toe));
        if (err_is_fail(err)) {
            errno = EINVAL;
            return -1;
        }
    }

    int retevents = 0;
    while(!toe.fired && retevents == 0) {
        if(timeout == 0) {
            // Just poll once, don't block
            err = event_dispatch_non_block(&efd->ws);
            assert(err_is_ok(err) || err_no(err) == LIB_ERR_NO_EVENT);
            toe.fired = true;
        } else {
            err = event_dispatch(&efd->ws);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "Error in event_dispatch.");
            }
        }

        // Return ready file descriptors
        for(struct _epoll_events_list *i = efd->events; i != NULL; i = i->next) {
            struct epoll_event *event = &i->event;
            struct fdtab_entry *e = fdtab_get(i->fd);

            assert(retevents < maxevents);
            events[retevents] = *event;
            events[retevents].events = 0;

            // Check errors (hangup)
            {
                switch (e->type) {
                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        lwip_mutex_lock();
                        if (!lwip_sock_is_open(e->fd)) {
                            events[retevents].events |= EPOLLHUP;
                        }
                        lwip_mutex_unlock();
                    }
                    break;

                default:
                    // No-Op
                    break;
                }
            }

            // Check readable FDs
            if(event->events & EPOLLIN) {
                switch (e->type) {
                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        lwip_mutex_lock();
                        if (lwip_sock_ready_read(e->fd)) {
                            events[retevents].events |= EPOLLIN;
                        }
                        lwip_mutex_unlock();
                    }
                    break;

                case FDTAB_TYPE_UNIX_SOCKET:
                    {
                        struct _unix_socket *us = e->handle;

                        if (us->passive) { /* passive side */
                            /* Check for pending connection requests. */
                            for (int j = 0; j < us->u.passive.max_backlog; j++)
                                {
                                    if (us->u.passive.backlog[j] != NULL) {
                                        events[retevents].events |= EPOLLIN;
                                        break;
                                    }
                                }
                        } else { /* active side */
                            /* Check for incoming data. */
                            if (us->recv_buf_valid > 0) {
                                events[retevents].events |= EPOLLIN;
                            }
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "epoll_wait() on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }

            // Check writeable FDs
            if(event->events & EPOLLOUT) {
                switch (e->type) {
                case FDTAB_TYPE_LWIP_SOCKET:
                    {
                        lwip_mutex_lock();
                        if (lwip_sock_ready_write(e->fd)) {
                            events[retevents].events |= EPOLLOUT;
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
                                events[retevents].events |= EPOLLOUT;
                            }
                            break;
                        }
                    }
                    break;

                default:
                    {
                        fprintf(stderr, "epoll_wait() on FD type %d NYI.\n",
                                e->type);
                        assert(!"NYI");
                        errno = EBADF;
                        return -1;
                    }
                }
            }

            // If any events were returned, go to next entry in array
            if(events[retevents].events != 0) {
                retevents++;
            }
        }
    }

    // Remove timeout from waitset if it was set and not fired
    if(timeout > 0 && !toe.fired) {
        deferred_event_cancel(&timeout_event);
    }

    // Restore old waitsets
    for(struct _epoll_events_list *i = efd->events; i != NULL; i = i->next) {
        struct fdtab_entry *e = fdtab_get(i->fd);
        struct epoll_event *event = &i->event;

        switch (e->type) {
        case FDTAB_TYPE_LWIP_SOCKET:
            {
                lwip_mutex_lock();
                if(event->events & EPOLLIN) {
                    err = lwip_sock_waitset_deregister_read(e->fd);
                    if (err_is_fail(err) &&
                        err_no(err) != LIB_ERR_CHAN_NOT_REGISTERED) {
                        USER_PANIC_ERR(err, "error deregister read channel for "
                                       "lwip socket");
                    }
                }
                if(event->events & EPOLLOUT) {
                    err = lwip_sock_waitset_deregister_write(e->fd);
                    if (err_is_fail(err) &&
                        err_no(err) != LIB_ERR_CHAN_NOT_REGISTERED) {
                        USER_PANIC_ERR(err, "error deregister write channel for "
                                       "lwip socket");
                    }
                }
                lwip_mutex_unlock();
            }
            break;

        case FDTAB_TYPE_UNIX_SOCKET:
            {
                // NYI
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

    return retevents;
}

int epoll_pwait(int epfd, struct epoll_event *events,
                int maxevents, int timeout,
                const sigset_t *sigmask)
{
    assert(!"NYI");
}
