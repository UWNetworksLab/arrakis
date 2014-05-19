/*
 * Copyright (c) 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <sys/socket.h>
#include <netdb.h>
#include <errno.h>
#include <unistd.h>
#include <barrelfish/barrelfish.h>
#include <lwip/sys.h>
#include "posixcompat.h"
#include <vfs/fdtab.h>
#include "unixsock.h"

#define MIN(a,b)        ((a) < (b) ? (a) : (b))

ssize_t recv(int sockfd, void *buf, size_t len, int flags)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;

            // XXX: Don't support flags
            assert(flags == 0);

            thread_mutex_lock(&us->mutex);

            if(us->passive
               || us->u.active.mode != _UNIX_SOCKET_MODE_CONNECTED) {
                errno = ENOTCONN;
                thread_mutex_unlock(&us->mutex);
                return -1;
            }

            if(us->recv_buf_valid == 0) {
                // No more data
                if(us->nonblocking) {
                    errno = EAGAIN;
                    thread_mutex_unlock(&us->mutex);
                    return -1;
                } else {
                    struct waitset ws;
                    errval_t err;

                    waitset_init(&ws);

                    err = us->u.active.binding->change_waitset
                        (us->u.active.binding, &ws);
                    if(err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }

                    while(us->recv_buf_valid == 0) {
                        err = event_dispatch(&ws);
                        if(err_is_fail(err)) {
                            USER_PANIC_ERR(err, "waitset_destroy");
                        }
                    }

                    // XXX: Assume it was on the default waitset
                    err = us->u.active.binding->change_waitset
                        (us->u.active.binding, get_default_waitset());
                    if(err_is_fail(err)) {
                        USER_PANIC_ERR(err, "change_waitset");
                    }

                    err = waitset_destroy(&ws);
                    if(err_is_fail(err)) {
                        USER_PANIC_ERR(err, "waitset_destroy");
                    }
                }
            }

            size_t recved = 0;
            while(recved < len && us->recv_list != NULL) {
                struct _unix_socket_recv *usr = us->recv_list;
                size_t consume = MIN(len - recved, usr->size - usr->consumed);

                memcpy(buf + recved, &usr->msg[usr->consumed], consume);
                usr->consumed += consume;
                us->recv_buf_valid -= consume;
                recved += consume;

                if(usr->consumed == usr->size) {
                    us->recv_list = usr->next;
                    if(us->recv_list == NULL) {
                        us->recv_list_end = NULL;
                    }
                    free(usr->msg);
                    free(usr);
                } else {
                    assert(recved == len);
                }
            }

            thread_mutex_unlock(&us->mutex);
            return recved;
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        ssize_t ret = lwip_recv(e->fd, buf, len, flags);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *src_addr, socklen_t *addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        assert(!"NYI");
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        ssize_t ret = lwip_recvfrom(e->fd, buf, len, flags, src_addr, addrlen);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

static void unixsock_sent(void *arg)
{
    struct _unix_socket *us = arg;

    assert(us != NULL);
    assert(us->send_buf != NULL);

    free(us->send_buf);
    us->send_buf = NULL;
}

ssize_t send(int sockfd, const void *buf, size_t len, int flags)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;

            // XXX: Don't support flags
            assert(flags == 0);

            thread_mutex_lock(&us->mutex);

            if(us->passive
               || us->u.active.mode != _UNIX_SOCKET_MODE_CONNECTED) {
                errno = ENOTCONN;
                thread_mutex_unlock(&us->mutex);
                return -1;
            }

            if(us->send_buf != NULL) {
                if(us->nonblocking) {
                    errno = EAGAIN;
                    thread_mutex_unlock(&us->mutex);
                    return -1;
                } else {
                    assert(!"NYI");
                }
            }

            // Bleh. Gotta copy here. I can't just wait until the
            // message is fully sent, as that might block
            // indefinitely.
            us->send_buf = malloc(len);
            memcpy(us->send_buf, buf, len);

            struct event_closure ec = {
                .handler = unixsock_sent,
                .arg = us,
            };
            errval_t err = us->u.active.binding->tx_vtbl.
                send(us->u.active.binding, ec, us->send_buf, len);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "unixsock->send");
                thread_mutex_unlock(&us->mutex);
                return -1;
            }

            // Wait until all data sent if blocking
            if(!us->nonblocking) {
                struct waitset ws;
                waitset_init(&ws);

                err = us->u.active.binding->change_waitset
                    (us->u.active.binding, &ws);
                if(err_is_fail(err)) {
                    USER_PANIC_ERR(err, "change_waitset");
                }

                while(us->send_buf != NULL) {
                    err = event_dispatch(&ws);
                    if(err_is_fail(err)) {
                        USER_PANIC_ERR(err, "waitset_destroy");
                    }
                }

                // XXX: Assume it was on the default waitset
                err = us->u.active.binding->change_waitset
                    (us->u.active.binding, get_default_waitset());
                if(err_is_fail(err)) {
                    USER_PANIC_ERR(err, "change_waitset");
                }

                err = waitset_destroy(&ws);
                if(err_is_fail(err)) {
                    USER_PANIC_ERR(err, "waitset_destroy");
                }
            }

            // XXX: We send all or nothing
            thread_mutex_unlock(&us->mutex);
            return len;
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        ssize_t ret = lwip_send(e->fd, buf, len, flags);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
               const struct sockaddr *dest_addr, socklen_t addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        assert(!"NYI");
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        ssize_t ret = lwip_sendto(e->fd, buf, len, flags, dest_addr, addrlen);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

ssize_t sendmsg(int sockfd, const struct msghdr *msg, int flags)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        assert(!"NYI");
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        assert(msg != NULL);
        assert(msg->msg_control == NULL);
        assert(msg->msg_controllen == 0);

#if 0
        // XXX: Copy all buffers into one. Should instead have an lwIP interface for this.
        size_t totalsize = 0;
        for(int i = 0; i < msg->msg_iovlen; i++) {
            totalsize += msg->msg_iov[i].iov_len;
        }

        char *buf = malloc(totalsize);

        size_t pos = 0;
        for(int i = 0; i < msg->msg_iovlen; i++) {
            memcpy(&buf[pos], msg->msg_iov[i].iov_base, msg->msg_iov[i].iov_len);
            pos += msg->msg_iov[i].iov_len;
        }

        lwip_mutex_lock();
        ssize_t ret = lwip_sendto(e->fd, buf, totalsize, flags,
                                  msg->msg_name, msg->msg_namelen);
        lwip_mutex_unlock();
        free(buf);
#else
        lwip_mutex_lock();
        ssize_t ret = lwip_sendmsg(e->fd, msg, flags);
        lwip_mutex_unlock();
#endif

        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

int socket(int domain, int type, int protocol)
{
    struct fdtab_entry e;

    switch(domain) {
    case AF_UNIX:
        if(type != SOCK_STREAM) {
            errno = EPROTOTYPE;
            return -1;
        }

        if(protocol != 0) {
            errno = EPROTONOSUPPORT;
            return -1;
        }

        struct _unix_socket *us = malloc(sizeof(struct _unix_socket));
        assert(us != NULL);

        memset(us, 0, sizeof(struct _unix_socket));
        us->type = type;
        us->protocol = protocol;
        us->recv_buf_size = _UNIX_SOCKET_RECV_BUF_SIZE;

        /*
         * XXX: Even though POSIX says the result of
         * get{sock,peer}name() shall be unspecified if not bound,
         * some programs still expect its type to be of AF_UNIX.
         */
        us->sockaddr.sun_len = us->peer.sun_len = sizeof(struct sockaddr_un);
        us->sockaddr.sun_family = us->peer.sun_family = AF_UNIX;

        e.type = FDTAB_TYPE_UNIX_SOCKET;
        e.handle = us;
        e.inherited = false;
        e.epoll_fd = -1;
        break;

    case AF_INET:
        {
            lwip_mutex_lock();
            int fd = lwip_socket(domain, type, protocol);
            lwip_mutex_unlock();

            if(fd == -1) {
                return fd;
            }
			
            e.type = FDTAB_TYPE_LWIP_SOCKET;
            e.fd = fd;
            e.inherited = false;
            e.epoll_fd = -1;
        }
        break;

    default:
        errno = EAFNOSUPPORT;
        return -1;
    }

    int fd = fdtab_alloc(&e);
    POSIXCOMPAT_DEBUG("socket(%d, %d, %d) as fd %d\n", domain, type, protocol, fd);
    if (fd < 0) {
        return -1;
    } else {
        return fd;
    }
}

int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        assert(addrlen >= sizeof(struct sockaddr_un));
        assert(addr->sa_family == AF_UNIX);
        struct _unix_socket *us = e->handle;
        memcpy(&us->sockaddr, addr, sizeof(struct sockaddr_un));

        POSIXCOMPAT_DEBUG("bind(%d, %p (%s), %u)\n", sockfd, addr,
                          us->sockaddr.sun_path, addrlen);

        // XXX: Should fail if file already exists

        // Create socket file
        errval_t err = vfs_create(us->sockaddr.sun_path, &us->vfs_handle);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "vfs_create");
            return -1;
        }
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret = lwip_bind(e->fd, addr, addrlen);
        lwip_mutex_unlock();
        return ret;

    default:
        return -1;
    }

    return 0;
}

static void unixsock_recv(struct unixsock_binding *b, uint8_t *msg, size_t size)
{
    struct _unix_socket *us = b->st;

    assert(us != NULL);
    assert(!us->passive);
    assert(us->u.active.mode == _UNIX_SOCKET_MODE_CONNECTED);

    struct _unix_socket_recv *usr = malloc(sizeof(struct _unix_socket_recv));
    assert(usr != NULL);

    // Append to receive queue
    usr->next = NULL;
    usr->prev = us->recv_list_end;
    usr->msg = msg;
    usr->size = size;
    usr->consumed = 0;
    if(us->recv_list_end != NULL) {
        assert(us->recv_list_end->next == NULL);
        us->recv_list_end->next = usr;
    } else {
        us->recv_list = usr;
    }
    us->recv_list_end = usr;

    us->recv_buf_valid += size;
}

static struct unixsock_rx_vtbl unixsock_vtbl = {
    .send = unixsock_recv,
};

static void unixsock_listening(void *st, errval_t err, iref_t iref)
{
    struct _unix_socket *us = st;

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "unixsock_listening");
    }

    us->u.passive.listen_iref = iref;
}

static errval_t unixsock_connected(void *st, struct unixsock_binding *b)
{
    struct _unix_socket *us = st;
    int i;

    b->rx_vtbl = unixsock_vtbl;

    assert(us->passive);
    for(i = 0; i < us->u.passive.max_backlog; i++) {
        if(us->u.passive.backlog[i] == NULL) {
            us->u.passive.backlog[i] = b;
            break;
        }
    }

    if(i == us->u.passive.max_backlog) {
        USER_PANIC("connection backlog exceeded");
    } else {
        return SYS_ERR_OK;
    }
}

int listen(int sockfd, int backlog)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        POSIXCOMPAT_DEBUG("listen(%d, %d)\n", sockfd, backlog);
        struct _unix_socket *us = e->handle;

        errval_t err =
            unixsock_export(us, unixsock_listening, unixsock_connected,
                            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
        if(err_is_fail(err)) {
            DEBUG_ERR(err, "unixsock_export failed");
            return -1;
        }
        while(us->u.passive.listen_iref == NULL_IREF) {
            // XXX: Should wait only on monitor
            event_dispatch(get_default_waitset());
        }

        us->passive = true;
        us->u.passive.max_backlog = backlog;
        us->u.passive.backlog = calloc(backlog, sizeof(struct unixsock_binding *));

        char str[128];
        snprintf(str, 128, "%"PRIuIREF, us->u.passive.listen_iref);
        err = vfs_write(us->vfs_handle, str, strlen(str), NULL);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "vfs_write");
        }
        break;

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret = lwip_listen(e->fd, backlog);
        lwip_mutex_unlock();
        return ret;

    default:
        return -1;
    }

    return 0;
}

int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;
            int i;

            if(!us->passive) {
                return -1;
            }

            for(;;) {
                for(i = 0; i < us->u.passive.max_backlog; i++) {
                    if(us->u.passive.backlog[i] != NULL) {
                        break;
                    }
                }

                if(i != us->u.passive.max_backlog) {
                    // Found a pending connection
                    break;
                }

                if(us->nonblocking) {
                    errno = EAGAIN;
                    return -1;
                }

                // XXX: This should wait only on the monitor
                event_dispatch(get_default_waitset());
            }

            // XXX: Using socket() to create new socket. Probably dangerous.
            int newfd = socket(AF_UNIX, us->type, us->protocol);
            assert(newfd != -1);
            struct _unix_socket *newus = fdtab_get(newfd)->handle;
            newus->u.active.binding = us->u.passive.backlog[i];
            newus->u.active.mode = _UNIX_SOCKET_MODE_CONNECTED;
            memcpy(&newus->sockaddr, &us->sockaddr, sizeof(struct sockaddr_un));

            // Associate binding with new socket and remove from backlog
            assert(newus->u.active.binding->st == NULL);
            newus->u.active.binding->st = newus;
            us->u.passive.backlog[i] = NULL;

            if(addr != NULL) {
                assert(addrlen != NULL);

                // TODO: Should request address from peer (if peer is bound)

                if(*addrlen > sizeof(struct sockaddr_un)) {
                    *addrlen = sizeof(struct sockaddr_un);
                }

                memcpy(addr, &newus->peer, *addrlen);
            }

            return newfd;
        }

	case FDTAB_TYPE_LWIP_SOCKET:
            {
                lwip_mutex_lock();
                int newfd = lwip_accept(e->fd, addr, addrlen);
                lwip_mutex_unlock();

                if(newfd != -1) {
                    struct fdtab_entry newe;
                    newe.type = FDTAB_TYPE_LWIP_SOCKET;
                    newe.fd = newfd;
                    newe.inherited = false;
                    newe.epoll_fd = -1;

                    newfd = fdtab_alloc(&newe);
                    POSIXCOMPAT_DEBUG("accept(%d, _, _) as fd %d\n", sockfd, newfd);
                    if (newfd < 0) {
                        return -1;
                    }
                }
			
                return newfd;
            }

    default:
        return -1;
    }
}

int getsockopt(int sockfd, int level, int optname, void *restrict optval,
               socklen_t *restrict optlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            /* struct _unix_socket *us = e->handle; */

            switch(level) {
            case SOL_SOCKET:
                switch(optname) {
                case SO_ERROR:
                    {
                        int *val = optval;

                        if(*optlen < sizeof(int)) {
                            // XXX: Store nothing if we can't store it all
                            return 0;
                        }

                        *optlen = sizeof(int);
                        // XXX: We have no errors so far
                        *val = 0;

                        return 0;
                    }

                default:
                    errno = EINVAL;
                    return -1;
                }

            default:
                errno = EINVAL;
                return -1;
            }
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret = lwip_getsockopt(e->fd, level, optname, optval, optlen);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }

    return 0;
}

int setsockopt(int sockfd, int level, int optname, const void *optval,
               socklen_t optlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_LWIP_SOCKET:
        return lwip_setsockopt(e->fd, level, optname, optval, optlen);

    default:
        assert(!"NYI");
        break;
    }
}

static void unixsock_bound(void *st, errval_t err, struct unixsock_binding *b)
{
    struct _unix_socket *us = st;

    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "unixsock_bound");
    }

    assert(us != NULL);
    assert(b != NULL);
    assert(!us->passive);
    assert(us->u.active.mode == _UNIX_SOCKET_MODE_CONNECTING);

    b->rx_vtbl = unixsock_vtbl;
    b->st = st;
    us->u.active.binding = b;
    us->u.active.mode = _UNIX_SOCKET_MODE_CONNECTED;
}

int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;

            if(us->passive) {
                return -1;
            }

            if(addr->sa_family != AF_UNIX
               || addrlen != sizeof(struct sockaddr_un)) {
                errno = EINVAL;
                return -1;
            }

            memcpy(&us->peer, addr, addrlen);

            errval_t err = vfs_open(us->peer.sun_path, &us->vfs_handle);
            if(err_is_fail(err)) {
                if(err_no(err) == FS_ERR_NOTFOUND
                   || err_no(err) == FS_ERR_NOTDIR) {
                    errno = ENOENT;
                } else {
                    DEBUG_ERR(err, "vfs_open");
                }
                return -1;
            }

            char str[128];
            err = vfs_read(us->vfs_handle, str, 128, NULL);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "vfs_read");
            }

            iref_t iref = atoi(str);

            err = unixsock_bind(iref, unixsock_bound, us, get_default_waitset(),
                                IDC_BIND_FLAGS_DEFAULT);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "unixsock_bind");
            }

            us->u.active.mode = _UNIX_SOCKET_MODE_CONNECTING;

            if(us->nonblocking) {
                errno = EINPROGRESS;
                return -1;
            }

            while(us->u.active.mode == _UNIX_SOCKET_MODE_CONNECTING) {
                // XXX: Should wait only on monitor
                event_dispatch(get_default_waitset());
            }

            assert(us->u.active.mode == _UNIX_SOCKET_MODE_CONNECTED);
            return 0;
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret =  lwip_connect(e->fd, addr, addrlen);
        lwip_mutex_unlock();
        return ret;

    default:
        return -1;
    }
}

int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch(e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;

            if(*addrlen > sizeof(struct sockaddr_un)) {
                *addrlen = sizeof(struct sockaddr_un);
            }

            memcpy(addr, &us->sockaddr, *addrlen);

            return 0;
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        lwip_mutex_lock();
        int ret = lwip_getsockname(e->fd, addr, addrlen);
        lwip_mutex_unlock();
        return ret;

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

struct hostent *gethostbyname(const char *name)
{
    if(!strcmp(name, "localhost")) {
        POSIXCOMPAT_DEBUG("gethostbyname(\"%s\") always returns NULL.\n", name);
        errno = HOST_NOT_FOUND;
        return NULL;
    } else {
        lwip_mutex_lock();
        struct hostent *ret =  lwip_gethostbyname(name);
        lwip_mutex_unlock();
        return ret;
    }
}

struct hostent *gethostbyaddr(const void *addr, socklen_t len, int type)
{
    assert(!"NYI");
    return NULL;
}

int getaddrinfo(const char *restrict nodename,
                const char *restrict servname,
                const struct addrinfo *restrict hints,
                struct addrinfo **restrict res)
{
    POSIXCOMPAT_DEBUG("getaddrinfo called %s, %s\n", nodename, servname);
    return lwip_getaddrinfo(nodename, servname, hints, res);
}

void freeaddrinfo(struct addrinfo *ai)
{
    return lwip_freeaddrinfo(ai);
}

const char *gai_strerror(int ecode)
{
    return "No error";
}

int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch (e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        {
            struct _unix_socket *us = e->handle;

            if (*addrlen > sizeof(struct sockaddr_un)) {
                *addrlen = sizeof(struct sockaddr_un);
            }

            memcpy(addr, &us->peer, *addrlen);

            return 0;
        }

    case FDTAB_TYPE_LWIP_SOCKET:
        return lwip_getpeername(e->fd, addr, addrlen);

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

int shutdown(int sockfd, int how)
{
    struct fdtab_entry *e = fdtab_get(sockfd);

    switch (e->type) {
    case FDTAB_TYPE_UNIX_SOCKET:
        assert(!"NYI");
        return -1;

    case FDTAB_TYPE_LWIP_SOCKET:
        return lwip_shutdown(e->fd, how);

    case FDTAB_TYPE_AVAILABLE:
        errno = EBADF;
        return -1;

    default:
        errno = ENOTSOCK;
        return -1;
    }
}

/**
 * \brief Create a pair of connected sockets.
 */
int socketpair(int domain, int type, int protocol, int sockfd[2])
{
    int sock;
    struct sockaddr_un tmpaddr = {
        .sun_family = AF_UNIX,
    };

    // Only support AF_UNIX sockets
    if(domain != AF_UNIX) {
        errno = EOPNOTSUPP;
        return -1;
    }

    snprintf(tmpaddr.sun_path, 104, "/tmp/posixcompat.tmp.socket.%" PRIu64, rdtsc());

    if((sockfd[0] = socket(domain, type, protocol)) == -1) {
        return -1;
    }
    if((sockfd[1] = socket(domain, type, protocol)) == -1) {
        return -1;
    }

    if(bind(sockfd[0], (struct sockaddr *)&tmpaddr, sizeof(tmpaddr)) != 0) {
        close(sockfd[0]);
        close(sockfd[1]);
        return -1;
    }

    if(listen(sockfd[0], 1) != 0) {
        close(sockfd[0]);
        close(sockfd[1]);
        return -1;
    }

    if(connect(sockfd[1], (struct sockaddr *)&tmpaddr, sizeof(tmpaddr)) != 0) {
        close(sockfd[0]);
        close(sockfd[1]);
        return -1;
    }

    if((sock = accept(sockfd[0], NULL, NULL)) == -1) {
        close(sockfd[0]);
        close(sockfd[1]);
        return -1;
    }

    // Delete listening socket and return connected socket
    close(sockfd[0]);
    sockfd[0] = sock;

    // XXX: Should delete temporary socket name

    return 0;
}
