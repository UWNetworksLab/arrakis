/*-
 * Copyright (c) 1982, 1985, 1986, 1988, 1993, 1994
 *      The Regents of the University of California.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)socket.h    8.4 (Berkeley) 2/21/94
 * $FreeBSD: head/sys/sys/socket.h 215178 2010-11-12 13:02:26Z luigi $
 */

#ifndef SOCKET_H
#define SOCKET_H

#include <sys/cdefs.h>
#include <lwip/sockets.h>
#include <sys/_iovec.h>

/*
 * Message header for recvmsg and sendmsg calls.
 * Used value-result for recvmsg, value only for sendmsg.
 */
struct msghdr {
        void            *msg_name;              /* optional address */
        socklen_t        msg_namelen;           /* size of address */
        struct iovec    *msg_iov;               /* scatter/gather array */
        int              msg_iovlen;            /* # elements in msg_iov */
        void            *msg_control;           /* ancillary data, see below */
        socklen_t        msg_controllen;        /* ancillary data buffer len */
        int              msg_flags;             /* flags on received message */
};

#if __BSD_VISIBLE
#define AF_LOCAL        AF_UNIX         /* local to host (pipes, portals) */
#endif
#define AF_UNIX         1               /* standardized name for AF_LOCAL */
#define AF_INET6        28              /* IPv6 */
#if __BSD_VISIBLE
#define AF_MAX          38
#endif

#if __BSD_VISIBLE
/*
 * Protocol families, same as address families for now.
 */
#define PF_UNSPEC       AF_UNSPEC
#define PF_LOCAL        AF_LOCAL
#define PF_UNIX         PF_LOCAL        /* backward compatibility */
#define PF_INET         AF_INET
#define PF_IMPLINK      AF_IMPLINK
#define PF_PUP          AF_PUP
#define PF_CHAOS        AF_CHAOS
#define PF_NETBIOS      AF_NETBIOS
#define PF_ISO          AF_ISO
#define PF_OSI          AF_ISO
#define PF_ECMA         AF_ECMA
#define PF_DATAKIT      AF_DATAKIT
#define PF_CCITT        AF_CCITT
#define PF_SNA          AF_SNA
#define PF_DECnet       AF_DECnet
#define PF_DLI          AF_DLI
#define PF_LAT          AF_LAT
#define PF_HYLINK       AF_HYLINK
#define PF_APPLETALK    AF_APPLETALK
#define PF_ROUTE        AF_ROUTE
#define PF_LINK         AF_LINK
#define PF_XTP          pseudo_AF_XTP   /* really just proto family, no AF */
#define PF_COIP         AF_COIP
#define PF_CNT          AF_CNT
#define PF_SIP          AF_SIP
#define PF_IPX          AF_IPX
#define PF_RTIP         pseudo_AF_RTIP  /* same format as AF_INET */
#define PF_PIP          pseudo_AF_PIP
#define PF_ISDN         AF_ISDN
#define PF_KEY          pseudo_AF_KEY
#define PF_INET6        AF_INET6
#define PF_NATM         AF_NATM
#define PF_ATM          AF_ATM
#define PF_NETGRAPH     AF_NETGRAPH
#define PF_SLOW         AF_SLOW
#define PF_SCLUSTER     AF_SCLUSTER
#define PF_ARP          AF_ARP
#define PF_BLUETOOTH    AF_BLUETOOTH
#define PF_IEEE80211    AF_IEEE80211
#define PF_INET_SDP     AF_INET_SDP
#define PF_INET6_SDP    AF_INET6_SDP

#define PF_MAX          AF_MAX
#endif

#ifndef _SA_FAMILY_T_DECLARED
typedef __sa_family_t	sa_family_t;
#define _SA_FAMILY_T_DECLARED
#endif

#include <sys/_sockaddr_storage.h>

__BEGIN_DECLS
ssize_t recv(int sockfd, void *buf, size_t len, int flags);
ssize_t recvfrom(int sockfd, void *buf, size_t len, int flags,
                 struct sockaddr *src_addr, socklen_t *addrlen);
ssize_t recvmsg(int, struct msghdr *, int);
ssize_t send(int sockfd, const void *buf, size_t len, int flags);
ssize_t sendto(int sockfd, const void *buf, size_t len, int flags,
               const struct sockaddr *dest_addr, socklen_t addrlen);
ssize_t sendmsg(int, const struct msghdr *, int);
int socket(int domain, int type, int protocol);
int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int listen(int sockfd, int backlog);
int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int getsockopt(int sockfd, int level, int optname, void *optval,
               socklen_t *optlen);
int setsockopt(int sockfd, int level, int optname, const void *optval,
               socklen_t optlen);
int connect(int sockfd, const struct sockaddr *addr, socklen_t addrlen);
int getsockname(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int getpeername(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
int shutdown(int sockfd, int how);
int socketpair(int domain, int type, int protocol, int sockfd[2]);
__END_DECLS

#endif
