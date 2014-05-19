/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef ARRAKIS_ARRANET_H
#define ARRAKIS_ARRANET_H

struct packet {
    struct packet       *next;
    uint8_t             *payload;
    lpaddr_t            pa;
    size_t              len;
    uint64_t            flags;
    struct socket       *sock;
    void                *opaque;
};

typedef void (*arranet_tx_done_fn)(void *opaque);

int recvfrom_arranet(int sockfd, void **buf, struct packet **p,
                     struct sockaddr *src_addr, socklen_t *addrlen);
void arranet_recv_free(struct packet *p);
int sendmsg_arranet(int sockfd, const struct msghdr *msg);
void arranet_register_tx_done_callback(arranet_tx_done_fn callback);

#define SENDMSG_WITH_COPY

#endif
