#ifndef SOCK_SERIALISE_H
#define SOCK_SERIALISE_H

#include "lwip/sockets.h"
#include "lwip/tcp.h"
#include "lwip/api.h"

// IK
// ports are in host order
struct lwip_sockinfo {
    struct ip_addr local_ip;
    u16_t local_port;
    struct ip_addr remote_ip;
    u16_t remote_port;
    struct tcp_pcb tcp_state;
    struct netconn netconn_state;

};

int lwip_serialise_sock(int s, struct lwip_sockinfo *si);
int lwip_deserialise_sock(int s, struct lwip_sockinfo *si);

#endif
