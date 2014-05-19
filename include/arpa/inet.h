#ifndef _ARPA_INET_H
#define _ARPA_INET_H

#include <lwip/sockets.h>

const char *
inet_ntop(int af, const void * __restrict src, char * __restrict dst,
          socklen_t size);
int inet_pton(int af, const char * __restrict src, void * __restrict dst);

#endif
