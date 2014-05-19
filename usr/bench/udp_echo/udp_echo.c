/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef BARRELFISH
#define _POSIX_C_SOURCE 199309L
#endif

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#ifdef BARRELFISH
#       include <netif/e1000.h>
#       include <arranet.h>
#       include <arranet_debug.h>
extern int posix_recv_time[POSIX_TRANSA];       // Time until packet at exit of recvfrom
extern size_t posix_recv_transactions;
#else
#	include <time.h>
#       include <strings.h>
#endif

/* #include "hash.c" */

#define	timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)

#define BUFSIZE                 1024
#define MAX_ROUNDS              1000

//#define SIMULATE_PROC_TIME
//#define ARRANET_RECVFROM

#define MIN(a,b)        ((a) < (b) ? (a) : (b))
#define MAX(a,b)        ((a) > (b) ? (a) : (b))

static char initbuf[] = "123456789012345678901234567890123456789012345678901234567890123";

static struct timeval tvs[MAX_ROUNDS];

/*
 * error - wrapper for perror
 */
static void error(char *msg) {
  perror(msg);
  exit(1);
}

#ifndef BARRELFISH
static inline uint64_t rdtsc(void)
{
    uint32_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return ((uint64_t)edx << 32) | eax;
}
#endif

#ifndef SENDMSG_WITH_COPY
static void tx_done(void *opaque)
{
    /* printf("Handling %p back to stack\n", opaque); */
    arranet_recv_free(opaque);
}
#endif

int main(int argc, char **argv) {
  int sockfd; /* socket */
  int portno; /* port to listen on */
  socklen_t clientlen; /* byte size of client's address */
  struct sockaddr_in serveraddr; /* server's addr */
  struct sockaddr_in clientaddr; /* client addr */
  /* struct hostent *hostp; /\* client host info *\/ */
  static char buf[BUFSIZE]; /* message buf */
  /* char *hostaddrp; /\* dotted decimal host addr string *\/ */
#ifndef BARRELFISH
  int optval; /* flag value for setsockopt */
#endif
  int proctime = 0;
  int n; /* message byte size */

#ifdef BARRELFISH
  lwip_arrakis_start(&argc, &argv);

  // LLC misses
  invoke_perfmon_activate(cap_perfmon,
                          0x2e,   // Event to monitor
                          0x41,   // UMASK
                          false,      // Kernel
                          0,      // Counter ID
                          0,    // number of events to cause overflow
                          0);

#ifndef SENDMSG_WITH_COPY
  arranet_register_tx_done_callback(tx_done);
#endif

#endif

  printf("argc = %d\n", argc);

  for(int i = 0; i < argc; i++) {
      printf("argv[%d] = '%s'\n", i, argv[i]);
  }

  int clientmode = (argc > 2 && *argv[2] != '0');

  /* 
   * check command line arguments 
   */
  if (argc < 2) {
    fprintf(stderr, "usage: %s <port> [server IP|0] [proc time ns]\n", argv[0]);
    exit(1);
  }
  portno = atoi(argv[1]);
  if(argc == 4) {
      proctime = atoi(argv[3]);
  }

  if(!clientmode) {
      printf("Server on port %d, proctime %d\n", portno, proctime);
  } else {
      printf("Client mode\n");
  }

  /* 
   * socket: create the parent socket 
   */
  sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0) 
    error("ERROR opening socket");

#ifndef BARRELFISH
  /* setsockopt: Handy debugging trick that lets 
   * us rerun the server immediately after we kill it; 
   * otherwise we have to wait about 20 secs. 
   * Eliminates "ERROR on binding: Address already in use" error. 
   */
  optval = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR,
             (const void *)&optval , sizeof(int));
#endif

  clientlen = sizeof(clientaddr);

  if(clientmode) {
      bzero((char *) &clientaddr, sizeof(clientaddr));
      clientaddr.sin_family = AF_INET;
      clientaddr.sin_addr.s_addr = inet_addr(argv[2]);
      if(clientaddr.sin_addr.s_addr == INADDR_NONE) {
	printf("Error on inet_addr()\n");
	exit(1);
      }
      clientaddr.sin_port = htons((unsigned short)portno);

      // client mode -- initiate packet send
      memcpy(buf, initbuf, sizeof(initbuf));
      n = sendto(sockfd, buf, strlen(buf), 0,
                 (struct sockaddr *) &clientaddr, clientlen);
      if (n < 0)
          error("ERROR in first sendto");
  } else {
    /*
     * build the server's Internet address
     */
    bzero((char *) &serveraddr, sizeof(serveraddr));
    serveraddr.sin_family = AF_INET;
    serveraddr.sin_addr.s_addr = htonl(INADDR_ANY);
    serveraddr.sin_port = htons((unsigned short)portno);

    /* 
     * bind: associate the parent socket with a port 
     */
    if (bind(sockfd, (struct sockaddr *) &serveraddr, 
	     sizeof(serveraddr)) < 0) 
      error("ERROR on binding");
  }

  /* 
   * main loop: wait for a datagram, then echo it
   */

  for(int i = 0; i < MAX_ROUNDS; i = (clientmode ? i + 1 : 0)) {
    /*
     * recvfrom: receive a UDP datagram from a client
     */
    /* bzero(buf, BUFSIZE); */
#ifndef ARRANET_RECVFROM
    n = recvfrom(sockfd, buf, BUFSIZE, 0,
		 (struct sockaddr *) &clientaddr, &clientlen);

#ifdef DEBUG_LATENCIES
    if(posix_recv_transactions < POSIX_TRANSA) {
        protocol_binary_request_no_extras *mypayload = (void *)(buf + UDP_HEADLEN);
        posix_recv_time[posix_recv_transactions] = get_time() - mypayload->message.header.request.opaque;
        posix_recv_transactions++;
    }
#endif
#else
    void *mybuf = NULL;
    struct packet *myp = NULL;
    n = recvfrom_arranet(sockfd, &mybuf, &myp,
                         (struct sockaddr *) &clientaddr, &clientlen);

#ifdef DEBUG_LATENCIES
    if(posix_recv_transactions < POSIX_TRANSA) {
        protocol_binary_request_no_extras *mypayload = (void *)(mybuf + UDP_HEADLEN);
        posix_recv_time[posix_recv_transactions] = get_time() - mypayload->message.header.request.opaque;
        posix_recv_transactions++;
    }
#endif

    /* printf("udp_echo: Got %p from stack\n", myp); */
#endif
    if (n < 0)
      error("ERROR in recvfrom");

    /* printf("server received %zd/%d bytes: %s\n", strlen(buf), n, buf); */
    /* printf("server received %d bytes\n", n); */

    if(argc > 2) {
        gettimeofday(&tvs[i], NULL);
    }

/* #ifndef ARRANET_RECVFROM */
/*     uint32_t *hashval = (uint32_t *)&buf[24]; */
/*     *hashval = hash(buf, 64, 0); */
/* #else */
/*     uint32_t *hashval = (uint32_t *)(mybuf + 24); */
/*     *hashval = hash(mybuf, 64, 0); */
/* #endif */

#ifdef SIMULATE_PROC_TIME
    uint64_t now = rdtsc();
    while(rdtsc() - now < proctime);
    /* if(proctime > 0) { */
    /*     struct timespec req = { */
    /*         .tv_sec = 0, */
    /*         .tv_nsec = proctime, */
    /*     }; */
    /*     nanosleep(&req, NULL); */
    /* } */
#endif

    /*
     * sendto: echo the input back to the client 
     */
#ifndef ARRANET_RECVFROM
#       ifdef SENDMSG_WITH_COPY
    n = sendto(sockfd, buf, n, 0, 
	       (struct sockaddr *) &clientaddr, clientlen);
#       else
    {
        struct iovec io = {
            .iov_base = (void *)buf,
            .iov_len = n,
            .iov_opaque = buf,
        };

        struct msghdr msg = {
            .msg_name = (void *)&clientaddr,
            .msg_namelen = clientlen,
            .msg_iov = &io,
            .msg_iovlen = 1,
            .msg_flags = 0,
        };

        n = sendmsg(sockfd, &msg, 0);
    }
#       endif
#else
#       ifdef SENDMSG_WITH_COPY
    n = sendto(sockfd, mybuf, n, 0,
	       (struct sockaddr *) &clientaddr, clientlen);

    arranet_recv_free(myp);
#       else
    {
        /* printf("udp_echo: Sending %p back to stack\n", myp); */

        struct iovec io = {
            .iov_base = (void *)mybuf,
            .iov_len = n,
            .iov_opaque = myp,
        };

        struct msghdr msg = {
            .msg_name = (void *)&clientaddr,
            .msg_namelen = clientlen,
            .msg_iov = &io,
            .msg_iovlen = 1,
            .msg_flags = 0,
        };

        n = sendmsg(sockfd, &msg, 0);
    }
#       endif
#endif
    if (n < 0)
        error("ERROR in sendto");
  }

  if(argc > 2) {
      unsigned long sum = 0, min = 99999, max = 0;

      for(int i = 1; i < MAX_ROUNDS; i++) {
          struct timeval res;
          timersub(&tvs[i], &tvs[i - 1], &res);
          unsigned long r = res.tv_sec * 1000000 + res.tv_usec;
          printf("%lu us\n", r);
          sum += r;
          min = MIN(min, r);
          max = MAX(max, r);
      }

      printf("average %lu us, min %lu us, max %lu us\n",
             sum / (MAX_ROUNDS - 1), min, max);
  }

  return 0;
}
