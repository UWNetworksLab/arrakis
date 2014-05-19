/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <inttypes.h>
#include <sys/types.h> 
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/time.h>
#include <strings.h>
#include <assert.h>
#include <pthread.h>

#define	timersub(a, b, result)						      \
  do {									      \
    (result)->tv_sec = (a)->tv_sec - (b)->tv_sec;			      \
    (result)->tv_usec = (a)->tv_usec - (b)->tv_usec;			      \
    if ((result)->tv_usec < 0) {					      \
      --(result)->tv_sec;						      \
      (result)->tv_usec += 1000000;					      \
    }									      \
  } while (0)

#define BUFSIZE         1024
#define MAX_ROUNDS      10000000

#define MIN(a,b)        ((a) < (b) ? (a) : (b))
#define MAX(a,b)        ((a) > (b) ? (a) : (b))

static struct timeval tvs[MAX_ROUNDS], tst[MAX_ROUNDS];

/*
 * error - wrapper for perror
 */
static void error(char *msg) {
  perror(msg);
  exit(1);
}

static int sockfd; /* socket */
static struct sockaddr_in clientaddr; /* client addr */
static socklen_t clientlen; /* byte size of client's address */
static int delay = 0;
static size_t rounds, packets = 0;

static void *receiver_func(void *unused)
{
  char buf[BUFSIZE]; /* message buf */

  for(int i = 0; i < rounds; i++) {
    /*
     * recvfrom: receive a UDP datagram from a client
     */
      int n = recvfrom(sockfd, buf, BUFSIZE, 0, NULL, NULL);
      if (n < 0)
          error("ERROR in recvfrom");

      /* printf("server received %d bytes\n", n); */
      assert(n == BUFSIZE);

      /* uint64_t *cnt = (uint64_t *)buf; */
      struct timeval *tstamp = (struct timeval *)&buf[8];
      gettimeofday(&tst[i], NULL);
      timersub(&tst[i], tstamp, &tvs[i]);
      packets++;
      /* if(*cnt != i) { */
      /*     printf("Packets reordered? %d != %" PRIu64 "\n", i, *cnt); */
      /*     exit(1); */
      /* } */
  }

  return NULL;
}

int main(int argc, char **argv) {
  int portno; /* port to listen on */
  int optval; /* flag value for setsockopt */
  char buf[BUFSIZE]; /* message buf */

  /* 
   * check command line arguments 
   */
  if (argc < 6) {
    fprintf(stderr, "usage: %s <port> <server IP> <delay us> <rounds> <start_val>\n", argv[0]);
    exit(1);
  }
  portno = atoi(argv[1]);
  delay = atoi(argv[3]);
  rounds = atoi(argv[4]);
  assert(rounds < MAX_ROUNDS);
  size_t start = atoi(argv[5]);

  /* 
   * socket: create the parent socket 
   */
  sockfd = socket(AF_INET, SOCK_DGRAM, 0);
  if (sockfd < 0) 
    error("ERROR opening socket");

  /* setsockopt: Handy debugging trick that lets 
   * us rerun the server immediately after we kill it; 
   * otherwise we have to wait about 20 secs. 
   * Eliminates "ERROR on binding: Address already in use" error. 
   */
  optval = 1;
  setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR,
             (const void *)&optval , sizeof(int));

  clientlen = sizeof(clientaddr);

  bzero((char *) &clientaddr, sizeof(clientaddr));
  clientaddr.sin_family = AF_INET;
  clientaddr.sin_addr.s_addr = inet_addr(argv[2]);
  if(clientaddr.sin_addr.s_addr == INADDR_NONE) {
      printf("Error on inet_addr()\n");
      exit(1);
  }
  clientaddr.sin_port = htons((unsigned short)portno);

  pthread_t sender;
  int ret = pthread_create(&sender, NULL, receiver_func, NULL);
  assert(ret == 0);

  uint64_t *cnt = (uint64_t *)buf;
  struct timeval *tstamp = (struct timeval *)&buf[8];

  memset(tstamp, 0, sizeof(struct timeval));

  for(*cnt = 0; *cnt < rounds; (*cnt)++) {
      struct timeval oldstamp, diff;

      oldstamp = *tstamp;

      do {
          gettimeofday(tstamp, NULL);
          timersub(tstamp, &oldstamp, &diff);
          /* printf("now = %lu, oldstamp = %lu, diff = %lu, delay = %d\n", */
          /*        tstamp->tv_sec * 1000000 + tstamp->tv_usec, */
          /*        oldstamp.tv_sec * 1000000 + oldstamp.tv_usec, */
          /*        diff.tv_sec * 1000000 + diff.tv_usec, delay); */
      } while((uint64_t)(diff.tv_sec * 1000000 + diff.tv_usec) < (uint64_t)delay);

      int n = sendto(sockfd, buf, BUFSIZE, 0,
                     (struct sockaddr *) &clientaddr, clientlen);
      if (n < 0)
          error("ERROR in sendto");
  }

  ret = pthread_cancel(sender);
  assert(ret == 0);

  unsigned long sum = 0, min = 99999, max = 0;

  for(int i = 0; i < packets; i++) {
      unsigned long r = tvs[i].tv_sec * 1000000 + tvs[i].tv_usec;
      unsigned long t = tst[i].tv_sec * 1000000 + tst[i].tv_usec;
      if(t != 0) {
          printf("%zd %lu %lu %lu us\n", i + start, t - r, t, r);
          sum += r;
          min = MIN(min, r);
          max = MAX(max, r);
      }
  }

  printf("average %lu us, min %lu us, max %lu us\n",
         sum / (packets - 1), min, max);

  return 0;
}
