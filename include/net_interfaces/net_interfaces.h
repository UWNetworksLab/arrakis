/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NET_INTERFACES_H_
#define NET_INTERFACES_H_

#include <stdlib.h>

#include <barrelfish/barrelfish.h>
#include <net_interfaces/flags.h>


extern void *buffer_base;
extern size_t buffer_size;
extern size_t buffer_count;


void benchmark_init(void);
void benchmark_argument(char *arg);
void benchmark_rx_done(size_t idx, size_t len, uint64_t more, uint64_t flags);
void benchmark_tx_done(size_t idx);
void benchmark_do_pending_work(void);

void net_if_init(const char* cardname, uint64_t qid);
void net_if_terminate(void);
errval_t buffer_tx_add(size_t idx, size_t offset, size_t len,
                       size_t more_chunks, uint64_t flags);
errval_t buffer_rx_add(size_t idx);
void benchmark_get_mac_address(uint8_t *mac);
uint64_t get_tx_bufferid(void);
uint64_t get_rx_bufferid(void);

#endif // ndef NET_INTERFACES_H_

