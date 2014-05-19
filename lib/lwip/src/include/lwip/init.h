/*
 * Copyright (c) 2001-2004 Swedish Institute of Computer Science.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT
 * SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT
 * OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY
 * OF SUCH DAMAGE.
 *
 * This file is part of the lwIP TCP/IP stack.
 *
 * Author: Adam Dunkels <adam@sics.se>
 *
 */
#ifndef __LWIP_INIT_H__
#define __LWIP_INIT_H__

//#include <barrelfish/barrelfish.h>
#include "lwip/opt.h"
#include "lwip/err.h"
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/** X.x.x: Major version of the stack */
#define LWIP_VERSION_MAJOR      1U
/** x.X.x: Minor version of the stack */
#define LWIP_VERSION_MINOR      3U
/** x.x.X: Revision of the stack */
#define LWIP_VERSION_REVISION   1U
/** For release candidates, this is set to 1..254
  * For official releases, this is set to 255 (LWIP_RC_RELEASE)
  * For development versions (CVS), this is set to 0 (LWIP_RC_DEVELOPMENT) */
#define LWIP_VERSION_RC         255U

/** LWIP_VERSION_RC is set to LWIP_RC_RELEASE for official releases */
#define LWIP_RC_RELEASE         255U
/** LWIP_VERSION_RC is set to LWIP_RC_DEVELOPMENT for CVS versions */
#define LWIP_RC_DEVELOPMENT     0U

#define LWIP_VERSION_IS_RELEASE     (LWIP_VERSION_RC == LWIP_RC_RELEASE)
#define LWIP_VERSION_IS_DEVELOPMENT (LWIP_VERSION_RC == LWIP_RC_DEVELOPMENT)
#define LWIP_VERSION_IS_RC          ((LWIP_VERSION_RC != LWIP_RC_RELEASE) && (LWIP_VERSION_RC != LWIP_RC_DEVELOPMENT))

/** Provides the version of the stack */
#define LWIP_VERSION   (LWIP_VERSION_MAJOR << 24   | LWIP_VERSION_MINOR << 16 | \
                        LWIP_VERSION_REVISION << 8 | LWIP_VERSION_RC)

enum net_ports_port_type_t;

// FIXME: remove this
void perform_ownership_housekeeping(uint16_t(*alloc_tcp_ptr) (void),
                                        uint16_t(*alloc_udp_ptr) (void),
                                        uint16_t(*bind_port_ptr) (uint16_t,
                                                enum net_ports_port_type_t),
                                        void (*close_port_ptr) (uint16_t,
                                                enum net_ports_port_type_t));

// global variables
struct waitset;
struct thread_mutex;

// *********************************************************************
// function prototypes
// *********************************************************************

// Tells if this app is special or not
bool is_this_special_app(void);

// To be called from "netd" which is responsible for ARP table
struct netif *owner_lwip_init(char *card_name, uint64_t queueid);

// initialize networkign with specific card and queue
bool lwip_init(const char *card_name, uint64_t queueid);

// initialize networking when cardname and queue is not known
bool lwip_init_auto(void);

// FIXME: remove these functions as they are not used anymore
int is_lwip_loaded(void);
uint64_t lwip_packet_drop_count(void);


uint64_t wrapper_perform_lwip_work(void);

void lwip_benchmark_control(int connection, uint8_t state, uint64_t trigger,
        uint64_t cl);
uint8_t lwip_driver_benchmark_state(int direction, uint64_t *delta,
        uint64_t *cl);
void lwip_debug_show_spp_status(int connection);

enum Recorded_Events {
    RE_ALL,
    RX_ALL_PROCESS,
    RE_REG_PBUF,
    RE_PBUF_REPLACE,
    RE_PBUF_REPLACE_1,
    RE_PBUF_REPLACE_2,
    RE_PBUF_QUEUE,
    RE_PKT_RCV_CS,
    RE_PBUF_REPLACE_3,
    TX_SP,
    TX_SP1,
    TX_SPP_FULL,
    TX_SN_WAIT,
    TX_SN_SEND,
    TX_A_SP_RN_CS,
    TX_A_SP_RN_T,
    TX_SND_PKT_C,
    TX_SND_PKT_S,
/*
    RPC_RECV_T,
    RPC_CALLBACK_T,
    RPC_RECV_OUT_T,
*/
    RPC_CALL_T,
    NFS_READCB_T,
    NFS_READ_T,
    NFS_READ_1_T,
    NFS_READ_w_T,
    RECORDED_EVENTS_COUNT  // MUST BE THE LAST ELEMENT!!
};

extern struct netbench_details *nb;
void lwip_print_interesting_stats(void);
void lwip_record_event_simple(uint8_t event_type, uint64_t ts);

bool lwip_init_auto_ex(struct waitset *opt_waitset,
                       struct thread_mutex *opt_mutex);
bool lwip_init_ex(const char *card_name, uint64_t queueid,
                  struct waitset *opt_waitset, struct thread_mutex *opt_mutex);

// For supporting hardware features
enum enabled_hardware_features {
    IPv4_CHECKSUM_HW = 1,
    IPv6_CHECKSUM_HW,
    UDP_IPV4_CHECKSUM_HW,
    TCP_IPV4_CHECKSUM_HW,
};
bool is_hw_feature_enabled(int hw_feature);
#ifdef __cplusplus
}
#endif

#endif // __LWIP_INIT_H__

