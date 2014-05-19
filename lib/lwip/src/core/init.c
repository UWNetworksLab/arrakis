/**
 * @file
 * Modules initialization
 *
 */

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

#include "lwip/opt.h"

#include "lwip/init.h"
#include "lwip/stats.h"
#include "lwip/sys.h"
#include "lwip/mem.h"
#include "lwip/memp.h"
#include "lwip/pbuf.h"
#include "lwip/netif.h"
#include "lwip/sockets.h"
#include "lwip/ip.h"
#include "lwip/raw.h"
#include "lwip/udp.h"
#include "lwip/tcp.h"
#include "lwip/snmp_msg.h"
#include "lwip/autoip.h"
#include "lwip/igmp.h"
#include "lwip/dns.h"
#include "netif/bfeth.h"
#include "netif/etharp.h"

#include <idc_barrelfish.h>
#include <mem_barrelfish.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/deferred.h>
#include <barrelfish/net_constants.h>
#include <contmng/netbench.h>


/* FIXME: Move this to config */
#define MYDEBUGLWIP 1

#ifdef MYDEBUGLWIP
#define DEBUGPRINTPS(arg...) printf(arg)
#else
#define DEBUGPRINTPS(arg...) ((void)0)
#endif // MYDEBUGLWIP







/* Compile-time sanity checks for configuration errors.
 * These can be done independently of LWIP_DEBUG, without penalty.
 */
#ifndef BYTE_ORDER
#error "BYTE_ORDER is not defined, you have to define it in your cc.h"
#endif
#if (!IP_SOF_BROADCAST && IP_SOF_BROADCAST_RECV)
#error "If you want to use broadcast filter per pcb on recv operations, you have to define IP_SOF_BROADCAST=1 in your lwipopts.h"
#endif
#if (!LWIP_ARP && ARP_QUEUEING)
#error "If you want to use ARP Queueing, you have to define LWIP_ARP=1 in your lwipopts.h"
#endif
#if (!LWIP_UDP && LWIP_UDPLITE)
#error "If you want to use UDP Lite, you have to define LWIP_UDP=1 in your lwipopts.h"
#endif
#if (!LWIP_UDP && LWIP_SNMP)
#error "If you want to use SNMP, you have to define LWIP_UDP=1 in your lwipopts.h"
#endif
#if (!LWIP_UDP && LWIP_DHCP)
#error "If you want to use DHCP, you have to define LWIP_UDP=1 in your lwipopts.h"
#endif
#if (!LWIP_UDP && LWIP_IGMP)
#error "If you want to use IGMP, you have to define LWIP_UDP=1 in your lwipopts.h"
#endif
#if (!LWIP_UDP && LWIP_DNS)
#error "If you want to use DNS, you have to define LWIP_UDP=1 in your lwipopts.h"
#endif
#if (LWIP_ARP && (ARP_TABLE_SIZE > 0x7f))
#error "If you want to use ARP, ARP_TABLE_SIZE must fit in an s8_t, so, you have to reduce it in your lwipopts.h"
#endif
#if (LWIP_ARP && ARP_QUEUEING && (MEMP_NUM_ARP_QUEUE<=0))
#error "If you want to use ARP Queueing, you have to define MEMP_NUM_ARP_QUEUE>=1 in your lwipopts.h"
#endif
#if (LWIP_RAW && (MEMP_NUM_RAW_PCB<=0))
#error "If you want to use RAW, you have to define MEMP_NUM_RAW_PCB>=1 in your lwipopts.h"
#endif
#if (LWIP_UDP && (MEMP_NUM_UDP_PCB<=0))
#error "If you want to use UDP, you have to define MEMP_NUM_UDP_PCB>=1 in your lwipopts.h"
#endif
#if (LWIP_TCP && (MEMP_NUM_TCP_PCB<=0))
#error "If you want to use TCP, you have to define MEMP_NUM_TCP_PCB>=1 in your lwipopts.h"
#endif
#if (LWIP_TCP && (TCP_WND > 0xffff))
#error "If you want to use TCP, TCP_WND must fit in an u16_t, so, you have to reduce it in your lwipopts.h"
#endif
#if (LWIP_TCP && (TCP_SND_QUEUELEN > 0xffff))
#error "If you want to use TCP, TCP_SND_QUEUELEN must fit in an u16_t, so, you have to reduce it in your lwipopts.h"
#endif
#if (LWIP_TCP && ((TCP_MAXRTX > 12) || (TCP_SYNMAXRTX > 12)))
#error "If you want to use TCP, TCP_MAXRTX and TCP_SYNMAXRTX must less or equal to 12 (due to tcp_backoff table), so, you have to reduce them in your lwipopts.h"
#endif
#if (LWIP_TCP && TCP_LISTEN_BACKLOG && (TCP_DEFAULT_LISTEN_BACKLOG < 0) || (TCP_DEFAULT_LISTEN_BACKLOG > 0xff))
#error "If you want to use TCP backlog, TCP_DEFAULT_LISTEN_BACKLOG must fit into an u8_t"
#endif
#if (LWIP_IGMP && (MEMP_NUM_IGMP_GROUP<=1))
#error "If you want to use IGMP, you have to define MEMP_NUM_IGMP_GROUP>1 in your lwipopts.h"
#endif
#if (PPP_SUPPORT && (NO_SYS==1))
#error "If you want to use PPP, you have to define NO_SYS=0 in your lwipopts.h"
#endif
#if (LWIP_NETIF_API && (NO_SYS==1))
#error "If you want to use NETIF API, you have to define NO_SYS=0 in your lwipopts.h"
#endif
#if ((LWIP_SOCKET || LWIP_NETCONN) && (NO_SYS==1))
#error "If you want to use Sequential API, you have to define NO_SYS=0 in your lwipopts.h"
#endif
#if ((LWIP_NETCONN || LWIP_SOCKET) && (MEMP_NUM_TCPIP_MSG_API<=0))
#error "If you want to use Sequential API, you have to define MEMP_NUM_TCPIP_MSG_API>=1 in your lwipopts.h"
#endif
#if (!LWIP_NETCONN && LWIP_SOCKET)
#error "If you want to use Socket API, you have to define LWIP_NETCONN=1 in your lwipopts.h"
#endif
#if (((!LWIP_DHCP) || (!LWIP_AUTOIP)) && LWIP_DHCP_AUTOIP_COOP)
#error "If you want to use DHCP/AUTOIP cooperation mode, you have to define LWIP_DHCP=1 and LWIP_AUTOIP=1 in your lwipopts.h"
#endif
#if (((!LWIP_DHCP) || (!LWIP_ARP)) && DHCP_DOES_ARP_CHECK)
#error "If you want to use DHCP ARP checking, you have to define LWIP_DHCP=1 and LWIP_ARP=1 in your lwipopts.h"
#endif
#if (!LWIP_ARP && LWIP_AUTOIP)
#error "If you want to use AUTOIP, you have to define LWIP_ARP=1 in your lwipopts.h"
#endif
#if (LWIP_SNMP && (SNMP_CONCURRENT_REQUESTS<=0))
#error "If you want to use SNMP, you have to define SNMP_CONCURRENT_REQUESTS>=1 in your lwipopts.h"
#endif
#if (LWIP_SNMP && (SNMP_TRAP_DESTINATIONS<=0))
#error "If you want to use SNMP, you have to define SNMP_TRAP_DESTINATIONS>=1 in your lwipopts.h"
#endif
#if (LWIP_TCP && ((LWIP_EVENT_API && LWIP_CALLBACK_API) || (!LWIP_EVENT_API && !LWIP_CALLBACK_API)))
#error "One and exactly one of LWIP_EVENT_API and LWIP_CALLBACK_API has to be enabled in your lwipopts.h"
#endif
/* There must be sufficient timeouts, taking into account requirements of the subsystems. */
#if ((NO_SYS==0) && (MEMP_NUM_SYS_TIMEOUT < (LWIP_TCP + IP_REASSEMBLY + LWIP_ARP + (2*LWIP_DHCP) + LWIP_AUTOIP + LWIP_IGMP + LWIP_DNS + PPP_SUPPORT)))
#error "MEMP_NUM_SYS_TIMEOUT is too low to accomodate all required timeouts"
#endif
#if (IP_REASSEMBLY && (MEMP_NUM_REASSDATA > IP_REASS_MAX_PBUFS))
#error "MEMP_NUM_REASSDATA > IP_REASS_MAX_PBUFS doesn't make sense since each struct ip_reassdata must hold 2 pbufs at least!"
#endif
#if (MEM_LIBC_MALLOC && MEM_USE_POOLS)
#error "MEM_LIBC_MALLOC and MEM_USE_POOLS may not both be simultaneously enabled in your lwipopts.h"
#endif
#if (MEM_USE_POOLS && !MEMP_USE_CUSTOM_POOLS)
#error "MEM_USE_POOLS requires custom pools (MEMP_USE_CUSTOM_POOLS) to be enabled in your lwipopts.h"
#endif

#if (PBUF_POOL_BUFSIZE <= MEM_ALIGNMENT)
#error "PBUF_POOL_BUFSIZE must be greater than MEM_ALIGNMENT or the offset may take the full first pbuf"
#endif

#if (TCP_QUEUE_OOSEQ && !LWIP_TCP)
#error "TCP_QUEUE_OOSEQ requires LWIP_TCP"
#endif
#if (DNS_LOCAL_HOSTLIST && !DNS_LOCAL_HOSTLIST_IS_DYNAMIC && !(defined(DNS_LOCAL_HOSTLIST_INIT)))
#error "you have to define define DNS_LOCAL_HOSTLIST_INIT {{'host1', 0x123}, {'host2', 0x234}} to initialize DNS_LOCAL_HOSTLIST"
#endif


/* Compile-time checks for deprecated options.
 */
#ifdef MEMP_NUM_TCPIP_MSG
#error "MEMP_NUM_TCPIP_MSG option is deprecated. Remove it from your lwipopts.h."
#endif
#ifdef MEMP_NUM_API_MSG
#error "MEMP_NUM_API_MSG option is deprecated. Remove it from your lwipopts.h."
#endif
#ifdef TCP_REXMIT_DEBUG
#error "TCP_REXMIT_DEBUG option is deprecated. Remove it from your lwipopts.h."
#endif
#ifdef RAW_STATS
#error "RAW_STATS option is deprecated. Remove it from your lwipopts.h."
#endif
#ifdef ETHARP_QUEUE_FIRST
#error "ETHARP_QUEUE_FIRST option is deprecated. Remove it from your lwipopts.h."
#endif
#ifdef ETHARP_ALWAYS_INSERT
#error "ETHARP_ALWAYS_INSERT option is deprecated. Remove it from your lwipopts.h."
#endif
#if SO_REUSE
/* I removed the lot since this was an ugly hack. It broke the raw-API.
   It also came with many ugly goto's, Christiaan Simons. */
#error "SO_REUSE currently unavailable, this was a hack"
#endif

#ifdef LWIP_DEBUG
static void lwip_sanity_check(void)
{
    /* Warnings */
#if LWIP_NETCONN
    if (MEMP_NUM_NETCONN >
        (MEMP_NUM_TCP_PCB + MEMP_NUM_TCP_PCB_LISTEN + MEMP_NUM_UDP_PCB +
         MEMP_NUM_RAW_PCB))
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: MEMP_NUM_NETCONN should be less than the sum of MEMP_NUM_{TCP,RAW,UDP}_PCB+MEMP_NUM_TCP_PCB_LISTEN\n"));
#endif                          /* LWIP_NETCONN */
#if LWIP_TCP
    if (MEMP_NUM_TCP_SEG < TCP_SND_QUEUELEN)
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: MEMP_NUM_TCP_SEG should be at least as big as TCP_SND_QUEUELEN\n"));
    if (TCP_SND_QUEUELEN < (2 * (TCP_SND_BUF / TCP_MSS)))
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: TCP_SND_QUEUELEN must be at least as much as (2 * TCP_SND_BUF/TCP_MSS) for things to work\n"));
    if (TCP_SNDLOWAT > TCP_SND_BUF)
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: TCP_SNDLOWAT must be less than or equal to TCP_SND_BUF.\n"));
    if (TCP_WND > (PBUF_POOL_SIZE * PBUF_POOL_BUFSIZE))
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: TCP_WND is larger than space provided by PBUF_POOL_SIZE*PBUF_POOL_BUFSIZE\n"));

    if (TCP_WND < TCP_MSS)
        LWIP_PLATFORM_DIAG(("lwip_sanity_check: WARNING: TCP_WND is smaller than MSS\n"));
#endif                          /* LWIP_TCP */
}
#else                           /* LWIP_DEBUG */
#define lwip_sanity_check()
#endif                          /* LWIP_DEBUG */

static int is_ctl = 0;
struct netbench_details *nb = NULL;

static struct netif netif;

bool is_this_special_app(void)
{
    return (is_ctl > 0);
} // end function: is_this_special_app

static void remaining_lwip_initialization(char *card_name, uint64_t queueid)
{
    nb = netbench_alloc("app", RECORDED_EVENTS_COUNT);
    //asq: connect to the NIC driver, before doing anything else
    idc_connect_to_driver(card_name, queueid);
    DEBUGPRINTPS("Connected to driver [%s]\n", card_name);
    stats_init();
    sys_init();
    DEBUGPRINTPS("remaining_lwip_init: allocating pbuf memory\n");
#ifdef CONFIG_QEMU_NETWORK
    printf("#### Networking with small amount of memory #####\n");
#endif // CONFIG_QEMU_NETWORK
    printf("#### [%u:%"PRIuDOMAINID":%s] [%s] [%d] MEM_SIZE[%d], "
            "PBUF_POOL_SIZE[%d], MEMP_MAX[%d],  RECEIVE_BUFFERS[%d] qid[%"PRIu64"]####\n",
       disp_get_core_id(), disp_get_domain_id(), disp_name(),
       MEM_CONF_LOC, is_ctl, MEM_SIZE, PBUF_POOL_SIZE, MEMP_MAX,
       RECEIVE_BUFFERS, queueid);

    memp_init();                // 0'st buffer

    DEBUGPRINTPS("remaining_lwip_init: allocating memory for sending\n");
    mem_init();                 // 1'th buffer
    DEBUGPRINTPS("remaining_lwip_init: done with memroy allocation\n");

    DEBUGPRINTPS("LWIP: lwip_starting\n");
    netif_init();
#if LWIP_SOCKET
    lwip_socket_init();
#endif                          /* LWIP_SOCKET */
    ip_init();
    DEBUGPRINTPS("r_lwip_init: done ip_init\n");
#if LWIP_ARP
    etharp_init();
#endif                          /* LWIP_ARP */
#if LWIP_RAW
    raw_init();
#endif                          /* LWIP_RAW */
#if LWIP_UDP
    udp_init();
    DEBUGPRINTPS("r_lwip_init: done udp_init\n");
#endif                          /* LWIP_UDP */
#if LWIP_TCP
    tcp_init();
    DEBUGPRINTPS("r_lwip_init: done tcp_init\n");
#endif                          /* LWIP_TCP */
#if LWIP_SNMP
    snmp_init();
    DEBUGPRINTPS("r_lwip_init: done snmp_init\n");
#endif                          /* LWIP_SNMP */
#if LWIP_AUTOIP
    autoip_init();
    DEBUGPRINTPS("r_lwip_init: done autoip_init\n");
#endif                          /* LWIP_AUTOIP */
#if LWIP_IGMP
    igmp_init();
    DEBUGPRINTPS("r_lwip_init: done igmp_init\n");
#endif                          /* LWIP_IGMP */
    DEBUGPRINTPS("r_lwip_init: done2 igmp_init\n");
#if LWIP_DNS
    DEBUGPRINTPS("r_lwip_init: starting DNS_init\n");
    dns_init();
    DEBUGPRINTPS("r_lwip_init: done DNS_init\n");
#endif                          /* LWIP_DNS */
    DEBUGPRINTPS("LWIP: lwip_started\n");
}

extern struct waitset *lwip_waitset;    // idc_barrelfish.c
extern struct thread_mutex *lwip_mutex; // idc_barrelfish.c

/**
 * To be called by the app which wants to perform DHCP on it's own
 * In current implementation, it is netd.
 * Perform Sanity check of user-configurable values, and initialize all modules.
 */
struct netif *owner_lwip_init(char *card_name, uint64_t queueid)
{
    DEBUGPRINTPS("owner_lwip_init: Inside lwip_init\n");
    is_ctl = 1;
    lwip_init(card_name, queueid);
    DEBUGPRINTPS("LWIP: owner_lwip_init: done with connection setup\n");
    return &netif;
}

static void call_tcp_tmr(void)
{
    lwip_mutex_lock();
    tcp_tmr();
    lwip_mutex_unlock();
}

/**
 * Perform Sanity check of user-configurable values, and initialize all modules.
 *
 * \param card_name Name of service implementing ethernet driver
 * \param queueid Queueid which is allocated to this application
 * \param opt_waitset Optional pointer to waitset to be used by LWIP
 * \param opt_mutex Optional pointer to mutex to protect multi-threaded domains
 *
 * \returns True iff init completes
 */
bool lwip_init_ex(const char *card_name, uint64_t queueid,
                  struct waitset *opt_waitset, struct thread_mutex *opt_mutex)
{
    printf("lwip_init_ex: starting......................\n");
    DEBUGPRINTPS("LWIP_other: Inside lwip_init\n");
    static bool run_once;

    if (run_once) {
        return false;
    }
    run_once = true;

    if (opt_waitset == NULL) {
        printf("#### %s Going ahead with default wait-set\n", disp_name());
        lwip_waitset = get_default_waitset();
    } else {
        printf("#### %s Going ahead with non-default wait-set\n", disp_name());
//        lwip_waitset = get_default_waitset();
        lwip_waitset = opt_waitset;
    }

    if (opt_mutex != NULL) {
        lwip_mutex = opt_mutex;
    }

    /* Sanity check user-configurable values */
    lwip_sanity_check();
    DEBUGPRINTPS("LWIP: lwip_init: done with sanity check\n");
    printf("LWIP: done with sanity check\n");
    /* Modules initialization */
    char port_manager_name[MAX_NET_SERVICE_NAME_LEN];

    snprintf(port_manager_name, sizeof(port_manager_name), "%s%s",
             card_name, NET_PORTS_MNG_SUFFIX);

    // Connecting to the port_manager_service
    idc_connect_port_manager_service(port_manager_name);


    if (is_ctl != 1) {
        // connecting to ARP lookup service
        // Doing this before everything else so that we know all needed
        // services are up and running.
        char ARP_service_name[MAX_NET_SERVICE_NAME_LEN];
        snprintf(ARP_service_name, sizeof(ARP_service_name), "%s%s",
             card_name, NET_ARP_LOOKUP_SUFFIX);
        idc_connect_ARP_lookup_service(ARP_service_name);
    }

    DEBUGPRINTPS("LWIP: lwip_init: done with connection setup\n");
    printf("LWIP: done with connection setup\n");
    remaining_lwip_initialization((char *) card_name, queueid);

    if (is_ctl != 1) {
        DEBUGPRINTPS("getting IP from ARP service\n");
        printf("LWIP: getting IP from ARP service\n");
        idc_get_ip_from_ARP_lookup();
    }

    // Register timers... (TCP only)
    // FIXME: These timers should be added only when first TCP connection
    // is requested and not when networking is started!!!!
    static struct periodic_event tcp_timer;
    errval_t err = periodic_event_create(&tcp_timer, lwip_waitset,
                                         TCP_TMR_INTERVAL * 1000,
                                         MKCLOSURE((void (*)(void *))
                                                   call_tcp_tmr, NULL));
    assert(err_is_ok(err));

    // FIXME: I am not sure if this should be in the codepath for both
    // is_ctl and non-is_ctl.  Specially becasuse non is_ctl is anyways
    // adding one interface with idc_get_ip* call.

    // Bring interface up
    struct ip_addr ipaddr, netmask, gw;

    ip_addr_set(&ipaddr, IP_ADDR_ANY);
    ip_addr_set(&netmask, IP_ADDR_ANY);
    ip_addr_set(&gw, IP_ADDR_ANY);
    struct netif *n = netif_add(&netif, &ipaddr, &netmask, &gw,
                                NULL, bfeth_init, ethernet_input);

    assert(n != NULL);
    extern bool lwip_init_done;
    lwip_init_done = true;

    printf("lwip_init_ex: done......................\n");
    return true;
}


/**
 * Figure out the best NIC card to connect and initialize library network stack.
 */
bool lwip_init_auto_ex(struct waitset * opt_waitset,
                       struct thread_mutex * opt_mutex)
{
    char *card_name = NULL;
    uint64_t default_queueid = 0;
    /* Figure out the best NIC card that can be used */
    /* FIXME: hardcoding the NIC card right now, will do smarter detection
       in future. */

#ifndef __scc__
#ifdef CONFIG_QEMU_NETWORK
    card_name = "rtl8029";
#else
    // FIXME: also check for e10k
    // FIXME: get this from kaluga
    card_name = "e1000";
    //card_name = "e10k";
    //card_name = "vmkitmon_eth";
#endif // CONFIG_QEMU_NETWORK
#else
    static char cid[100];

    snprintf(cid, sizeof(cid), "eMAC2_%u", disp_get_core_id());
    card_name = cid;
#endif // __scc__

    return lwip_init_ex(card_name, default_queueid, opt_waitset, opt_mutex);
}                               // end function: lwip_init_auto_ex


/**
 *
 */
bool lwip_init_auto(void)
{
    return lwip_init_auto_ex(NULL, NULL);
}

/**
 * Perform Sanity check of user-configurable values, and initialize all modules.
 */
bool lwip_init(const char *card_name, uint64_t queueid)
{
    if (card_name == NULL) {
        return lwip_init_auto_ex(NULL, NULL);
    } else {
        return lwip_init_ex(card_name, queueid, NULL, NULL);
    }
}


void lwip_debug_show_spp_status(int connection)
{
    debug_show_spp_status(connection);
}

uint64_t wrapper_perform_lwip_work(void)
{
    return perform_lwip_work();
}

void lwip_benchmark_control(int direction, uint8_t state, uint64_t trigger,
        uint64_t cl)
{
//  printf("calling lwip_benchmark_control\n");
    //idc_benchmark_control(direction, state, trigger, cl);
}  // end function: lwip_benchmark_control

uint8_t lwip_driver_benchmark_state(int direction, uint64_t *delta,
        uint64_t *cl)
{
   return get_driver_benchmark_state(direction, delta, cl);
}

#include <contmng/contmng.h>
#define FREE_SLOT_THRESHOLD    100
int is_lwip_loaded(void)
{
    // Check for availability of free pbufs
    if (free_pbuf_pool_count() == 0) {
        return 1;
    }

    int slots = lwip_check_sp_capacity(TRANSMIT_CONNECTION);
    if (slots < FREE_SLOT_THRESHOLD) {
        return 2;
    }

    // for receivign connection, one should check if queue is free
    slots = lwip_check_sp_capacity(RECEIVE_CONNECTION);
    if (slots < FREE_SLOT_THRESHOLD) {
//        printf("slots left are %d\n", slots);
        return 5;
    }

    // Check load on RX connection
    slots = idc_check_capacity(RECEIVE_CONNECTION);

    if (slots < FREE_SLOT_THRESHOLD) {
        return 3;
    }
    // Check load on TX connection
    slots = idc_check_capacity(TRANSMIT_CONNECTION);
    if (slots < FREE_SLOT_THRESHOLD) {
        return 4;
    }
    // Check for load the driver itself
    uint64_t tx_slots_left = idc_check_driver_load();

    if (tx_slots_left < (MAX_QUEUE_SIZE + 10)) {
//        return 5;
    }

    // Everything is great!
    return 0;
}                               // end function: is_lwip_loaded?



uint64_t lwip_packet_drop_count(void)
{
    return idc_get_packet_drop_count();
}                               // end function: lwip_packet_drop_count


// ******************************************************************
// Functions to assist in benchmarking the network path


// Records the time (ts) spent in the event (event_type)
// Simplified version of netbench_record_event_simple by giving default
// nbp pointer
void lwip_record_event_simple(uint8_t event_type, uint64_t ts)
{
    netbench_record_event_simple(nb, event_type, ts);
} // end function: lwip_record_event_simple

void lwip_print_interesting_stats(void)
{

    netbench_print_event_stat(nb, RPC_CALL_T,  "U: RPC_CALL_T", 1);
    netbench_print_event_stat(nb, NFS_READCB_T,  "U: NFS_READCB_T", 1);
    netbench_print_event_stat(nb, NFS_READ_T,  "U: NFS_READ_T", 1);
    netbench_print_event_stat(nb, NFS_READ_1_T,  "U: NFS_READ_1_T", 1);
    netbench_print_event_stat(nb, NFS_READ_w_T,  "U: NFS_READ_w_T", 1);
    /*
    netbench_print_event_stat(nb, RPC_RECV_T, "U: RPC_RECV_T", 1);
    netbench_print_event_stat(nb, RPC_CALLBACK_T, "U: RPC_CALLBACK_T", 1);
    netbench_print_event_stat(nb, RPC_RECV_OUT_T, "U: RPC_RECV_OUT_T", 1);
    */

/*
    netbench_print_event_stat(nb, RE_ALL,            "U: RX ALL", 1);
    netbench_print_event_stat(nb, RX_ALL_PROCESS ,   "U: RX ALL process", 1);
    netbench_print_event_stat(nb, RE_PBUF_REPLACE,   "U: RX Replace pbuf", 1);
    netbench_print_event_stat(nb, TX_A_SP_RN_CS, "U: Notification pending", 1);
*/

/*
    netbench_print_event_stat(nb, RE_PBUF_REPLACE_1, "U: RX Replace pbuf_1", 1);
    netbench_print_event_stat(nb, RE_PBUF_REPLACE_2, "U: RX Replace pbuf_2", 1);
    netbench_print_event_stat(nb, RE_PBUF_REPLACE_3, "U: RX Replace pbuf_3", 1);
    netbench_print_event_stat(nb, RE_PBUF_QUEUE,     "U: RX RPL PbufQ", 1);
    netbench_print_event_stat(nb, RE_PKT_RCV_CS,     "U: RX PKT RCV CS", 1);
*/

/*
    netbench_print_event_stat(nb, TX_SP,         "U: TX send-pt", 1);
    netbench_print_event_stat(nb, TX_SP1,        "U: TX send-pt no-noti", 1);
    netbench_print_event_stat(nb, TX_SPP_FULL,   "U: TX D SPP FUL", 0);
    netbench_print_event_stat(nb, TX_SN_WAIT,    "U: TX SN WAIT", 1);
    netbench_print_event_stat(nb, TX_SN_SEND,    "U: TX SN SEND", 1);
    netbench_print_event_stat(nb, TX_A_SP_RN_CS, "U: TX SP RN CS", 1);
    netbench_print_event_stat(nb, TX_A_SP_RN_T,  "U: TX SP RN T", 1);
    netbench_print_event_stat(nb, TX_SND_PKT_S,  "U: TX SND PKT SLOT", 0);
    netbench_print_event_stat(nb, TX_SND_PKT_C,  "U: TX SND PKTS", 0);
*/

}


// checks and tells if given hardware feature is enabled or not.
// NOTE: This code is kept in this file because the information about the
// card name is available here. But we should move into separate file
// in future.
bool is_hw_feature_enabled(int hw_feature)
{
    // FIXME: based on the cardname and configuration, hardware feature's
    // activeness should be decided
    return false;
} // end function: is_hw_feature_enabled

