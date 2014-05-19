/**
 * \file
 * \brief simple udp benchmark
 */

/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <stdio.h>
#include <lwip/pbuf.h>
#include <lwip/udp.h>
#include <lwip/init.h>
#include <netif/etharp.h>
#include <contmng/netbench.h>

/* Enable tracing only when it is globally enabled */
#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define UDP_BENCHMARK_TRACE 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK


//#define TOTAL_DATA_SIZE  629188608
#define MAX_DATA   1330
#define MULTIPLIER 100

//#define TEST_BUFFER_MANAGEMENT      1

#ifdef TEST_BUFFER_MANAGEMENT
#define TEST_TYPE   "With BUFF Mng"
#else
#define TEST_TYPE   "Without BUFF Mng"
#endif // TEST_BUFFER_MANAGEMENT


static int connection_type = 0;  // 0 for using PBUF_POOL (RX path)
//static int connection_type = 1;  // 1 for PBUF_RAM (TX path) Horribly slow!!

static uint64_t pkt_count = 0;
static uint64_t rx_data_size = 0;
static uint64_t recv_start_c = 0;
static uint64_t recv_stop_c = 0;

static uint64_t iterations = 2;

static struct waitset *ws = NULL;

static void
loop_forever(void)
{
    errval_t r;
    // Loop forever
    while (1) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }
}

static void
refresh_cache(struct ip_addr *dst_ip)
{
    struct netif *netif;
    netif = ip_route(dst_ip);

    errval_t r = etharp_request(netif, dst_ip);
    assert(err_is_ok(r));

   while (is_ip_present_in_arp_cache(dst_ip) == false) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: till arp not present
}

static uint64_t stats[10] = {0, 0, 0, 0};
static    uint64_t start_tx = 0;
static    uint64_t iter = 0; // Iteration counter
static    uint64_t failed = 0; // Failure counter
static void stop_benchmark(uint64_t stop, uint64_t driver_runtime,
        uint64_t drv_pkt_count)
{
    // FIXME: Make sure that all data is gone
//    uint64_t stop = rdtsc();
    uint64_t delta = stop - start_tx;

    // sending debug message marking the stop of benchmark
//    lwip_benchmark_control(connection_type, BMS_STOP_REQUEST, 0, 0);


    printf("U: Test [%s], PBUF type %s\n", TEST_TYPE,
            connection_type?"PBUF_RAM":"PBUF_POOL");
    lwip_print_interesting_stats();
    printf("U: Time taken by APP %"PU" to send %"PRIu64" packets"
            "(%"PRIu64" failed)\n",
            in_seconds(delta), iter, failed);
    if (driver_runtime > 0) {
        printf("U: Time taken by DRV %"PU" to send %"PRIu64" packets\n",
                in_seconds(driver_runtime), drv_pkt_count);
    }
    uint64_t data_size = iter * MAX_DATA;
    printf("U: TX speed (app view) = data(%"PRIu64") / time(%"PU") = [%f] KB \n",
            data_size, in_seconds(delta), ((data_size/in_seconds(delta))/1024));

    if (driver_runtime > 0) {
        data_size = drv_pkt_count * MAX_DATA;
        printf("U: TX speed (DRV view) = data(%"PRIu64") / time(%"PU") = [%f] KB \n",
            data_size, in_seconds(driver_runtime),
            ((data_size/in_seconds(driver_runtime))/1024));
    }
    for (int j = 0; j < 6; ++j) {
        printf("U: Stats  %d: [%"PRIu64"] \n", j, stats[j]);
    }

    loop_forever();
}

static void wait_for_lwip(void)
{
   errval_t r;
    int ans;
   while ((ans = is_lwip_loaded()) > 0) {
//       printf("is_lwip_loaded returned %d\n", ans);
        ++stats[ans];
/*        if(ans == 1) {
            printf("stopping the benchmark as no more pbufs\n");
            stop_benchmark();
        }
*/
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: lwip_loaded
   ++stats[ans];
} // end function: wait_for_lwip

static struct pbuf *
get_pbuf_wrapper(void)
{
    struct pbuf *p = NULL;
    if (connection_type == 1) {
        p = pbuf_alloc(PBUF_TRANSPORT, MAX_DATA, PBUF_RAM);
    } else {
        p = pbuf_alloc(PBUF_TRANSPORT, MAX_DATA, PBUF_POOL);
        // setting ref to zero as we are using it for sending and
        // not receiving
    }
    if (p == NULL){
        printf("pbuf_alloc failed while counter %"PRIu16" \n",
                free_pbuf_pool_count());
    }
    assert(p != NULL);
    assert(p->payload != NULL);
    assert(p->len == p->tot_len);
    assert(p->len == MAX_DATA);
//    memset(p->payload, 'd', p->len);
    return p;

} // end function: get_pbuf_wrapper


static bool wait_for_driver_ready(void)
{
    errval_t r;
    uint8_t ans;
    uint64_t delta;
    uint64_t cl;

    while (1) {
        ans = lwip_driver_benchmark_state(connection_type, &delta, &cl);
        if (ans == BMS_RUNNING) {
            return true;
        }
        assert(ans == 1);

        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
    } // end while: lwip_loaded
    return false;
}

static bool check_for_driver_done(uint64_t *delta, uint64_t *cl)
{
    uint8_t ans;
    ans = lwip_driver_benchmark_state(connection_type, delta, cl);
    if (ans == BMS_STOPPED) {
        return true;
    }
    return false;
}

static void
udp_sender(struct udp_pcb *upcb, struct ip_addr recv_ip,
        uint16_t recv_port)
{
    uint64_t driver_delta;
    uint64_t cl;

    struct pbuf *p = NULL;
    printf("U: Going in UDP_SENDER mode\n");

    // connect with peer
    errval_t r = udp_connect(upcb, &recv_ip, recv_port);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "udp_connect:");
    }

#ifndef TEST_BUFFER_MANAGEMENT
    // create a pbuf
    printf("U: Testing without buffer manager\n");
    p = get_pbuf_wrapper();
    printf("U: pbuf len %"PRIu16", tot_len %"PRIu16"\n",
            p->len, p->tot_len);
    void *payload_ptr = p->payload;

    // Set the data to zero
    memset(p->payload, 'd', p->len);
#else
    printf("U: Testing *with* buffer manager!\n");
#endif // TEST_BUFFER_MANAGEMENT

    refresh_cache(&recv_ip);

    printf("U: Trying to send %"PRIu64" packets\n", iterations);

    lwip_benchmark_control(connection_type, BMS_START_REQUEST, iterations, 0);
    wait_for_driver_ready();
    start_tx = rdtsc();

    // send data
//    for (iter = 0; iter < iterations; ++iter) {
    iter = 0;
    while (1) {
//        wait_for_lwip();

#ifdef TEST_BUFFER_MANAGEMENT
        p = get_pbuf_wrapper();
#else
        /* resetting the values as they will be changed by
         * pbuf_header function */
        p->len = MAX_DATA;
        p->tot_len = MAX_DATA;
        p->payload = payload_ptr;
#endif // TEST_BUFFER_MANAGEMENT

        r = udp_send(upcb, p);
        if (err_is_fail(r)) {
            ++failed;
//            printf("udp_send failed(%"PRIu64") for iter %"PRIu64"\n",
//                    failed, iter);

//            DEBUG_ERR(r, "udp_send:");
            wait_for_lwip();
        } // end if: failed
        else {
            ++iter;
        }
//        printf("Sent packet no. %"PRIu64"\n", i);


#ifdef TEST_BUFFER_MANAGEMENT
        pbuf_free(p);
#endif // TEST_BUFFER_MANAGEMENT

        if (iter == (iterations)) {
            driver_delta = 0;
            break;
        }

        if (check_for_driver_done(&driver_delta, &cl) == true) {
            break;
        }

    } // end while :

    lwip_benchmark_control(connection_type, BMS_STOP_REQUEST, 0, 0);

    while (check_for_driver_done(&driver_delta, &cl) == false) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }

    uint64_t stop_tx = rdtsc();
    stop_benchmark(stop_tx, driver_delta, cl);
    wait_for_lwip();
} // end function: udp_sender



// ################################################ receiver benchmark ####

static bool udp_recv_bm_shown = false;
static void
udp_receiver_done(void)
{
    // Record the stop timer
    recv_stop_c = rdtsc();
    uint64_t delta = recv_stop_c - recv_start_c;
    lwip_benchmark_control(connection_type, BMS_STOP_REQUEST, 0, 0);

    lwip_print_interesting_stats();
    // print the statistics
    printf("U: Time taken %"PU" to recv %"PRIu64" data"
            "(%"PRIu64" packets)\n", in_seconds(delta),
            rx_data_size, pkt_count);
    printf("U: RX speed = data(%"PRIu64") / time(%"PU") = [%f] KB \n",
           rx_data_size, in_seconds(delta),
           ((rx_data_size/in_seconds(delta))/1024));
    udp_recv_bm_shown = true;
} // end function: udp_receiver_done

static void
udp_recv_handler(void *arg, struct udp_pcb *pcb, struct pbuf *pbuf,
                    struct ip_addr *addr, u16_t port)
{
    assert(pbuf != NULL);
    assert(pbuf->payload != NULL);
    assert(pbuf->tot_len > 0);
    rx_data_size = rx_data_size + pbuf->tot_len;
    if(pkt_count == 0){
        // record starting time
        recv_start_c = rdtsc();
    }
    ++pkt_count;

    /*
    if (pkt_count % 1000 == 0) {
        printf("U: APP %"PRIu64" packets in\n", pkt_count);
    }
*/

#if UDP_BENCHMARK_TRACE
    trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_APP_SEE, pkt_count);
#endif // UDP_BENCHMARK_TRACE

    if (pkt_count >= 3300) {
//        printf("APP %"PRIu64" packets in *\n", pkt_count);
    }

//    if (rx_data_size >= (iterations * MAX_DATA) ) {
    if (rx_data_size >= (1024 * 1024 * 1024) ) {
        // condition meet
        if (!udp_recv_bm_shown) {
            udp_receiver_done();
        }
    }
    pbuf_free(pbuf);
} // end function: udp_recv_handler


static void
udp_receiver(struct udp_pcb *upcb, struct ip_addr *listen_ip,
        uint16_t listen_port)
{
    printf("U: Going in UDP_RECEIVER mode\n");
    // Bind to specified port
    errval_t r = udp_bind(upcb, listen_ip, listen_port);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "udp_bind:");
    }

    lwip_benchmark_control(connection_type, BMS_START_REQUEST,
            iterations, rdtsc());
    udp_recv(upcb, udp_recv_handler, 0 /*client data, arg in callback*/);

    while (true) {
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }
} // end function: udp_receiver


int main(int argc, char *argv[])
{

    struct ip_addr peer_ip;  // IP address of peer
    uint16_t port = 0;  // Port number of the peer

    ws = get_default_waitset();

     // Parse args
    if (argc != 5) {
        printf("Usage: %s <direction> <IP> <Port> <packets * %d>\n",
                argv[0], MULTIPLIER);
        printf("eg (to send microbenchmark): %s 1 10.110.4.41 3000 1000\n", argv[0]);
        printf("eg (to recv microbenchmark): %s 0 10.110.4.41 3000 1000\n", argv[0]);
        return 1;
    }

    // Flag to choose between sender(1) and receiver(0)
    int as_sender = atoi(argv[1]);

    struct in_addr peer_ip_gen;
    int ret = inet_aton(argv[2], &peer_ip_gen);
    if (ret == 0) {
        printf("Invalid IP addr: %s\n", argv[2]);
        return 1;
    } // end if : ip validation
    peer_ip.addr = peer_ip_gen.s_addr;

    port = atoi(argv[3]);
    if (port <= 0) {
        printf("Invalid port given [%s] == [%"PRIu16"]\n",
                argv[3], port);
        return 1;
    } // end if : port validation

    iterations = atoi(argv[4]);
    if (iterations <= 0) {
        printf("Invalid no. of iterations [%s] == [%"PRIu64"]\n",
                argv[4], iterations);
        return 1;
    } // end if : port validation
    iterations = iterations * MULTIPLIER;

    if (lwip_init_auto() == false) {
        printf("ERROR: lwip_init_auto failed!\n");
        return 1;
    }

//    lwip_init("e1000");
    // create pcb for connection
    struct udp_pcb *upcb;
    upcb = udp_new();

    assert(upcb != NULL);

    printf("U: #####################################\n");
    printf("U: %d.%"PRIuDOMAINID": Performing [%"PRIu64"] iterations\n",
                disp_get_core_id(), disp_get_domain_id(),
                iterations);

#if UDP_BENCHMARK_TRACE
    errval_t err = trace_control(TRACE_EVENT(TRACE_SUBSYS_BNET,
                                    TRACE_EVENT_BNET_START, 0),
                        TRACE_EVENT(TRACE_SUBSYS_BNET,
                                    TRACE_EVENT_BNET_STOP, 0), 0);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "trace_control failed");
    }
    printf("U: Tracing enabled!!!!\n");
//    trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_START, 0);
#endif // UDP_BENCHMARK_TRACE

    if(as_sender == 1) {
        udp_sender(upcb, peer_ip, port);
    } else {
        udp_receiver(upcb, IP_ADDR_ANY, port);
    } // end else:


    printf("U: Init finished.\n");

    while (1) {
        errval_t r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            break;
        }
    }

    udp_remove(upcb);
} // end function: main

