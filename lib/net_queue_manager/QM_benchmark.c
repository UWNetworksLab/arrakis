/**
 * \file
 * \brief A support file holding benchmark and debug related code
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "QM_benchmark.h"

static void bm_print_interesting_stats(uint8_t type)
{
    switch (type) {
        case 0:
            netbench_print_event_stat(bm, RE_PROCESSING_ALL,
                    "D: RX processing time", 1);
            netbench_print_event_stat(bm, RE_USEFUL, "D: RX useful time", 1);
            netbench_print_event_stat(bm, RE_FILTER,  "D: RX Filter time", 1);
            netbench_print_event_stat(bm, RE_COPY, "D: RX copy time", 1);
            netbench_print_event_stat(bm, RE_PBUF_REG, "D: RX pbuf reg T", 1);
            netbench_print_event_stat(bm, RE_DROPPED, "D: RX dropped time", 1);
            netbench_print_event_stat(bm, RE_TX_SP_MSG, "D: SP_MSG", 1);
            netbench_print_event_stat(bm, RE_TX_SP_MSG_Q, "D: SP_MSG_Q", 1);
            netbench_print_event_stat(bm, RE_PENDING_WORK, "D: pending wrk", 1);
            netbench_print_event_stat(bm, RE_PENDING_1, "D: pending 1", 1);
            netbench_print_event_stat(bm, RE_PENDING_2, "D: pending 2", 1);
            netbench_print_event_stat(bm, RE_PENDING_2, "D: pending 3", 1);
            netbench_print_event_stat(bm, RE_PENDING_4, "D: pending 4", 1);
            break;

        case 1:
            netbench_print_event_stat(bm, RE_TX_NOTI_CS,
                    "D: NOTI FROM APPLI", 0);
            netbench_print_event_stat(bm, RE_TX_T, "D: TX T", 0);
            netbench_print_event_stat(bm, RE_TX_SP_S, "D: TX SP_S", 0);
            netbench_print_event_stat(bm, RE_TX_SP_F, "D: TX SP_F", 0);
            netbench_print_event_stat(bm, RE_TX_DONE, "D: TX DONE", 0);
            netbench_print_event_stat(bm, RE_TX_W_ALL, "D: TX W_ALL", 0);
            netbench_print_event_stat(bm, RE_TX_DONE_NN, "D: TX DONE_NN", 0);
            netbench_print_event_stat(bm, RE_TX_DONE_N, "D: TX DONE_N", 0);
            netbench_print_event_stat(bm, RE_TX_SP_MSG, "D: TX SP_MSG", 0);
            netbench_print_event_stat(bm, RE_TX_SP_MSG_Q, "D: TX SP_MSG_Q", 0);
        break;

        default:
            printf("Invalid type given to print stats\n");
    } // end switch
}


void reset_client_closure_stat(struct client_closure *cc)
{
    cc->debug_print = 0;
    cc->start_ts = 0;
    cc->start_ts_tx = 0;
    cc->pkt_count = 0;

    cc->dropped_pkt_count = 0;
    cc->in_dropped_app_buf_full = 0;

    cc->out_trigger_counter = 0;

    cc->in_filter_matched = 0;
    cc->in_filter_matched_p = 0;
    cc->in_filter_matched_f = 0;

    cc->pbuf_count = 0;
}

// *****************************************************
// Interface: benchmark_control
// *****************************************************

// Wrapper function:
static errval_t send_benchmark_control_response(struct q_entry e)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        e.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.benchmark_control_response(b,
                    MKCONT(cont_queue_callback, ccl->q),
                    e.plist[0], e.plist[1], e.plist[2], e.plist[3]);
                    // queueid, state, delta, cl
    } else {
        ETHERSRV_DEBUG("send_benchmark_control_response Flounder busy.."
                " will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void send_benchmark_control(struct net_queue_manager_binding *cc,
        uint64_t queueid, uint64_t state, uint64_t delta, uint64_t cl)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_benchmark_control_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = queueid;
    entry.plist[1] = state;
    entry.plist[2] = delta;
    entry.plist[3] = cl;
    enqueue_cont_q(ccl->q, &entry);
}


void benchmark_control_request(struct net_queue_manager_binding *cc,
        uint64_t queueid, uint8_t state, uint64_t trigger, uint64_t cl_data)
{
    uint64_t ts;
    uint8_t bm_type = 0; // 0 = RX benchmark, 1 = TX benchmark
//    printf("setting the debug status to %x and trigger [%"PRIu64"]\n",
//            state, trigger);
    struct client_closure *cl = ((struct client_closure *) (cc->st));

    assert(cl->queueid == queueid);
    cl->debug_state = state;
    cl->debug_print = state;
    switch (state) {

        case BMS_STOP_REQUEST:  // PRINTING stats

            if (bm_type == 0) {
                ts = rdtsc() - cl->start_ts;
            } else {
                ts = rdtsc() - cl->start_ts_tx;
            }
            printf("# D: Stopping MBM time[%"PU"],"
                   "TX Pbufs[%" PRIu64 "], TX pkts[%" PRIu64 "], "
                   "D[%" PRIu64 "], "
                   "in SP Q[%" PRIu64 "]\n",
                   in_seconds(ts), cl->pbuf_count, cl->pkt_count,
                   cl->dropped_pkt_count,
                   sp_queue_elements_count(cl->spp_ptr));

            printf( "# D: RX FM[%"PRIu64"], FMF[%"PRIu64"], FMP[%"PRIu64"]\n",
                  cl->in_filter_matched, cl->in_filter_matched_f,
                  cl->in_filter_matched_p);

            uint64_t data_size = cl->in_filter_matched_p * 1330;
            printf("# D: RX speed (D view) = data(%"PRIu64") / time(%"PU") "
                    "= [%f] KB \n",
                    data_size,
                    in_seconds(ts), ((data_size/in_seconds(ts))/1024));

/*
            printf("### RX OK[%"PRIu64"], D_CQ_full[%"PRIu64"], "
                  "D_invalid[%"PRIu64"], D_NO_APP[%"PRIu64"], "
                  "D_APP_BUF_FULL[%"PRIu64"], D_APP_BUF_INV[%"PRIu64"]\n",
                  cl->in_success, cl->in_dropped_q_full,
                  cl->in_dropped_invalid_pkt, cl->in_dropped_no_app,
                  cl->in_dropped_app_buf_full, cl->in_dropped_app_invalid_buf);
            printf("### RX D_NTF_PROB[%"PRIu64"], D_NTF_PRO2[%"PRIu64"], "
                  "Other_pkt_OK[%"PRIu64"], in_ARP[%"PRIu64"], "
                  "in_NETD[%"PRIu64"], in_paused[%"PRIu64"]\n",
                  cl->in_dropped_notification_prob,
                  cl->in_dropped_notification_prob2, cl->in_other_pkts,
                  cl->in_arp_pkts, cl->in_netd_pkts, cl->in_paused_pkts);
*/

            bm_print_interesting_stats(bm_type);

            printf("D: Interrupt count [%"PRIu64"], loop count[%"PRIu64"], "
                    "Time in interrupt handling [%"PU"]\n",
                    interrupt_counter, total_rx_p_count,
                    in_seconds(total_interrupt_time));

            printf("D: Driver spp state");
            sp_print_metadata(cl->spp_ptr);

            send_benchmark_control(cc, cl->queueid, BMS_STOPPED, ts,
                    (cl->pkt_count - cl->dropped_pkt_count));

            cl->out_trigger_counter = trigger;

            cl->debug_state = BMS_STOPPED;
            cl->debug_state_tx = BMS_STOPPED;
            g_cl = NULL;
            break;

        case BMS_START_REQUEST:  // Resetting stats, for new round of recording
            interrupt_counter = 0;
            total_rx_p_count = 0;
            total_interrupt_time = 0;
            g_cl = cl;
            reset_client_closure_stat(cl);
//          assert(cl->spp_ptr->sp->read_reg.value == 0);

            cl->out_trigger_counter = trigger;
            cl->debug_state = 3;
            cl->debug_state_tx = 3;
            cl->pkt_count = 0;
            // Resetting receive path stats
            netbench_reset(bm);
            bm->status = 1;
            printf("# D: Starting MBM now \n");
            cl->start_ts = rdtsc();
            cl->start_ts_tx = rdtsc();
            send_benchmark_control(cc, cl->queueid, BMS_RUNNING,
                    cl->start_ts, trigger);
            break;


        default:
            printf("# D: MBM: invalid state %x \n", state);
    } // end switch:

} // end function: benchmark_control_request

