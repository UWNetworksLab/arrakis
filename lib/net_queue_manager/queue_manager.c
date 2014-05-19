/**
 * \file
 * \brief Generic server part for most ethernet drivers.
 * Current drivers using this server code are
 * -- e1000n
 * -- rtl8029
 * -- eMAC
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>
#include <stdio.h>
#include <string.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <net_queue_manager/net_queue_manager.h>
#include <if/net_queue_manager_defs.h>

#include "QM_benchmark.h"
#include "queue_manager_debug.h"
#include "queue_manager_local.h"


/* Enable tracing based on the global settings. */
#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif                          // CONFIG_TRACE && NETWORK_STACK_TRACE



// FIXME: This is repeated, make it common
#define MAX_SERVICE_NAME_LEN  256   // Max len that a name of service can have

/*****************************************************************
 * Global datastructure
 *****************************************************************/
// True iff we use software filtering
static bool use_sf;

// True iff we use the raw interface (implies !use_sf)
static bool use_raw_if = true;

// True if sofware filtering was disabled using the command-line parameter, this
// means that software filtering is not used, even if we are on queue 0.
static bool force_disable_sf = false;

struct netbench_details *bm = NULL; // benchmarking data holder

struct buffer_descriptor *buffers_list = NULL;

/*****************************************************************
 * Prototypes
 *****************************************************************/

static void register_buffer(struct net_queue_manager_binding *cc,
        struct capref cap, struct capref sp, uint64_t queueid,
        uint64_t slots, uint8_t role);
static void raw_add_buffer(struct net_queue_manager_binding *cc,
                           uint64_t offset, uint64_t length,
                           uint64_t more, uint64_t flags);
static void get_mac_addr_qm(struct net_queue_manager_binding *cc,
        uint64_t queueid);
static void print_statistics_handler(struct net_queue_manager_binding *cc,
        uint64_t queueid);
static void terminate_queue(struct net_queue_manager_binding *cc);

static void do_pending_work(struct net_queue_manager_binding *b);

/*****************************************************************
 * VTABLE
 *****************************************************************/

// Initialize service
static struct net_queue_manager_rx_vtbl rx_nqm_vtbl = {
    .register_buffer = register_buffer,
    .raw_add_buffer = raw_add_buffer,
    .get_mac_address = get_mac_addr_qm,
    .print_statistics = print_statistics_handler,
    .benchmark_control_request = benchmark_control_request,
    .terminate_queue = terminate_queue,
};

/*****************************************************************
 * Pointers to driver functionalities:
 *****************************************************************/
static ether_terminate_queue ether_terminate_queue_ptr = NULL;
static ether_get_mac_address_t ether_get_mac_address_ptr = NULL;
static ether_transmit_pbuf_list_t ether_transmit_pbuf_list_ptr = NULL;
static ether_get_tx_free_slots tx_free_slots_fn_ptr = NULL;
static ether_handle_free_TX_slot handle_free_tx_slot_fn_ptr = NULL;
ether_rx_register_buffer rx_register_buffer_fn_ptr = NULL;
ether_rx_get_free_slots rx_get_free_slots_fn_ptr = NULL;

/*****************************************************************
 * Local states:
 *****************************************************************/
static char exported_queue_name[MAX_SERVICE_NAME_LEN] = {0}; // exported Q name
static uint64_t exported_queueid = 0; // id of queue
static size_t rx_buffer_size = 0;

// client_no used to give id's to clients
// WARN: should start at 0 as loopback table lookup depends on this assumption
static int client_no = 0;  // number of clients(apps) connected
static struct net_queue_manager_binding *all_apps[1024];

static uint64_t buffer_id_counter = 0; // number of buffers registered
// FIXME: following should be gone in next version of code
static uint64_t netd_buffer_count = 0; // way to identify the netd

static struct buffer_descriptor *first_app_b = NULL;

// *************************************************************
//  local loopback device related code
// *************************************************************
// support for loopback device
bool is_loopback_device = false;

struct loopback_mapping{
    int tx_cl_no;   // sender client number
    int rx_cl_no;   // corrosponding rx client number
    struct buffer_descriptor *lo_rx_buf;
};
// We currently support only two applications on loopback device.
static struct loopback_mapping lo_map_tbl[4] = {{0,0, NULL},};

// to ensure that lo_map indexes will work irrespective of
// client_no initialization value
static int lo_tbl_idx = 0;

// fill up the lo_map_tbl with valid entries
// Assumptions:
//  * client_no starts at 0
//  * First connection is RX (ie 0) and second is TX (ie 1)
static void populate_lo_mapping_table(int cur_cl_no,
            struct buffer_descriptor *buffer_ptr)
{
    // sanity checks

    printf("populate called for client %d\n", cur_cl_no);
    assert(is_loopback_device); // ensuring loopback device
    assert(cur_cl_no == lo_tbl_idx); // ensure monotonic increase
    assert(lo_tbl_idx < 4); // we currently support only 2 applications for lo

    // and I should validate the buffer types
    assert(RX_BUFFER_ID == 0); // ensure RX is 0

    if((lo_tbl_idx % 4) != (buffer_ptr->role)) {
        printf(" tlb_idx %d, role %"PRIu8"\n", lo_tbl_idx, buffer_ptr->role);
    }
    assert((lo_tbl_idx % 4) == (buffer_ptr->role));

    // populate the table entries
    lo_map_tbl[lo_tbl_idx].tx_cl_no = -1;
    lo_map_tbl[lo_tbl_idx].rx_cl_no = -1;
    lo_map_tbl[lo_tbl_idx].lo_rx_buf = buffer_ptr;

    switch(lo_tbl_idx) {
        case 0:
        case 2:
            // intermediate state, nothing to do!
            break;

        case 1:
            printf("populate case 1 crossing 0 %d\n", cur_cl_no);
            // Assuming only one app, so mapping tx to rx
            lo_map_tbl[lo_tbl_idx].tx_cl_no = cur_cl_no;
            lo_map_tbl[lo_tbl_idx].rx_cl_no = 0;
            lo_map_tbl[lo_tbl_idx].lo_rx_buf = lo_map_tbl[0].lo_rx_buf;
            break;

        case 3:
            // Assuming are two apps, so mapping them

            // mapping 3 to 0
            lo_map_tbl[lo_tbl_idx].tx_cl_no = cur_cl_no;
            lo_map_tbl[lo_tbl_idx].rx_cl_no = 0;
            lo_map_tbl[lo_tbl_idx].lo_rx_buf = lo_map_tbl[0].lo_rx_buf;

            // mapping 1 to 2
            lo_map_tbl[1].tx_cl_no = 1;
            lo_map_tbl[1].rx_cl_no = 2;
            lo_map_tbl[1].lo_rx_buf = lo_map_tbl[2].lo_rx_buf;
            break;

        default:
            USER_PANIC("More than two clients are not supported for lo");
            abort();
            break;
    } // end switch:

    ++lo_tbl_idx;
} // end function: populate_lo_mapping_table


struct buffer_descriptor *get_lo_receiver(void *opaque)
{
    // making sure that this is indeed loopback device
    assert(is_loopback_device);
    assert(opaque != NULL);

    struct buffer_state_metadata *bsm = opaque;

    struct net_queue_manager_binding *b = bsm->binding;

    // find client_no of sending app
    assert(b != NULL);
    struct client_closure *cc = (struct client_closure *)b->st;
    assert(cc != NULL);

    int cl_no = cc->cl_no;

    // Make sure that cc->cl_no is valid for lo_map table
    assert(lo_map_tbl[cl_no].rx_cl_no != -1);

    // get buffer_ptr from the lookup table
    return (lo_map_tbl[cl_no].lo_rx_buf);
} // end function: get_lo_receiver

// *************************************************************
// Client and buffer management code
// *************************************************************



// Creates a new client for given connection
static struct client_closure *create_new_client(
        struct net_queue_manager_binding *b)
{
    struct client_closure *cc =
      (struct client_closure *) malloc(sizeof(struct client_closure));
    if (cc == NULL) {
        ETHERSRV_DEBUG("create_new_client: out of memory\n");
        return NULL;
    }
    memset(cc, 0, sizeof(struct client_closure));

    struct buffer_descriptor *buffer =
      (struct buffer_descriptor *) malloc(sizeof(struct buffer_descriptor));
    if (buffer == NULL) {
        ETHERSRV_DEBUG("create_new_client: out of memory\n");
        free(cc);
        return NULL;
    }
    memset(buffer, 0, sizeof(struct buffer_descriptor));

    b->st = cc;
    cc->app_connection = b;

    // FIXME: I should not need this as now netd is normal app
    // save it if it is netd app
    if (client_no < 2) {
        netd[client_no] = b;
    }

    cc->buffer_ptr = buffer;
    cc->debug_state = 0;        // Default debug state : no debug
    reset_client_closure_stat(cc);
    cc->start_ts = rdtsc();

    all_apps[client_no] = b;

    cc->cl_no = client_no++;

    char name[64];
    sprintf(name, "ether_a_%d_%s", cc->cl_no,
                ((cc->cl_no % 2) == 0)? "RX" : "TX");
    cc->q = create_cont_q(name);
    if (cc->q == NULL) {
        ETHERSRV_DEBUG("create_new_client: queue allocation failed\n");
        free(buffer);
        free(cc);
        return NULL;
    }
    // cc->q->debug = 1;
    return cc;
} // end function: create_new_client

// populates the given buffer with given capref
static errval_t populate_buffer(struct buffer_descriptor *buffer,
        struct capref cap)
{

    buffer->cap = cap;
    struct frame_identity pa;
    errval_t err = invoke_frame_identify(cap, &pa);
    if (!err_is_ok(err)) {
        printf("invoke_frame_identify failed\n");
        abort();
    }
    buffer->pa = pa.base;
    buffer->bits = pa.bits;

#ifdef __scc__
    err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
                                  VREGION_FLAGS_READ_WRITE_MPB, NULL, NULL);
#else
    err = vspace_map_one_frame(&buffer->va, (1L << buffer->bits), cap,
            NULL, NULL);
#endif

/*
    err = vspace_map_one_frame_attr(&buffer->va, (1L << buffer->bits), cap,
                    VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
*/

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        // FIXME: report more sensible error
        return(ETHERSRV_ERR_TOO_MANY_BUFFERS);
    }
    netd_buffer_count++;
    buffer_id_counter++;
    buffer->buffer_id = buffer_id_counter;
//    printf("### buffer gets id %"PRIu64"\n", buffer->buffer_id);
    if (buffer->buffer_id == 3) {
        first_app_b = buffer;
    }

    buffer->next = buffers_list;
    // Adding the buffer on the top of buffer list.
//    buffers_list = buffer;
    return SYS_ERR_OK;
} // end function: populate_buffer


// Find buffer in the list of all registered buffer using buffer_id
// FIXME: it is singly linked list, hence very slow if no of apps increases
struct buffer_descriptor *find_buffer(uint64_t buffer_id)
{
    struct buffer_descriptor *elem = buffers_list;
    while(elem) {
        if (elem->buffer_id == buffer_id) {
            return elem;
        }
        elem = elem->next;
    }
    printf("Could not find buffer with id %"PRIu64"\n", buffer_id);
    // abort here because in some cases, returning NULL crashes the driver
    // specially in e1000n.c: transmit_pbuf_list_fn() call
    abort();
    return NULL;
} // end function: buffer_descriptor


// **********************************************************
// Interface  implementation
// **********************************************************

// *********** Interface: register_buffer *******************
static errval_t send_new_buffer_id(struct q_entry entry)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
                    entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.new_buffer_id(b, MKCONT(cont_queue_callback, ccl->q),
                                        entry.plist[0],
                                        entry.plist[1],
                                        entry.plist[2]);
        // entry.error, entry.queueid, entry.buffer_id
    } else {
        ETHERSRV_DEBUG("send_new_buffer_id Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void report_register_buffer_result(struct net_queue_manager_binding *cc,
        errval_t err, uint64_t queueid, uint64_t buffer_id)
{
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_new_buffer_id;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;

    entry.plist[0] = err;
    entry.plist[1] = queueid;
    entry.plist[2] = buffer_id;
    //   error, queue_id, buffer_id

    struct waitset *ws = get_default_waitset();
    int passed_events = 0;
    while (can_enqueue_cont_q(ccl->q) == false) {

       // USER_PANIC("queue full, can go further\n");

        if (passed_events > 5) {
            USER_PANIC("queue full, can go further\n");
            //return CONT_ERR_NO_MORE_SLOTS;
        }
        event_dispatch_debug(ws);
        ++passed_events;
    }


    enqueue_cont_q(ccl->q, &entry);
}


// Actual register_buffer function with all it's logic
static void register_buffer(struct net_queue_manager_binding *cc,
            struct capref cap, struct capref sp, uint64_t queueid,
            uint64_t slots, uint8_t role)
{

    ETHERSRV_DEBUG("ethersrv:register buffer called with slots %"PRIu64"\n",
            slots);
    errval_t err;
    struct client_closure *closure = (struct client_closure *)cc->st;
    assert(exported_queueid == queueid);
    closure->queueid = queueid;

    struct buffer_descriptor *buffer = closure->buffer_ptr;
    err = populate_buffer(buffer, cap);
    if (err_is_fail(err)) {
        report_register_buffer_result(cc, err, queueid, 0);
        return;
    }
    buffer->role = role;
    buffer->con = cc;
    buffer->queueid = queueid;


    // Create a list to hold metadata for sending
    buffer->rxq.buffer_state_size = slots;
    buffer->rxq.buffer_state = calloc(slots,
            sizeof(struct buffer_state_metadata));
    assert(buffer->rxq.buffer_state != NULL);
    buffer->rxq.buffer_state_head = 0;
    buffer->rxq.buffer_state_used = 0;

    // Create a list to hold metadata for receiving
    buffer->txq.buffer_state_size = slots;
    buffer->txq.buffer_state = calloc(slots,
            sizeof(struct buffer_state_metadata));
    assert(buffer->txq.buffer_state != NULL);
    buffer->txq.buffer_state_head = 0;
    buffer->txq.buffer_state_used = 0;

    if (is_loopback_device) {
        populate_lo_mapping_table(closure->cl_no, closure->buffer_ptr);
    } // end if: loopback device

    // Use raw interface if desired
    printf("net_queue_manager: Use raw interface\n");
    use_raw_if = true;

    buffers_list = buffer;
    report_register_buffer_result(cc, SYS_ERR_OK, queueid,
                                      buffer->buffer_id);
    return;
} // end function: register_buffer


static errval_t wrapper_send_raw_xmit_done(struct q_entry e)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        e.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        errval_t err = b->tx_vtbl.raw_xmit_done(b,
                MKCONT(cont_queue_callback, ccl->q),
                e.plist[0], e.plist[1], e.plist[2], e.plist[3]);
        return err;
    } else {
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static __attribute__((unused))  void
handle_single_event_nonblock(struct waitset *ws)
{
    errval_t err;

    while (1) {

        do_pending_work_for_all();

        err = event_dispatch_non_block(ws); // nonblocking for polling mode
        if (err != LIB_ERR_NO_EVENT && err_is_fail(err)) {
            ETHERSRV_DEBUG("Error in event_dispatch_non_block, returned %d\n",
                        (unsigned int)err);
            // There should be a serious panic and failure here
            USER_PANIC_ERR(err, "event_dispatch_non_block failed in handle_single_event\n");
            break;
        } else {
            // Successfully handled the event
           return;
        }
    } // end while: infinite
} // end function: handle_single_event_nonblock



static errval_t send_raw_xmit_done(struct net_queue_manager_binding *b,
                                   uint64_t offset, uint64_t length,
                                   uint64_t more, uint64_t flags)
{
    struct client_closure *ccl = (struct client_closure *) b->st;

    // Send notification to application
    struct q_entry entry;
    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = wrapper_send_raw_xmit_done;
    entry.binding_ptr = b;
    entry.plist[0] = offset;
    entry.plist[1] = length;
    entry.plist[2] = more;
    entry.plist[3] = flags;

    struct waitset *ws = get_default_waitset();
    int passed_events = 0;
    while (can_enqueue_cont_q(ccl->q) == false) {

//        USER_PANIC("queue full, can go further\n");

        if (passed_events > 5) {
            cont_queue_show_queue(ccl->q);
            printf("## queue full, dropping raw_xmit_done notification\n");
            // USER_PANIC("queue full, can't go further\n");
            return CONT_ERR_NO_MORE_SLOTS;
        }

//        errval_t err = handle_single_event_nonblock(ws);
        errval_t err = event_dispatch_debug(ws);
        if (err_is_fail(err)) {
            ETHERSRV_DEBUG("Error in event_dispatch, returned %d\n",
                        (unsigned int)err);
            // There should be a serious panic and failure here
            USER_PANIC_ERR(err, "event_dispatch_non_block failed in handle_single_event\n");
            break;
        }

        ++passed_events;
    }

    enqueue_cont_q(ccl->q, &entry);
    return SYS_ERR_OK;
} // end function: send_raw_xmit_done






// *********** Interface: get_mac_address ****************

// wrapper function for responce
static errval_t wrapper_send_mac_addr_response(struct q_entry entry)
{
    struct net_queue_manager_binding *b = (struct net_queue_manager_binding *)
        entry.binding_ptr;
    struct client_closure *ccl = (struct client_closure *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.get_mac_address_response(b,
                          MKCONT(cont_queue_callback, ccl->q),
                          entry.plist[0], entry.plist[1]);
        // queueid, hwaddr
    } else {
        ETHERSRV_DEBUG("send_mac_addr_response Flounder busy.. will retry\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

uint64_t get_mac_addr_from_device(void)
{
    union {
        uint8_t hwaddr[6];
        uint64_t hwasint;
    } u;

    u.hwasint = 0;
    ether_get_mac_address_ptr(u.hwaddr);
    return u.hwasint;
}

// function to handle incoming mac address requests
static void get_mac_addr_qm(struct net_queue_manager_binding *cc,
        uint64_t queueid)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = wrapper_send_mac_addr_response;
    entry.binding_ptr = (void *) cc;
    struct client_closure *ccl = (struct client_closure *) cc->st;
    assert(ccl->queueid == queueid);

    entry.plist[0] = queueid;
    entry.plist[1] = get_mac_addr_from_device();
       // queueid,  hwaddr

    struct waitset *ws = get_default_waitset();
    int passed_events = 0;
    while (can_enqueue_cont_q(ccl->q) == false) {

       // USER_PANIC("queue full, can go further\n");

        if (passed_events > 5) {
            USER_PANIC("queue full, can go further\n");
           // return CONT_ERR_NO_MORE_SLOTS;
        }
        event_dispatch_debug(ws);
        ++passed_events;
    }


    enqueue_cont_q(ccl->q, &entry);
}

// *********** Interface: print_statistics ****************

// FIXME: NYI
static void print_statistics_handler(struct net_queue_manager_binding *cc,
        uint64_t queueid)
{
    //ETHERSRV_DEBUG
    printf("ETHERSRV: print_statistics_handler: called.\n");
}


// **********************************************************
// Functionality: packet sending (TX path)
// **********************************************************

// function to do housekeeping after descriptors are transferred
bool handle_tx_done(void *opaque)
{

    assert(opaque != NULL);
    struct buffer_state_metadata *bsm = opaque;
    struct client_closure *cl = bsm->binding->st;
    struct buffer_descriptor *buffer = cl->buffer_ptr;

    assert((buffer->txq.buffer_state_used > 0));
    --buffer->txq.buffer_state_used;

    // Handle raw interface
    errval_t err = send_raw_xmit_done(bsm->binding, (uintptr_t)bsm->offset, 0,
            0, 0);
    if (err_is_ok(err)) {
        return true;
    } else {
        printf("handle_tx_done failed for client_no %d\n", cl->cl_no);
        return false;
    }

}// end function: handle_tx_done


// Do all the work related TX path for perticular client
// It includes
//   * Take care of descriptors which are sent.
static void do_pending_work(struct net_queue_manager_binding *b)
{
    // Handle raw interface
    if (use_raw_if) {
        while (handle_free_tx_slot_fn_ptr());
        return;
    }

} // end function: do_pending_work


// Do all the TX path related work for all the clients
void do_pending_work_for_all(void)
{
    struct buffer_descriptor *next_buf = buffers_list;
    while (next_buf != NULL) {
        do_pending_work(next_buf->con);
        next_buf = next_buf->next;
    }
}

/**
 * Called by driver when it receives a new packet.
 */
void process_received_packet(struct driver_rx_buffer* bufs, size_t count,
        uint64_t flags)
{
    size_t i;
    if (use_sf) {
        // FIXME: this is broken quite badly
        sf_process_received_packet(bufs, count, flags);
        return;
    }
    // If we do no software filtering we basically only have to tell the
    // application that a new packet is ready.

    // Handle raw interface
    if (use_raw_if) {
        for (i = 0; i < count; i++) {
            assert(bufs[i].opaque != NULL);
            struct buffer_state_metadata *bsm = bufs[i].opaque;
            struct client_closure *cl = bsm->binding->st;
            struct buffer_descriptor *buf = cl->buffer_ptr;
            assert(buf->rxq.buffer_state_used > 0);

            errval_t err = send_raw_xmit_done(bsm->binding, bsm->offset,
                    bufs[i].len, (i != count - 1), flags);
            if (err_is_ok(err)) {
                --buf->rxq.buffer_state_used;
                return;
            } else {
                // As application is not able to process the packet
                // we will drop this one
                USER_PANIC("send_raw_xmit_done failed as queue full, can't go further: 1\n");
                // FIXME: Don't crash. figure out how can you drop the packet
                // and continue working after dropping the packet
                --buf->rxq.buffer_state_used;
                return;
            }
        }
    }
} // end function: process_received_packet

static uint64_t rx_added = 0; // FIXME: for debugging. Remove this
static uint64_t sent_packets = 0; // FIXME: remove this
/**
 * Used in combination with software filtering, to copy a packet into a user
 * buffer.
 */
// FIXME: why is this not in soft filter management?
bool copy_packet_to_user(struct buffer_descriptor *buffer,
                         void *data, uint64_t len, uint64_t flags)
{
    // Must only be called if we use software filtering
    assert(use_sf);

    assert(len > 0);
    assert(data != NULL);
    assert(buffer != NULL);
    struct net_queue_manager_binding *b = buffer->con;
    assert(b != NULL);
    struct client_closure *cl = (struct client_closure *) b->st;
    assert(cl != NULL);

    // check if there are slots which can be used in app (!isempty)
    if(buffer->rxq.buffer_state_used == 0) {

        printf("[%s] Dropping packet as no space in userspace "
                "2cp pkt buf [%" PRIu64 "]: "
                "size[%zu] used[%zu], after [%"PRIu64"] sent"
                " added [%"PRIu64"] \n", disp_name(),
                buffer->buffer_id, buffer->rxq.buffer_state_size,
                buffer->rxq.buffer_state_used, sent_packets, rx_added);
        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_buf_full;
        }
        //abort(); // optional, should be removed
        return false;
    }

    if (!is_enough_space_in_queue(cl->q)) {

        printf("[%s] Dropping packet as app [%d] is not processing packets"
                "fast enough.  Cont queue is almost full [%d], pkt count [%"PRIu64"]\n",
                disp_name(), cl->cl_no, queue_free_slots(cl->q), sent_packets);
        if (cl->debug_state == 4) {
            ++cl->in_dropped_app_buf_full;
        }
//        abort(); // FIXME: temparary halt to simplify debugging
        return false;
    }

    // pop the latest buffer from head of queue (this is stack)
    --buffer->rxq.buffer_state_head;
    struct buffer_state_metadata *bsm = buffer->rxq.buffer_state +
        buffer->rxq.buffer_state_head;
    assert(bsm != NULL);
    uint64_t offset = bsm->offset;
    --buffer->rxq.buffer_state_used;

    assert(offset < (1L << buffer->bits));
    void *dst = (void *) (uintptr_t) buffer->va + offset;

    ETHERSRV_DEBUG("Copy packet pos %p %p %p\n", buffer->va, dst,
                   (buffer->va + (1L << buffer->bits)));

    uint64_t ts = rdtsc();

    memcpy_fast((void *) (uintptr_t)dst, data, len);
    if (cl->debug_state == 4) {
        netbench_record_event_simple(bm, RE_COPY, ts);
    }

    ++sent_packets; // FIXME: remove this!
    // add trace pkt cpy
#if TRACE_ETHERSRV_MODE
    uint32_t pkt_location = (uint32_t) ((uintptr_t) data);

    trace_event(TRACE_SUBSYS_NNET, TRACE_EVENT_NNET_NI_PKT_CPY, pkt_location);
#endif // TRACE_ETHERSRV_MODE

    // Handle raw interface
    errval_t err = send_raw_xmit_done(b, offset, len, 0, flags);
    if (err_is_ok(err)) {
        return true;
    } else {
        // As application is not able to process the packet
        // we will drop this one
        USER_PANIC("send_raw_xmit_done failed as queue full, can't go further: 2\n");
        // FIXME: Don't crash. ignore the packet, undo any changes done by it
        // and continue.  Ideally this shouldn't happen as we are checking for
        // free space before actually sending the packt.
        return false;
    }

    return true;
} // end function: copy_packet_to_user

/*****************************************************************
 * Interface related: raw interface
 ****************************************************************/
static void raw_add_buffer(struct net_queue_manager_binding *cc,
                           uint64_t offset, uint64_t length,
                           uint64_t more, uint64_t flags)
{
    struct client_closure *cl = (struct client_closure *) cc->st;
    struct buffer_descriptor *buffer = cl->buffer_ptr;
    errval_t err;
    uint64_t paddr;
    void *vaddr, *opaque;

    paddr = ((uint64_t)(uintptr_t) buffer->pa) + offset;
    vaddr = (void*) ((uintptr_t) buffer->va + (size_t)offset);

    if (buffer->role == TX_BUFFER_ID) {
        // Make sure that there is opaque slot available (isfull)
        assert(buffer->txq.buffer_state_used < (buffer->txq.buffer_state_size - 1));

        // Save state for handle_tx_done()/handle_receive_packet
        struct buffer_state_metadata *bsm = buffer->txq.buffer_state +
            buffer->txq.buffer_state_head;
        buffer->txq.buffer_state_head = (buffer->txq.buffer_state_head + 1)
            % buffer->txq.buffer_state_size;
        bsm->binding = cc;
        bsm->offset = offset;
        ++buffer->txq.buffer_state_used;

        opaque = (void*)bsm;

        // save information as list of packet-chunks before sending to HW
        cl->driver_buff_list[cl->chunk_counter].va = vaddr;
        cl->driver_buff_list[cl->chunk_counter].pa = paddr;
        cl->driver_buff_list[cl->chunk_counter].len = length;
        cl->driver_buff_list[cl->chunk_counter].opaque = opaque;
        cl->driver_buff_list[cl->chunk_counter].flags = flags;
        ++cl->chunk_counter;
        if (more == 0) {
            // ETHERSRV_DEBUG
//            printf("sending out packet\n");
            if (cl->chunk_counter > 1) {
                ETHERSRV_DEBUG
                //printf
                    ("%s:%s: handle=%p\n", disp_name(), __func__,
                        opaque);
            }
            err = ether_transmit_pbuf_list_ptr(cl->driver_buff_list,
                    cl->chunk_counter);
            assert(err_is_ok(err));
            cl->chunk_counter = 0;
        }
    } else { // RX_BUFFER_ID

        // Sanity check.  Making sure that more flag is not set
        if (more == 1) {
            USER_PANIC("broken buffer registerd with for RX buffer\n");
        }

        // Make sure that there is opaque slot available (isfull)
        assert(buffer->rxq.buffer_state_used <
                (buffer->rxq.buffer_state_size - 1));

        // Save state for handle_tx_done()/handle_receive_packet
        struct buffer_state_metadata *bsm = buffer->rxq.buffer_state +
            buffer->rxq.buffer_state_head;
        buffer->rxq.buffer_state_head = (buffer->rxq.buffer_state_head + 1)
            % buffer->rxq.buffer_state_size;
        bsm->binding = cc;
        bsm->offset = offset;
        ++buffer->rxq.buffer_state_used;
        ++rx_added;
        opaque = (void*)bsm;

        // role == RX_BUFFER_ID
        if (use_sf) {
            // nothing to do!

        } else {
            assert(length == rx_buffer_size);
            rx_register_buffer_fn_ptr(paddr, vaddr, opaque);
        }

        // FIXME: send a message back acking receiving of message.

    } // end else: RX_BUFFER_ID
} // end function: raw_add_buffer

/*****************************************************************
 * Interface related: Exporting and handling connections
 ****************************************************************/

static void export_ether_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service [%s] export failed", exported_queue_name);
        abort();
    }

   // ETHERSRV_DEBUG
    printf("service [%s] exported at iref %"PRIu32"\n", exported_queue_name,
           (uint32_t)iref);

    // register this iref with the name service
    err = nameservice_register(exported_queue_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]",
                exported_queue_name);
        abort();
    }
}

static void error_handler(struct net_queue_manager_binding *b, errval_t err)
{
    ETHERSRV_DEBUG("ether service error_handler: called\n");
    if (err == SYS_ERR_CAP_NOT_FOUND) {
        struct client_closure *cc = b->st;

        assert(cc != NULL);
        struct buffer_descriptor *buffer = cc->buffer_ptr;

        assert(buffer != NULL);
        free(buffer);
        free(cc);
    }
    ETHERSRV_DEBUG("ether service error_handler: terminated\n");
}

static errval_t connect_ether_cb(void *st, struct net_queue_manager_binding *b)
{
    ETHERSRV_DEBUG("ether service got a connection!44\n");

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_nqm_vtbl;
    b->error_handler = error_handler;

    // Create a new client for this connection
    struct client_closure *cc = create_new_client(b);
    if (cc == NULL) {
        return ETHERSRV_ERR_NOT_ENOUGH_MEM;
    }

    return SYS_ERR_OK;
} // end function: connect_ether_cb


/*****************************************************************
 * ethersrv initialization wrapper:
 * Equivalent of main function
 ****************************************************************/
void ethersrv_init(char *service_name, uint64_t queueid,
                   ether_get_mac_address_t get_mac_ptr,
                   ether_terminate_queue terminate_queue_ptr,
                   ether_transmit_pbuf_list_t transmit_ptr,
                   ether_get_tx_free_slots tx_free_slots_ptr,
                   ether_handle_free_TX_slot handle_free_tx_slot_ptr,
                   size_t rx_bufsz,
                   ether_rx_register_buffer rx_register_buffer_ptr,
                   ether_rx_get_free_slots rx_get_free_slots_ptr)
{
    errval_t err;

    ETHERSRV_DEBUG("in the server_init\n");
    assert(service_name != NULL);
    assert(get_mac_ptr != NULL);
    assert(transmit_ptr != NULL);
    assert(tx_free_slots_ptr != NULL);
    assert(handle_free_tx_slot_ptr != NULL);
    assert(rx_register_buffer_ptr != NULL);
    assert(rx_get_free_slots_ptr != NULL);

    exported_queueid = queueid;
    rx_buffer_size = rx_bufsz;
    ether_terminate_queue_ptr = terminate_queue_ptr;
    ether_get_mac_address_ptr = get_mac_ptr;
    ether_transmit_pbuf_list_ptr = transmit_ptr;
    tx_free_slots_fn_ptr = tx_free_slots_ptr;
    handle_free_tx_slot_fn_ptr = handle_free_tx_slot_ptr;
    rx_register_buffer_fn_ptr = rx_register_buffer_ptr;
    rx_get_free_slots_fn_ptr = rx_get_free_slots_ptr;
    snprintf(exported_queue_name, sizeof(exported_queue_name),
            "%s_%"PRIu64"", service_name, queueid);

    buffers_list = NULL;
    netd[0] = NULL;
    netd[1] = NULL;
    buffer_id_counter = 0;
    netd_buffer_count = 0;
    client_no = 0;


    uint8_t my_mac[6] = {0};
    ether_get_mac_address_ptr(my_mac);
    printf("############################################\n");
    printf("For service [%s] MAC= %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                            service_name,  my_mac[0], my_mac[1], my_mac[2],
                             my_mac[3], my_mac[4], my_mac[5]);
    bm = netbench_alloc("DRV", EVENT_LIST_SIZE);

    size_t driver_supported_buffers = tx_free_slots_fn_ptr();
    printf("using %zd slots for internal buffer\n", driver_supported_buffers);
    assert(driver_supported_buffers >= 1);

    /* exporting ether interface */
    err = net_queue_manager_export(NULL, // state for connect/export callbacks
                       export_ether_cb, connect_ether_cb, get_default_waitset(),
                       IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "%s export failed", exported_queue_name);
        abort();
    }

    // FIXME: How do we decide this reasonably
    use_sf = !force_disable_sf && (queueid == 0);

    if (use_sf || is_loopback_device) {
        // start software filtering service
        init_soft_filters_service(service_name, queueid, rx_bufsz);
    }
} // end function: ethersrv_init

void ethersrv_argument(const char* arg)
{
    static uint64_t minbase = -1ULL;
    static uint64_t maxbase = -1ULL;
    static bool affinity_set = false;

    if (!strncmp(arg, "affinitymin=", strlen("affinitymin="))) {
        minbase = atol(arg + strlen("affinitymin="));
    } else if(!strncmp(arg, "affinitymax=", strlen("affinitymax="))) {
        maxbase = atol(arg + strlen("affinitymax="));
    } else if (!strncmp(arg, "disable_sf=", strlen("disable_sf="))) {
        force_disable_sf = !!atol(arg + strlen("disable_sf="));
    }

    if (!affinity_set && minbase != -1ULL && maxbase != -1ULL) {
        ram_set_affinity(minbase, maxbase);
        affinity_set = true;
    }
}

static void terminate_queue(struct net_queue_manager_binding *cc)
{
    errval_t err;
    struct buffer_descriptor *buffer;

    // Free buffers
    for (buffer = buffers_list; buffer != NULL; buffer = buffer->next) {
        err = vspace_unmap(buffer->va);
        assert(err_is_ok(err));
        err = cap_delete(buffer->cap);
        assert(err_is_ok(err));
    }

    assert(ether_terminate_queue_ptr != NULL);
    ether_terminate_queue_ptr();
}

// **********************************************************
// Additional functions
// **********************************************************

// This function tells if netd is registered or not.
bool waiting_for_netd(void)
{
    return (netd_buffer_count < 2);
//    return ((netd[RECEIVE_CONNECTION] == NULL)
//            || (netd[TRANSMIT_CONNECTION] == NULL));
} // end function: is_netd_registered


// Optimzed memcpy function which chooses proper memcpy function automatically.
void *
memcpy_fast(void *dst0, const void *src0, size_t length)
{
//    return memcpy(dst0, src0, length);
#if defined(__scc__) && defined(SCC_MEMCPY)
    return memcpy_scc2(dst0, src0, length);
#else // defined(__scc__) && defined(SCC_MEMCPY)
    return memcpy(dst0, src0, length);
#endif // defined(__scc__) && defined(SCC_MEMCPY)
}

