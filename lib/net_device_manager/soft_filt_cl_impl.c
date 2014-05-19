/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/endian.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>
#include <barrelfish/bulk_transfer.h>
//#include <net_device_manager/net_ports_service.h>
#include <if/net_soft_filters_defs.h>
//#include <if/net_ports_defs.h>
#include <contmng/contmng.h>

// For filter generation
#include <bfdmuxtools/tools.h>
#include <bfdmuxtools/codegen.h>

#include <stdio.h>
#include <string.h>

#include "port_management_support.h"
#include "device_manager_debug.h"


#define NORMAL_FILTER       (1)
#define REDIRECT_FILTER      (2)

/****************************************************************
* Global datastructure
*****************************************************************/
// client closure for connection between netd and ethernet driver
struct client_closure_ND {
    struct cont_queue *q;
};


/****************************************************************
* Local states
*****************************************************************/

// handle for connection with soft filters service.
static struct net_soft_filters_binding *soft_filters_connection = NULL;

static bool soft_filters_ready = false;
// bulk_transfer used to move the packet filters between net_device_manager
// and queue_manager
static struct bulk_transfer bt_filter_tx;

static bool filter_mem_lock = true; // protects the above filter memory


// Local ip address assigned to the interface
struct ip_addr local_ip = {
        .addr = BFDMUX_IP_ADDR_ANY
    };

static bool valid_mac_addr_assigned = false; // marks valid mac address
static struct eth_addr mac; // = { .addr = {0, 0, 0, 0, 0, 0}};

// *****************************************************************
// * related to managing soft filters
// *****************************************************************

static uint64_t populate_rx_tx_filter_mem(uint16_t port,
        net_ports_port_type_t type, int32_t *len_rx, int32_t *len_tx);

static void connect_soft_filters_service(char *dev_name, qid_t qid);
static void register_arp_soft_filter(uint64_t id, uint64_t len_rx,
                                    uint64_t len_tx);

errval_t register_soft_filt_impl(uint16_t port,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t qid);

static void unregister_soft_filter(uint64_t filter_id, qid_t qid);

static struct filters_tx_vtbl soft_filts_mng = {
    .type = "Soft_filters",
    .init_filters = connect_soft_filters_service,
    .reg_arp_filters = register_arp_soft_filter,
    .reg_filters = register_soft_filt_impl,
    .unreg_filters = unregister_soft_filter,
};

/*****************************************************************
* Prototypes
*****************************************************************/
static void share_common_memory_with_filter_manager(void);
static void sf_mac_lookup(void);

static void register_filter_memory_response(
                        struct net_soft_filters_binding *st,
                        errval_t err);
static void send_soft_filter(uint64_t id, uint64_t len_rx, uint64_t len_tx,
                                uint64_t buffer_id_rx, uint64_t buffer_id_tx,
                                uint8_t ftype, uint8_t paused);


static void deregister_filter_response(struct net_soft_filters_binding *st,
                                       errval_t err, uint64_t filter_id);
static void register_filter_response(struct net_soft_filters_binding *st,
                                     uint64_t id, errval_t err,
                                     uint64_t filter_id, uint64_t buffer_id_rx,
                                     uint64_t buffer_id_tx, uint64_t ftype);
static void register_arp_filter_response(struct net_soft_filters_binding *st,
                                         uint64_t id, errval_t err);


static void sf_mac_address_response(struct net_soft_filters_binding *st,
                      errval_t err,  uint64_t mac);


static struct net_soft_filters_rx_vtbl rx_vtbl = {
    .register_filter_memory_response = register_filter_memory_response,
    .register_filter_response = register_filter_response,
    .deregister_filter_response = deregister_filter_response,
    .register_arp_filter_response = register_arp_filter_response,
//    .pause_response = pause_response,
    .mac_address_response = sf_mac_address_response,
};

// *****************************************************************
// * Get signature of this service
// *****************************************************************
struct filters_tx_vtbl *get_soft_filt_mng_sign(void)
{
    return &soft_filts_mng;
} // end function: get_filt_mng_sign


// *****************************************************************
// * converting port requests into filters
// *****************************************************************


errval_t register_soft_filt_impl(uint16_t port,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t qid)
{
    int32_t len_rx, len_tx;
    /* NOTE: check if someone else is using the filter location */
    if (filter_mem_lock) {
        /* FIXME: as there is only one registered location for filter
           transfer, only one filter registration can be done at one time. */
        NDM_DEBUG("filter memory is busy.\n");
        return FILTER_ERR_FILTER_BUSY;
    }

    /* create rx, tx filter around that port */
    filter_mem_lock = true;     /* NOTE: filter memory is in use
                                   till "registered_filter" is called by filter_manager */
    uint64_t id = populate_rx_tx_filter_mem(port, type, &len_rx, &len_tx);

    /* Register the filter with soft_filters */
    NDM_DEBUG("get_port: trying to register the filter with id %" PRIu64 "\n",
               id);
    send_soft_filter(id, len_rx, len_tx, buffer_id_rx, buffer_id_tx,
                        NORMAL_FILTER, 0);

    return SYS_ERR_OK;
}


// *****************************************************************
// * Connect with soft filter service
// *****************************************************************

static void soft_filters_bind_cb(void *st, errval_t err,
                                  struct net_soft_filters_binding *enb)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
    NDM_DEBUG("soft_filters_bind_cb: started\n");


    struct client_closure_ND *ccnd = (struct client_closure_ND *)
      malloc(sizeof(struct client_closure_ND));

    memset(ccnd, 0, sizeof(struct client_closure_ND));
    ccnd->q = create_cont_q("SF_C");

    // set client closure
    enb->st = ccnd;
    // copy my message receive handler vtable to the binding
    enb->rx_vtbl = rx_vtbl;

    soft_filters_connection = enb;
    NDM_DEBUG(" soft_filters_bind_cb: connection made,"
               " now registering memory \n");
    NDM_DEBUG("soft_filters_bind_cb: terminated\n");
}


// \brief: Establishes connection with soft_filters service
static void connect_soft_filters_service(char *dev_name, qid_t qid)
{
    assert(dev_name != NULL);
    NDM_DEBUG("c_sf_mng: connecting to dev [%s]\n", dev_name);

    errval_t err;

    // The service name
    char service_name[MAX_NET_SERVICE_NAME_LEN];

    snprintf(service_name, sizeof(service_name), "%s_%" PRIu64 "%s",
            dev_name, qid, FILTER_SERVICE_SUFFIX);
    NDM_DEBUG("c_sf_mng: resolving service [%s]\n", service_name);

    // locate the service
    iref_t iref = 0;
    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "c_sf_mng: could not connect to soft filter manager .\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    // Connect to the service
    NDM_DEBUG("c_sf_mng: connecting\n");

    err = net_soft_filters_bind(iref, soft_filters_bind_cb, NULL,
                         get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));

    // waiting for connection to succeed.
    NDM_DEBUG("connect_to_ether_filter_manager: wait connection\n");
    while (soft_filters_connection == NULL) {
        messages_wait_and_handle_next();
    }

    // providing buffers for sending soft_filters
    NDM_DEBUG("c_sf_mng: [%s] sharing memory\n", service_name);
    share_common_memory_with_filter_manager();

    NDM_DEBUG("c_sf_mng: [%s] sharing memory\n", service_name);

    sf_mac_lookup();

    printf("################################******\n");
    printf("For service [%s] MAC= %02hhx:%02hhx:%02hhx:%02hhx:%02hhx:%02hhx\n",
                        service_name,  mac.addr[0], mac.addr[1], mac.addr[2],
                        mac.addr[3], mac.addr[4], mac.addr[5]);

} // end function: connect_soft_filters_manager


// *****************************************************************
// * filter memory registration
// * One time process
// *****************************************************************
static errval_t send_filter_memory_cap(struct q_entry e)
{
    struct net_soft_filters_binding *b =
      (struct net_soft_filters_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_memory_request(b,
                                                         MKCONT
                                                         (cont_queue_callback,
                                                          ccnc->q), e.cap);
    } else {
        NDM_DEBUG("send_filter_memory_cap: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}


/**
* \brief: Called by driver when memory is registered with driver.
*/
static void register_filter_memory_response(
                        struct net_soft_filters_binding *st,
                        errval_t err)
{
    assert(err_is_ok(err));
    soft_filters_ready = true;
    NDM_DEBUG("########################################################\n");
    NDM_DEBUG("memory registration successful.\n");
}


/**
 * \brief sends cap to the memory which is to be shared between filter_manager
 *   of network driver and netd.
 *
 */
static void register_filter_memory(struct capref cap)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filter_memory_cap;
    struct net_soft_filters_binding *b = soft_filters_connection;

    entry.binding_ptr = (void *) b;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.cap = cap;
    enqueue_cont_q(ccnc->q, &entry);

    NDM_DEBUG("register_filter_memory: terminated\n");
}


/**
* \brief: share the memory so that filter passing can be started.
*/
static void share_common_memory_with_filter_manager(void)
{
    errval_t err;
    struct capref frame;
    size_t size = BASE_PAGE_SIZE * 2;
    size_t total_size = size * 7;

    NDM_DEBUG("SCMWFM: started\n");

    NDM_DEBUG("SCMWFM: allocating %lu bytes of memory.\n", size);

#ifdef __scc__
    err = bulk_create(total_size, size, &frame, &bt_filter_tx, true);
#else
    err = bulk_create(total_size, size, &frame, &bt_filter_tx, false);
#endif

    assert(err_is_ok(err));

    NDM_DEBUG("SCMWFM: registering netd filter memory\n");
    register_filter_memory(frame);
    NDM_DEBUG("SCMWFM: terminated\n");

    // waiting for connection to succeed.
    NDM_DEBUG("connect_to_ether_filter_manager: wait connection\n");
    while (!soft_filters_ready) {
        messages_wait_and_handle_next();
    }

    filter_mem_lock = false; // marking memory as ready to use
} // end function: share_common_memory_with_filter_manager


static errval_t send_mac_address_request(struct q_entry e)
{
    struct net_soft_filters_binding *b =
      (struct net_soft_filters_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.mac_address_request(b,
                MKCONT(cont_queue_callback, ccnc->q));
    } else {
        NDM_DEBUG("send_mac_address_request: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
} // end function: send_mac_address_request

// lookup the mac address
static void sf_mac_lookup(void)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_mac_address_request;
    struct net_soft_filters_binding *b = soft_filters_connection;

    entry.binding_ptr = (void *) b;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    enqueue_cont_q(ccnc->q, &entry);

    // waiting for mac address response.
    NDM_DEBUG("connect_to_ether_filter_manager: wait connection\n");
    while (!valid_mac_addr_assigned) {
        messages_wait_and_handle_next();
    }


    NDM_DEBUG("sf_mac_lookup: terminated\n");
} // end function: sf_mac_lookup

// *****************************************************************
// * filter memory registration
// * One time process
// *****************************************************************

static void deregister_filter_response(struct net_soft_filters_binding *st,
                                       errval_t err, uint64_t filter_id)
{
    if (err_is_ok(err)) {
        NDM_DEBUG("filter at id %" PRIu64 " deregistered.\n", filter_id);
    }
}

static void register_filter_response(struct net_soft_filters_binding *st,
                                     uint64_t id, errval_t err,
                                     uint64_t filter_id, uint64_t buffer_id_rx,
                                     uint64_t buffer_id_tx, uint64_t ftype)
{
    assert(err_is_ok(err));
    NDM_DEBUG("filter at id [%" PRIu64 "] type[%" PRIu64
               "] registered with filt_id %" PRIu64 ".\n", id, ftype,
               filter_id);

    /* free the memory in shared area */
    errval_t free_err = bulk_free(&bt_filter_tx, id);

    assert(err_is_ok(free_err));
    filter_mem_lock = false; // NOTE: filter memory can be used by others now

    handle_filter_response(id, err, filter_id, buffer_id_rx, buffer_id_tx,
            ftype);

}

static void register_arp_filter_response(struct net_soft_filters_binding *st,
                                         uint64_t id, errval_t err)
{

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "register_arp_filter_response failed for ID %" PRIu64 "",
                  id);
        abort();
    }
    NDM_DEBUG("register_arp_filter_response: ARP filter ID %" PRIu64
               " registered\n", id);

    assert(!"NYI register_arp_filter_response");
}

// Support code to convert mac address from uint64_t into eth_addr type
union mac_addr_un1 {
    struct eth_addr ethaddr;
    uint64_t mac_addr;
};

static struct eth_addr my_convert_uint64_to_eth_addr(uint64_t given_mac)
{
    union mac_addr_un1 tmp_mac;
    tmp_mac.mac_addr = given_mac;

    // FIXME: make sure that this works irrespective of endianness of a machine
    return tmp_mac.ethaddr;
}


static void sf_mac_address_response(struct net_soft_filters_binding *st,
                      errval_t err,  uint64_t mac_addr)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sf_mac_address_response failed\n");
        abort();
    }
    assert(mac_addr != 0);

    NDM_DEBUG("sf_mac_address_response: reported MAC addr %" PRIu64 "\n",
            mac_addr);

    mac = my_convert_uint64_to_eth_addr(mac_addr);
    valid_mac_addr_assigned = true;
}

/*********  Functionality for filter registration *********/

static errval_t send_filter_for_registration(struct q_entry e)
{
    struct net_soft_filters_binding *b =
      (struct net_soft_filters_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_filter_request(b,
                                                  MKCONT(cont_queue_callback,
                                                         ccnc->q), e.plist[0],
                                                  e.plist[1], e.plist[2],
                                                  e.plist[3], e.plist[4],
                                                  e.plist[5], e.plist[6]);
     // id, len_rx,  len_tx,  buf_id_rx, buf_id_rx, ftype, paused

    } else {
        NDM_DEBUG("send_filter_for_registration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
} // end function:

/**
 * \brief sends the filter for registration to network driver.
 *
 */
static void send_soft_filter(uint64_t id, uint64_t len_rx,
                                uint64_t len_tx, uint64_t buffer_id_rx,
                                uint64_t buffer_id_tx, uint8_t ftype,
                                uint8_t paused)
{
    NDM_DEBUG("send_soft_filter: called for id %" PRIu64
               " and type %x, paused = %d\n", id, ftype, paused);

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filter_for_registration;

    struct net_soft_filters_binding *b = soft_filters_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = id;
    entry.plist[1] = len_rx;
    entry.plist[2] = len_tx;
    entry.plist[3] = buffer_id_rx;
    entry.plist[4] = buffer_id_tx;
    entry.plist[5] = ftype;
    entry.plist[6] = paused;
    /* e.plist[0], e.plist[1], e.plist[2], e.plist[3],     e.plist[4],     e.plist[4]);
       e.id,       e.len_rx,   e.len_tx,   e.buffer_id_rx, e.buffer_id_rx, ftype */

    enqueue_cont_q(ccnc->q, &entry);

    NDM_DEBUG("send_soft_filter: terminated for id %" PRIu64 "\n", id);
} // end function: send_soft_filter

static errval_t send_filterID_for_deregistration(struct q_entry e)
{
    struct net_soft_filters_binding *b =
      (struct net_soft_filters_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.deregister_filter_request(b,
                                                    MKCONT(cont_queue_callback,
                                                           ccnc->q),
                                                    e.plist[0]);
        /*  e.filterID */

    } else {
        NDM_DEBUG("send_filterID_for_deregistration: ID %" PRIu64
                   " Flounder busy,rtry++\n", e.plist[0]);
        return FLOUNDER_ERR_TX_BUSY;
    }
} // end function: send_filterID_for_deregistration

/**
 * \brief sends the filterID for de-registration to network driver.
 *
 */
static void unregister_soft_filter(uint64_t filter_id, qid_t qid)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_filterID_for_deregistration;

    struct net_soft_filters_binding *b = soft_filters_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = filter_id;
    /*    e.plist[0]
       e.filter_id */

    enqueue_cont_q(ccnc->q, &entry);;
} // end function: unregister_soft_filter


static errval_t send_arp_filter_for_registration(struct q_entry e)
{
    struct net_soft_filters_binding *b =
      (struct net_soft_filters_binding *) e.binding_ptr;
    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.register_arp_filter_request(b,
                                                      MKCONT
                                                      (cont_queue_callback,
                                                       ccnc->q), e.plist[0],
                                                      e.plist[1], e.plist[2]);
        /*  id,         e.len_rx,   e.len_tx */

    } else {
        NDM_DEBUG("send_arp_filter_for_registration: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

/**
 * \brief sends the filter for registration to network driver.
 *
 */
static void register_arp_soft_filter(uint64_t id, uint64_t len_rx,
                                    uint64_t len_tx)
{
    NDM_DEBUG("register_arp_soft_filter: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_arp_filter_for_registration;

    struct net_soft_filters_binding *b = soft_filters_connection;

    entry.binding_ptr = (void *) b;

    struct client_closure_ND *ccnc = (struct client_closure_ND *) b->st;

    entry.plist[0] = id;
    entry.plist[1] = len_rx;
    entry.plist[2] = len_tx;
    /*    e.plist[0], e.plist[1], e.plist[2]
       id,         e.len_rx,   e.len_tx   */

    enqueue_cont_q(ccnc->q, &entry);

    NDM_DEBUG("register_arp_soft_filter: terminated\n");
} // end function: register_arp_soft_filter



static uint64_t populate_rx_tx_filter_mem(uint16_t port, net_ports_port_type_t type,
                                          int32_t * len_rx, int32_t * len_tx)
{

    struct bulk_buf *bb;

    /* get memory chunk from shared memory */
    do {
        bb = bulk_alloc(&bt_filter_tx);
        if (bb == NULL) {
            // dispatch one
            NDM_DEBUG("bulk_alloc is returning NULL!!!! for filter memory\n");
            event_dispatch(get_default_waitset());
        }
    } while (bb == NULL);
    void *bbuf = bulk_buf_get_mem(bb);


    uint8_t *filter_mem = NULL;
    char *filter;


    // rx filter
    if (type == net_ports_PORT_TCP) {
        filter = build_ether_dst_ipv4_tcp_filter(mac,
                                BFDMUX_IP_ADDR_ANY,
                                htonl(local_ip.addr),
                                PORT_ANY,
                                (port_t) port
                                );
    } else {
        filter = build_ether_dst_ipv4_udp_filter(mac,
                                BFDMUX_IP_ADDR_ANY,
                                htonl(local_ip.addr),
                                PORT_ANY,
                                (port_t) port
                                );
    }
    /* FIXME: shouldn't be above two ports be wrapped in htons(port)? */
//    printf("##### The created filter is [%s]\n", filter);
    compile_filter(filter, &filter_mem, len_rx);
    assert(*len_rx < BASE_PAGE_SIZE);

    assert(filter_mem != NULL);
    memcpy(bbuf, filter_mem, *len_rx);
    free(filter);
    free(filter_mem);

    // tx filter
    if (type == net_ports_PORT_TCP) {
        filter = build_ether_src_ipv4_tcp_filter(mac, htonl(local_ip.addr),
                                                 BFDMUX_IP_ADDR_ANY,
                                                 (port_t) port, PORT_ANY);
    } else {
        filter = build_ether_src_ipv4_udp_filter(mac, htonl(local_ip.addr),
                                                 BFDMUX_IP_ADDR_ANY,
                                                 (port_t) port, PORT_ANY);
    }
    compile_filter(filter, &filter_mem, len_tx);
    assert(*len_tx < BASE_PAGE_SIZE);

    void *bbuf_tx = bbuf + BASE_PAGE_SIZE;

    memcpy(bbuf_tx, filter_mem, *len_tx);

    free(filter);
    free(filter_mem);
    uint64_t id = bulk_prepare_send(bb);

    return id;
}



