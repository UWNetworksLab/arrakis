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
#include <net_device_manager/net_ports_service.h>
#include <if/net_soft_filters_defs.h>
#include <if/net_ports_defs.h>

// for handling contiuations
#include <contmng/contmng.h>

// standard libraries
#include <stdio.h>
#include <string.h>

// For port management
#include "portalloc.h"

#include "port_management_support.h"
#include "device_manager_debug.h"


/****************************************************************
* Global datastructure
*****************************************************************/

/****************************************************************
* Local states
*****************************************************************/
// the device name for which we are managing the ports
static char my_dev_name[MAX_NET_SERVICE_NAME_LEN] = {0};

// is service exported? marks that initialization is done
static bool port_service_exported = false;

// singly linked list of apps connected with this service
struct net_user *registerd_app_list = NULL;

/*****************************************************************
// House-keeping prototypes
*****************************************************************/

static void wrapper_get_port_response(struct net_ports_binding *cc,
                errval_t err, uint16_t port_no);
static void wrapper_bind_port_response(struct net_ports_binding *cc,
        errval_t err);
static void wrapper_close_port_response(struct net_ports_binding *cc,
            errval_t err);

/*****************************************************************
* Prototypes
*****************************************************************/

static struct buffer_port_translation *find_filter_id(
            struct buffer_port_translation *port_list,
            uint16_t port_no, uint64_t type);



// gets any next available port number
// To be used on client side which does not care about the port number
// Will be called by calls like connect, sendto
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Find free port number
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void get_port(struct net_ports_binding *cc,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid);

// Allocates the specified port number to the application
// To be used on server side who wants to listen on perticular port number
// will be called by listen
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Make sure that requested port is available
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void bind_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid);


// Close the specified port number
// Algorithm
//   *. Make sure that requested port is open and belongs to requesting app
//   *. Find out the filter number associated with this port
//   *. Send request to remove the filter
//   *. Once you get ack for filter being removed successfully,
//          send back the response (done by another funcation)
static void close_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    uint64_t appid,
                    uint64_t queueid);


// Get the mac address for given machine
static void get_mac_address(struct net_ports_binding *cc);

// service mappings
static struct net_ports_rx_vtbl rx_net_ports_vtbl = {
//    .get_ip_info_call = get_ip_info,
    .get_mac_address_call = get_mac_address,
    .get_port_call = get_port,
    .bind_port_call = bind_port,
    .close_port_call = close_port,
};

/*****************************************************************
* Dealing with new connections
*****************************************************************/

static errval_t connect_ports_cb(void *st, struct net_ports_binding *b)
{
    errval_t err = SYS_ERR_OK;
    NDM_DEBUG("new application came in\n");

    // using the b->st to store session specific data (net_user)
    struct net_user *new_net_app = malloc(sizeof(struct net_user));

    if (new_net_app == NULL) {
        NDM_DEBUG("error: malloc failed...\n");
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        return err;
    }

    memset(new_net_app, 0, sizeof(struct net_user));
    new_net_app->next = registerd_app_list;
    registerd_app_list = new_net_app;
    b->st = (void *) new_net_app;

    new_net_app->q = create_cont_q("NDM2APP");
    b->rx_vtbl = rx_net_ports_vtbl;
    return err;
} // end function: connect_ports_cb

/*****************************************************************
* exporting service
*****************************************************************/

static void export_ports_cb(void *st, errval_t err, iref_t iref)
{
    char service_name[MAX_NET_SERVICE_NAME_LEN];

    snprintf(service_name, sizeof(service_name), "%s%s", my_dev_name,
             NET_PORTS_MNG_SUFFIX);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service[%s] export failed", service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }

    NDM_DEBUG("service [%s] exported at iref %u\n", service_name, iref);

    // register this iref with the name service
    err = nameservice_register(service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }
    port_service_exported = true;
    NDM_DEBUG("export successful!\n");
} // end function: export_ports_cb


// Initialzes the port number management service
int init_ports_service(char *dev_name)
{
    errval_t err = SYS_ERR_OK; // default return value

    // sanity check on parameter
    assert(dev_name != NULL);

    // initialize the filter management for each queue
    for (qid_t i = 0; i < total_queues; ++i) {
        // Connect with soft_filters_service
        qlist[i].filt_mng->init_filters(dev_name, i);
    } // end for: for each queue

    // FIXME: initialize the ports in portalloc
    init_free_ports();

    // start the port management service
    strncpy(my_dev_name, dev_name, sizeof(my_dev_name));

    NDM_DEBUG("init_ports_service called for device [%s]\n", my_dev_name);

   // exporting net_ports interface
    err = net_ports_export(NULL, export_ports_cb, connect_ports_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC("net_ports_export failed!");
        return err;
    }

    // wait till ports export is actually done
    struct waitset *ws = get_default_waitset();
    while (!port_service_exported) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch for init_port_service");
            return err;
        }
    } // end while:

    return err;
} // end function: init_ports_service

/*****************************************************************
* Actual service
*****************************************************************/

static errval_t res_port(struct net_ports_binding *cc,
                    uint16_t port,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid,
                    bool is_bind)
{
    NDM_DEBUG("res_port called\n");

    // sanity check
    assert(port > 0);

    errval_t err = SYS_ERR_OK; // default return value

    // locate hte application
    struct net_user *this_net_app = (struct net_user *) cc->st;

    // create connection flow structure
    struct buffer_port_translation *bp;
    bp = (struct buffer_port_translation *)
      malloc(sizeof(struct buffer_port_translation));
    if (bp == NULL) {
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        NDM_DEBUG("netd is out of memory.\n");
        /* send continuation msg about new port */
        return err;
    }

    // initialize it
    memset(bp, 0, sizeof(struct buffer_port_translation));

    // add information about this session to the list of live sessions
    bp->st = cc;
    bp->local_port = port;
    bp->type = type;
    bp->buffer_id_rx = buffer_id_rx;
    bp->buffer_id_tx = buffer_id_tx;
    bp->active = false;
    bp->bind = is_bind;
    bp->closing = false;
    bp->redirected = false;

    // FIXME: qlist[queueid].insert_rule();
    err = qlist[queueid].filt_mng->reg_filters(port, type, buffer_id_rx,
            buffer_id_tx, appid, queueid);
    if (err_is_fail(err)) {
        // close the port which was allocated
        free_port(port, type);
        // free up the allocated memory
        free(bp);
        return err;
    } // end if: err

    // add this flow to the list of all flows for this app
    bp->next = this_net_app->open_ports;
    this_net_app->open_ports = bp;

    NDM_DEBUG("res_port: waiting for response\n");
    return err;
} // end function: res_port



// gets any next available port number
// To be used on client side which does not care about the port number
// Will be called by calls like connect, sendto
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Find free port number
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void get_port(struct net_ports_binding *cc,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid)
{
    NDM_DEBUG("get_port called\n");

    errval_t err = SYS_ERR_OK;
    uint16_t port;

    /* FIXME: get free port from portalloc system */
    if (type == net_ports_PORT_TCP) {
        port = alloc_tcp_port();
    } else {
        port = alloc_udp_port();
    }

    // If could not allocate the port
    if (port == 0) {
        err = PORT_ERR_NO_MORE_PORT;
        NDM_DEBUG("all the ports for this user are allocated!\n");
        wrapper_get_port_response(cc, err, 0);
        return;
    }

    err = res_port(cc, port, type, buffer_id_rx, buffer_id_tx, appid, queueid,
            false);
    if (err_is_fail(err)) {
        wrapper_get_port_response(cc, err, 0);
        return;
    }

    NDM_DEBUG("get_port: waiting for response\n");
    return;
} // end function: get_port


// Allocates the specified port number to the application
// To be used on server side who wants to listen on perticular port number
// will be called by listen
// FIXME: eventually buffer_ids should be removed and only appid should be used
// Algorithm
//   *. Make sure that requested port is available
//   *. Create appropriate filter
//   *. Insert appropriate filter
//   *. Once you get ack for filter being inserted successfully,
//          send back the response (done by another funcation)
static void bind_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t queueid)
{
    // FIXME: too much of code repetation in get_port and bind_port. FIX it.
    NDM_DEBUG("bind_port called\n");
    errval_t err = SYS_ERR_OK;
    uint16_t port;

    NDM_DEBUG("bind_port: called for port %" PRIu16 " with RX[%" PRIu64
               "] and TX[%" PRIu64 "]\n", port_no, buffer_id_rx, buffer_id_tx);

    port = (uint64_t) alloc_specific_port((uint16_t) port_no, type);

    if (port == 0) {
        err = PORT_ERR_IN_USE;
        NDM_DEBUG("Requested port is in use!\n");
        wrapper_bind_port_response(cc, err);
        return;
    }

    err = res_port(cc, port, type, buffer_id_rx, buffer_id_tx, appid, queueid,
            true);
    if (err_is_fail(err)) {
        wrapper_bind_port_response(cc, err);
        return;
    }
    NDM_DEBUG("bind_port: exiting\n");

    return;
} // end function: bind_port



// Close the specified port number
// Algorithm
//   *. Make sure that requested port is open and belongs to requesting app
//   *. Find out the filter number associated with this port
//   *. Send request to remove the filter
//   *. Once you get ack for filter being removed successfully,
//          send back the response (done by another funcation)
static void close_port(struct net_ports_binding *cc,
                    port_type_t type,
                    uint16_t port_no,
                    uint64_t appid,
                    uint64_t queueid)
{
    NDM_DEBUG("close_port called\n");

    errval_t err = SYS_ERR_OK;

    NDM_DEBUG("close_port: called\n");
    struct buffer_port_translation *bp;
    struct net_user *this_net_app = (struct net_user *) cc->st;

    bp = find_filter_id(this_net_app->open_ports, port_no, type);

    if (bp != NULL) {
        qlist[queueid].filt_mng->unreg_filters(bp->filter_id,
                                               this_net_app->qid);
        // close the port
        free_port(port_no, type);
        // FIXME: Adjust the pointers in the linked list to safely free the bp
        // FIXME: Release the memory
        // free(bp);
    } else {
        NDM_DEBUG("close_port: port not found\n");
        err = PORT_ERR_NOT_FOUND;
        wrapper_close_port_response(cc, err);
    }

    return;
} // end function: close_port


// Get the mac address for given machine
static void get_mac_address(struct net_ports_binding *cc)
{
    NDM_DEBUG("get_mac_address called\n");
    assert(!"get_mac_address: NYI");
    return;
} // end function: get_mac_address


// Finds filter_id from the given ports list
static struct buffer_port_translation *find_filter_id(struct
                                                      buffer_port_translation
                                                      *port_list,
                                                      uint16_t port_no,
                                                      uint64_t type)
{
    while (port_list) {
        if (port_list->local_port == port_no && type == port_list->type) {
            port_list->closing = true;
            NDM_DEBUG("find_filter_id: found, and has id %" PRIu64 "\n",
                       port_list->filter_id);
            return (port_list);
        }
        port_list = port_list->next;
    } // end while
    return NULL;
} // end function: find_filter_id


/*****************************************************************
* housekeeping functions
* FIXME: move them in separate file
*****************************************************************/

/**
* \brief:
        Responsible for calling appropriate callback in app, reporting the
        status. It is some kind of IDC forwarding mechanism.
        (or a gateway for that matter)
*/
void handle_filter_response(uint64_t id, errval_t err, uint64_t filter_id,
        uint64_t buffer_id_rx, uint64_t buffer_id_tx, uint64_t ftype)
{
    // FIXME: This is very ugly way to lookup the the request to be processed.
    // We need to send the state (ie: bp) with call so that we don't have
    // to do this serial lookup.

    assert(err_is_ok(err));
    NDM_DEBUG("filter at id [%" PRIu64 "] type[%" PRIu64
               "] registered with filt_id %" PRIu64 ".\n", id, ftype,
               filter_id);

    /* Ensure that, after filter is successfully registered, callback
       will be called informing successful registration of the port using
       idc_new_port */

    // We don't know yet what exactly the filter is registered for.
    //   So, find it out.
    struct net_user *one_app = registerd_app_list;
    struct buffer_port_translation *bp;
    struct buffer_port_translation *prev;

    /* FIXME: modify the code to work without buffer_id_rx and buffer_id_tx.
     * Instead, use the id field to locate the request. */
    while (one_app) {
        bp = one_app->open_ports;
        prev = NULL;
        while (bp) {   // It is two dimensional linked list.

            if (bp->buffer_id_rx == buffer_id_rx &&
                bp->buffer_id_tx == bp->buffer_id_tx) {
                // this is the entry for which we got the response
                if (err == SYS_ERR_OK) {
                    // wanted case: filter registration successful
                    bp->active = true;
                    bp->filter_id = filter_id;
                } else if (err == ETHERSRV_ERR_BUFFER_NOT_FOUND) {
                    NDM_DEBUG("no buffer found on the driver\n");
                    err = FILTER_ERR_BUFF_NOT_FOUND;
                }

                // OK, we found the correct entry, now call the proper
                //   function to inform app about registration

                if (bp->paused) {
                    assert(!"NYI filter pause");
                    // idc_redirect_pause_response(bp->st, err);
                } else if (bp->redirected) {
                    assert(!"NYI filter redirect");
                    // idc_redirect_response(bp->st, err);
                } else if (bp->bind) {
                    wrapper_bind_port_response(bp->st, err);
                } else {
                    wrapper_get_port_response(bp->st, err, bp->local_port);
                }

                // cleaning up
                /* when err is not SYS_ERR_OK, one should release the bp
                   as it seems that filter_registration is failed,
                   and user should retry. */
                if (!bp->active) {
                    if (prev == NULL) {
                        one_app->open_ports = bp->next;
                    } else {
                        prev->next = bp->next;
                    }
                    // It is safe to release memory here,
                    //   as wrapper_*_port_response makes the copy of all the
                    //   info needed.
                    free(bp);
                } // end if: not active

                // skipping remaining list as we found the match,
                //   and triggered the callback.
                return;

            } // end if: match found
            prev = bp;
            bp = bp->next;
        } // end while : for each registration within app

        one_app = one_app->next;
    } // end while : for each app registered

    USER_PANIC("client buffer_id not found");

} // end function: handle_filter_response



/*****************************************************************
* housekeeping functions
* FIXME: move them in separate file
*****************************************************************/
#include <contmng/contmng.h>
// get_port response: helper functions
static errval_t send_new_port(struct q_entry e)
{
    struct net_ports_binding *b = (struct net_ports_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;
    assert(nu != NULL);

    if (b->can_send(b)) {
        return b->tx_vtbl.get_port_response(b,
                            MKCONT(cont_queue_callback, nu->q),
                            e.plist[0], e.plist[1]);
        /*  e.err, e.port_no */
    } else {
        NDM_DEBUG("send_new_port: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

static void wrapper_get_port_response(struct net_ports_binding *cc,
                errval_t err, uint16_t port_no)
{

    NDM_DEBUG("wrapper_get_port_response: called\n");
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_new_port;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;
    entry.plist[1] = port_no;

    /*      e.plist[0], e.plist[1]
       e.err,     e.port_no  */

    struct net_user *nu = (struct net_user *) cc->st;
    assert(nu != NULL);
    enqueue_cont_q(nu->q, &entry);

    NDM_DEBUG("wrapper_get_port_response: terminated\n");
} // end function: wrapper_get_port_response


// bind_port response: helper functions
static errval_t send_bound_port(struct q_entry e)
{
    struct net_ports_binding *b = (struct net_ports_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.bind_port_response(b,
                                             MKCONT(cont_queue_callback, nu->q),
                                             e.plist[0]);
        /* entry.err */
    } else {
        NDM_DEBUG("send_bound_port: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

}

static void wrapper_bind_port_response(struct net_ports_binding *cc,
        errval_t err)
{

    NDM_DEBUG("idc_bound_port: called\n");

    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_bound_port;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;

    /* plist[0]
     * entry.err
     */

    struct net_user *nu = (struct net_user *) cc->st;
    assert(nu != NULL);

    enqueue_cont_q(nu->q, &entry);

    NDM_DEBUG("idc_bound_port: terminated\n");
}


// close_port response: helper functions
static errval_t send_close_port_response(struct q_entry e)
{
    struct net_ports_binding *b = (struct net_ports_binding *) e.binding_ptr;
    struct net_user *nu = (struct net_user *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.close_port_response(b,
                                              MKCONT(cont_queue_callback,
                                                     nu->q), e.plist[0]);
        /*  e.err */
    } else {
        NDM_DEBUG("send_close_port_response: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }

} // end function: send_close_port_response

static void wrapper_close_port_response(struct net_ports_binding *cc,
            errval_t err)
{

    NDM_DEBUG("wrapper_close_port_response: called\n");
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_close_port_response;
    entry.binding_ptr = (void *) cc;

    entry.plist[0] = err;

    /*  e.plist[0]
       e.err    */

    struct net_user *nu = (struct net_user *) cc->st;

    enqueue_cont_q(nu->q, &entry);

    NDM_DEBUG("wrapper_close_port_response: terminated\n");
}


