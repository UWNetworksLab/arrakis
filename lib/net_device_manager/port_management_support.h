/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PORT_MNG_SUPPORT_H_
#define PORT_MNG_SUPPORT_H_

#include <barrelfish/barrelfish.h>
#include <if/net_ports_defs.h>

typedef net_ports_port_type_t port_type_t;
typedef net_ports_appid_t appid_t;
typedef net_ports_qid_t qid_t;
typedef net_ports_bufid_t bufid_t;

// *****************************************************************
// * function pointer types for filter management (HW/SW filters)
// *****************************************************************
typedef void (*init_filters_service_t)(char *dev_name, qid_t qid);
typedef void (*register_arp_filter_t)(uint64_t id, uint64_t len_rx,
                                    uint64_t len_tx);
typedef errval_t (*register_filter_t)(uint16_t port,
                    port_type_t type,
                    bufid_t buffer_id_rx,
                    bufid_t buffer_id_tx,
                    appid_t appid,
                    qid_t qid);
typedef void (*deregister_filter_t)(uint64_t filter_id, qid_t qid);


// Struct to capture the signature of different filter managers
struct filters_tx_vtbl {
    char *type;
    init_filters_service_t init_filters;
    register_arp_filter_t reg_arp_filters;
    register_filter_t reg_filters;
    deregister_filter_t unreg_filters;
};

// *****************************************************************
// * prototypes
// *****************************************************************

// Get the signature for software filter manager
struct filters_tx_vtbl *get_soft_filt_mng_sign(void);

// Get the signature for e10k hardware filter manager
struct filters_tx_vtbl *get_e10k_filt_mng_sign(void);

// Initialize the port number management service
int init_ports_service(char *dev_name);

// based on the response received from queue_manager,
// report the success/failure of the call to an application
void handle_filter_response(uint64_t id, errval_t err, uint64_t filter_id,
        uint64_t buffer_id_rx, uint64_t buffer_id_tx, uint64_t ftype);


// *****************************************************************
// * queue list data-structures
// *****************************************************************

// queue closure
struct NIC_q_closure {
    qid_t qid; // queue_id
    struct filters_tx_vtbl *filt_mng; // filter management functionality
};

struct NIC_q_closure *qlist;  // list of all queues
qid_t total_queues; // Total no. of valid queues (0, ..., total_queues-1)



// *****************************************************************
// * application closure/state related
// *****************************************************************

// The queue abstraction
// One application can hold one or more queues
// A queue has
//      a list of assosited buffers
//      ports/flows mapped to queue
//      list of functions to register filters




/**
 * This is the structure used to hold all of the ports allocated to a
 * networking application and their relavanet buffer. This also keeps the
 * state of the filter registration/deregistration sequence.
 */
struct buffer_port_translation {
    uint64_t buffer_id_rx;
    uint64_t buffer_id_tx;
    uint64_t filter_id;
    uint64_t type;
    uint16_t local_port;
    uint32_t local_ip;
    uint16_t remote_port;
    uint32_t remote_ip;
    bool redirected;
    bool active;
    bool bind;
    bool closing;
    bool paused;
    struct net_ports_binding *st;
    struct buffer_port_translation *next;
};


/**
 * Represents a network user application. This can easily be extended later.
 */
struct net_user {
    struct cont_queue *q;
    struct net_user *next;
    struct buffer_port_translation *open_ports;
    bool died;

    // Information about queue
    qid_t qid;

};

/* FIXME: check if you can remove any of following global variables. */
/**
 * Networking daemon state
 */
struct net_user *registerd_app_list;

/*
 * The IP assigned to this netd and its card interface
 */
//extern struct ip_addr local_ip;


#endif // PORT_MNG_SUPPORT_H_

