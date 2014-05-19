/**
 * \file
 * \brief Communication between LWIP and net_ports deamon
 *
 *  This code provides interface to commuincate with net_ports for purposes like
 *  opening/closing ports, get IP address, etc
 */

/*
 * Copyright (c) 2007-11 ETH Zurich
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <net_interfaces/net_interfaces.h>
#include <trace/trace.h>
#include <netif/etharp.h>
#include <netif/bfeth.h>
#include "lwip/init.h"
#include <contmng/contmng.h>
#include <contmng/netbench.h>
#include <if/net_ports_defs.h>
#include <if/net_ports_rpcclient_defs.h>
#include <stdio.h>
#include <assert.h>
#include "lwip/barrelfish.h"
#include "idc_barrelfish.h"

#include "lwip_barrelfish_debug.h"

// Can be used
#define DISABLE_PORTMNG 1


/*
 * If we are the owner of lwip stack, then we dont need rpc
 */
static bool is_owner = 0;
static uint16_t(*alloc_tcp_port) (void) = NULL;
static uint16_t(*alloc_udp_port) (void) = NULL;

static uint16_t(*bind_port) (uint16_t port, net_ports_port_type_t type) = NULL;
static void (*close_port) (uint16_t port, net_ports_port_type_t type) = NULL;

/*************************************************************
 * \defGroup LocalStates Local states
 *
 * @{
 *
 ****************************************************************/
static struct net_ports_rpc_client net_ports_rpc;
static bool net_ports_service_connected = false;

static net_ports_appid_t appid_delete = 0;

static struct netif netif;

//static struct thread *trace_thread = NULL;
void thread_debug_regs(struct thread *t);

// Variables shared with idc_barrelfish.c
extern struct waitset *lwip_waitset;
extern uint64_t lwip_queue_id;

/**
 * \brief handle msgs on the tx, rx and then the rest connections in that priority
 */
void network_polling_loop(void)
{
    errval_t err;

    while (1) {
        err = event_dispatch(lwip_waitset);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
#if 0
        if (trace_thread != NULL) {
            static int iter = 0;

            iter++;
            if (iter % 10 == 0) {
                thread_debug_regs(trace_thread);
            }
        }
#endif
    }
}

errval_t lwip_err_to_errval(err_t e)
{
    switch (e) {
        case ERR_OK:
            return SYS_ERR_OK;
        case ERR_MEM:
            return LWIP_ERR_MEM;
        case ERR_BUF:
            return LWIP_ERR_BUF;
        case ERR_TIMEOUT:
            return LWIP_ERR_TIMEOUT;
        case ERR_RTE:
            return LWIP_ERR_RTE;
        case ERR_ABRT:
            return LWIP_ERR_ABRT;
        case ERR_RST:
            return LWIP_ERR_RST;
        case ERR_CLSD:
            return LWIP_ERR_CLSD;
        case ERR_CONN:
            return LWIP_ERR_CONN;
        case ERR_VAL:
            return LWIP_ERR_VAL;
        case ERR_ARG:
            return LWIP_ERR_ARG;
        case ERR_USE:
            return LWIP_ERR_USE;
        case ERR_IF:
            return LWIP_ERR_IF;
        case ERR_ISCONN:
            return LWIP_ERR_ISCONN;
        case ERR_INPROGRESS:
            return LWIP_ERR_INPROGRESS;
        default:
            USER_PANIC("unknown LWIP error in lwip_err_to_errval");
    }
}

/***************************************************************
    Adding new code to communicate with net_ports server
*/

/****************************************************************
 * \defGroup net_ports_connectivity  Code to connect and work with net_ports.
 *
 * @{
 *
 *****************************************************************/
/**
 * \brief Callback function when bind is successful.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void net_ports_bind_cb(void *st, errval_t err, struct net_ports_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for net_ports");
        abort();
    }
    LWIPBF_DEBUG("net_ports_bind_cb: called\n");

    err = net_ports_rpc_client_init(&net_ports_rpc, b);
    if (!err_is_ok(err)) {
        printf("net_ports_bind_cb failed in init\n");
        abort();
    }

    net_ports_service_connected = true;
    LWIPBF_DEBUG("net_ports_bind_cb: net_ports bind successful!\n");
}

/**
 * \brief Connects the lwip instance with net_ports daemon.
 *  Code inspired (ie. copied) from "start_client" function.
 */
static void init_net_ports_connection(char *service_name)
{
    LWIPBF_DEBUG("init_net_ports_connection: called\n");
    assert(service_name != NULL);
    LWIPBF_DEBUG("init_net_ports_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    LWIPBF_DEBUG("init_net_ports_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the net_ports driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("init_net_ports_connection: connecting\n");

    err = net_ports_bind(iref, net_ports_bind_cb, NULL, lwip_waitset,
                    IDC_BIND_FLAGS_DEFAULT);
    if (!err_is_ok(err)) {
        printf("net_ports_bind_cb failed in init\n");
        abort();
    }

    LWIPBF_DEBUG("init_net_ports_connection: terminated\n");
}


// Connects to the port manager service
// Blocking call: returns only when connection is done
// In case of error, it will panic!!
void idc_connect_port_manager_service(char *service_name)
{
    //LWIPBF_DEBUG
    printf("idc_c_port_mng_srv: trying to [%s]\n", service_name);

    /* FIXME: decide if this is the best place to connect with net_ports */
    init_net_ports_connection(service_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!net_ports_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding");
        }
    }
    //LWIPBF_DEBUG
    printf("idc_c_port_mng_srv: success [%s]\n", service_name);
}


void idc_get_ip(void)
{
    if (is_owner) {
        assert(!"owner of lwip should never ask for ip through API\n");
        abort();
    }
    LWIPBF_DEBUG("On the way of getting IP\n");

    errval_t err;
    struct ip_addr ip, gw, nm;

    err = net_ports_rpc.vtbl.get_ip_info(&net_ports_rpc, &ip.addr, &gw.addr,
            &nm.addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("got answer, now setting up things\n");
    netif_add(&netif, &ip, &nm, &gw, NULL, bfeth_init, ethernet_input);
    netif_set_default(&netif);
    netif_set_up(&netif);

    LWIPBF_DEBUG("client1: owner has the IP address %d.%d.%d.%d\n",
                 ip4_addr1(&netif.ip_addr), ip4_addr2(&netif.ip_addr),
                 ip4_addr3(&netif.ip_addr), ip4_addr4(&netif.ip_addr));
}



/***********************************************************/
/************* Port management *******************/

static err_t idc_close_port(uint16_t port, int port_type)
{
    LWIPBF_DEBUG("idc_close_port: called\n");
    if (is_owner) {
        close_port((uint64_t) port, port_type);
        return ERR_OK;
    }

    LWIPBF_DEBUG("idc_close_port: called\n");

    errval_t err, msgerr;

    err = net_ports_rpc.vtbl.close_port(&net_ports_rpc, port_type, port,
                                  appid_delete, lwip_queue_id,
                                  &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_close_tcp_port: returning\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_close_udp_port(uint16_t port)
{
    return idc_close_port(port, net_ports_PORT_UDP);
}


err_t idc_close_tcp_port(uint16_t port)
{
    return idc_close_port(port, net_ports_PORT_TCP);
}

static err_t idc_bind_port(uint16_t port, net_ports_port_type_t port_type)
{
    if (is_owner) {
        LWIPBF_DEBUG("idc_bind_port: called by owner\n");
        return bind_port(port, port_type);
    }

    LWIPBF_DEBUG("idc_bind_port: called\n");

    errval_t err, msgerr;

    /* getting the proper buffer id's here */
    err = net_ports_rpc.vtbl.bind_port(&net_ports_rpc, port_type, port,
                                  /* buffer for RX */
                                   get_rx_bufferid(),
                                  /* buffer for TX */
                                   get_tx_bufferid(),
                                  appid_delete, lwip_queue_id,
                                  &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    LWIPBF_DEBUG("idc_new_tcp_port: terminated\n");

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    }                           // FIXME: other errors?
    return ERR_OK;
}


err_t idc_bind_udp_port(uint16_t port)
{
    return idc_bind_port(port, net_ports_PORT_UDP);
}


err_t idc_bind_tcp_port(uint16_t port)
{
    return idc_bind_port(port, net_ports_PORT_TCP);
}

static err_t idc_new_port(uint16_t * port_no, net_ports_port_type_t port_type)
{
    /* NOTE: function with same name exists in Kaver's code for reference
       purpose */
    errval_t err, msgerr;

    //printf
    LWIPBF_DEBUG
        ("idc_new_port: ################################### called\n");

    // antoinek: FIXME: Need to figure out how to deal with this
    //assert(!"NYI");

    /* getting the proper buffer id's here */
    err = net_ports_rpc.vtbl.get_port(&net_ports_rpc, port_type,
                                 /* buffer for RX */
                                 get_rx_bufferid(),
                                 /* buffer for TX */
                                 get_tx_bufferid(),
                                 appid_delete, lwip_queue_id,
                                 &msgerr, port_no);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending get_ip_info");
    }

    //printf
    LWIPBF_DEBUG
        ("idc_new_port: ################################### terminated\n");
    return msgerr;
}

err_t idc_tcp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_tcp_port();
        return SYS_ERR_OK;
    }

    return idc_new_port(port_no, net_ports_PORT_TCP);
}


err_t idc_udp_new_port(uint16_t * port_no)
{
    if (is_owner) {
        *port_no = alloc_udp_port();
        return SYS_ERR_OK;

    }

    return idc_new_port(port_no, net_ports_PORT_UDP);
}


static err_t idc_redirect(struct ip_addr *local_ip, u16_t local_port,
                          struct ip_addr *remote_ip, u16_t remote_port,
                          net_ports_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t msgerr;
//    errval_t err;

    USER_PANIC("Pause: NYI");
    abort();

#if 0
    /* getting the proper buffer id's here */
    err =
      net_ports_rpc.vtbl.redirect(&net_ports_rpc, port_type, local_ip->addr, local_port,
                             remote_ip->addr, remote_port,
                             /* buffer for RX */
                             get_rx_bufferid(),
                             /* buffer for TX */
                             get_tx_bufferid(),
                             &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending redirect");
    }
#endif // 0

    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}

static err_t idc_pause(struct ip_addr *local_ip, u16_t local_port,
                       struct ip_addr *remote_ip, u16_t remote_port,
                       net_ports_port_type_t port_type)
{
    if (is_owner) {
        // redirecting doesn't make sense if we are the owner
        return ERR_USE;         // TODO: correct error
    }

    errval_t msgerr;
//    errval_t err;

    USER_PANIC("Pausing not support anymore");
    abort();
#if 0
    /* getting the proper buffer id's here */
    err =
      net_ports_rpc.vtbl.redirect_pause(&net_ports_rpc, port_type, local_ip->addr,
                                   local_port, remote_ip->addr, remote_port,
                                   /* buffer for RX */
                                   ((struct client_closure_NC *)
                                    driver_connection[RECEIVE_CONNECTION]->st)->
                                   buff_ptr->buffer_id,
                                   /* buffer for TX */
                                   ((struct client_closure_NC *)
                                    driver_connection[TRANSMIT_CONNECTION]->
                                    st)->buff_ptr->buffer_id, &msgerr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error sending pause");
    }

#endif // 0
    if (msgerr == PORT_ERR_IN_USE) {
        return ERR_USE;
    } else if (msgerr == PORT_ERR_REDIRECT) {
        return ERR_USE;         // TODO: correct error
    }
// FIXME: other errors?
    return ERR_OK;
}


/*
err_t idc_redirect_udp_port(uint16_t port)
{
    return idc_redirect_port(port, net_ports_PORT_UDP);
}
*/

err_t idc_redirect_tcp(struct ip_addr * local_ip, u16_t local_port,
                       struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_redirect(local_ip, local_port, remote_ip, remote_port,
                        net_ports_PORT_TCP);
}

err_t idc_pause_tcp(struct ip_addr * local_ip, u16_t local_port,
                    struct ip_addr * remote_ip, u16_t remote_port)
{
    return idc_pause(local_ip, local_port, remote_ip, remote_port,
                     net_ports_PORT_TCP);
}


void perform_ownership_housekeeping(uint16_t(*alloc_tcp_ptr) (void),
                                    uint16_t(*alloc_udp_ptr) (void),
                                    uint16_t(*bind_port_ptr) (uint16_t,
                                              enum net_ports_port_type_t),
                                    void (*close_port_ptr) (uint16_t,
                                              enum net_ports_port_type_t))
{
    is_owner = true;
    alloc_tcp_port = alloc_tcp_ptr;
    alloc_udp_port = alloc_udp_ptr;
    bind_port = bind_port_ptr;
    close_port = close_port_ptr;
}

