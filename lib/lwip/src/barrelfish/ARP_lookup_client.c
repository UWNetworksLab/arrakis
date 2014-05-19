/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>

// to connect with service
#include <barrelfish/nameservice_client.h>
#include <barrelfish/net_constants.h>

#include <if/net_ARP_defs.h>
#include <if/net_ARP_rpcclient_defs.h>

// standard include files
#include <stdio.h>

// for netif struct
#include <netif/bfeth.h>
#include <netif/etharp.h>

// local include files
#include "idc_barrelfish.h"
#include "lwip_barrelfish_debug.h"

// *****************************************************************
//     local states
// *****************************************************************
static struct net_ARP_rpc_client net_ARP_rpc;
static bool net_ARP_service_connected = false;
static struct netif netif;

/****************************************************************
* Global states
*****************************************************************/
extern struct waitset *lwip_waitset;

// *****************************************************************
// Dealing with new connections
// *****************************************************************
static void net_ARP_bind_cb(void *st, errval_t err, struct net_ARP_binding *b)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed for net_ARP");
        abort();
    }
    LWIPBF_DEBUG("net_ARP_bind_cb: called\n");

    err = net_ARP_rpc_client_init(&net_ARP_rpc, b);
    if (!err_is_ok(err)) {
        printf("net_ARP_bind_cb failed in init\n");
        abort();
    }

    net_ARP_service_connected = true;
    LWIPBF_DEBUG("net_ARP_bind_cb: net_ARP bind successful!\n");
}

static void init_net_ARP_connection(char *service_name)
{
    LWIPBF_DEBUG("init_net_ARP_connection: called\n");
    assert(service_name != NULL);
    LWIPBF_DEBUG("init_net_ARP_connection: connecting to [%s]\n", service_name);

    errval_t err;
    iref_t iref;

    LWIPBF_DEBUG("init_net_ARP_connection: resolving driver %s\n", service_name);

    err = nameservice_blocking_lookup(service_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "lwip: could not connect to the net_ARP driver.\n"
                  "Terminating.\n");
        abort();
    }
    assert(iref != 0);

    LWIPBF_DEBUG("init_net_ARP_connection: connecting\n");

    err = net_ARP_bind(iref, net_ARP_bind_cb, NULL, lwip_waitset,
                    IDC_BIND_FLAGS_DEFAULT);
    if (!err_is_ok(err)) {
        printf("net_ARP_bind_cb failed in init\n");
        abort();
    }

    LWIPBF_DEBUG("init_net_ARP_connection: terminated\n");
}


// Connects to the port manager service
// Blocking call: returns only when connection is done
// In case of error, it will panic!!
void idc_connect_ARP_lookup_service(char *service_name)
{
    LWIPBF_DEBUG("idc_c_ARP_lookup_srv: trying to [%s]\n", service_name);

    /* FIXME: decide if this is the best place to connect with net_ARP */
    init_net_ARP_connection(service_name);

    // XXX: dispatch on default waitset until bound
    struct waitset *dws = get_default_waitset();

    while (!net_ARP_service_connected) {
        errval_t err = event_dispatch(dws);

        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "in event_dispatch while binding ARP_service");
        }
    }
    LWIPBF_DEBUG("idc_c_ARP_lookup_srv: success [%s]\n", service_name);
}


// ************************************************************************
//                 ARP lookup interface function
// ************************************************************************


void idc_get_ip_from_ARP_lookup(void)
{
    /*
    if (is_owner) {
        assert(!"owner of lwip should never ask for ip through API\n");
        abort();
    }
    */
    LWIPBF_DEBUG("On the way of getting IP via ARP lookup\n");

    errval_t err;
    errval_t remote_err;
    struct ip_addr ip, gw, nm;
    uint32_t iface = 0;

    err = net_ARP_rpc.vtbl.ip_info(&net_ARP_rpc, iface, &remote_err,
            &ip.addr, &gw.addr, &nm.addr);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in making ip_info call");
    }

    if (err_is_fail(remote_err)) {
        USER_PANIC_ERR(remote_err, "error in getting ip_info");
    }

    LWIPBF_DEBUG("got answer, now setting up things\n");
    netif_add(&netif, &ip, &nm, &gw, NULL, bfeth_init, ethernet_input);
    netif_set_default(&netif);
    netif_set_up(&netif);

    LWIPBF_DEBUG("client: owner has the IP address %d.%d.%d.%d\n",
                 ip4_addr1(&netif.ip_addr), ip4_addr2(&netif.ip_addr),
                 ip4_addr3(&netif.ip_addr), ip4_addr4(&netif.ip_addr));
}

uint64_t idc_ARP_lookup(uint32_t ip)
{
    /*
    if (is_owner) {
        assert(!"ARP server should never use this API for ARP lookup\n");
        abort();
    }
    */
    LWIPBF_DEBUG("idc_ARP_lookup: On the way of ARP lookup\n");

    errval_t err;
    errval_t remote_err;
    uint32_t iface = 0;
    bool force = false;
    uint64_t mac = 0;

    err = net_ARP_rpc.vtbl.ARP_lookup(&net_ARP_rpc, ip, iface, force,
            &remote_err, &mac);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in making ARP_lookup call");
    }

    if (err_is_fail(remote_err)) {
        USER_PANIC_ERR(remote_err, "error in ARP lookup process");
    }
    assert(mac != 0);

    LWIPBF_DEBUG("idc_ARP_lookup: got answer\n");
    return mac;
} // end function: idc_ARP_lookup


