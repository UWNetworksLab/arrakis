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
#include <if/net_ARP_defs.h>

#include <barrelfish/waitset.h>

// for handling contiuations
#include <contmng/contmng.h>

// standard libraries
#include <stdio.h>
#include <string.h>

#include <lwip/init.h>
#include <netif/etharp.h>

// local includes
#include "netd.h"
#include "netd_debug.h"

// ***** Special cases ********
// local ip address (your own ip address) (valid request)
// broadcast ip address (invalid request)
// multicast IP address (invalid request)
//
//
//


// How can I get my own MAC address
// Where can I find the code which is looking up local MAC address?
//    struct eth_addr *srcaddr = (struct eth_addr *) netif->hwaddr;
//
//
//        return etharp_query(netif, ipaddr, q); // q is pbuf
//        lib/lwip/src/netif/etharp.c

//        find_entry
//        src/netif/etharp.c

/****************************************************************
* data-structures
*****************************************************************/
// app connection closure
struct ARP_user_cl {
    // This will be used to remember who all are waiting for response
    struct net_ARP_binding *cl; // binding
    struct cont_queue *q; // for continuation management
    struct ARP_user_cl *next; // for singly linked list
    bool died; // is the user still connected
};


/****************************************************************
* Local states
*****************************************************************/
// The name of exported service for ARP lookup (which we are implementing)
static char ARP_service_name[MAX_NET_SERVICE_NAME_LEN] = {0};

// is service exported? marks that initialization is done
static bool ARP_service_exported = false;

// singly linked list of apps connected with this service
struct ARP_user_cl *registerd_user_list = NULL;

/*****************************************************************
* Prototypes
*****************************************************************/

static void get_ip_info(struct net_ARP_binding *cc, uint32_t iface);
static void ARP_resolve_request(struct net_ARP_binding *cc,
            ipv4addr_t ip, uint32_t iface, bool force);

// service mappings
static struct net_ARP_rx_vtbl rx_ARP_vtbl = {
    .ip_info_call = get_ip_info,
    .ARP_lookup_call = ARP_resolve_request,
};


/*****************************************************************
*   Housekeeping functions prototypes
*****************************************************************/

static void wrapper_ip_info_response(struct net_ARP_binding *cc,
        errval_t err, ipv4addr_t ip, ipv4addr_t gw, ipv4addr_t nm);

static void wrapper_ARP_lookup_response(struct net_ARP_binding *cc,
        errval_t err, uint64_t mac);

/*****************************************************************
* Dealing with new connections
*****************************************************************/
static errval_t connect_ARP_cb(void *st, struct net_ARP_binding *b)
{
    errval_t err = SYS_ERR_OK;
    NETD_DEBUG("########### new application came in\n");

    // using the b->st to store session specific data (net_user)
    struct ARP_user_cl *new_app = malloc(sizeof(struct ARP_user_cl));

    if (new_app == NULL) {
        NETD_DEBUG("error: malloc failed...\n");
        err = PORT_ERR_NOT_ENOUGH_MEMORY;
        return err;
    }

    memset(new_app, 0, sizeof(struct ARP_user_cl));
    new_app->next = registerd_user_list;
    registerd_user_list = new_app;
    b->st = (void *) new_app;

    new_app->q = create_cont_q("ARP2APP");
    b->rx_vtbl = rx_ARP_vtbl;
    return err;
} // end function: connect_ARP_cb


/*****************************************************************
* exporting service
*****************************************************************/

static void export_ARP_cb(void *st, errval_t err, iref_t iref)
{

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "service[%s] export failed", ARP_service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }

    NETD_DEBUG("service [%s] exported at iref %u\n", ARP_service_name, iref);

    // register this iref with the name service
    err = nameservice_register(ARP_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed for [%s]", ARP_service_name);
        abort(); // FIXME: Do I need abort after DEBUG_ERR?
    }
    ARP_service_exported = true;
    NETD_DEBUG("service [%s] export successful!\n", ARP_service_name);
} // end function: export_ARP_cb



// Initialzes the ARP lookup service
int init_ARP_lookup_service(char *dev_name)
{
    errval_t err = SYS_ERR_OK; // default return value

    // sanity check on parameter
    assert(dev_name != NULL);

    // start the port management service
    snprintf(ARP_service_name, sizeof(ARP_service_name), "%s%s", dev_name,
             NET_ARP_LOOKUP_SUFFIX);

    NETD_DEBUG("init_ARP_lookup_service called [%s]\n", ARP_service_name);

   // exporting net_ports interface
    err = net_ARP_export(NULL, export_ARP_cb, connect_ARP_cb,
            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        USER_PANIC("net_ARP_export failed!");
        return err;
    }

    // wait till ports export is actually done
    struct waitset *ws = get_default_waitset();
    while (!ARP_service_exported) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch for init_ARP_service");
            return err;
        }
    } // end while:

    return err;
} // end function: init_ARP_service


// ************************************************************************
//                 ARP lookup interface function
// ************************************************************************

static void get_ip_info(struct net_ARP_binding *cc, uint32_t iface)
{
    printf("####### get IP info called ######\n");
    NETD_DEBUG("get_ip_info: client asking for ip over %"PRIu32"\n", iface);

    wrapper_ip_info_response(cc, SYS_ERR_OK, netif_ptr->ip_addr.addr,
                                netif_ptr->gw.addr, netif_ptr->netmask.addr);
    NETD_DEBUG("get_ip_info: terminating\n");
}

static uint64_t refresh_cache(uint32_t dst_ip_addr)
{
    struct ip_addr dst_ip;
    struct netif *netif;
    dst_ip.addr = dst_ip_addr;
    netif = ip_route(&dst_ip);

    NETD_DEBUG("refresh_cache: calling etharp_request\n");
    errval_t r = etharp_request(netif, &dst_ip);
    assert(err_is_ok(r));

    struct waitset *ws = NULL;
    ws = get_default_waitset();
   while (is_ip_present_in_arp_cache(&dst_ip) == false) {
//        NETD_DEBUG("refresh_arp_cache: event dispatched\n");
        r = event_dispatch(ws);
        if (err_is_fail(r)) {
            DEBUG_ERR(r, "in event_dispatch");
            abort();
        }
   } // end while: till arp not present
   return find_ip_arp_cache(&dst_ip);
}

static void ARP_resolve_request(struct net_ARP_binding *cc,
            ipv4addr_t ip, uint32_t iface, bool force)
{
    NETD_DEBUG("ARP_resolve_request: client asking ARP lookup for ip %"
            PRIu32" over iface %"PRIu32"\n", ip, iface);

    uint64_t found_mac = refresh_cache(ip);
    assert(found_mac != 0);
//    assert(!"NYI ARP resolve request");
    NETD_DEBUG("ARP_resolve_request: MAC found for ARP request ip %"
            PRIu32" over iface %"PRIu32" == %"PRIu64"\n",
            ip, iface, found_mac);
    wrapper_ARP_lookup_response(cc, SYS_ERR_OK, found_mac);
} // end function: ARP_resolve_request


// ************************************************************************
//                housekeeping functions
// ************************************************************************

static errval_t send_ip_info(struct q_entry e)
{
    struct net_ARP_binding *b = (struct net_ARP_binding *) e.binding_ptr;
    struct ARP_user_cl *au = (struct ARP_user_cl *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.ip_info_response(b,
                    MKCONT(cont_queue_callback, au->q),
                    e.plist[0], e.plist[1], e.plist[2], e.plist[2]);
                 // e.err, e.ip,   e.gw,   e.mask
    } else {
        NETD_DEBUG("send_assign_ip: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_ip_info_response(struct net_ARP_binding *cc,
        errval_t err, ipv4addr_t ip, ipv4addr_t gw, ipv4addr_t nm)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_ip_info;
    entry.binding_ptr = (void *) cc;

    entry.plist[1] = err;
    entry.plist[1] = ip;
    entry.plist[2] = gw;
    entry.plist[3] = nm;
    // err,  e.ip,   e.gw,   e.mask

    struct ARP_user_cl *au = (struct ARP_user_cl *) cc->st;

    enqueue_cont_q(au->q, &entry);
}

static errval_t send_ARP_lookup_info(struct q_entry e)
{
    struct net_ARP_binding *b = (struct net_ARP_binding *) e.binding_ptr;
    struct ARP_user_cl *au = (struct ARP_user_cl *) b->st;

    if (b->can_send(b)) {
        return b->tx_vtbl.ARP_lookup_response(b,
                    MKCONT(cont_queue_callback, au->q),
                    e.plist[0], e.plist[1]);
                 // e.err, e.mac
    } else {
        NETD_DEBUG("send_assign_ip: Flounder busy,rtry++\n");
        return FLOUNDER_ERR_TX_BUSY;
    }
}

static void wrapper_ARP_lookup_response(struct net_ARP_binding *cc,
        errval_t err, uint64_t mac)
{
    struct q_entry entry;

    memset(&entry, 0, sizeof(struct q_entry));
    entry.handler = send_ARP_lookup_info;
    entry.binding_ptr = (void *) cc;

    entry.plist[1] = err;
    entry.plist[1] = mac;
    // err,  e.mac,

    struct ARP_user_cl *au = (struct ARP_user_cl *) cc->st;

    enqueue_cont_q(au->q, &entry);
}


