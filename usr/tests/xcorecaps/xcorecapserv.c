/**
 * Tests cross core cap management
 *
 *
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <barrelfish/barrelfish.h>
#include <stdio.h>
#include <barrelfish/nameservice_client.h>
#include <if/xcorecap_defs.h>
#include "xcorecap.h"
/* --- Binding handlers --- */
static void send_cap(struct xcorecap_binding *b, struct capref ram_cap);
static void retype_cap(struct xcorecap_binding *b);
static void delete_cap(struct xcorecap_binding *b);
static void revoke_cap(struct xcorecap_binding *b);

static struct xcorecap_rx_vtbl rx_vtbl = {
    .send_cap = send_cap,
    .retype_cap = retype_cap,
    .delete_cap = delete_cap,
    .revoke_cap = revoke_cap,
};


/* --- Globals used by message handlers --- */
static struct capref sent_cap;


/* --- Message handlers --- */
static void send_cap(struct xcorecap_binding *b, struct capref ram_cap)
{
    sent_cap = ram_cap;
    
    printf("xcorecapserv got cap\n");
    fflush(stdout);
    
    b->tx_vtbl.send_done(b, NOP_CONT);
}

static void retype_cap(struct xcorecap_binding *b)
{
    errval_t err;
    struct capref vnode_cap;
    
    // retype the ram vnode (should fail if retyped on another core)
    err = slot_alloc(&vnode_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecapserv: Vnode slot alloc failed\n"); 
    }
    err = cap_retype(vnode_cap, sent_cap, ObjType_VNode_x86_64_ptable,
                     ALLOC_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecapserv: Retype to vnode failed\n");    
    } 

    printf("xcorecapserv retyped cap\n");
    fflush(stdout);

    b->tx_vtbl.send_done(b, NOP_CONT);
}

static void delete_cap(struct xcorecap_binding *b)
{
    errval_t err;
    err = cap_delete(sent_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecapserv: delete failed\n");    
    } 

    printf("xcorecapserv deleted cap\n");
    fflush(stdout);

    b->tx_vtbl.send_done(b, NOP_CONT);
}


static void revoke_cap(struct xcorecap_binding *b)
{
    errval_t err;
    printf("xcorecapserv do revoke cap\n");
    err = cap_revoke(sent_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecapserv: revoke failed\n");    
    } 

    printf("xcorecapserv revoked cap\n");
    fflush(stdout);

    b->tx_vtbl.send_done(b, NOP_CONT);
}

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    printf("xcorecapserv: service exported at iref %"PRIuIREF"\n", iref);
    fflush(stdout);
    // register this iref with the name service
    err = nameservice_register(my_service_name, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }
}

static errval_t connect_cb(void *st, struct xcorecap_binding *b)
{
    printf("xcorecapserv service got a connection!\n");
    fflush(stdout);

    // copy my message receive handler vtable to the binding
    b->rx_vtbl = rx_vtbl;

    // accept the connection (we could return an error to refuse it)
    return SYS_ERR_OK;
}


/* --- Setup functions --- */
static void init(void) 
{
    errval_t err;

    err = xcorecap_export(NULL, export_cb, connect_cb, get_default_waitset(),
                          IDC_EXPORT_FLAGS_DEFAULT);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecapserv: Error exporting\n");
        abort();    
    } 
}


int main (int argc, char* argv[]) 
{
    errval_t err;

    init();

    struct waitset *ws = get_default_waitset();
    while (1) {
        err = event_dispatch(ws);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "in event_dispatch");
            break;
        }
    }

    return EXIT_FAILURE;

}
