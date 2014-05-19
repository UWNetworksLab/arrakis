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
#include <if/xcorecap_defs.h>
#include <barrelfish/nameservice_client.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include "xcorecap.h"

/* --- Binding handlers --- */
static void state_machine(struct xcorecap_binding *b);

struct xcorecap_rx_vtbl rx_vtbl = {
    .send_done = state_machine,
};

/* --- Client functions --- */
static void create_ram_cap(struct capref * ram_cap) {
    errval_t err;

    // allocate some ram
    err = ram_alloc(ram_cap, ALLOC_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecap: RAM alloc failed\n");    
    }
}

static errval_t retype_cap(struct capref * ram_cap, struct capref * frame_cap)
{
    errval_t err;

    // retype this to a frame
    err = slot_alloc(frame_cap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecap: Frame slot alloc failed\n"); 
    }

    err = cap_retype(*frame_cap, *ram_cap, ObjType_Frame, ALLOC_BITS);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "xcorecap: Retype to frame failed\n");    
    }

    return err;
}

/* --- Globals used by stack-ripped functions --- */
struct capref ram_cap, frame_cap;

/* --- Stack-ripped message handlers --- */
static void send_cap(struct xcorecap_binding *b)
{
    create_ram_cap(&ram_cap);

    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 1);

    /* send cap */
    errval_t err = b->tx_vtbl.send_cap(b, NOP_CONT, ram_cap);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "sendcap failed");
        abort();
    }
}

static void trigger_remote_retype(void *arg)
{
    struct xcorecap_binding *b = arg;
    /* Trigger remote retype */
     errval_t err = b->tx_vtbl.retype_cap(b, NOP_CONT);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            err = b->register_send(b, ws, MKCONT(trigger_remote_retype, b));
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
                abort();
            }
        } else {
            DEBUG_ERR(err, "trigger retype failed");
            abort();
        }
    }
}
    
static void trigger_remote_delete(void *arg)
{
    struct xcorecap_binding *b = arg;

    /* Trigger remote delete */
     errval_t err = b->tx_vtbl.delete_cap(b, NOP_CONT);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            err = b->register_send(b, ws, MKCONT(trigger_remote_delete,b));
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
                abort();
            }
        } else {
            DEBUG_ERR(err, "trigger retype failed");
            abort();
        }
    }
}

static void trigger_remote_revoke(void *arg)
{
    struct xcorecap_binding *b = arg;

    /* Trigger remote revoke */
     errval_t err = b->tx_vtbl.revoke_cap(b, NOP_CONT);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            struct waitset *ws = get_default_waitset();
            err = b->register_send(b, ws, MKCONT(trigger_remote_revoke, b));
            if (err_is_fail(err)) {
                // note that only one continuation may be registered at a time
                DEBUG_ERR(err, "register_send on binding failed!");
                abort();
            }
        } else {
            DEBUG_ERR(err, "trigger retype failed");
            abort();
        }
    }
}


static void state_machine(struct xcorecap_binding *b)
{
    static int state = 0;

    switch(state) {
    case 0:
        printf("xcorecap sending cap\n");
        send_cap(b);
        state++;
        break;

    case 1:
        printf("xcorecap performing local retype\n");
        errval_t err = retype_cap(&ram_cap, &frame_cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "retype failed");
            abort();
        }
        printf("xcorecap trigger remote cap retype\n");
        trigger_remote_retype(b);
        state++;
        break;
        
    case 2:
        printf("xcorecap trigger remote cap revoke\n");
        trigger_remote_revoke(b);
        state++;
        break;
        
    case 3:
        printf("xcorecap sending another cap\n");
        send_cap(b);
        state++;
        break;

    case 4:
        printf("xcorecap delete remote cap\n");
        trigger_remote_delete(b);
        state++;
        break;

    case 5:
        printf("xcorecap on core %i done\n", disp_get_core_id());
        exit(0);
        break;
       
    default:
        printf("got into incorrect state of %d\n", state);
        break;
    }
}

static void bind_cb(void *st, errval_t err, struct xcorecap_binding *b)
{
    printf ("xcorecap: connected to server...\n");
    b->rx_vtbl = rx_vtbl;
    
    // start test
    state_machine(b);
}

/* --- Setup functions --- */
static void init(void) {
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup(my_service_name, &iref);
    if (err_is_fail(err)) {
        fprintf(stderr, "xcorecap: could not connect to the xcorecap server.\n"
                        "Terminating.\n");
        abort();
    }

    err = xcorecap_bind(iref, bind_cb, NULL, get_default_waitset(), 
                        IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        abort();
    }
} 


int main (int argc, char* argv[]) 
{    
    errval_t err;

    printf("Starting xcorecap on core %i\n", disp_get_core_id());

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
