/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <if/monitor_mem_defs.h>
#include <if/monitor_mem_rpcclient_defs.h>

static uint8_t mem_core_id;
static struct monitor_mem_rpc_client monitor_mem_client;
static bool mem_setup_complete = false;
iref_t monitor_mem_iref = 0;

/**
 * \brief Request for some memory (over the memory allocation channel)
 */
static void mem_alloc_handler(struct monitor_mem_binding *b,
                              uint8_t size_bits, genpaddr_t minbase,
                              genpaddr_t maxlimit)
{
    struct capref cap;
    monitor_mem_caprep_t caprep = {0,0,0,0};
    errval_t err;
    errval_t reterr = SYS_ERR_OK;

    /* This should only run on the core with the mem_serv. Or else the system
       will deadlock. */
    assert(bsp_monitor);
    err = ram_alloc(&cap, size_bits);
    if (err_is_fail(err)) {
        reterr = err_push(err, LIB_ERR_RAM_ALLOC);
        goto out;
    }

    struct capability cap_raw;
    err = monitor_cap_identify(cap, &cap_raw);
    if (err_is_fail(err)) {
        reterr = err_push(err, MON_ERR_CAP_IDENTIFY);
        err = cap_destroy(cap);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "cap_destroy failed");
        }
        goto out;
    }
    assert(cap_raw.type == ObjType_RAM);
    intermon_caprep_t caprep2;
    capability_to_caprep(&cap_raw, &caprep2);
    // XXX: work around stupid flounder behaviour: these types are identical!
    STATIC_ASSERT_SIZEOF(caprep, sizeof(caprep2));
    memcpy(&caprep, &caprep2, sizeof(caprep));

out:
    // RPC protocol, this can never fail with TX_BUSY
    err = b->tx_vtbl.alloc_response(b, NOP_CONT, reterr, caprep);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "reply failed");
    }
}

static errval_t mon_ram_alloc(struct capref *ret, uint8_t size_bits,
                              uint64_t minbase, uint64_t maxlimit)
{
    errval_t err;
    intermon_caprep_t caprep;
    errval_t reterr;

    err = monitor_mem_client.vtbl.alloc(&monitor_mem_client, size_bits,
                                        minbase, maxlimit, &reterr,
                                        (monitor_mem_caprep_t *) &caprep);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_RAM_ALLOC_ERR);
    } else if (err_is_fail(reterr)) {
        return err_push(reterr, MON_ERR_RAM_ALLOC_RETERR);
    }

    struct capability cap_raw;
    caprep_to_capability(&caprep, &cap_raw);
    assert(ObjType_RAM == cap_raw.type);

    err = slot_alloc(ret);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_SLOT_ALLOC);
    }

    err = monitor_cap_create(*ret, &cap_raw, mem_core_id);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_CAP_CREATE);
    }

    return reterr;
}

static struct monitor_mem_rx_vtbl the_monitor_mem_vtable = {
    .alloc_call = mem_alloc_handler,
};

static errval_t monitor_mem_connected(void *st, struct monitor_mem_binding *b)
{
    b->rx_vtbl = the_monitor_mem_vtable;
    return SYS_ERR_OK;
}

static void monitor_mem_listening(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "err in monitor_mem_listening failed");
    }
    monitor_mem_iref = iref;
}

errval_t mon_ram_alloc_serve(void)
{
    errval_t err;

    err = monitor_mem_export(NULL, monitor_mem_listening, monitor_mem_connected,
                             get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err;
    }

    return SYS_ERR_OK;
}

static void bind_cont(void *st, errval_t err, struct monitor_mem_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "err in bind_cont failed");
    }

    // setup RPC client above binding
    err = monitor_mem_rpc_client_init(&monitor_mem_client, b);
    if(err_is_fail(err)) {
        DEBUG_ERR(err, "in monitor_mem_rpc_client_init");
    } else {
        mem_setup_complete = true;
    }
}

errval_t mon_ram_alloc_init(coreid_t core_id, struct intermon_binding *b)
{
#ifdef __scc__
    assert(!"Should not be calling this on SCC platform");
#endif

    errval_t err;

    /* Set memcore_id */
    mem_core_id = core_id;

    // Get service IREF from core
    err = b->tx_vtbl.monitor_mem_iref_request(b, NOP_CONT);
    if (err_is_fail(err)) {
        return err_push(err, MON_ERR_SEND_REMOTE_MSG);
    }
    while(monitor_mem_iref == 0) {
        messages_wait_and_handle_next();
    }

    // Bind to service
    err = monitor_mem_bind(monitor_mem_iref, bind_cont, NULL,
                           get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "monitor_mem_bind failed");
    }
    while(!mem_setup_complete) {
        messages_wait_and_handle_next();
    }

    err = ram_alloc_set(mon_ram_alloc);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_RAM_ALLOC_SET);
    }

    return SYS_ERR_OK;
}
