/**
 * \file
 * \brief Client for interacting with the monitor
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/resource_ctrl.h>
#include <barrelfish/curdispatcher_arch.h>
#include <barrelfish/dispatcher_arch.h>
#include <if/monitor_blocking_rpcclient_defs.h>

errval_t rsrc_manifest(const char *manifest, rsrcid_t *id)
{
    struct monitor_blocking_rpc_client *b = get_monitor_blocking_rpc_client();
    struct dispatcher_generic *d = get_dispatcher_generic(curdispatcher());
    errval_t err, msgerr;

    err = b->vtbl.rsrc_manifest(b, d->dcb_cap, manifest, id, &msgerr);
    assert(err_is_ok(err));

    return msgerr;
}

errval_t rsrc_join(rsrcid_t id)
{
    struct monitor_blocking_rpc_client *b = get_monitor_blocking_rpc_client();
    struct dispatcher_generic *d = get_dispatcher_generic(curdispatcher());
    errval_t err, msgerr;

    err = b->vtbl.rsrc_join(b, id, d->dcb_cap, &msgerr);
    assert(err_is_ok(err));

    return msgerr;
}

errval_t rsrc_phase(rsrcid_t id, uint32_t phase)
{
    struct monitor_blocking_rpc_client *b = get_monitor_blocking_rpc_client();
    return b->vtbl.rsrc_phase(b, id, phase);
}
