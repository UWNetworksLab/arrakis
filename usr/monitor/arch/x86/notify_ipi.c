/**
 * \file
 * \brief IPI notification mechanism
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <notify_ipi.h>

#ifdef __scc__
static int glbl_chanid = 0;
#else
static int glbl_chanid = 1;
#endif

errval_t notification_set(int chanid, struct capref ep)
{
    return invoke_monitor_ipi_register(ep, chanid);
}

errval_t notification_allocate(struct capref ep, int *chanid)
{
    *chanid = glbl_chanid;
    glbl_chanid++;

    if(get_cap_addr(ep) != CPTR_NULL) {
        return notification_set(*chanid, ep);
    } else {
        return SYS_ERR_OK;
    }
}

errval_t notification_create_cap(int chanid, coreid_t coreid,
                                 struct capref *retcap)
{
    errval_t err;

    /* printf("%d: creating notify cap with chanid %d, coreid %d, caller %p\n", */
    /*        my_core_id, chanid, coreid, __builtin_return_address(0)); */

    /* Construct the notification capability */
    struct capability notify_cap = {
#ifdef __scc__
        .type = ObjType_Notify_RCK,
#else
        .type = ObjType_Notify_IPI,
#endif
        .rights = CAPRIGHTS_READ_WRITE, // XXX
#ifdef __scc__
        .u.notify_rck = {
#else
        .u.notify_ipi = {
#endif
            .coreid = coreid,
            .chanid = chanid
        }
    };

    err = slot_alloc(retcap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to allocate slot from channel_alloc");
        printf("Failed to allocate slot from channel_alloc\n");
        abort(); //XXX
    }
    err = monitor_cap_create(*retcap, &notify_cap, 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "monitor_cap_create failed");
        printf("monitor_cap_create failed\n");
        abort(); //XXX
    }

    return SYS_ERR_OK;
}
