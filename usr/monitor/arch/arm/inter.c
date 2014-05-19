/**
 * \file
 * \brief Arch-specific inter-monitor communication
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include "monitor.h"
#include <trace/trace.h>

/**
 * \brief A monitor receives request to setup a connection
 * with another newly booted monitor from a third monitor
 */
static void bind_monitor_request(struct intermon_binding *st,
                                 coreid_t core_id,
                                 intermon_caprep_t caprep)
{
    USER_PANIC("NYI!");
}

/**
 * \brief The monitor that proxied the request for one monitor to
 * setup a connection with another monitor gets the reply
 */
static void bind_monitor_reply(struct intermon_binding *closure, errval_t err)
{
    USER_PANIC("NYI!");
}

/**
 * \brief A monitor asks this monitor to proxy
 * its request to bind to another monitor
 */
static void bind_monitor_proxy(struct intermon_binding *st,
                               coreid_t dst_core_id,
                               intermon_caprep_t caprep)
{
    USER_PANIC("NYI!");
}

/**
 * \brief Notification of a newly booted monitor.
 *  Setup our connection and request the sender to proxy
 *  the bind request to the monitor
 */
static void new_monitor_notify(struct intermon_binding *st,
                               coreid_t core_id)
{
    USER_PANIC("NYI!");
}

errval_t arch_intermon_init(struct intermon_binding *b)
{
    b->rx_vtbl.bind_monitor_request = bind_monitor_request;
    b->rx_vtbl.bind_monitor_reply = bind_monitor_reply;
    b->rx_vtbl.bind_monitor_proxy = bind_monitor_proxy;
    b->rx_vtbl.new_monitor_notify = new_monitor_notify;

    return SYS_ERR_OK;
}
