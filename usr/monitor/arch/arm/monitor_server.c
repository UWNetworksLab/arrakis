/**
 * \file
 * \brief Monitor's connection with the dispatchers on the same core
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <barrelfish/debug.h> // XXX: To set the cap_identify_reply handler
#include <barrelfish/sys_debug.h> // XXX: for sys_debug_send_ipi
#include <trace/trace.h>
#include <if/mem_defs.h>
#include <barrelfish/monitor_client.h>
#include <if/monitor_loopback_defs.h>

errval_t monitor_server_arch_init(struct monitor_binding *b)
{
    // No-op
    return SYS_ERR_OK;
}
