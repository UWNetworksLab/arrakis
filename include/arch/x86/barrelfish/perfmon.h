/**
 * \file
 * \brief IA32 performance monitoring
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_PERFMON_H
#define BARRELFISH_PERFMON_H

errval_t perfmon_setup(dispatcher_handle_t handle, perfmon_counter_t counter,
                       perfmon_event_t evt, perfmon_mask_t umask, bool kernel);
const char *perfmon_str(uint64_t event);

#endif
