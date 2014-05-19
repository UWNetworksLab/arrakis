/** \file
 *  \brief Memory server benchmark tracing
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __MEMTEST_TRACE_H__
#define __MEMTEST_TRACE_H__

#include <barrelfish/barrelfish.h>
#include <trace_definitions/trace_defs.h>

errval_t init_tracing(void);
void start_tracing(void);
void stop_tracing(void);
void prepare_dump(void);
void dump_trace(void);

#endif /* __MEMTEST_TRACE_H__ */
