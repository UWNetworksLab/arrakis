/**
 * \file
 * \brief This file is a hack because essentially all subsystems need to link
 * against these variables and putting them in lib/trace means linking
 * everything against lib/trace
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <trace/trace.h>

lvaddr_t trace_buffer_master;
lvaddr_t trace_buffer_va;
