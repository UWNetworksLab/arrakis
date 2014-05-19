/**
 * \file
 * \brief Interface Definition: Flounder base definitions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_H
#define __FLOUNDER_H

// #include <setjmp.h>
#include <barrelfish/waitset.h>
#include <barrelfish/idc.h>
#include <barrelfish/idc_export.h>
#include <barrelfish/event_mutex.h>

/// No-op continuation, to be passed to message send functions
#define NOP_CONT    NOP_CLOSURE

/// Utility macro to construct a continuation structure (handler & arg)
#define MKCONT(h,a) MKCLOSURE(h,a)

#endif // __FLOUNDER_H
