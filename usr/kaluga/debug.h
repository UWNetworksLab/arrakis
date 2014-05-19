/**
 * \file
 * \brief Kaluga Debug Macros
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KALUGA_DEBUG_H_
#define KALUGA_DEBUG_H_

//#define KALUGA_SERVICE_DEBUG 1

#if defined(KALUGA_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define KALUGA_DEBUG(x...) debug_printf(x)
#else
#define KALUGA_DEBUG(x...) ((void)0)
#endif

#endif // KALUGA_DEBUG_H_
