/**
 * \file
 * \brief dist2 Debug Macros
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_DEBUG_H_
#define OCTOPUS_DEBUG_H_

#if defined(DIST_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define OCT_DEBUG(x...) debug_printf("octopus_service: " x)
#else
#define OCT_DEBUG(x...) ((void)0)
#endif

#endif // OCTOPUS_DEBUG_H_
