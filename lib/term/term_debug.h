/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef LIBTERM_TERM_DEBUG_H
#define LIBTERM_TERM_DEBUG_H

/*
 * Debug printer, turned on by setting 'term_debug = True' in Config.hs.
 */
#if defined(TERMINAL_LIBRARY_DEBUG) || defined(GLOBAL_DEBUG)
#define TERM_DEBUG(arg...) debug_printf(arg)
#else
#define TERM_DEBUG(arg...) ((void)0)
#endif

#endif // LIBTERM_TERM_DEBUG_H
