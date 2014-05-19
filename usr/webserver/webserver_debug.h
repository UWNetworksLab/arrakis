/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef WEBSERVER_DEBUG_H_
#define WEBSERVER_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

//#define WEBSERVER_DEBUG 1

#if defined(WEBSERVER_DEBUG) || defined(GLOBAL_DEBUG)
#define DEBUGPRINT(arg...) printf("WEBSERVER: " arg)
#else
#define DEBUGPRINT(arg...) ((void)0)
#endif // WEBSERVER_DEBUG

uint64_t get_time_delta(uint64_t *last_ts);
#endif // WEBSERVER_H_
