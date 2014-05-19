/*
 * Copyright (c) 2007-11 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef QUEUE_MANAGER_DEBUG_H_
#define QUEUE_MANAGER_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

void ethersrv_debug_printf(const char *fmt, ...);

//#define ETHERSRV_SERVICE_DEBUG 1

#if defined(ETHERSRV_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define ETHERSRV_DEBUG(x...) do { printf("NQM:%s.%d:%s:%d: ", \
            disp_name(), disp_get_core_id(), __func__, __LINE__); \
                printf(x);\
        } while (0)

#else
#define ETHERSRV_DEBUG(x...) ((void)0)
#endif // defined(ETHERSRV_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)

#endif // QUEUE_MANAGER_DEBUG_H_

