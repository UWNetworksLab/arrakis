/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef NFS_DEBUG_H_
#define NFS_DEBUG_H_


/*****************************************************************
 * Debug printer:
 *****************************************************************/

//#define NFS_CLIENT_DEBUG 1

#if defined(NFS_CLIENT_DEBUG) || defined(GLOBAL_DEBUG)
#define NFSDEBUGPRINT(arg...) printf("nfs: " arg)
#else
#define NFSDEBUGPRINT(arg...) ((void)0)
#endif

#endif // NFS_DEBUG_H_
