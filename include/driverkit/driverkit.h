/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DRIVERKIT_H
#define DRIVERKIT_H

#include <barrelfish/types.h> 
#include <errors/errno.h>

errval_t map_device_register(lpaddr_t, size_t, lvaddr_t*);
 
#endif // DRIVERKIT_H