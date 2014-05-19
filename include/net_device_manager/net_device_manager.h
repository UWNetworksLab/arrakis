/**
 * \file
 * \brief Header file for net_device_manager.h
 */

/*
 * Copyright (c) 2007-12 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef net_device_manager_H
#define net_device_manager_H

#include <barrelfish/barrelfish.h>

// initializes the hardware independent part of device manager
errval_t init_device_manager(char *dev_name, uint64_t valid_queues,
        uint8_t filt_mng_type);
#endif // net_device_manager_H


