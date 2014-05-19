/**
 * \file
 * \brief Shared data structure between the multicast domain and the monitor
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MULTICAST_H_
#define MULTICAST_H_


#define MAXAPICID 256

struct multicast {
    uint8_t multicast_matrix[MAXAPICID][MAXAPICID];
    uintptr_t indizes[MAXAPICID];
};


#endif //  MULTICAST_H_
