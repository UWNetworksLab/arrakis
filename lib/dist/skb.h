/**
 * \file
 * \brief Calling and processing results from SKB
 */

/*
 * Copyright (c) 2010-2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SKB_H__
#define __SKB_H__

#include <barrelfish/barrelfish.h>

errval_t get_cores_skb(coreid_t **cores, int *n_cores);

#endif /* __SKB_H__ */
