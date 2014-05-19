/**
 * \file
 * \brief internal functions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CLOCK_H
#define CLOCK_H

typedef int timestamp_t;

timestamp_t clock_get_timestamp(void);
errval_t clock_init(struct capref cap);

#endif // CLOCK_H
