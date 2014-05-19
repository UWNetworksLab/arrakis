/**
 * \file
 * \brief Header file for barrier functions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_BARRIER_H_
#define OCTOPUS_BARRIER_H_

errval_t oct_barrier_enter(const char*, char**, size_t);
errval_t oct_barrier_leave(const char*);

#endif /* OCTOPUS_BARRIER_H_ */
