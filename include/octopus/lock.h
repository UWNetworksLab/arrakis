/**
 * \file
 * \brief Header file for lock related functions.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_LOCK_H_
#define OCTOPUS_LOCK_H_

errval_t oct_lock(const char*, char**);
errval_t oct_unlock(const char*);

#endif /* OCTOPUS_LOCK_H_ */
