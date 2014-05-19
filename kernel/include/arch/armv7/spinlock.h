/**
 * \file
 * \brief interface for OMAP44XX hardware spinlock module
 */
/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaestr. 6, CH-8092 Zurich. 
 * Attn: Systems Group.
 */

#ifndef __SPINLOCK_H
#define __SPINLOCK_H

/* Need to include this for errval_t */
#include <errors/errno.h>

#define PRINTF_LOCK 0   //we currently only use one lock

/*
 * Initialize the module
 */
extern errval_t spinlock_init(void);
extern errval_t spinlock_early_init(void);

/*
 * aquire and release specific locks
 * chosen the names as a contrast to "aquire_spinlock", because the arguments differ
 * (here we want the index of the lock in the module, instead of a generic address)
 */
void spinlock_aquire(int locknumber);
void spinlock_release(int locknumber);


#endif //__SPINLOCK_H
