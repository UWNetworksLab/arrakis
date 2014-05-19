/** \file
 *  \brief Simple name-server based barriers
 */

/*
 * Copyright (c) 2010-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __BARRIER_H__
#define __BARRIER_H__

#include <barrelfish/barrelfish.h>

#define NAME_LEN 20

errval_t nsb_wait_n(int n, char *name);
errval_t nsb_wait_s(char *s1, char *s2);
errval_t nsb_wait_ready(char *name);
errval_t nsb_wait(char *name);
errval_t nsb_register(char *name);
errval_t nsb_register_n(int n, char *name);
errval_t nsb_register_s(char *s1, char *s2);
errval_t nsb_register_ready(char *name);
errval_t nsb_master(int min, int max, char *name);
errval_t nsb_master_l(coreid_t *cores, int n_cores, char *name);
errval_t nsb_worker(int num, char *name);

#endif /* __BARRIER_H__ */
