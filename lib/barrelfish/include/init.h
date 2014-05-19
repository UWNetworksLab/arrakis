/**
 * \file
 * \brief Barrelfish library initialization.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_INIT_H
#define LIBBARRELFISH_INIT_H

struct spawn_domain_params;
errval_t barrelfish_init_onthread(struct spawn_domain_params *params);
void barrelfish_libc_glue_init(void);

#endif
