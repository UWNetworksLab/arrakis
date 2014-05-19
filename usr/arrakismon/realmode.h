/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef REALMODE_H
#define REALMODE_H

#include "vmkitmon.h"

#define REALMODE_ERR_OK                 (0)
#define REALMODE_ERR_INVLD_ENV          (-1)
#define REALMODE_ERR_NOT_SUPP           (-2)

errval_t realmode_init (void);
void realmode_switch_to (struct guest *g);
void realmode_switch_from (struct guest *g);
int realmode_exec (void);

#endif // REALMODE_H
