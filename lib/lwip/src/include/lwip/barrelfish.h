/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BARRELFISH_LWIP_H
#define BARRELFISH_LWIP_H

#include <errors/errno.h>
#include <lwip/err.h>

errval_t lwip_err_to_errval(err_t e);

#endif
