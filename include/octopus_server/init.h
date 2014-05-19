/**
 * \file
 * \brief Header file for octopus server initialization.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_INIT_H_
#define OCTOPUS_INIT_H_

#include <barrelfish/barrelfish.h>

errval_t init_capstorage(void);
errval_t rpc_server_init(void);
errval_t oct_server_init(void);

#endif /* OCTOPUS_INIT_H_ */
