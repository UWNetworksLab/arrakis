/**
 * \file
 * \brief Header file for capability storage
 */
/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_CAPABILITY_STORAGE_H_
#define OCTOPUS_CAPABILITY_STORAGE_H_

errval_t oct_get_capability(const char*, struct capref*);
errval_t oct_put_capability(const char*, struct capref);
errval_t oct_remove_capability(const char*);

#endif /* OCTOPUS_CAPABILITY_STORAGE_H_ */
