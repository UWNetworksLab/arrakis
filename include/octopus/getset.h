/**
 * \file
 * \brief Header file for the octopus get/set API.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_GETSET_H_
#define OCTOPUS_GETSET_H_

#include <barrelfish/barrelfish.h>

typedef uint64_t oct_mode_t;

#define SET_DEFAULT		(0x0)       /*!< Normal set mode for records. */
#define SET_SEQUENTIAL	(0x1)       /*!< Append a monotonically increasing number
                                         (based on the record name) to the name
                                         of the record. */
#define SET_TRANSIENT	(0x1 << 1) /*!< Record gets deleted as soon as the
                                        domain has ended it's execution. */
// TODO SET_TRANSIENT NYI Due to limitations of barrelfish:
// (can't figure out if a domain is done/crashed...).

errval_t oct_get_names(char*** names, size_t*, const char*, ...);
errval_t oct_parse_names(char* input, char*** names, size_t*);
void oct_free_names(char**, size_t);

errval_t oct_get(char**, const char*, ...);
errval_t oct_set(const char*, ...);
errval_t oct_get_with_idcap(char**, struct capref);
errval_t oct_set_with_idcap(struct capref, const char*, ...);
errval_t oct_mset(oct_mode_t, const char*, ...);
errval_t oct_set_get(oct_mode_t, char**, const char*, ...);
errval_t oct_del(const char*, ...);
errval_t oct_exists(const char*, ...);

errval_t oct_read(const char*, const char*, ...);

#endif /* OCTOPUS_GETSET_H_ */
