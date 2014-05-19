/** \file
 *  \brief service helper functions
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __SERVICE_H__
#define __SERVICE_H__

#include <barrelfish/barrelfish.h>

errval_t register_service_local(coreid_t core, char *name, iref_t iref);
errval_t lookup_service(coreid_t core, char *name, iref_t *iref);


#endif
