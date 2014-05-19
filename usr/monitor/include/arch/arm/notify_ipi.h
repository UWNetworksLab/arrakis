/**
 * \file
 * \brief IPI notification mechanism
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARM_NOTIFY_IPI_H
#define ARM_NOTIFY_IPI_H

#include <barrelfish/barrelfish.h>
#include <barrelfish_kpi/types.h>

errval_t notification_set(int chanid, struct capref ep);
errval_t notification_allocate(struct capref ep, int *chanid);
errval_t notification_create_cap(int chanid, coreid_t coreid,
                                 struct capref *retcap);

#endif
