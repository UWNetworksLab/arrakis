/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <storage/vsa.h>
#include <storage/vsic.h>
#include <assert.h>

errval_t storage_vsa_alloc(struct storage_vsa *vsa, size_t size)
{
    // TODO: No capabilities yet. We need a manager process.
    // XXX: We just always grant the request.
    vsa->vsacap = NULL_CAP;
    vsa->size = size;
    return SYS_ERR_OK;
}

errval_t storage_vsa_resize(struct storage_vsa *vsa, size_t newsize)
{
    assert(!"NYI");
}
