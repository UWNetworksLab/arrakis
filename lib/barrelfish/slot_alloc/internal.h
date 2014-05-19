/**
 * \file
 * \brief internal functions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SLOT_ALLOC_INTERNAL_H_
#define SLOT_ALLOC_INTERNAL_H_

errval_t multi_alloc(struct slot_allocator *ca, struct capref *ret);
errval_t multi_free(struct slot_allocator *ca, struct capref cap);

#endif //SLOT_ALLOC_INTERNAL_H_
