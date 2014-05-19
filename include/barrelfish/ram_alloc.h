/**
 * \file
 * \brief RAM allocator code (client-side) definitions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef BARRELFISH_RAM_ALLOC_H
#define BARRELFISH_RAM_ALLOC_H

#include <sys/cdefs.h>

__BEGIN_DECLS

typedef errval_t (* ram_alloc_func_t)(struct capref *ret, uint8_t size_bits,
                                      uint64_t minbase, uint64_t maxlimit);

errval_t ram_alloc_fixed(struct capref *ret, uint8_t size_bits,
                         uint64_t minbase, uint64_t maxlimit);
errval_t ram_alloc(struct capref *retcap, uint8_t size_bits);
errval_t ram_available(genpaddr_t *available, genpaddr_t *total);
errval_t ram_alloc_set(ram_alloc_func_t local_allocator);
void ram_set_affinity(uint64_t minbase, uint64_t maxlimit);
void ram_get_affinity(uint64_t *minbase, uint64_t *maxlimit);
void ram_alloc_init(void);

__END_DECLS

#endif // BARRELFISH_RAM_ALLOC_H
