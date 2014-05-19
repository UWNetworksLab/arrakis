/**
 * \file
 * \brief Distributed (percore) memory server: stealing related code
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __STEAL_H__
#define __STEAL_H__

extern coreid_t mycore;

errval_t percore_steal_handler_common(uint8_t bits,
                                      genpaddr_t minbase, 
                                      genpaddr_t maxlimit,
                                      struct capref *retcap);
void try_steal(errval_t *ret, struct capref *cap, uint8_t bits,
               genpaddr_t minbase, genpaddr_t maxlimit);
errval_t init_peers(coreid_t core, int len_cores, coreid_t *cores);


#endif
