/**
 * \file
 * \brief Distributed (percore) memory server: code for non_stealing version
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <inttypes.h>
#include <barrelfish/barrelfish.h>

#include "mem_serv.h"
#include "steal.h"

coreid_t mycore;


errval_t percore_steal_handler_common(uint8_t bits,
                                      genpaddr_t minbase, 
                                      genpaddr_t maxlimit,
                                      struct capref *retcap)
{
    *retcap = NULL_CAP;
    return ERR_NOTIMP;
}

void try_steal(errval_t *ret, struct capref *cap, uint8_t bits,
                   genpaddr_t minbase, genpaddr_t maxlimit)
{
    //DEBUG_ERR(ret, "allocating %d bits in 0x%"PRIxGENPADDR "-0x%" 
    //          PRIxGENPADDR " failed", bits, minbase, maxlimit);
    // debug_printf("failed percore alloc request: bits: %d NOT STEALING\n", 
    //             bits);
    *cap = NULL_CAP;
}


errval_t init_peers(coreid_t core, int len_cores, coreid_t *cores) 
{
    mycore = core;
    return SYS_ERR_OK;
}


int main(int argc, char ** argv)
{
    return common_main(argc, argv);
}

