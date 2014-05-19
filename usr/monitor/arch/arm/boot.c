/**
 * \file
 * \brief Code for handling booting additional cores
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <inttypes.h>
#include <elf/elf.h>

errval_t spawn_xcore_monitor(coreid_t coreid, int hwid, enum cpu_type cpu_type,
                             const char* cmdline,
                             struct intermon_binding **ret_binding)
{
    USER_PANIC("NYI!");
    return SYS_ERR_OK;
}

/**
 * \brief Initialize monitor running on app cores
 */
errval_t boot_arch_app_core(int argc, char *argv[],
                            coreid_t *ret_parent_coreid,
                            struct intermon_binding **ret_binding)
{

    USER_PANIC("NYI!");
    return SYS_ERR_OK;
}
