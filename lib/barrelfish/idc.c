/**
 * \file
 * \brief common IDC code
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/idc.h>
#include <barrelfish/idc_export.h>

void idc_init(void)
{
    idc_export_init();
#ifdef CONFIG_INTERCONNECT_DRIVER_LMP
    lmp_init();
#endif
#ifdef CONFIG_INTERCONNECT_DRIVER_UMP
    ump_init();
#endif
#if defined(CONFIG_FLOUNDER_BACKEND_UMP_IPI)
    ipi_init();
#endif
#ifdef CONFIG_INTERCONNECT_DRIVER_MULTIHOP
    multihop_init();
#endif
}
