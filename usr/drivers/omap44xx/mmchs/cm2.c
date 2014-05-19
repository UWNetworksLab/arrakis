/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */
#include <barrelfish/barrelfish.h>
#include <driverkit/driverkit.h>

#include <arch/arm/omap44xx/device_registers.h>
#include <dev/omap/omap44xx_l3init_cm2_dev.h>
#include <dev/omap/omap44xx_ckgen_cm2_dev.h>
#include <dev/omap/omap44xx_l4per_cm2_dev.h>

#include "omap44xx_cm2.h"

static omap44xx_l3init_cm2_t l3init_cm2;
static omap44xx_l4per_cm2_t l4per_cm2;
static omap44xx_ckgen_cm2_t clkgen_cm2;

void cm2_enable_hsmmc1(void)
{
    omap44xx_l3init_cm2_cm_l3init_clkstctrl_clktrctrl_wrf(&l3init_cm2, 0x2);
    omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_modulemode_wrf(&l3init_cm2, 0x2);
    while (omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_idlest_rdf(&l3init_cm2) != 0x0);
}

void cm2_enable_i2c(size_t i2c_index)
{
    assert (i2c_index < 4);

    omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_modulemode_wrf(&l4per_cm2, i2c_index, 0x2);
    while (omap44xx_l4per_cm2_cm_l4per_i2c_clkctrl_idlest_rdf(&l4per_cm2, i2c_index)
            != 0x0);
}

void cm2_init(void)
{
    lvaddr_t l3init_vaddr;
    errval_t err = map_device_register(OMAP44XX_CM2, 0x1000, &l3init_vaddr);
    assert(err_is_ok(err));
    omap44xx_l3init_cm2_initialize(&l3init_cm2, (mackerel_addr_t)l3init_vaddr);

    lvaddr_t clkgen_vaddr;
    err = map_device_register(OMAP44XX_CLKGEN_CM2, 0x1000, &clkgen_vaddr);
    assert(err_is_ok(err));
    omap44xx_ckgen_cm2_initialize(&clkgen_cm2, (mackerel_addr_t)clkgen_vaddr);

    //lvaddr_t l4per_vaddr;
    //err = map_device_register(OMAP44XX_L4PER_CM2, 0x1000, &l4per_vaddr);
    //assert(err_is_ok(err));
    omap44xx_l4per_cm2_initialize(&l4per_cm2, (mackerel_addr_t)l3init_vaddr);
}

int cm2_get_hsmmc1_base_clock(void)
{
    return omap44xx_l3init_cm2_cm_l3init_hsmmc1_clkctrl_clksel_rdf(&l3init_cm2) == 0x0 ?
           64000000 : 96000000;
}
