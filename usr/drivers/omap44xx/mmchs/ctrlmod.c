/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/debug.h>
#include <barrelfish/inthandler.h>
#include <barrelfish/waitset.h>

#include <driverkit/driverkit.h>

#include "twl6030.h"
#include "omap44xx_ctrlmod.h"

#include "mmchs_debug.h"

#if defined(CTRLMOD_SERVICE_DEBUG) || defined(MMCHS_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
#define CTRLMOD_DEBUG(x...) debug_printf(x)
#else
#define CTRLMOD_DEBUG(x...) ((void)0)
#endif

#define SYSCTRL_PADCONF_CORE 0x4a100000u
static omap44xx_sysctrl_padconf_core_t ctrlmod;

static volatile uint32_t dummy = 0;
static void wait_msec(long msec)
{
    int i = 0, sum = 0;
    long end = (1200000 * msec / 8);

    // Cannot use volatile variables in loop
    while (++i < end)  {
        sum += i + end;
    }

    dummy += sum;
}

/*
 * \brief Initialization of control module
 */
void ctrlmod_init(void)
{
    CTRLMOD_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    lvaddr_t vaddr;
    errval_t err = map_device_register(SYSCTRL_PADCONF_CORE, 0x1000, &vaddr);
    assert(err_is_ok(err));

    // Initialize Mackerel
    omap44xx_sysctrl_padconf_core_initialize(&ctrlmod, (mackerel_addr_t) vaddr);
}

/*
 * We need to configure the extended-drain I/O pads to the right voltage and
 * turn on the external power supply (TWL6030 on the pandaboard)
 */
void sdmmc1_enable_power(void)
{
    // compare with Table 18-109 in OMAP TRM, p3681
    // Step 1: software must keep PWRDNZ low when setting up voltages
    CTRLMOD_DEBUG("%s: Step 1\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x0);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x0);

    // Step 2: preliminary settings for MMC1_PBIAS and MMC1 I/O cell
    CTRLMOD_DEBUG("%s: Step 2\n", __FUNCTION__);
    //  1. turn of hiz mode
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_hiz_mode_wrf(&ctrlmod, 0x0);

    //  2. setup PBIAS_IRQ (MA_IRQ_75)
    // We don't use the interrupt

    //  3. pad multiplexing -- looks ok when dumping pad registers, so I'm
    //  not doing anything right now -SG

    //  4. set MMC1 speed control to 26MHz@30pF (0x0) -- alternative 65MHz@30pF (0x1)
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr0_speedctrl_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr1_speedctrl_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_dr2_speedctrl_wrf(&ctrlmod,
            0x0);

    //  5. set MMC1 pullup strength to 10-50kOhm (0x1) -- alt. 50-110kOhm (0x0)
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp0_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp1_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp2_wrf(&ctrlmod,
            0x0);
    omap44xx_sysctrl_padconf_core_control_mmc1_sdmmc1_pustrength_grp3_wrf(&ctrlmod,
            0x0);

    // Step 3: Program desired SDMMC1_VDDS for MMC I/O in I2C attached power
    // controller (3.0V)
    errval_t err = ti_twl6030_set_vmmc_vsel(3000);
    assert(err_is_ok(err));

    // Step 4: Set VMODE bit according to Step 3 (0x1 == 3.0V)
    CTRLMOD_DEBUG("%s: Step 4\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_wrf(&ctrlmod, 0x1);

    // Step 5: wait for SDMMC1_VDDS voltage to stabilize TODO
    // might already be stable after reset? -SG

    // Step 6: Disable PWRDNZ mode for MMC1_PBIAS and MMC1 I/O cell
    CTRLMOD_DEBUG("%s: Step 6\n", __FUNCTION__);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x1);
    wait_msec(100);
    omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x1);

    CTRLMOD_DEBUG("%s:%d: wait until supply_hi_out is 0x1\n", __FUNCTION__, __LINE__);
    while (omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_supply_hi_out_rdf(&ctrlmod)
            != 0x1) {}

    // Step 7: Store SUPPLY_HI_OUT bit
    uint8_t supply_hi_out =
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_supply_hi_out_rdf(&ctrlmod);
    CTRLMOD_DEBUG("%s: Step 7: supply_hi_out = %d\n", __FUNCTION__, supply_hi_out);
    CTRLMOD_DEBUG("%s: Step 7: vmode_error = %d\n", __FUNCTION__,
                  omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_error_rdf(&ctrlmod));
    CTRLMOD_DEBUG("%s: Step 8\n", __FUNCTION__);

    // Step 8: check VMODE_ERROR and set PWRDNZ if error
    if (omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_error_rdf(&ctrlmod)) {
        CTRLMOD_DEBUG("got VMODE error\n");
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pwrdnz_wrf(&ctrlmod, 0x0);
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_pwrdnz_wrf(&ctrlmod, 0x0);
    }

    // Step 9: check if SUPPLY_HI_OUT corresponds to SDMMC1_VDDS (3.0V)
    if (supply_hi_out != 0x1) {
        CTRLMOD_DEBUG("SDMMC1_VDDS seems to be != 3.0V\n");
        // TODO: redo setting SDMMC1_VDDS
    } else {
        // supply_hi_out should be 0x1 (3.0V)
        assert(supply_hi_out == 0x1);
        // set VMODE bit to supply_hi_out
        omap44xx_sysctrl_padconf_core_control_pbiaslite_mmc1_pbiaslite_vmode_wrf(&ctrlmod, supply_hi_out);
    }

    // Step 12: clear PBIAS IRQ
    // Ignored
}
