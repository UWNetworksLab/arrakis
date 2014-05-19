/**
 * \file
 * \brief x86 machine check architecture initialisation / "driver"
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <x86.h>
#include <arch/x86/mcheck.h>
#include <dev/ia32_dev.h>
#include <dev/amd64_dev.h>

/**
 * \brief Enable machine check reporting, if supported
 *
 * Intel SDM vol 3 sec 14.6 p14-15 "Machine-check initialization" pseudocode
 */
void mcheck_init(void)
{
    // use CPUID to check for support and get processor family
    uint32_t eax, edx;
    cpuid(1, &eax, NULL, NULL, &edx);

    bool mca_supported = (edx & (1 << 14)) != 0;
    bool mce_supported = (edx & (1 << 7)) != 0;

    uint8_t proc_family = (eax >> 8) & 0xf;

    // if MCE unsupported, skip this entirely
    if (!mce_supported) {
        return;
    }

    debug(SUBSYS_STARTUP, "Machine-check exceptions supported\n");

    if (mca_supported) {
        debug(SUBSYS_STARTUP, "Machine-check architecture supported, "
                              "family 0x%x\n", proc_family);

        // if the ctl register is present, enable all MCA features
        ia32_mcg_cap_t mcg_cap = ia32_mcg_cap_rd(NULL);

	int num_banks = ia32_mcg_cap_count_extract(mcg_cap);

        if (ia32_mcg_cap_ctl_p_extract(mcg_cap)) {
            ia32_mcg_ctl_wr(NULL, ia32_mc_enable);
        }

        if (proc_family == 0x6) {
            // enable logging of all errors except for mc0_ctl register
            for (int i = 1; i < num_banks; i++) {
                ia32_mc_ctl_wr(NULL, i, ia32_mc_enable);
            }

            // clear all errors
            for (int i = 0; i < num_banks; i++) {
                ia32_mc_status_wr(NULL, i, 0UL);
            }
        } else if (proc_family == 0xf) { // any processor extended family
            // enable logging of all errors including mc0_ctl
            for (int i = 0; i < num_banks; i++) {
                ia32_mc_ctl_wr(NULL, i, ia32_mc_enable);
            }

            // clear all errors
            for (int i = 0; i < num_banks; i++) {
                ia32_mc_status_wr(NULL, i, 0UL);
            }
        }
    }

    // enable machine-check exceptions
    amd64_cr4_mce_wrf(NULL,1);
}
