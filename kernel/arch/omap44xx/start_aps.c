/**
 * \file
 * \brief Start the application processors
 *
 *  This file sends all needed IPIs to the other cores to start them.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <paging_kernel_arch.h>
#include <arm_hal.h>
#include <cp15.h>
#include <arch/armv7/start_aps.h>

#define STARTUP_TIMEOUT         0xffffff

/**
 * \brief Boot an arm app core
 *
 * \param core_id   ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int start_aps_arm_start(uint8_t core_id, lvaddr_t entry)
{
    //printf("----> %s (%s:%d): core_id=%u entry=0x%lx\n",
    //       __FUNCTION__, __FILE__, __LINE__,
    //       core_id, entry);

    /* pointer to the pseudo-lock used to detect boot up of new core */
    volatile uint32_t *ap_wait = (uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);
    *ap_wait = AP_STARTING_UP;
    cp15_invalidate_d_cache();

    // map AUX_CORE_BOOT section
    static lvaddr_t aux_core_boot = 0;
    if (aux_core_boot == 0)
        aux_core_boot = paging_map_device(AUX_CORE_BOOT_SECT, ARM_L1_SECTION_BYTES);

    volatile lvaddr_t *aux_core_boot_0, *aux_core_boot_1;
    // The AUX_CORE_BOOT_0 register is used to store the startup state
    aux_core_boot_0 = (void *)(aux_core_boot + AUX_CORE_BOOT_0_OFFSET);
    aux_core_boot_1 = (void *)(aux_core_boot + AUX_CORE_BOOT_1_OFFSET);

    //write entry address of new kernel to SYSFLAG reg
    // Set address where the other core should jump
    debug(SUBSYS_STARTUP, "setting AUX_CORE_BOOT_1 to 0x%"PRIxLVADDR"\n", entry);
    *aux_core_boot_1 = entry;

    // Tell ROM code to start other core
    debug(SUBSYS_STARTUP, "AUX_CORE_BOOT_0 |= 1<< 2\n");
    *aux_core_boot_0 |= 1 << 2;

    // send signal to app core to start
    debug(SUBSYS_STARTUP, "sending event to other core(s?)\n");
    __asm__ volatile ("SEV");

    debug(SUBSYS_STARTUP, "waiting for response\n");
    while (*aux_core_boot_0 != 2<<2)
        ;

    debug(SUBSYS_STARTUP, "booted CPU%hhu\n", core_id);

	return 0;
}
