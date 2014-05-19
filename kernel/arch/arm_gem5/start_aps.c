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
#include <arch/armv7/start_aps.h>
#include <arm_hal.h>

#define STARTUP_TIMEOUT         0xffffff

/**
 * \brief Boot an arm app core
 *
 * \param core_id   APIC ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int start_aps_arm_start(uint8_t core_id, lvaddr_t entry)
{
	//volatile uint32_t *ap_lock = (volatile uint32_t*)local_phys_to_mem(AP_LOCK_PHYS);

	/* pointer to the pseudo-lock used to detect boot up of new core */
	volatile uint32_t *ap_wait = (volatile uint32_t*)local_phys_to_mem(AP_WAIT_PHYS);

	*ap_wait = AP_STARTING_UP;

	//write entry address of new kernel to SYSFLAG reg
	write_sysflags_reg(entry);

	//raise SWI to signal app core to start
	gic_raise_softirq((1 << core_id), 1);

	while (*ap_wait != AP_STARTED) {}

	debug(SUBSYS_STARTUP, "booted CPU%hhu\n", core_id);

	return 0;
}
