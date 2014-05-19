
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
#include <arch/x86/apic.h>
#include <arch/x86/start_aps.h>
#include <x86.h>
#include <arch/x86/cmos.h>
#include <init.h>
#include <arch/x86/kputchar.h>
#include "xapic_dev.h"
#include <target/x86_64/offsets_target.h>
#include <target/x86_32/offsets_target.h>

#define STARTUP_TIMEOUT         0xffffff

/**
 * start_ap and start_ap_end mark the start end the end point of the assembler
 * startup code to be copied
 */

extern uint64_t x86_64_start_ap;
extern uint64_t x86_64_start_ap_end;
extern uint64_t x86_64_init_ap_absolute_entry;
extern uint64_t x86_64_init_ap_wait;
extern uint64_t x86_64_init_ap_lock;
extern uint64_t x86_64_start;
extern uint64_t x86_64_init_ap_global;

extern uint64_t x86_32_start_ap;
extern uint64_t x86_32_start_ap_end;
extern uint64_t x86_32_init_ap_absolute_entry;
extern uint64_t x86_32_init_ap_wait;
extern uint64_t x86_32_init_ap_lock;
extern uint64_t x86_32_start;
extern uint64_t x86_32_init_ap_global;

/**
 * \brief Boot a app core of x86_64 type
 *
 * The processors are started by a sequency of INIT and STARTUP IPIs
 * which are sent by this function. 
 *
 * \param core_id   APIC ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t given in genvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int start_aps_x86_64_start(uint8_t core_id, genvaddr_t entry)
{
    /* Copy the startup code to the real-mode address */
    uint8_t *real_dest = (uint8_t*) local_phys_to_mem(X86_64_REAL_MODE_LINEAR_OFFSET);
    uint8_t *real_src = (uint8_t *) &x86_64_start_ap;
    uint8_t *real_end = (uint8_t *) &x86_64_start_ap_end;
    memcpy(real_dest, real_src, real_end - real_src);

    /* Pointer to the entry point called from init_ap.S */
    volatile uint64_t *absolute_entry_ptr = (volatile uint64_t *)
        local_phys_to_mem((lpaddr_t) &x86_64_init_ap_absolute_entry - ((lpaddr_t) &x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);
    //copy the address of the function start (in boot.S) to the long-mode
    //assembler code to be able to perform an absolute jump
    *absolute_entry_ptr = entry;

    /* pointer to the pseudo-lock used to detect boot up of new core */
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t) &x86_64_init_ap_wait - ((lpaddr_t) &x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);

    /* Pointer to the lock variable in the realmode code */
    volatile uint8_t *ap_lock = (volatile uint8_t*)
        local_phys_to_mem((lpaddr_t) &x86_64_init_ap_lock - ((lpaddr_t) &x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);

    /* pointer to the shared global variable amongst all kernels */
    volatile uint64_t *ap_global = (volatile uint64_t *)
        local_phys_to_mem((lpaddr_t) &x86_64_init_ap_global - ((lpaddr_t) &x86_64_start_ap) +
                          X86_64_REAL_MODE_LINEAR_OFFSET);
    *ap_global = (uint64_t)mem_to_local_phys((lvaddr_t)global);

    lvaddr_t *init_vector;
    init_vector = (lvaddr_t*)local_phys_to_mem(CMOS_RAM_BIOS_WARM_START_INIT_VECTOR);

    *ap_wait = AP_STARTING_UP;

    if (CPU_IS_M5_SIMULATOR) {
        printk(LOG_WARN, "Warning: skipping shutdown/init of APs on M5\n");
    } else {
        //set shutdown status to WARM_SHUTDOWN and set start-vector
        cmos_write( CMOS_RAM_SHUTDOWN_ADDR, CMOS_RAM_WARM_SHUTDOWN);
        *init_vector = X86_64_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(X86_64_REAL_MODE_SEGMENT,
                                                          X86_64_REAL_MODE_OFFSET);

        //INIT 1 assert
        apic_send_init_assert(core_id, xapic_none);

        //set shutdown status to WARM_SHUTDOWN and set start-vector
        cmos_write( CMOS_RAM_SHUTDOWN_ADDR, CMOS_RAM_WARM_SHUTDOWN);
        *init_vector = X86_64_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(X86_64_REAL_MODE_SEGMENT,
                                                          X86_64_REAL_MODE_OFFSET);

        //INIT 2 de-assert
        apic_send_init_deassert();
    }

    //SIPI1
    apic_send_start_up(core_id, xapic_none,
                       X86_64_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(X86_64_REAL_MODE_SEGMENT));

    //SIPI2
    apic_send_start_up(core_id, xapic_none,
                       X86_64_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(X86_64_REAL_MODE_SEGMENT));

    //give the new core a bit time to start-up and set the lock
    for (uint64_t i = 0; i < STARTUP_TIMEOUT; i++) {
        if (*ap_lock != 0) {
            break;
        }
    }

    //if the lock is set, the core has been started, otherwise assume, that
    //a core with this APIC ID doesn't exist.
    if (*ap_lock != 0) {
        while (*ap_wait != AP_STARTED);
        *ap_lock = 0;
        debug(SUBSYS_STARTUP, "booted CPU%hhu\n", core_id);
        return 0;
    }
    return -1;
}

/**
 * \brief Boot a app core of x86_32 type
 *
 * The processors are started by a sequency of INIT and STARTUP IPIs
 * which are sent by this function. 
 *
 * \param core_id   APIC ID of the core to try booting
 * \param entry     Entry address for new kernel in the destination
 *                  architecture's lvaddr_t given in genvaddr_t
 *
 * \returns Zero on successful boot, non-zero (error code) on failure
 */
int start_aps_x86_32_start(uint8_t core_id, genvaddr_t entry)
{
    /* Copy the startup code to the real-mode address */
    uint8_t *real_dest = (uint8_t*) local_phys_to_mem(X86_32_REAL_MODE_LINEAR_OFFSET);
    uint8_t *real_src = (uint8_t *) &x86_32_start_ap;
    uint8_t *real_end = (uint8_t *) &x86_32_start_ap_end;
    memcpy(real_dest, real_src, real_end - real_src);

    /* Pointer to the entry point called from init_ap.S */
    volatile uint64_t *absolute_entry_ptr = (volatile uint64_t *)
        local_phys_to_mem((lpaddr_t) &x86_32_init_ap_absolute_entry - ((lpaddr_t) &x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);
    //copy the address of the function start (in boot.S) to the long-mode
    //assembler code to be able to perform an absolute jump
    *absolute_entry_ptr = entry;

    /* pointer to the pseudo-lock used to detect boot up of new core */
    volatile uint32_t *ap_wait = (volatile uint32_t *)
        local_phys_to_mem((lpaddr_t) &x86_32_init_ap_wait - ((lpaddr_t) &x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);

    /* Pointer to the lock variable in the realmode code */
    volatile uint8_t *ap_lock = (volatile uint8_t*)
        local_phys_to_mem((lpaddr_t) &x86_32_init_ap_lock - ((lpaddr_t) &x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);

    /* pointer to the shared global variable amongst all kernels */
    volatile uint64_t *ap_global = (volatile uint64_t *)
        local_phys_to_mem((lpaddr_t) &x86_32_init_ap_global - ((lpaddr_t) &x86_32_start_ap) +
                          X86_32_REAL_MODE_LINEAR_OFFSET);
    *ap_global = (uint64_t)mem_to_local_phys((lvaddr_t)global);

    lvaddr_t *init_vector;
    init_vector = (lvaddr_t*)local_phys_to_mem(CMOS_RAM_BIOS_WARM_START_INIT_VECTOR);

    *ap_wait = AP_STARTING_UP;

    //set shutdown status to WARM_SHUTDOWN and set start-vector
    cmos_write( CMOS_RAM_SHUTDOWN_ADDR, CMOS_RAM_WARM_SHUTDOWN);
    *init_vector = X86_32_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(X86_32_REAL_MODE_SEGMENT,
                                                      X86_32_REAL_MODE_OFFSET);

    //INIT 1 assert
    apic_send_init_assert(core_id, xapic_none);

    //set shutdown status to WARM_SHUTDOWN and set start-vector
    cmos_write( CMOS_RAM_SHUTDOWN_ADDR, CMOS_RAM_WARM_SHUTDOWN);
    *init_vector = X86_32_REAL_MODE_ADDR_TO_REAL_MODE_VECTOR(X86_32_REAL_MODE_SEGMENT,
                                                      X86_32_REAL_MODE_OFFSET);

    //INIT 2 de-assert
    apic_send_init_deassert();

    //SIPI1
    apic_send_start_up(core_id, xapic_none,
                       X86_32_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(X86_32_REAL_MODE_SEGMENT));

    //SIPI2
    apic_send_start_up(core_id, xapic_none,
                       X86_32_REAL_MODE_SEGMENT_TO_REAL_MODE_PAGE(X86_32_REAL_MODE_SEGMENT));

    //give the new core a bit time to start-up and set the lock
    for (uint64_t i = 0; i < STARTUP_TIMEOUT; i++) {
        if (*ap_lock != 0) {
            break;
        }
    }

    //if the lock is set, the core has been started, otherwise assume, that
    //a core with this APIC ID doesn't exist.
    if (*ap_lock != 0) {
        while (*ap_wait != AP_STARTED);
        *ap_lock = 0;
        debug(SUBSYS_STARTUP, "booted CPU%hhu\n", core_id);
        return 0;
    }
    return -1;
}
