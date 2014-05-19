/**
 * \file
 * \brief Definitions for the startup of application processors.
 *
 *  This file contains the prototypes for the functions which start
 *  the application processors
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef START_APS_H_
#define START_APS_H_

#if defined(__gem5__)

    #define AP_STARTING_UP 1
    #define AP_STARTED     2

    //#define AP_LOCK_PHYS	0x20000
    #define AP_WAIT_PHYS	0x20000
    #define AP_GLOBAL_PHYS	0x21000

#elif defined(__pandaboard__)

    #define AP_STARTING_UP  4422
    #define AP_STARTED      6633
    #define AP_WAIT_PHYS    ((lpaddr_t)0x80020000)
    #define AP_GLOBAL_PHYS  ((lpaddr_t)0x80021000)
    #define AUX_CORE_BOOT_0 ((lpaddr_t)0x48281800)
    #define AUX_CORE_BOOT_1 ((lpaddr_t)0x48281804)

    // address of the section needed to map AUX_CORE vars
    #define AUX_CORE_BOOT_SECT       (AUX_CORE_BOOT_0 & ~ARM_L1_SECTION_MASK)
    // offset of AUX_CORE_BOOT_0 in the section
    #define AUX_CORE_BOOT_0_OFFSET   (AUX_CORE_BOOT_0 & ARM_L1_SECTION_MASK)
    // offset of AUX_CORE_BOOT_1 in the section
    #define AUX_CORE_BOOT_1_OFFSET   (AUX_CORE_BOOT_1 & ARM_L1_SECTION_MASK)

#else
    #error "Unknown ARM arch"
#endif

int start_aps_arm_start(uint8_t core_id, lvaddr_t entry);

#endif // START_APS_H_
