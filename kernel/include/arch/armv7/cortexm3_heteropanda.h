/**
 * \file
 * \brief header for
 *  heteropanda-specific setup code
 *  assumes that there is a heteropanda_slave image,
 *  sets up the cortex-m3 processor by:
 *      loading and relocating the heteropanda_slave image into memory
 *      setting the clock signals for the MMU and processor
 *      setting up the L2 MMU TLB to contain the mappings necessary to bootstrap
 *      writing the necessary entries in the vectortable
 *      actually starting the processor
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CORTEXM3_HETEROPANDA_H
#define CORTEXM3_HETEROPANDA_H

//only do anything if the heteropanda config option has been chosen
#if defined(HETEROPANDA)

//cortex-m3 specific signals
#define CM_MPU_M3_MPU_M3_CLKCTRL 0x4a008920
#define CM_MPU_M3_CLKSTCTRL 0x4a008900
#define RM_MPU_M3_RSTST 0x4a306914
#define RM_MPU_M3_RSTCTRL 0x4a306910

#define SLAVE_LOADED_START    0x90000000  //address where the loaded image will end up at
//preferably corresponds to mmap start address of slave image

//initialize mackerel devices. assumes direct access to physica address, without remapping
//device to high memory
void cortex_m3_early_init(void);

//set up and run heteropanda_slave image on cortex-m3
void prepare_and_start_m3(void* start_image);


#endif  //defined(HETEROPANDA)



#endif  //CORTEXM3_HETEROPANDA_H
