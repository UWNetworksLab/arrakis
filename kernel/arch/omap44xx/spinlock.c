/**
 * \file
 * \brief kernel driver for the spinlock module, used for serial output
 * see OMAP4460 TRM chapter 21 for a functional description
 */

/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <arm.h>
#include <paging_kernel_arch.h>

#include <spinlock.h>

#include <dev/omap/omap44xx_spinlock_dev.h>
#include <omap44xx_map.h>

//
// spinlock interface
//
#define NUM_LOCKS 32
#define PRINTF_LOCK 0   //use lock number 0 for serial interface


static omap44xx_spinlock_t spinlock;

static bool locks_initialized;  //allows early initialization

//map the spinlock device into memory and reset it
errval_t spinlock_init(void)
{
    if (locks_initialized) {
	printf("spinlock_init: already initialized; skipping.\n");
	return SYS_ERR_OK;
    }
    
    lvaddr_t base = paging_map_device(OMAP44XX_MAP_L4_CFG_SPINLOCK, OMAP44XX_MAP_L4_CFG_SPINLOCK_SIZE);
    // paging_map_device returns an address pointing to the beginning of
    // a section, need to add the offset for within the section again
    uint32_t offset = (OMAP44XX_MAP_L4_CFG_SPINLOCK & ARM_L1_SECTION_MASK);
    printf("spinlock_init: base = 0x%"PRIxLVADDR" 0x%"PRIxLVADDR"\n", base, base + offset);
    omap44xx_spinlock_initialize(&spinlock, (mackerel_addr_t)base + offset);

    omap44xx_spinlock_sysconfig_softreset_wrf(&spinlock, 1);//reset module (XXX probably not needed)
    
    printf("testing spinlock: first read (should be 0): 0x%x\n", 
                omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, 2));
                
    printf("testing spinlock: second read (should be 1): 0x%x\n", 
                omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, 2));
                
    omap44xx_spinlock_lock_reg_i_taken_wrf(&spinlock, 2, 0);//clear lock            
                
    locks_initialized = true;
    
    printf("spinlock_init: done.\n");
    return SYS_ERR_OK;
}

//directly use physical address
errval_t spinlock_early_init(void)
{
    assert(locks_initialized == 0);
    omap44xx_spinlock_initialize(&spinlock, (mackerel_addr_t)OMAP44XX_MAP_L4_CFG_SPINLOCK);
    return SYS_ERR_OK;
}

//chose this name instead of "aquire_spinlock", because the usage is different
//(this takes the number of the spinlock instead of an address in memory)
inline void spinlock_aquire(int locknumber){
    assert(locknumber < NUM_LOCKS);//we only have 32 hardware spinlocks
    //reading "0" means we have the lock, "1" means we have to try again
    while(omap44xx_spinlock_lock_reg_i_taken_rdf(&spinlock, locknumber)){}
}

inline void spinlock_release(int locknumber){
    omap44xx_spinlock_lock_reg_i_taken_wrf(&spinlock, locknumber, 0);//clears lock
}

