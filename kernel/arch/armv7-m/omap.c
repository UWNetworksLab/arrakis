/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Abstraction layer for cortex-m3
 * mostly NVIC and SysTick
 */

#include <kernel.h>
#include <paging_kernel_arch.h>

#include <dev/omap/omap44xx_cortex_m3_nvic_dev.h>

#include <arm_hal.h>



//not sure where to put this (or what to call it - should this go into omap44xx_map.h?)
#define OMAP_CORTEX_M3_NVIC_BASE 0xE000E000
#define OMAP_CORTEX_M3_NVIC_SIZE 0x1000         //4KB


//hardcoded bc gem5 doesn't set board id in ID_Register
//XXX: change so it makes sense for the pandaboard -SG
#define VEXPRESS_ELT_BOARD_ID		0x8e0
uint32_t hal_get_board_id(void)
{
    return VEXPRESS_ELT_BOARD_ID;
}

uint8_t hal_get_cpu_id(void)
{
    return 0;//XXX: for the moment, pretend to be the bsp
}

bool hal_cpu_is_bsp(void)
{
    return hal_get_cpu_id() == 0;
}



/*
 * \brief initialize NVIC
 * IMPORTANT: the NVIC is in a private section, which means it can NOT be mapped into
 * memory where we want it. ALL accesses to virtual address 0xE000XXXX will ALWAYS
 * go to this private region regardless of any translation rules 
 */
void nvic_init(void){
    omap44xx_cortex_m3_nvic_initialize(&nvic, (mackerel_addr_t)OMAP_CORTEX_M3_NVIC_BASE);
    printf("NVIC: %d interrupt lines detected\n", (omap44xx_cortex_m3_nvic_ICTR_rd(&nvic)+1)*32);
    
#if defined(GLOBAL_DEBUG)     //additional debug output
    printf("ICTR: 0x%x\n", omap44xx_cortex_m3_nvic_ICTR_rd(&nvic));
    printf("CPUID_BASE: 0x%x\n", omap44xx_cortex_m3_nvic_CPUID_BASE_rd(&nvic));
    printf("ICSR: 0x%x\n", omap44xx_cortex_m3_nvic_ICSR_rd(&nvic));
    printf("VTOR: 0x%x\n", omap44xx_cortex_m3_nvic_VTOR_rd(&nvic));
    printf("AIRCR: 0x%x\n", omap44xx_cortex_m3_nvic_AIRCR_rd(&nvic));
    printf("CCR: 0x%x\n", omap44xx_cortex_m3_nvic_CCR_rd(&nvic));
    printf("SHCSR: 0x%x\n", omap44xx_cortex_m3_nvic_SHCSR_rd(&nvic));
    printf("CFSR: 0x%x\n", omap44xx_cortex_m3_nvic_CFSR_rd(&nvic));
    printf("SYSTICK_CTRL: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CTRL_rd(&nvic));
    printf("SYSTICK_CALV: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CALV_rd(&nvic));
#endif    //defined(GLOBAL_DEBUG)

    //allow return to thread mode from any handler by using the right return address
    omap44xx_cortex_m3_nvic_CCR_nonbasethrdena_wrf(&nvic, 1);
    //allow stack to be aligned to "only" 4 bytes on entry
    omap44xx_cortex_m3_nvic_CCR_stkalign_wrf(&nvic, 0);
    
//TODO: set some register values:
//  - set priotiry grouping? (which priorities do not preempt each other)
//  - set priorities
    enable_all_system_interrupts();
}


/*
 * \brief enable a specific interrupt on this core
 */
void nvic_enable_interrupt(uint32_t int_id, uint16_t prio){
    printf("nvic_enable_interrupt called -> implement ----------------\n");
//TODO: heteropanda: actually implement
//decide on a enumeration (should "0" be vectortable offset 0 or external interrupt 0 (offset 16)? probably the latter)
//find out which register to set (two sets of 32 each to choose from)
//first write priority
//write set enable
}


uint32_t nvic_get_active_irq(void)
{
	return omap44xx_cortex_m3_nvic_ICSR_vectactive_rdf(&nvic);
}


/*
 * \brief enable all system interrupts (IRQ 2 - 15) that we might need
 */
void enable_all_system_interrupts(void){
    //TODO: maybe set priorities?
    omap44xx_cortex_m3_nvic_SHCSR_usgfaultena_wrf(&nvic, 1);
    omap44xx_cortex_m3_nvic_SHCSR_busfaultena_wrf(&nvic, 1);
    omap44xx_cortex_m3_nvic_SHCSR_memfaultena_wrf(&nvic, 1);
    //service calls, NMI, hardfaults are always enabled
    //SysTick interrupts are enabled in systick_init
    
    //enable interrupts on division by 0 or unaligned memory access
    omap44xx_cortex_m3_nvic_CCR_div_0_trp_wrf(&nvic, 1);
    omap44xx_cortex_m3_nvic_CCR_unalign_trp_wrf(&nvic, 1);
}

/*
 * \brief set up SysTick, assumes nvic_init has already been called
 * as there is no clock reference on pandaboard, we count actual cycles
 */
void systick_init(uint32_t tick_cycles){
    //we do not actually need to map any memory here - SysTick is a subdevice of NVIC
    
    //use core clock (so we count processor cycles)
    omap44xx_cortex_m3_nvic_SYSTICK_CTRL_clksource_wrf(&nvic,1);
    //enable interrupts when countdown reaches 0
    omap44xx_cortex_m3_nvic_SYSTICK_CTRL_tickint_wrf(&nvic,1);
    
    //number of cycles between interrupts (only 24 bit value!)
    omap44xx_cortex_m3_nvic_SYSTICK_RV_reload_wrf(&nvic, tick_cycles);
}

/*
 * \brief start SysTick timer and set value to reload value
 */
void systick_start(void){
    omap44xx_cortex_m3_nvic_SYSTICK_CRTV_wr(&nvic,0);//reset counter
    omap44xx_cortex_m3_nvic_SYSTICK_CTRL_enable_wrf(&nvic,1);//run timer
}

/*
 * \brief stop SysTick timer
 */
void systick_stop(void){
    omap44xx_cortex_m3_nvic_SYSTICK_CTRL_enable_wrf(&nvic,0);//stop timer
}


//only here so I do not have to change pure debug code
inline uint32_t tsc_read(void){
    printf("tsc_read called, ignored\n");
    return 0;
}

inline uint32_t tsc_get_hz(void){
    printf("tsc_get_hz called, ignored\n");
    return 0;
}
