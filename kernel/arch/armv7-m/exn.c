/*
 * Copyright (c) 2009-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>
#include <arm.h>
#include <arm_hal.h>    //needed for nvic device
#include <exceptions.h>
#include <exec.h>
#include <misc.h>
#include <stdio.h>
#include <wakeup.h>
#include <paging_kernel_arch.h>     //for mmu device
#include <dev/omap/omap44xx_cortex_m3_nvic_dev.h>



//TODO: heteropanda: actually handle the interrupts, instead of aborting

/*
 * Try to find out what address generated page fault, and then tell the dispatcher what
 * happened. Some faults will not be recoverable (e.g. stack faults), because the 
 * context has already been lost
 */
void handle_user_page_fault(arch_registers_state_t* save_area)
{
    lvaddr_t fault_address;//not passed as argument, because there is not just one place to look
 /*   
    //print out registers for debugging
    printf("page fault. registers:\n");
    for(uint32_t i = 0; i<NUM_REGS; i++){
        printf("0x%x\n", save_area->regs[i]);
    }
    uint32_t regval;
    __asm volatile ("mrs %[regval], xpsr" : [regval] "=r"(regval));
    printf("current XPSR register: 0x%x\n", regval);
    
    printf("M3 MMU address: 0x%x\n", *((uint32_t*) &mmu));
    printf("M3 MMU_FAULT_AD register: 0x%x\n", omap44xx_mmu_fault_ad_rd(&mmu));
    printf("M3 MMU_FAULT_STATUS register: 0x%x\n", omap44xx_mmu_fault_status_rd(&mmu));
    printf("M3 MMU_FAULT_PC register: 0x%x\n", omap44xx_mmu_fault_pc_rd(&mmu));
    printf("M3 MMU_IRQSTATUS register: 0x%x\n", omap44xx_mmu_irqstatus_rd(&mmu));
    
    printf("ICTR: 0x%x\n", omap44xx_cortex_m3_nvic_ICTR_rd(&nvic));
    printf("CPUID_BASE: 0x%x\n", omap44xx_cortex_m3_nvic_CPUID_BASE_rd(&nvic));
    printf("ICSR: 0x%x\n", omap44xx_cortex_m3_nvic_ICSR_rd(&nvic));
    printf("VTOR: 0x%x\n", omap44xx_cortex_m3_nvic_VTOR_rd(&nvic));
    printf("AIRCR: 0x%x\n", omap44xx_cortex_m3_nvic_AIRCR_rd(&nvic));
    printf("CCR: 0x%x\n", omap44xx_cortex_m3_nvic_CCR_rd(&nvic));
    printf("SHCSR: 0x%x\n", omap44xx_cortex_m3_nvic_SHCSR_rd(&nvic));
    printf("CFSR: 0x%x\n", omap44xx_cortex_m3_nvic_CFSR_rd(&nvic));
    printf("BFAR: 0x%x\n", omap44xx_cortex_m3_nvic_BFAR_rd(&nvic));
    printf("SYSTICK_CTRL: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CTRL_rd(&nvic));
    printf("SYSTICK_CALV: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CALV_rd(&nvic));
  */ 
    if (omap44xx_cortex_m3_nvic_SHCSR_busfaultact_rdf(&nvic)){
        //triggered by bus fault    
        if (omap44xx_mmu_irqstatus_rd(&mmu)){
            //L2 MMU triggered fault: either no valid mapping, or two mappings in TLB
            //XXX: cachemarker: once we have chaching enabled, this is the place to
            //look at table entry for special permission bits.
            
            //XXX: MMU_FAULT_ADDR register seems to just contain the last address that was
            //requested. By this time this is probably just a kernelspace address.
            //I am not sure if the M3 can actually find out what the faulting address really was
            fault_address = omap44xx_mmu_fault_ad_rd(&mmu);
        }
        else{
            //"regular" bus fault -> look in NVIC entries
            if (omap44xx_cortex_m3_nvic_CFSR_bfarvalid_rdf(&nvic)){
                //bus fault address register valid
                fault_address = omap44xx_cortex_m3_nvic_BFAR_rd(&nvic);
            }
            else{
                //one of the bus faults that do not write the BFAR -> faulting address
                //literally unknown to system
                printk(LOG_WARN, "user bus fault with unknown faulting address\n");
                fault_address = (lvaddr_t) NULL;
            }
        }
    }
    else{
        //memory management fault (probably access violation)
        if (omap44xx_cortex_m3_nvic_CFSR_mmarvalid_rdf(&nvic)){
            //MMAR contains faulting address
            fault_address = omap44xx_cortex_m3_nvic_MMAR_rd(&nvic);
        }
        else{
            //MMAR not written. probably executing in noexecute region
            assert(omap44xx_cortex_m3_nvic_CFSR_iaccviol_rdf(&nvic));
            //so we can assume the pc caused the fault
            fault_address = save_area->named.pc;
        }
    }
    
    lvaddr_t handler;
    struct dispatcher_shared_arm *disp = get_dispatcher_shared_arm(dcb_current->disp);
    uintptr_t saved_pc = save_area->named.pc;

    disp->d.disabled = dispatcher_is_disabled_ip(dcb_current->disp, saved_pc);
    bool disabled = (disp->d.disabled != 0);

    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);

    printk(LOG_WARN, "user page fault%s in '%.*s': addr %"PRIxLVADDR
                      " IP %"PRIxPTR"\n",
           disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
           disp->d.name, fault_address, saved_pc);

    if (disabled) {
        assert(save_area == &disp->trap_save_area);
        handler = disp->d.dispatcher_pagefault_disabled;
        dcb_current->faults_taken++;
    }
    else {
        assert(save_area == &disp->enabled_save_area);
        handler = disp->d.dispatcher_pagefault;
    }

    if (dcb_current->faults_taken > 2) {
        printk(LOG_WARN, "handle_user_page_fault: too many faults, "
               "making domain unrunnable\n");
        dcb_current->faults_taken = 0; // just in case it gets restarted
        scheduler_remove(dcb_current);
        dispatch(schedule());
    }
    else {
        // Upcall to dispatcher


        struct dispatcher_shared_generic *disp_gen =
            get_dispatcher_shared_generic(dcb_current->disp);

        union registers_arm resume_area;

        //make sure we do not accidentaly create an IT block when upcalling
        resume_area.named.cpsr = 0; 

        resume_area.named.pc   = handler;
        resume_area.named.r0   = disp_gen->udisp;
        resume_area.named.r1   = fault_address;
        resume_area.named.r2   = 0;
        resume_area.named.r3   = saved_pc;
        resume_area.named.rtls = disp_gen->udisp;
        resume_area.named.r10  = disp->got_base;
        
        //we need some temporary stack to exit handler mode. memory in userspace would be
        //better, but for the moment we can use this temporary region
        resume_area.named.stack   = (uint32_t) &irq_save_pushed_area_top;
        

        // Upcall user to save area
        disp->d.disabled = true;
        resume(&resume_area);
    }
}


/*
 * \brief handles undefined instructions, unaligned accesses and division by 0
 */
void handle_user_undef(arch_registers_state_t* save_area)
{
    lvaddr_t fault_address = save_area->named.pc;//not passed as argument
    union registers_arm resume_area;

    struct dispatcher_shared_arm *disp = get_dispatcher_shared_arm(dcb_current->disp);

    bool disabled = dispatcher_is_disabled_ip(dcb_current->disp, save_area->named.pc);
    disp->d.disabled = disabled;

    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    if (disabled) {
        assert(save_area == &disp->trap_save_area);
    }
    else {
        assert(save_area == &disp->enabled_save_area);
    }

    printk(LOG_WARN, "user usage fault%s in '%.*s': IP %" PRIuPTR "\n",
           disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
           disp->d.name, fault_address);

    struct dispatcher_shared_generic *disp_gen =
        get_dispatcher_shared_generic(dcb_current->disp);
        
        
    //make sure we do not accidentaly create an IT block when upcalling
    resume_area.named.cpsr = 0;
    resume_area.named.pc   = disp->d.dispatcher_trap;
    resume_area.named.r0   = disp_gen->udisp;
    resume_area.named.r1   = ARM_7M_EVECTOR_USAGE;
    resume_area.named.r2   = 0;
    resume_area.named.r3   = fault_address;
    resume_area.named.rtls = disp_gen->udisp;
    resume_area.named.r10  = disp->got_base;

    //we need some temporary stack to exit handler mode. memory in userspace would be
    //better, but for the moment we can use this temporary region
    resume_area.named.stack   = (uint32_t) &irq_save_pushed_area_top;

    // Upcall user to save area
    disp->d.disabled = true;
    resume(&resume_area);
}

/*
//XXX: probably broken since I had to change bkpt for thumb
static int32_t bkpt_decode(lvaddr_t fault_address)
{
    int32_t bkpt_id = -1;
    if ((fault_address & 3) == 0 && fault_address >= KERNEL_OFFSET) {
        const uint32_t bkpt_mask = 0xfff000f0;
        const uint32_t bkpt_isn  = 0xe1200070;

        uintptr_t isn = *((uintptr_t*)fault_address);
        if ((isn & bkpt_mask) == bkpt_isn) {
            bkpt_id = (int32_t)((isn & 0xf) | ((isn & 0xfff00) >> 4));
        }
    }
    return bkpt_id;
}
*/

void fatal_kernel_fault(uint32_t evector, lvaddr_t address, arch_registers_state_t* save_area
    )
{
//TODO: heteropanda: clean this up and print something sensible here
    int i;
    printk(LOG_PANIC, "Kernel fault at %08"PRIxLVADDR
                      " vector %08"PRIx32"\n\n", address, evector);
    printk(LOG_PANIC, "Processor save_area at: %p\n", save_area);

    for (i = 0; i < 16; i++) {
        const char *extrainfo = "";

        switch(i) {
        case 13:
            extrainfo = "\t(sp)";
            break;

        case 14:
            extrainfo = "\t(lr)";
            break;

        case 15:
            {
                char str[128];
                snprintf(str, 128, "\t(pc)\t%08lx",
                         save_area->regs[R0_REG + i] -
                         local_phys_to_mem((uint32_t)&kernel_first_byte) +
                         0x100000);
                extrainfo = str;
            }
            break;
        }

        printk(LOG_PANIC, "r%d\t%08"PRIx32"%s\n", i, save_area->regs[R0_REG + i], extrainfo);
    }
    printk(LOG_PANIC, "xpsr\t%08"PRIx32"\n", save_area->regs[CPSR_REG]);
    printk(LOG_PANIC, "called from: %p\n", __builtin_return_address(0) -
           local_phys_to_mem((uint32_t)&kernel_first_byte) + 0x100000);


    printk(LOG_PANIC,"Error registers:\n");
    printk(LOG_PANIC,"M3 MMU address: 0x%x\n", *((uint32_t*) &mmu));
    printk(LOG_PANIC,"M3 MMU_FAULT_AD register: 0x%x\n", omap44xx_mmu_fault_ad_rd(&mmu));
    printk(LOG_PANIC,"M3 MMU_FAULT_STATUS register: 0x%x\n", omap44xx_mmu_fault_status_rd(&mmu));
    printk(LOG_PANIC,"M3 MMU_IRQSTATUS register: 0x%x\n", omap44xx_mmu_irqstatus_rd(&mmu));
    
    printk(LOG_PANIC,"ICTR: 0x%x\n", omap44xx_cortex_m3_nvic_ICTR_rd(&nvic));
    printk(LOG_PANIC,"CPUID_BASE: 0x%x\n", omap44xx_cortex_m3_nvic_CPUID_BASE_rd(&nvic));
    printk(LOG_PANIC,"ICSR: 0x%x\n", omap44xx_cortex_m3_nvic_ICSR_rd(&nvic));
    printk(LOG_PANIC,"VTOR: 0x%x\n", omap44xx_cortex_m3_nvic_VTOR_rd(&nvic));
    printk(LOG_PANIC,"AIRCR: 0x%x\n", omap44xx_cortex_m3_nvic_AIRCR_rd(&nvic));
    printk(LOG_PANIC,"CCR: 0x%x\n", omap44xx_cortex_m3_nvic_CCR_rd(&nvic));
    printk(LOG_PANIC,"SHCSR: 0x%x\n", omap44xx_cortex_m3_nvic_SHCSR_rd(&nvic));
    printk(LOG_PANIC,"CFSR: 0x%x\n", omap44xx_cortex_m3_nvic_CFSR_rd(&nvic));
    printk(LOG_PANIC,"BFAR: 0x%x\n", omap44xx_cortex_m3_nvic_BFAR_rd(&nvic));
    printk(LOG_PANIC,"SYSTICK_CTRL: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CTRL_rd(&nvic));
    printk(LOG_PANIC,"SYSTICK_CALV: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CALV_rd(&nvic));

    switch (evector) {
        case ARM_7M_EVECTOR_USAGE:
        //TODO: heteropanda: distinguish further
            panic("Usage fault.\n");
            break;

        case ARM_7M_EVECTOR_BUS:
          //TODO: heteropanda: distinguish further
            panic("Bus fault\n");
            break;
      //TODO: heteropanda: distinguish further
      default:
        panic("Caused by evector: %02"PRIx32, evector);
        break;
    }

}



/*
 * \brief handler pretty much any usermode IRQ except system calls
 */
void handle_irq(uint32_t irq, arch_registers_state_t* save_area)
{
    /*
    printf("handle_irq: registers:\n");//dump content for debugging reasons
    for(uint32_t i = 0; i<NUM_REGS; i++){
        printf("0x%x\n", save_area->regs[i]);
    }*/
    
  /*  
    uint32_t regval;
    __asm volatile ("mrs %[regval], xpsr" : [regval] "=r"(regval));
    printf("current XPSR register: 0x%x\n", regval);
    
    printf("M3 MMU address: 0x%x\n", *((uint32_t*) &mmu));
    printf("M3 MMU_FAULT_AD register: 0x%x\n", omap44xx_mmu_fault_ad_rd(&mmu));
    printf("M3 MMU_FAULT_STATUS register: 0x%x\n", omap44xx_mmu_fault_status_rd(&mmu));
    printf("M3 MMU_FAULT_PC register: 0x%x\n", omap44xx_mmu_fault_pc_rd(&mmu));
    printf("M3 MMU_IRQSTATUS register: 0x%x\n", omap44xx_mmu_irqstatus_rd(&mmu));
    
    printf("ICTR: 0x%x\n", omap44xx_cortex_m3_nvic_ICTR_rd(&nvic));
    printf("CPUID_BASE: 0x%x\n", omap44xx_cortex_m3_nvic_CPUID_BASE_rd(&nvic));
    printf("ICSR: 0x%x\n", omap44xx_cortex_m3_nvic_ICSR_rd(&nvic));
    printf("VTOR: 0x%x\n", omap44xx_cortex_m3_nvic_VTOR_rd(&nvic));
    printf("AIRCR: 0x%x\n", omap44xx_cortex_m3_nvic_AIRCR_rd(&nvic));
    printf("CCR: 0x%x\n", omap44xx_cortex_m3_nvic_CCR_rd(&nvic));
    printf("SHCSR: 0x%x\n", omap44xx_cortex_m3_nvic_SHCSR_rd(&nvic));
    printf("CFSR: 0x%x\n", omap44xx_cortex_m3_nvic_CFSR_rd(&nvic));
    printf("BFAR: 0x%x\n", omap44xx_cortex_m3_nvic_BFAR_rd(&nvic));
    printf("SYSTICK_CTRL: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CTRL_rd(&nvic));
    printf("SYSTICK_CALV: 0x%x\n", omap44xx_cortex_m3_nvic_SYSTICK_CALV_rd(&nvic));
    */
    uintptr_t fault_pc = save_area->named.pc;//read faulting pc from pushed context
    
    debug(SUBSYS_DISPATCH, "IRQ %"PRIu32" while %s\n", irq,
          dcb_current ? (dcb_current->disabled ? "disabled": "enabled") : "in kernel");


    if (dcb_current != NULL) {
        dispatcher_handle_t handle = dcb_current->disp;
        if (save_area == dispatcher_get_disabled_save_area(handle)) {
            assert(dispatcher_is_disabled_ip(handle, fault_pc));
            dcb_current->disabled = true;
        } else {
/*            debug(SUBSYS_DISPATCH,
                  "save_area=%p, dispatcher_get_enabled_save_are(handle)=%p\n",
                   save_area, dispatcher_get_enabled_save_area(handle));
*/

            assert(save_area == dispatcher_get_enabled_save_area(handle));
            assert(!dispatcher_is_disabled_ip(handle, fault_pc));
            dcb_current->disabled = false;
        }
    }
    //TODO: heteropanda: make a case distinction on the type of interrupt, and  
    //actually handle it

    if (irq == ARM_7M_EVECTOR_SYSTICK) {
        // Timer interrupt
        assert(kernel_ticks_enabled);
        kernel_now += kernel_timeslice;
        wakeup_check(kernel_now);
        printf(".");//to see how many we get & their distribution, without spamming lines
        dispatch(schedule());
    }
    else {
        // send_user_interrupt(irq);
        panic("Unhandled IRQ %"PRIu32"\n", irq);
    }

}
