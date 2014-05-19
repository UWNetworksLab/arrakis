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
#include <arm_hal.h>
#include <cp15.h>
#include <exceptions.h>
#include <exec.h>
#include <misc.h>
#include <stdio.h>
#include <wakeup.h>
#include <irq.h>

void handle_user_page_fault(lvaddr_t                fault_address,
                            arch_registers_state_t* save_area)
{
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
        //
        // Upcall to dispatcher
        //
        // NB System might be cleaner with a prototype
        // dispatch context that has R0-R3 to be overwritten
        // plus initial stack, thread, and gic registers. Could do
        // a faster resume_for_upcall().
        //

        struct dispatcher_shared_generic *disp_gen =
            get_dispatcher_shared_generic(dcb_current->disp);

        union registers_arm resume_area;

        resume_area.named.cpsr = CPSR_F_MASK | ARM_MODE_USR;
        resume_area.named.pc   = handler;
        resume_area.named.r0   = disp_gen->udisp;
        resume_area.named.r1   = fault_address;
        resume_area.named.r2   = 0;
        resume_area.named.r3   = saved_pc;
        resume_area.named.rtls = disp_gen->udisp;
        resume_area.named.r10  = disp->got_base;

        // SP is set by handler routine.

        // Upcall user to save area
        disp->d.disabled = true;
        resume(&resume_area);
    }
}

void handle_user_undef(lvaddr_t fault_address,
                       arch_registers_state_t* save_area)
{
    union registers_arm resume_area;

    struct dispatcher_shared_arm *disp = get_dispatcher_shared_arm(dcb_current->disp);

    bool disabled = dispatcher_is_disabled_ip(dcb_current->disp, save_area->named.pc);
    disp->d.disabled = disabled;

    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    if (disabled) {
        //        assert(save_area == &disp->trap_save_area);
    }
    else {
        assert(save_area == &disp->enabled_save_area);
    }

    printk(LOG_WARN, "user undef fault%s in '%.*s': IP %" PRIuPTR "\n",
           disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
           disp->d.name, fault_address);

    struct dispatcher_shared_generic *disp_gen =
        get_dispatcher_shared_generic(dcb_current->disp);

    resume_area.named.cpsr = CPSR_F_MASK | ARM_MODE_USR;
    resume_area.named.pc   = disp->d.dispatcher_trap;
    resume_area.named.r0   = disp_gen->udisp;
    resume_area.named.r1   = ARM_EVECTOR_UNDEF;
    resume_area.named.r2   = 0;
    resume_area.named.r3   = fault_address;
    resume_area.named.rtls = disp_gen->udisp;
    resume_area.named.r10  = disp->got_base;

    // Upcall user to save area
    disp->d.disabled = true;
    resume(&resume_area);
}

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

void fatal_kernel_fault(uint32_t evector, lvaddr_t address, arch_registers_state_t* save_area
    )
{
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
    printk(LOG_PANIC, "cpsr\t%08"PRIx32"\n", save_area->regs[CPSR_REG]);
    printk(LOG_PANIC, "called from: %p\n", __builtin_return_address(0) -
           local_phys_to_mem((uint32_t)&kernel_first_byte) + 0x100000);

    switch (evector) {
        case ARM_EVECTOR_UNDEF:
            panic("Undefined instruction.\n");
            break;

      case ARM_EVECTOR_PABT: {
            int ifsr = cp15_read_ifsr();
            if (ifsr == 0) {
                int bkpt = bkpt_decode(address);
                if (bkpt >= 0) {
                    panic("Breakpoint: %4x\n", bkpt);
                }
            }
            panic("Prefetch abort: ifsr %08x\n", ifsr);
      }
      break;

      case ARM_EVECTOR_DABT:
          {
              uint32_t dfsr = cp15_read_dfsr();

              printf("\n");

              if((dfsr >> 11) & 1) {
                  printf("On write access\n");
              } else {
                  printf("On read access\n");
              }

              switch((dfsr & 0xf) | (dfsr & 0x400)) {
              case 1:
                  printf("Alignment fault\n");
                  break;

              case 4:
                  printf("Instruction cache-maintenance fault\n");
                  break;

              case 5:
                  printf("Translation fault on section\n");
                  break;

              case 6:
                  printf("Translation fault on page\n");
                  break;

              case 8:
                  printf("Synchronous external abort\n");
                  break;

              default:
                  printf("Unknown fault\n");
                  break;
              }

              panic("Data abort: dfsr %08"PRIx32"\n", dfsr);
          }

      default:
        panic("Caused by evector: %02"PRIx32, evector);
        break;
    }
}

void handle_irq(arch_registers_state_t* save_area, uintptr_t fault_pc)
{
    uint32_t irq = 0;
#if defined(__ARM_ARCH_7A__)
    irq = gic_get_active_irq();
#else
    // this is for ARMv5, -SG
    irq = pic_get_active_irq();
#endif

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

    if (pit_handle_irq(irq)) {
        // Timer interrupt, pit_handle_irq acks it at the timer.
        assert(kernel_ticks_enabled);
        kernel_now += kernel_timeslice;
        wakeup_check(kernel_now);
        dispatch(schedule());
    }
    // this is the (still) unacknowledged startup interrupt sent by the BSP
    // we just acknowledge it here
    else if(irq == 1)
    {
#if defined(__ARM_ARCH_7A__)
    	gic_ack_irq(irq);
#else
        // this is for ARMv5, -SG
        pic_ack_irq(irq);
#endif
    	dispatch(schedule());
    }
    else {
#if defined(__ARM_ARCH_7A__)
        gic_ack_irq(irq);
        send_user_interrupt(irq);
        panic("Unhandled IRQ %"PRIu32"\n", irq);
#else
        // SK: No support for user-level interrupts on ARMv5 and XScale
        panic("Unhandled IRQ %"PRIu32". User-level IRQs only supported on ARMv7!\n", irq);
#endif
    }

}
