/**
 * \file
 * \brief x86-32 interrupt/exception handling utility functions
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*********************************************************************
 *
 * Copyright (C) 2003-2004,  Karlsruhe University
 *
 * File path:     glue/v4-amd64/hwirq.h
 * Description:   Macros to define interrupt handler stubs for AMD64
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $Id: hwirq.h,v 1.3 2006/10/19 22:57:35 ud3 Exp $
 *
 ********************************************************************/

#include <kernel.h>
#include <stdio.h>
#include <string.h>
#include <irq.h>
#include <gdb_stub.h>
#include <x86.h>
#include <dispatch.h>
#include <wakeup.h>
#include <arch/x86/pic.h>
#include <arch/x86/apic.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <asmoffsets.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <exec.h>
#ifdef __scc__
#       include <rck.h>
#else
#       include <arch/x86/ipi_notify.h>
#endif
#include <arch/x86/timing.h>
#include <arch/x86/syscall.h>
#include <barrelfish_kpi/cpu_arch.h>

#ifdef FPU_LAZY_CONTEXT_SWITCH
#  include <fpu.h>
#endif

/**
 * \brief Define IRQ handler number 'num'.
 *
 * This defines an interrupt handler for vector #num. The way this is done is
 * quite tricky: A block of assembly is emitted, with a label pointing to
 * the beginning of that block. The label is made known as a symbol by
 * having a C function _declaration_ directly in front of the block. The
 * symbol has to be defined extern, so it is global, but its ELF visibility
 * is set "hidden", so that the symbol does not end up in the GOT. This is
 * very important for keeping the code position-independent.
 *
 * The NOERR/ERR variants depend on whether the hardware delivers an error code.
 */
#define HW_EXCEPTION_NOERR(num)                                         \
    void __attribute__ ((visibility ("hidden"))) hwexc_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwexc_"#num",@function                 \n\t"        \
           "hwexc_"#num":                                  \n\t"        \
           "pushl $0                /* dummy error code */ \n\t"        \
           "pushl $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

#define HW_EXCEPTION_ERR(num)                                           \
    void __attribute__ ((visibility ("hidden"))) hwexc_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwexc_"#num",@function                 \n\t"        \
           "hwexc_"#num":                                  \n\t"        \
           "pushl $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

#define XHW_IRQ(num)                                                    \
    void __attribute__ ((visibility ("hidden"))) hwirq_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwirq_"#num",@function                 \n\t"        \
           "hwirq_"#num":                                  \n\t"        \
           "pushl $"#num"           /* vector number */    \n\t"        \
           "jmp    hwirq_common     /* common stuff */     \n\t"        \
                                                                        )
/// Noop wrapper for HW_IRQ to deal with CPP stringification problems
#define HW_IRQ(num) XHW_IRQ(num)

#define STR(x) #x
#define XTR(x) STR(x)

__asm (
    ".text                                              \n\t"
    "   .type hwexc_common ,@function                   \n\t"
    "hwexc_common:                                      \n\t"
    "testb $3, 12(%esp) /* if CS.CPL == 0 */            \n\t"
    "jz kernel_fault                                    \n\t"

    /* User exception: save full state and return to the user.
     * This path could be optimized by only saving the non-volatile
     * registers (since the kernel's C path will maintain them), and
     * having the user-mode code save them if needed. Since the
     * current user code always does need them, we just save the full
     * set here. */

    /* decide where to save the state, the options are:
     *    pagefault and enabled -> enabled save area
     *    pagefault while disabled or any other trap -> trap save area
     */
    "pushl %ecx                                         \n\t"
    "call __i686.get_pc_thunk.cx                        \n\t"
    "addl $_GLOBAL_OFFSET_TABLE_, %ecx                  \n\t"
    "movl dcb_current@GOTOFF(%ecx), %ecx /* ecx = dcb_current */       \n\t"
    "movl "XTR(OFFSETOF_DCB_DISP)"(%ecx), %ecx /* ecx = dcb_current->disp */\n\t"
    "cmpl $14, 4(%esp)       /* is pagefault? */        \n\t"
    "jne save_trap                                      \n\t"
    "cmpl $0, "XTR(OFFSETOF_DISP_DISABLED)"(%ecx) /* disp->disabled ? */\n\t"
    "jne save_trap                                      \n\t"
    "pushl %ebx                                         \n\t"
    "movl 4*4(%esp), %ebx     /* ebx = faulting IP */   \n\t"
    "cmpl "XTR(OFFSETOF_DISP_X86_32_CRIT_PC_LOW)"(%ecx), %ebx /* crit_pc_low <= rip? */\n\t"
    "jae disabled_test                                  \n\t"
    "\nsave_enabled:                                    \n\t"
    "popl %ebx                                          \n\t"
    "addl $"XTR(OFFSETOF_DISP_X86_32_ENABLED_AREA)", %ecx /* ecx = enabled_save_area */\n\t"
    "jmp do_save                                        \n\t"
    "\ndisabled_test:                                   \n\t"
    "cmpl "XTR(OFFSETOF_DISP_X86_32_CRIT_PC_HIGH)"(%ecx), %ebx /* crit_pc_high > rip? */\n\t"
    "jae save_enabled                                   \n\t"
    "popl %ebx                                          \n\t"
    "\nsave_trap:                                       \n\t"
    "addl $"XTR(OFFSETOF_DISP_X86_32_TRAP_AREA)", %ecx /* trap_save_area */\n\t"

    /* save to the save area. at this point, ecx = save area ptr,
     * esp+8 = exception num, esp+16 = CPU-stacked error and registers */
    "\ndo_save:                                         \n\t"
    "movl %eax,  0*4(%ecx)                              \n\t"
    "popl %eax                    /* original ecx */    \n\t"
    "movl %ebx,  1*4(%ecx)                              \n\t"
    "movl %eax,  2*4(%ecx)                              \n\t"
    "movl %edx,  3*4(%ecx)                              \n\t"
    "movl %esi,  4*4(%ecx)                              \n\t"
    "movl %edi,  5*4(%ecx)                              \n\t"
    "movl %ebp,  6*4(%ecx)                              \n\t"
    "mov %fs, "XTR(OFFSETOF_FS_REG)"(%ecx)              \n\t"
    "mov %gs, "XTR(OFFSETOF_GS_REG)"(%ecx)              \n\t"
    "pushl %ecx                                         \n\t"
    "lea 3*4(%esp), %ecx                                \n\t"
    "pushl %ecx                                         \n\t"
    "calll generic_handle_user_exception                \n\t"
    /* Get all the function arguments off the stack again */
    "addl $4*4, %esp                                    \n\t"
    // Load disp->udisp into EDI
    "call __i686.get_pc_thunk.di                        \n\t"
    "addl $_GLOBAL_OFFSET_TABLE_, %edi                  \n\t"
    "movl dcb_current@GOTOFF(%edi), %edi /* edi = dcb_current */       \n\t"
    "movl "XTR(OFFSETOF_DCB_DISP)"(%edi), %edi /* edi = dcb_current->disp */\n\t"
    "movl "XTR(OFFSETOF_DISP_UDISP)"(%edi), %edi /* edi = disp->udisp */\n\t"
    "iretl                                              \n\t"

    /* a kernel fault means something bad happened, so we stack
     * everything for the debugger to use, in the GDB frame format */
    "\nkernel_fault:                                    \n\t"
    "pushl $0x10     /* SS */                           \n\t"
    "pushl 4*4(%esp) /* CS */                           \n\t"
    "pushl 6*4(%esp) /* EFLAGS */                       \n\t"
    "pushl 5*4(%esp) /* EIP */                          \n\t"
    "pushl %esp      /* ESP */                          \n\t"
    "pushl %ebp                                         \n\t"
    "pushl %edi                                         \n\t"
    "pushl %esi                                         \n\t"
    "pushl %edx                                         \n\t"
    "pushl %ecx                                         \n\t"
    "pushl %ebx                                         \n\t"
    "pushl %eax                                         \n\t"
    "pushl %esp              /* save area ptr*/         \n\t"
    "pushl 14*4(%esp)        /* error code   */         \n\t"
    "pushl 14*4(%esp)        /* vector number */        \n\t"
    "call generic_handle_kernel_exception               \n\t"

    /* (Device) interrupt. */
    "   .type hwirq_common ,@function                   \n\t"
    "hwirq_common:                                      \n\t"
    /* If happened in kernel_mode, simply make userspace runnable */
    "testb $3, 8(%esp)  /* if CS.CPL == 0 */            \n\t"
    "jz call_handle_irq                                 \n\t"

    /* Happened in user mode.
     * we need to save everything to the dispatcher. */
    /* decide where to save the state, either enabled or disabled save areas */
    "pushl %edx                                         \n\t"
    "call __i686.get_pc_thunk.dx                        \n\t"
    "addl $_GLOBAL_OFFSET_TABLE_, %edx                  \n\t"
    "movl dcb_current@GOTOFF(%edx), %edx /* edx = dcb_current */       \n\t"
    "movl "XTR(OFFSETOF_DCB_DISP)"(%edx), %edx /* edx = dcb_current->disp */\n\t"
    "cmpl $0, "XTR(OFFSETOF_DISP_DISABLED)"(%edx) /* disp->disabled ? */\n\t"
    "jne irq_save_disabled                              \n\t"
    "pushl %ebx                                         \n\t"
    "movl 3*4(%esp), %ebx     /* ebx = faulting IP */   \n\t"
    "cmpl "XTR(OFFSETOF_DISP_X86_32_CRIT_PC_LOW)"(%edx), %ebx /* crit_pc_low <= rip? */\n\t"
    "jae irq_disabled_test                              \n\t"
    "\nirq_save_enabled:                                \n\t"
    "popl %ebx                                          \n\t"
    "addl $"XTR(OFFSETOF_DISP_X86_32_ENABLED_AREA)", %edx /* edx = enabled_save_area */\n\t"
    "jmp irq_do_save                                    \n\t"
    "\nirq_disabled_test:                               \n\t"
    "cmpl "XTR(OFFSETOF_DISP_X86_32_CRIT_PC_HIGH)"(%edx), %ebx /* crit_pc_high > rip? */\n\t"
    "jae irq_save_enabled                               \n\t"
    "popl %ebx                                          \n\t"
    "\nirq_save_disabled:                               \n\t"
    "addl $"XTR(OFFSETOF_DISP_X86_32_DISABLED_AREA)", %edx /* disabled_save_area */\n\t"

    /* save to the save area. at this point, edx = save area ptr,
     * esp+8 = vector number, esp+12 = CPU-stacked regisers */
    "\nirq_do_save:                                     \n\t"
    "movl %eax,  0*4(%edx)                              \n\t"
    "movl %ebx,  1*4(%edx)                              \n\t"
    "movl %ecx,  2*4(%edx)                              \n\t"
    "popl %eax                    /* original edx */    \n\t"
    "movl %eax,  3*4(%edx)                              \n\t"
    "movl %esi,  4*4(%edx)                              \n\t"
    "movl %edi,  5*4(%edx)                              \n\t"
    "movl %ebp,  6*4(%edx)                              \n\t"
    "mov %fs, "XTR(OFFSETOF_FS_REG)"(%edx)              \n\t"
    "mov %gs, "XTR(OFFSETOF_GS_REG)"(%edx)              \n\t"
    "lea 4(%esp), %esi            /* CPU save area */   \n\t"
    "pushl %esi                                         \n\t"
    "pushl %edx                                         \n\t"
    "calll generic_handle_irq /* NB: edx = disp save ptr*/\n\t"

    "\ncall_handle_irq:                                 \n\t"
    "calll handle_irq                                   \n\t"
);

// CPU exceptions
HW_EXCEPTION_NOERR(0);
HW_EXCEPTION_NOERR(1);
HW_EXCEPTION_NOERR(2);
HW_EXCEPTION_NOERR(3);
HW_EXCEPTION_NOERR(4);
HW_EXCEPTION_NOERR(5);
HW_EXCEPTION_NOERR(6);
HW_EXCEPTION_NOERR(7);
HW_EXCEPTION_ERR(8);
HW_EXCEPTION_NOERR(9);
HW_EXCEPTION_ERR(10);
HW_EXCEPTION_ERR(11);
HW_EXCEPTION_ERR(12);
HW_EXCEPTION_ERR(13);
HW_EXCEPTION_ERR(14);
HW_EXCEPTION_NOERR(16);
HW_EXCEPTION_ERR(17);
HW_EXCEPTION_NOERR(18);
HW_EXCEPTION_NOERR(19);

// Classic PIC interrupts
HW_IRQ(32);
HW_IRQ(33);
HW_IRQ(34);
HW_IRQ(35);
HW_IRQ(36);
HW_IRQ(37);
HW_IRQ(38);
HW_IRQ(39);
HW_IRQ(40);
HW_IRQ(41);
HW_IRQ(42);
HW_IRQ(43);
HW_IRQ(44);
HW_IRQ(45);
HW_IRQ(46);
HW_IRQ(47);

// Generic interrupts
HW_IRQ(48);
HW_IRQ(49);
HW_IRQ(50);
HW_IRQ(51);
HW_IRQ(52);
HW_IRQ(53);
HW_IRQ(54);
HW_IRQ(55);
HW_IRQ(56);
HW_IRQ(57);
HW_IRQ(58);
HW_IRQ(59);
HW_IRQ(60);
HW_IRQ(61);

// Trace IPIs
HW_IRQ(62);
HW_IRQ(63);

// Local APIC interrupts
HW_IRQ(249);
HW_IRQ(250);
HW_IRQ(251);
HW_IRQ(252);
HW_IRQ(253);
HW_IRQ(254);

// Reserved as "unhandled exception" handler
HW_EXCEPTION_NOERR(666);

#define ERR_PF_PRESENT          (1 << 0)
#define ERR_PF_READ_WRITE       (1 << 1)
#define ERR_PF_USER_SUPERVISOR  (1 << 2)
#define ERR_PF_RESERVED         (1 << 3)
#define ERR_PF_INSTRUCTION      (1 << 4)

/// Number of (reserved) hardware exceptions
#define NEXCEPTIONS             32

/// Size of hardware IRQ dispatch table == #NIDT - #NEXCEPTIONS exceptions
#define NDISPATCH               (NIDT - NEXCEPTIONS)

/**
 * \brief Interrupt Descriptor Table (IDT) for processor this kernel is running
 * on.
 */
static struct gate_descriptor idt[NIDT] __attribute__ ((aligned (16)));

/**
 * \brief User-space IRQ dispatch table.
 *
 * This is essentially a big CNode holding #NDISPATCH capability
 * entries to local endpoints of user-space applications listening to
 * the interrupts.
 */
static struct cte irq_dispatch[NDISPATCH];

/// System call entry point
void syscall_entry(void);

/**
 * \brief Send interrupt notification to user-space listener.
 *
 * Sends an interrupt notification IDC to a local endpoint that
 * listens for IRQ notifications.
 *
 * \param irq   IRQ# to send in notification.
 */
static void send_user_interrupt(int irq)
{
    assert(irq >= 0 && irq < NDISPATCH);
    struct capability   *cap = &irq_dispatch[irq].cap;

    // Return on null cap (unhandled interrupt)
    if(cap->type == ObjType_Null) {
        printk(LOG_WARN, "unhandled IRQ %d\n", irq);
        return;
    }

    // Otherwise, cap needs to be an endpoint
    assert(cap->type == ObjType_EndPoint);
    errval_t err = lmp_deliver_notification(cap);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            struct dispatcher_shared_generic *disp =
                get_dispatcher_shared_generic(cap->u.endpoint.listener->disp);
            printk(LOG_DEBUG, "%.*s: IRQ message buffer overflow\n",
                   DISP_NAME_LEN, disp->name);
        } else {
            printk(LOG_ERR, "Unexpected error delivering IRQ\n");
        }
    }

#ifdef SCHEDULER_RR
    /* XXX: run the handler dispatcher immediately
     * we shouldn't do this (we should let the scheduler decide), but because
     * our default scheduler is braindead, this is a quick hack to make sure
     * that mostly-sane things happen
     */
    dispatch(cap->u.endpoint.listener);
#else
    dispatch(schedule());
#endif
}

errval_t irq_table_set(unsigned int nidt, capaddr_t endpoint)
{
    errval_t err;
    struct cte *recv;

    err = caps_lookup_slot(&dcb_current->cspace.cap, endpoint,
                           CPTR_BITS, &recv, CAPRIGHTS_WRITE);
    if (err_is_fail(err)) {
        return err_push(err, SYS_ERR_IRQ_LOOKUP);
    }

    assert(recv != NULL);

    // Return w/error if cap is not an endpoint
    if(recv->cap.type != ObjType_EndPoint) {
        return SYS_ERR_IRQ_NOT_ENDPOINT;
    }

    // Return w/error if no listener on endpoint
    if(recv->cap.u.endpoint.listener == NULL) {
        return SYS_ERR_IRQ_NO_LISTENER;
    }

    if(nidt < NDISPATCH) {
        // check that we don't overwrite someone else's handler
        if (irq_dispatch[nidt].cap.type != ObjType_Null) {
            printf("kernel: installing new handler for IRQ %d\n", nidt);
        }
        err = caps_copy_to_cte(&irq_dispatch[nidt], recv, false, 0, 0);
#if 0
        if (err_is_ok(err)) {
            // Unmask interrupt if on PIC
            if(nidt < 16) {
                pic_toggle_irq(nidt, true);
            }
        }
#endif
        return err;
    } else {
        return SYS_ERR_IRQ_INVALID;
    }
}

errval_t irq_table_delete(unsigned int nidt)
{
    if(nidt < NDISPATCH) {
        irq_dispatch[nidt].cap.type = ObjType_Null;
#if 0
        pic_toggle_irq(nidt, false);
#endif
        return SYS_ERR_OK;
    } else {
        return SYS_ERR_IRQ_INVALID;
    }
}

/**
 * \brief Handles kernel exceptions
 *
 * \param vec   Vector number of exception
 * \param error Error code from CPU, or 0 for an exception without an error code
 * \param save_area Pointer to save area for registers stacked by trap handler
 */
static __attribute__ ((used,noreturn))
    void generic_handle_kernel_exception(int vec, uint32_t error,
                            struct registers_x86_32 *save_area)
{
    uintptr_t rip = save_area->eip;
    lvaddr_t fault_address;

    if (vec == 666) {
        panic("unhandled kernel exception");
    }

    assert(vec < NEXCEPTIONS);
    // CS.CPL == 0, ie. a kernel-mode fault
    assert((save_area->cs & 0x3) == 0);

    printk(LOG_PANIC, "exception %d (error code 0x%"PRIx32"): ", vec, error);

    switch(vec) {
    case 0:     // Divide Error (#DE)
        printf("divide error\n");
        break;
    case 1:     // Debug Exception (#DB)
        printf("debug exception\n");
        break;
    case 2:     // NMI Interrupt
        printf("NMI Interrupt\n");
        break;
    case 3:     // Breakpoint (#BP)
        printf("breakpoint\n");
        break;
    case 4:     // Overflow (#OF)
        printf("overflow\n");
        break;
    case 5:     // BOUND Range Exceeded (#BR)
        printf("BOUND Range Exceeded\n");
        break;
    case 6:     // Invalid Opcode (#UD)
        printf("invalid opcode\n");
        break;
    case 7:     // Device Not Available (#NM)
        printf("device not available\n");
        break;
    case 8:     // Double fault (#DF)
        printf("double fault\n");
        break;
    case 9:     // Coprocessor Segment Overrun
        printf("coprocessor segment overrun\n");
        break;
    case 10:    // Invalid TSS (#TS)
        printf("invalid TSS\n");
        break;
    case 11:    // Segment Not Present (#NP)
        printf("segment not present\n");
        break;
    case 12:    // Stack Fault (#SS)
        printf("stack fault\n");
        break;
    case 13:    // General Protection Fault (#GP)
        printf("general protection fault\n");
        break;
    case 14:    // Page Fault (#PF)
        printf("%s page fault due to %s%s, while in %s mode%s\n",
               error & ERR_PF_READ_WRITE ? "write" : "read",
               error & ERR_PF_PRESENT ? "access violation" : "page not present",
               error & ERR_PF_RESERVED ? ", reserved bits set in page table"
               : "",
               error & ERR_PF_USER_SUPERVISOR ? "user" : "supervisor",
               error & ERR_PF_INSTRUCTION ? ", by instruction fetch" : "");

        __asm volatile("mov %%cr2, %[fault_address]"
                       : [fault_address] "=r" (fault_address));
        printf("Address that caused the fault: 0x%"PRIxLVADDR"\n", fault_address);
        break;
    case 17:    // Alignment Check Exception (#AC)
        printf("alignment check exception\n");
        break;

    default:
        printf("unhandled exception!\n");
        break;
    }

    if(dcb_current != NULL) {
        dispatcher_handle_t handle = dcb_current->disp;
        struct dispatcher_shared_generic *disp =
            get_dispatcher_shared_generic(handle);

        printf("On behalf of: %.*s\n", DISP_NAME_LEN, disp->name);
    } else {
        printf("No active process\n");
    }

    // Print faulting instruction pointer
    printf("Faulting instruction pointer (or following instruction): "
           "0x%"PRIxPTR" (0x %"PRIxPTR" in binary)\n", rip,
           rip - (uintptr_t)&_start_kernel + X86_32_START_KERNEL_PHYS);

    // Print some important registers
    printf("EAX 0x%"PRIx32" EBX 0x%"PRIx32" ECX 0x%"PRIx32
          " EDX 0x%"PRIx32" ESP 0x%"PRIx32"\n",
           save_area->eax, save_area->ebx,
           save_area->ecx, save_area->edx,
           save_area->esp);

    // Print the top 10 stack words
    printf("Top o' stack:\n");
    for(int i = 0; i < 20; i++) {
        unsigned long *p = (unsigned long *)save_area->esp + i;
        printf("0x%lx ", *p);
    }
    printf("\n");

    // Drop to the debugger
    gdb_handle_exception(vec, (uintptr_t*)save_area);
    panic("gdb_handle_exception returned");
}

/**
 * \brief copies CPU-stacked registers to a dispatcher save area
 */
static void copy_cpu_frame_to_dispatcher(
    uintptr_t * NONNULL COUNT(X86_SAVE_AREA_SIZE) cpu_save_area,
    struct registers_x86_32 *disp_save_area)
{
    // sanity checks
    assert(cpu_save_area[X86_SAVE_SS] == USER_SS);
    assert(cpu_save_area[X86_SAVE_CS] == USER_CS);
    assert((cpu_save_area[X86_SAVE_EFLAGS] & USER_EFLAGS) == USER_EFLAGS);

    disp_save_area->ss = cpu_save_area[X86_SAVE_SS];
    disp_save_area->esp = cpu_save_area[X86_SAVE_ESP];
    disp_save_area->eflags = cpu_save_area[X86_SAVE_EFLAGS];
    disp_save_area->cs = cpu_save_area[X86_SAVE_CS];
    disp_save_area->eip = cpu_save_area[X86_SAVE_EIP];
}

/**
 * \brief Handles user-mode exceptions
 *
 * \param vec   Vector number of exception
 * \param error Error code from CPU, or 0 for an exception without an error code
 * \param cpu_save_area  Pointer to save area for registers stacked by CPU
 * \param disp_save_area Pointer to save area in dispatcher
 */
static __attribute__ ((used))
void generic_handle_user_exception(uintptr_t *cpu_save_area,
                                   struct registers_x86_32 *disp_save_area,
                                   int vec, uint32_t error)
{
    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    uintptr_t eip = cpu_save_area[X86_SAVE_EIP];
    lvaddr_t fault_address, handler, param;

    assert(vec < NEXCEPTIONS);
    assert((cpu_save_area[X86_SAVE_CS] & 0x3) != 0); // CS.CPL > 0

    copy_cpu_frame_to_dispatcher(cpu_save_area, disp_save_area);

    bool disabled = dcb_current->disabled =
        dispatcher_is_disabled_ip(handle, eip);

    // Store FPU state if it's used
    // Do this for every trap when the current domain used the FPU
    // Do it for FPU not available traps in any case (to save the last FPU user)
    // XXX: Need to reset fpu_dcb when that DCB is deleted
    if(fpu_dcb != NULL &&
       (fpu_dcb == dcb_current || vec == IDT_NM)) {
        struct dispatcher_shared_generic *dst =
            get_dispatcher_shared_generic(fpu_dcb->disp);

        // Turn FPU trap off temporarily for saving its state
        bool trap = fpu_trap_get();
        fpu_trap_off();

        if(fpu_dcb->disabled) {
            fpu_save(dispatcher_get_disabled_fpu_save_area(fpu_dcb->disp));
	    dst->fpu_used = 1;
        } else {
            assert(!fpu_dcb->disabled);
            fpu_save(dispatcher_get_enabled_fpu_save_area(fpu_dcb->disp));
	    dst->fpu_used = 2;
        }

        if(trap) {
            fpu_trap_on();
        }
    }

    if (vec == IDT_PF) { // Page fault
        // Get fault address
        __asm volatile("mov %%cr2, %[fault_address]"
                       : [fault_address] "=r" (fault_address));

        printk(LOG_WARN, "user page fault%s in '%.*s': addr %"PRIxLVADDR
                         " IP %"PRIxPTR"  error %"PRIx32"\n",
               disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
               disp->name, fault_address, eip, error);

        /* sanity-check that the trap handler saved in the right place */
        assert((disabled && disp_save_area == dispatcher_get_trap_save_area(handle))
               || (!disabled && disp_save_area == dispatcher_get_enabled_save_area(handle)));
        if (disabled) {
            dcb_current->faults_taken++;
            handler = disp->dispatcher_pagefault_disabled;
        } else {
            handler = disp->dispatcher_pagefault;
        }
        param = fault_address;
    } else if (vec == IDT_NM) {
        debug(SUBSYS_DISPATCH, "FPU trap in %.*s (%p) at 0x%" PRIxPTR ", %s\n",
              DISP_NAME_LEN, disp->name, dcb_current, eip, disabled ? "DISABLED" : "ENABLED");

        /* Intel system programming part 1: 2.3.1, 2.5, 11, 12.5.1
         * clear the TS flag (flag that says, that the FPU is not available)
         */
        clts();

        // Remember FPU-using DCB
        fpu_dcb = dcb_current;

        // Wipe FPU for protection and to initialize it in case we trapped while
        // disabled
        fpu_init();

        if(disabled) {
            // Initialize FPU (done earlier) and ignore trap
            dispatch(dcb_current);
        } else {
            // defer trap to user-space
            // FPUs are switched eagerly while disabled, there should be no trap
            assert(disp_save_area == dispatcher_get_trap_save_area(handle));
            handler = disp->dispatcher_trap;
            param = vec;
        }
    } else if (vec == IDT_NMI) {
        printk(LOG_WARN, "NMI - ignoring\n");
        dispatch(schedule());
    } else if (vec == IDT_MF) {
        uint16_t fpu_status;

        __asm volatile("fnstsw %0" : "=a" (fpu_status));

        printk(LOG_WARN, "FPU error%s in '%.*s': IP %"PRIxPTR" FPU status %x\n",
               disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
               disp->name, eip, fpu_status);

        handler = disp->dispatcher_trap;
        param = vec;
    } else { // All other traps
        printk(LOG_WARN, "user trap #%d in '%.*s': IP %"PRIxPTR
                         ", error %"PRIu32"\n",
               vec, DISP_NAME_LEN, disp->name, eip, error);
        assert(disp_save_area == dispatcher_get_trap_save_area(handle));
        if(vec != 1) {
            dcb_current->faults_taken++;
        }

        handler = disp->dispatcher_trap;
        param = vec;
    }

    // Make unrunnable if it has taken too many faults
    if (dcb_current->faults_taken > 2) {
        printk(LOG_WARN, "generic_handle_user_exception: too many faults, "
               "making domain unrunnable\n");
        dcb_current->faults_taken = 0; // just in case it gets restarted
        scheduler_remove(dcb_current);
        dispatch(schedule());
    }

    /* resume user to save area */
    disp->disabled = 1;
    cpu_save_area[X86_SAVE_EIP] = handler;
    cpu_save_area[X86_SAVE_EFLAGS] = USER_EFLAGS;

    /* XXX: get GCC to load up the argument registers before returning */
    register uintptr_t arg0 __asm ("%eax") = param;
    register uintptr_t arg1 __asm ("%ecx") = error;
    register uintptr_t arg2 __asm ("%edx") = eip;
    __asm volatile("" :: "r" (arg0), "r" (arg1), "r" (arg2));
}

/// Handle an IRQ that arrived, either while in user or kernel mode (HLT)
static __attribute__ ((used)) void handle_irq(int vector)
{
    debug(SUBSYS_DISPATCH, "IRQ vector %d while %s\n", vector,
          dcb_current ? (dcb_current->disabled ? "disabled": "enabled") : "in kernel");

    int irq = vector - NEXCEPTIONS;

    // if we were in wait_for_interrupt(), unmask timer before running userspace
    if (dcb_current == NULL && kernel_ticks_enabled) {
        apic_unmask_timer();
    }

    // APIC timer interrupt: handle in kernel and reschedule
    if (vector == APIC_TIMER_INTERRUPT_VECTOR) {
        apic_eoi();
        assert(kernel_ticks_enabled);
        // Ignore timeslice if it happens too closely (less than half
        // of the TSC ticks that are supposed to pass) to the last.
        // In that case we have just synced timers and see a spurious
        // APIC timer interrupt.
        uint64_t tsc_now = rdtsc();
        if(tsc_now - tsc_lasttime >
           (kernel_timeslice * timing_get_tsc_per_ms()) / 2) {
            kernel_now += kernel_timeslice;
        }
        tsc_lasttime = tsc_now;
        trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_TIMER, kernel_now);
        wakeup_check(kernel_now);
    } else if (vector == APIC_ERROR_INTERRUPT_VECTOR) {
        printk(LOG_ERR, "APIC error interrupt fired!\n"); // XXX: do something?
        apic_eoi();
    } else if (vector == APIC_INTER_CORE_VECTOR) {
        apic_eoi();
#ifdef __scc__
        rck_handle_notification();
#else
        ipi_handle_notify();
#endif
    }
#if 0
 else if (irq >= 0 && irq <= 15) { // classic PIC device interrupt
     printk(LOG_NOTE, "got interrupt %d!\n", irq);

        apic_eoi();

        // only handle PIC interrupts on the BSP core
        if (apic_is_bsp()) {
            if (pic_have_interrupt(irq)) {
                pic_eoi(irq);
                send_user_interrupt(irq);
            } else { // no interrupt pending, check for a different one (!)
                irq = pic_pending_interrupt();
                if (irq == -1) { // really nothing pending
                    printk(LOG_NOTE, "spurious interrupt (IRQ %d)\n", irq);
                } else { // why does this happen?! -AB
                    printk(LOG_NOTE, "IRQ %d reported on wrong vector (%d)\n",
                           irq, vector - NEXCEPTIONS);
                    pic_eoi(irq);
                    send_user_interrupt(irq);
                }
            }
        }
    }
#endif
    else { // APIC device interrupt (or IPI)
        //printk(LOG_NOTE, "interrupt %d vector %d!\n", irq, vector);
        apic_eoi();
#ifdef __scc__
        // Gotta reset the line
        rck_reset_lint1();
#endif
        send_user_interrupt(irq);
    }

    // reschedule (because the runnable processes may have changed) and dispatch
    /* FIXME: the round-robin scheduler doesn't do the best thing here:
     * it always picks the next task, but we only really want to do that on
     * a timer tick
     */
    dispatch(schedule());
    panic("dispatch() returned");
}

/**
 * \brief Handles device interrupts that arrive while in user mode
 *
 * \param vector    Vector number
 * \param cpu_save_area  Pointer to save area for registers stacked by CPU
 * \param disp_save_area Pointer to save area in dispatcher
 */
static __attribute__ ((used)) void
generic_handle_irq(struct registers_x86_32 *disp_save_area, uintptr_t *cpu_save_area,
                   int vector)
{
    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    dispatcher_handle_t handle = dcb_current->disp;
    uintptr_t eip = cpu_save_area[X86_SAVE_EIP];
    assert(vector < NIDT && vector >= NEXCEPTIONS);

    // Copy CPU-saved registers to dispatcher save area
    copy_cpu_frame_to_dispatcher(cpu_save_area, disp_save_area);

    /* sanity-check that the trap handler saved in the right place,
     * and update disabled flag in DCB */
    if (disp_save_area == dispatcher_get_disabled_save_area(handle)) {
        assert(dispatcher_is_disabled_ip(handle, eip));
        dcb_current->disabled = 1;
    } else {
        assert(disp_save_area == dispatcher_get_enabled_save_area(handle));
        assert(!dispatcher_is_disabled_ip(handle, eip));
        dcb_current->disabled = 0;
    }

    handle_irq(vector);
}

/* Utility function for code below; initialises a gate_descriptor */
static void setgd(struct gate_descriptor *gd, void (* handler)(void),
                  int type, int dpl, int selector)
{
    memset(gd, 0, sizeof(struct gate_descriptor));
    gd->gd_looffset = (uintptr_t)handler & ((1UL << 16) - 1);
    gd->gd_hioffset = (uintptr_t)handler >> 16;
    gd->gd_selector = selector;
    gd->gd_type = type;
    gd->gd_dpl = dpl;
    gd->gd_p = 1;
}

/**
 * \brief Sets up the default IDT for current CPU.
 */
void setup_default_idt(void)
{
    struct region_descriptor region = {         // set default IDT
        .rd_limit = NIDT * sizeof(idt[0]) - 1,
        .rd_base = (uint32_t)&idt
    };
    int i;

    // reset IDT
    memset((void *)&idt, 0, NIDT * sizeof(idt[0]));

    // initialize IDT with default generic handlers
    for (i = 0; i < NIDT; i++)
        setgd(&idt[i], hwexc_666, SDT_SYSIGT, SEL_KPL,
              GSEL(KCODE_SEL, SEL_KPL));

    /* Setup exception handlers */
    setgd(&idt[0], hwexc_0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[1], hwexc_1, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[2], hwexc_2, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[3], hwexc_3, SDT_SYSIGT, SEL_UPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[4], hwexc_4, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[5], hwexc_5, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[6], hwexc_6, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[7], hwexc_7, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[8], hwexc_8, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[9], hwexc_9, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[10], hwexc_10, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[11], hwexc_11, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[12], hwexc_12, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[13], hwexc_13, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[14], hwexc_14, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupt 15 is undefined
    setgd(&idt[16], hwexc_16, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[17], hwexc_17, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[18], hwexc_18, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[19], hwexc_19, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupts 20 - 31 are reserved

    /* Setup classic PIC interrupt handlers */
    setgd(&idt[32], hwirq_32, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[33], hwirq_33, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[34], hwirq_34, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[35], hwirq_35, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[36], hwirq_36, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[37], hwirq_37, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[38], hwirq_38, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[39], hwirq_39, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[40], hwirq_40, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[41], hwirq_41, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[42], hwirq_42, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[43], hwirq_43, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[44], hwirq_44, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[45], hwirq_45, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[46], hwirq_46, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[47], hwirq_47, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // Setup generic interrupt handlers
    setgd(&idt[48], hwirq_48, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[49], hwirq_49, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[50], hwirq_50, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[50], hwirq_50, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[51], hwirq_51, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[52], hwirq_52, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[53], hwirq_53, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[54], hwirq_54, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[55], hwirq_55, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[56], hwirq_56, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[57], hwirq_57, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[58], hwirq_58, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[59], hwirq_59, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[60], hwirq_60, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[61], hwirq_61, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // XXX Interrupts used for TRACE IPIs
    setgd(&idt[62], hwirq_62, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[63], hwirq_63, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // Setup local APIC interrupt handlers
    setgd(&idt[249], hwirq_249, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[250], hwirq_250, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[251], hwirq_251, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[252], hwirq_252, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[253], hwirq_253, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[254], hwirq_254, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // Setup system call interrupt
    setgd(&idt[255], syscall_entry, SDT_SYSIGT, SEL_UPL,
          GSEL(KCODE_SEL, SEL_KPL));

    /* Load IDT register */
    __asm volatile("lidt %0" :: "m" (region));
}
