/**
 * \file
 * \brief x86-64 interrupt/exception handling utility functions
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
#include <exec.h>
#include <gdb_stub.h>
#include <arch_gdb_stub.h>
#include <x86.h>
#include <dispatch.h>
#include <wakeup.h>
#include <arch/x86/perfmon.h>
#include <arch/x86/barrelfish_kpi/perfmon.h>
#include <arch/x86/pic.h>
#include <arch/x86/apic.h>
#include <barrelfish_kpi/dispatcher_shared_target.h>
#include <asmoffsets.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>
#include <arch/x86/timing.h>
#include <arch/x86/syscall.h>
#include <arch/x86/ipi_notify.h>
#include <barrelfish_kpi/cpu_arch.h>

#include <dev/ia32_dev.h>

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
           "pushq $0                /* dummy error code */ \n\t"        \
           "pushq $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

#define HW_EXCEPTION_ERR(num)                                           \
    void __attribute__ ((visibility ("hidden"))) hwexc_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwexc_"#num",@function                 \n\t"        \
           "hwexc_"#num":                                  \n\t"        \
           "pushq $"#num"           /* vector number */    \n\t"        \
           "jmp    hwexc_common     /* common stuff */     \n\t"        \
                                                                        )

#define XHW_IRQ(num)                                                    \
    void __attribute__ ((visibility ("hidden"))) hwirq_##num(void);     \
    __asm (                                                             \
           "\t.text                                        \n\t"        \
           "\t.type hwirq_"#num",@function                 \n\t"        \
           "hwirq_"#num":                                  \n\t"        \
           "pushq $"#num"           /* vector number */    \n\t"        \
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
    "testb $3, 24(%rsp) /* if CS.CPL == 0 */            \n\t"
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
    "pushq %rcx                                         \n\t"
    "movq dcb_current(%rip), %rcx /* rcx = dcb_current */       \n\t"
    "movq "XTR(OFFSETOF_DCB_DISP)"(%rcx), %rcx /* rcx = dcb_current->disp */\n\t"
    "cmpq $14, 8(%rsp)       /* is pagefault? */        \n\t"
    "jne save_trap                                      \n\t"
    "cmpl $0, "XTR(OFFSETOF_DISP_DISABLED)"(%rcx) /* disp->disabled ? */\n\t"
    "jne save_trap                                      \n\t"
    "pushq %rbx                                         \n\t"
    "movq 4*8(%rsp), %rbx     /* rbx = faulting IP */   \n\t"
    "cmpq "XTR(OFFSETOF_DISP_X86_64_CRIT_PC_LOW)"(%rcx), %rbx /* crit_pc_low <= rip? */\n\t"
    "jae disabled_test                                  \n\t"
    "\nsave_enabled:                                    \n\t"
    "popq %rbx                                          \n\t"
    "addq $"XTR(OFFSETOF_DISP_X86_64_ENABLED_AREA)", %rcx /* rcx = enabled_save_area */\n\t"
    "jmp do_save                                        \n\t"
    "\ndisabled_test:                                   \n\t"
    "cmpq "XTR(OFFSETOF_DISP_X86_64_CRIT_PC_HIGH)"(%rcx), %rbx /* crit_pc_high > rip? */\n\t"
    "jae save_enabled                                   \n\t"
    "popq %rbx                                          \n\t"
    "\nsave_trap:                                       \n\t"
    "addq $"XTR(OFFSETOF_DISP_X86_64_TRAP_AREA)", %rcx /* trap_save_area */\n\t"

    /* save to the save area. at this point, rcx = save area ptr,
     * rsp+8 = exception num, rsp+16 = CPU-stacked error and registers */
    "\ndo_save:                                         \n\t"
    "movq %rax,  0*8(%rcx)                              \n\t"
    "popq %rax                    /* original rcx */    \n\t"
    "movq %rbx,  1*8(%rcx)                              \n\t"
    "movq %rax,  2*8(%rcx)                              \n\t"
    "movq %rdx,  3*8(%rcx)                              \n\t"
    "movq %rsi,  4*8(%rcx)                              \n\t"
    "movq %rdi,  5*8(%rcx)                              \n\t"
    "movq %rbp,  6*8(%rcx)                              \n\t"
    "movq %r8,   8*8(%rcx)                              \n\t"
    "movq %r9,   9*8(%rcx)                              \n\t"
    "movq %r10, 10*8(%rcx)                              \n\t"
    "movq %r11, 11*8(%rcx)                              \n\t"
    "movq %r12, 12*8(%rcx)                              \n\t"
    "movq %r13, 13*8(%rcx)                              \n\t"
    "movq %r14, 14*8(%rcx)                              \n\t"
    "movq %r15, 15*8(%rcx)                              \n\t"
    "mov %fs, "XTR(OFFSETOF_FS_REG)"(%rcx)              \n\t"
    "mov %gs, "XTR(OFFSETOF_GS_REG)"(%rcx)              \n\t"
    "popq %rdi                    /* vector number */   \n\t"
    "popq %rsi                    /* error code */      \n\t"
    "movq %rsp, %rdx              /* CPU save area */   \n\t"
    "callq generic_handle_user_exception                \n\t"
    "iretq                                              \n\t"

    /* a kernel fault means something bad happened, so we stack
     * everything for the debugger to use, in the GDB frame format */
    "\nkernel_fault:                                    \n\t"
    "pushq 6*8(%rsp) /* SS */                           \n\t"
    "pushq 4*8(%rsp) /* CS */                           \n\t"
    "pushq 7*8(%rsp) /* EFLAGS */                       \n\t"
    "pushq 5*8(%rsp) /* RIP */                          \n\t"
    /* TODO: extend frame size and save FS/GS so we can resume afterwards */
    "pushq %r15                                         \n\t"
    "pushq %r14                                         \n\t"
    "pushq %r13                                         \n\t"
    "pushq %r12                                         \n\t"
    "pushq %r11                                         \n\t"
    "pushq %r10                                         \n\t"
    "pushq %r9                                          \n\t"
    "pushq %r8                                          \n\t"
    "pushq 17*8(%rsp) /* RSP */                         \n\t"
    "pushq %rbp                                         \n\t"
    "pushq %rdi                                         \n\t"
    "pushq %rsi                                         \n\t"
    "pushq %rdx                                         \n\t"
    "pushq %rcx                                         \n\t"
    "pushq %rbx                                         \n\t"
    "pushq %rax                                         \n\t"
    "movq 20*8(%rsp), %rdi  /* vector number */         \n\t"
    "movq 21*8(%rsp), %rsi  /* error code   */          \n\t"
    "movq %rsp, %rdx       /* save area ptr*/           \n\t"
    "jmp generic_handle_kernel_exception                \n\t"


    /* (Device) interrupt. */
    "   .type hwirq_common ,@function                   \n\t"
    "hwirq_common:                                      \n\t"
    /* If it happened in kernel_mode, simply make userspace runnable.
     * This is a special case, since interrupts are normally disabled when
     * entering the kernel. However, they are enabled when there is nothing
     * to do, and the kernel goes to sleep using wait_for_interrupts() */
    "testb $3, 16(%rsp) /* if CS.CPL == 0 */            \n\t"
    "jz call_handle_irq                                 \n\t"

    /* Happened in user mode.
     * we need to save everything to the dispatcher. */
    /* decide where to save the state, either enabled or disabled save areas */
    "pushq %rdx                                         \n\t"
    "movq dcb_current(%rip), %rdx /* rdx = dcb_current */       \n\t"
    "movq "XTR(OFFSETOF_DCB_DISP)"(%rdx), %rdx /* rdx = dcb_current->disp */\n\t"
    "cmpl $0, "XTR(OFFSETOF_DISP_DISABLED)"(%rdx) /* disp->disabled ? */\n\t"
    "jne irq_save_disabled                              \n\t"
    "pushq %rbx                                         \n\t"
    "movq 24(%rsp), %rbx     /* rbx = faulting IP */    \n\t"
    "cmpq "XTR(OFFSETOF_DISP_X86_64_CRIT_PC_LOW)"(%rdx), %rbx /* crit_pc_low <= rip? */\n\t"
    "jae irq_disabled_test                              \n\t"
    "\nirq_save_enabled:                                \n\t"
    "popq %rbx                                          \n\t"
    "addq $"XTR(OFFSETOF_DISP_X86_64_ENABLED_AREA)", %rdx /* rdx = enabled_save_area */\n\t"
    "jmp irq_do_save                                    \n\t"
    "\nirq_disabled_test:                               \n\t"
    "cmpq "XTR(OFFSETOF_DISP_X86_64_CRIT_PC_HIGH)"(%rdx), %rbx /* crit_pc_high > rip? */\n\t"
    "jae irq_save_enabled                               \n\t"
    "popq %rbx                                          \n\t"
    "\nirq_save_disabled:                               \n\t"
    "addq $"XTR(OFFSETOF_DISP_X86_64_DISABLED_AREA)", %rdx /* disabled_save_area */\n\t"

    /* save to the save area. at this point, rdx = save area ptr,
     * rsp+8 = vector number, rsp+16 = CPU-stacked regisers */
    "\nirq_do_save:                                     \n\t"
    "movq %rax,  0*8(%rdx)                              \n\t"
    "movq %rbx,  1*8(%rdx)                              \n\t"
    "movq %rcx,  2*8(%rdx)                              \n\t"
    "popq %rax                    /* original rdx */    \n\t"
    "movq %rax,  3*8(%rdx)                              \n\t"
    "movq %rsi,  4*8(%rdx)                              \n\t"
    "movq %rdi,  5*8(%rdx)                              \n\t"
    "movq %rbp,  6*8(%rdx)                              \n\t"
    "movq %r8,   8*8(%rdx)                              \n\t"
    "movq %r9,   9*8(%rdx)                              \n\t"
    "movq %r10, 10*8(%rdx)                              \n\t"
    "movq %r11, 11*8(%rdx)                              \n\t"
    "movq %r12, 12*8(%rdx)                              \n\t"
    "movq %r13, 13*8(%rdx)                              \n\t"
    "movq %r14, 14*8(%rdx)                              \n\t"
    "movq %r15, 15*8(%rdx)                              \n\t"
    "mov %fs, "XTR(OFFSETOF_FS_REG)"(%rdx)              \n\t"
    "mov %gs, "XTR(OFFSETOF_GS_REG)"(%rdx)              \n\t"
    "popq %rdi                    /* vector number */   \n\t"
    "movq %rsp, %rsi              /* CPU save area */   \n\t"
    "jmp generic_handle_irq /* NB: rdx = disp save ptr*/\n\t"

    "\ncall_handle_irq:                                 \n\t"
    "popq %rdi                                          \n\t"
    "callq handle_irq                                   \n\t"
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

#if CONFIG_TRACE && NETWORK_STACK_TRACE
#define TRACE_ETHERSRV_MODE 1
#endif // CONFIG_TRACE && NETWORK_STACK_TRACE

#if CONFIG_TRACE && NETWORK_STACK_BENCHMARK
#define TRACE_N_BM 1
#endif // CONFIG_TRACE && NETWORK_STACK_BENCHMARK

/**
 * \brief Send interrupt notification to user-space listener.
 *
 * Sends an interrupt notification IDC to a local endpoint that
 * listens for IRQ notifications.
 *
 * \param irq   IRQ# to send in notification.
 */
static uint32_t interrupt_count = 0;
static void send_user_interrupt(int irq)
{
    assert(irq >= 0 && irq < NDISPATCH);
    struct capability   *cap = &irq_dispatch[irq].cap;

    // Return on null cap (unhandled interrupt)
    if(cap->type == ObjType_Null) {
        printk(LOG_WARN, "unhandled IRQ %d\n", irq);
        return;
    }

    if (irq == 0) {
//    	printf("packet interrupt\n");
        ++interrupt_count;
#if TRACE_N_BM
#include<trace/trace.h>
    trace_event(TRACE_SUBSYS_BNET, TRACE_EVENT_BNET_I, interrupt_count);
#endif // TRACE_N_BM
/*
        if (interrupt_count >= 60){
            printf("interrupt number %"PRIu32"\n", interrupt_count);
        }
*/
    }
//    printf("Interrupt %d\n", irq);
    // Otherwise, cap needs to be an endpoint
    assert(cap->type == ObjType_EndPoint);

    // send empty message as notification
    errval_t err = lmp_deliver_notification(cap);
    if (err_is_fail(err)) {
        if (err_no(err) == SYS_ERR_LMP_BUF_OVERFLOW) {
            struct dispatcher_shared_generic *disp =
                get_dispatcher_shared_generic(cap->u.endpoint.listener->disp);
            printk(LOG_DEBUG, "%.*s: IRQ message buffer overflow on IRQ %d\n",
                   DISP_NAME_LEN, disp->name, irq);
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

        printf("kernel: %u: installing handler for IRQ %d\n", my_core_id, nidt);
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
 * \param gdb_save_frame Pointer to save area for registers stacked by trap handler
 */
static __attribute__ ((used,noreturn))
    void generic_handle_kernel_exception(uint64_t vec, uint64_t error,
                                         uintptr_t *gdb_save_frame)
{
    lvaddr_t fault_address;
    char *descr;

    if (vec == 666) {
        panic("unhandled kernel exception (vector 666)");
    }

    assert(vec < NEXCEPTIONS);

    printk(LOG_PANIC, "exception %d (error code 0x%lx): ", (int)vec, error);
    
    if (vec == ia32_vec_pf) {
        printf("%s page fault due to %s%s, while in %s mode%s\n",
               error & ERR_PF_READ_WRITE ? "write" : "read",
               error & ERR_PF_PRESENT ? "access violation" : "page not present",
               error & ERR_PF_RESERVED ? ", reserved bits set in page table"
               : "",
               error & ERR_PF_USER_SUPERVISOR ? "user" : "supervisor",
               error & ERR_PF_INSTRUCTION ? ", by instruction fetch" : "");

        __asm volatile("mov %%cr2, %[fault_address]"
                       : [fault_address] "=r" (fault_address));
        printf("Address that caused the fault: 0x%lx\n", fault_address);

    } else if ((descr = ia32_exc_vec_describe(vec))) {
        printf("%s\n", descr);
    } else {
        printf("unhandled exception!\n");
    }

    // Print faulting instruction pointer
    uintptr_t rip = gdb_save_frame[GDB_X86_64_RIP_REG];
    printf("Faulting instruction pointer (or next instruction): 0x%lx\n", rip);
    printf("  => i.e. unrelocated kernel address 0x%lx\n", 
           rip - (uintptr_t)&_start_kernel + X86_64_START_KERNEL_PHYS);
    
    printf("Registers:\n");
    printf(" rax: 0x%016lx  r8 : 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RAX_REG],
           gdb_save_frame[GDB_X86_64_R8_REG]);
    printf(" rbx: 0x%016lx  r9 : 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RBX_REG],
           gdb_save_frame[GDB_X86_64_R9_REG]);
    printf(" rcx: 0x%016lx  r10: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RCX_REG],
           gdb_save_frame[GDB_X86_64_R10_REG]);
    printf(" rdx: 0x%016lx  r11: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RDX_REG],
           gdb_save_frame[GDB_X86_64_R11_REG]);
    printf(" rsp: 0x%016lx  r12: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RSP_REG],
           gdb_save_frame[GDB_X86_64_R12_REG]);
    printf(" rdi: 0x%016lx  r13: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RDI_REG],
           gdb_save_frame[GDB_X86_64_R13_REG]);
    printf(" rsi: 0x%016lx  r14: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RSI_REG],
           gdb_save_frame[GDB_X86_64_R14_REG]);
    printf(" rip: 0x%016lx  r15: 0x%016lx\n", 
           gdb_save_frame[GDB_X86_64_RIP_REG],
           gdb_save_frame[GDB_X86_64_R15_REG]);

    // Print the top 10 stack words
    printf("Top o' stack:\n");
    for(int i = 0; i < 10; i++) {
        unsigned long *p = (unsigned long *)gdb_save_frame[GDB_X86_64_RSP_REG] + i;
        printf(" %d \t 0x%016lx (%lu)\n", i, *p, *p);
    }

    // Drop to the debugger
    gdb_handle_exception(vec, gdb_save_frame);
    panic("gdb_handle_exception returned");
}

/**
 * \brief copies CPU-stacked registers to a dispatcher save area
 */
static void copy_cpu_frame_to_dispatcher(
    uintptr_t * NONNULL COUNT(X86_SAVE_AREA_SIZE) cpu_save_area,
    struct registers_x86_64 *disp_save_area)
{
    // sanity checks
    assert((cpu_save_area[X86_SAVE_EFLAGS] & USER_EFLAGS) == USER_EFLAGS);

    disp_save_area->rsp = cpu_save_area[X86_SAVE_RSP];
    disp_save_area->eflags = cpu_save_area[X86_SAVE_EFLAGS];
    disp_save_area->rip = cpu_save_area[X86_SAVE_RIP];
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
    void generic_handle_user_exception(int vec, uint64_t error,
                uintptr_t * NONNULL COUNT(X86_SAVE_AREA_SIZE) cpu_save_area,
                struct registers_x86_64 *disp_save_area)
{
    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    dispatcher_handle_t handle = dcb_current->disp;
    struct dispatcher_shared_generic *disp =
        get_dispatcher_shared_generic(handle);
    uint64_t rip = cpu_save_area[X86_SAVE_RIP];
    uint64_t rsp = cpu_save_area[X86_SAVE_RSP];
    lvaddr_t fault_address, handler = 0, param = 0;

    assert(vec < NEXCEPTIONS);
    assert((cpu_save_area[X86_SAVE_CS] & 0x3) != 0); // CS.CPL > 0

    copy_cpu_frame_to_dispatcher(cpu_save_area, disp_save_area);

    bool disabled = dispatcher_is_disabled_ip(handle, rip);
    dcb_current->disabled = disabled;

    if (disabled) {
        dcb_current->faults_taken++;
    }

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

        printk(LOG_WARN, "user page fault%s in '%.*s': addr %lx IP %lx SP %lx "
                         "error 0x%lx\n",
               disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
               disp->name, fault_address, rip, rsp, error);

        /* sanity-check that the trap handler saved in the right place */
        assert((disabled && disp_save_area == dispatcher_get_trap_save_area(handle))
               || (!disabled && disp_save_area == dispatcher_get_enabled_save_area(handle)));
        if (disabled) {
            handler = disp->dispatcher_pagefault_disabled;
        } else {
            handler = disp->dispatcher_pagefault;
        }
        param = fault_address;
    } else if (vec == IDT_NMI) {
        printk(LOG_WARN, "NMI - ignoring\n");
        dispatch(dcb_current);
    } else if (vec == IDT_NM) {     // device not available (FPU) exception
        debug(SUBSYS_DISPATCH, "FPU trap in %.*s at 0x%" PRIxPTR "\n",
              DISP_NAME_LEN, disp->name, rip);
        assert(!dcb_current->is_vm_guest);

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
    } else if (vec == IDT_MF) {
        uint16_t fpu_status;

        __asm volatile("fnstsw %0" : "=a" (fpu_status));

        printk(LOG_WARN, "FPU error%s in '%.*s': IP %" PRIxPTR " FPU status %x\n",
               disabled ? " WHILE DISABLED" : "", DISP_NAME_LEN,
               disp->name, rip, fpu_status);

        handler = disp->dispatcher_trap;
        param = vec;
    } else if (vec == IDT_MC) {
        // TODO: provide more useful information about the cause
        panic("machine check exception while in user mode");
    } else { // All other traps
        printk(LOG_WARN, "user trap #%d%s in '%.*s': IP %lx, error %lx\n",
               vec, disabled ? " WHILE DISABLED" : "",
               DISP_NAME_LEN, disp->name, rip, error);
        assert(disp_save_area == dispatcher_get_trap_save_area(handle));
        if (disabled) {
            if (vec == IDT_DB) { // debug exception: just continue
                resume(dispatcher_get_trap_save_area(handle));
            } else {
                // can't handle a trap while disabled: nowhere safe to deliver it
                scheduler_remove(dcb_current);
                dispatch(schedule());
            }
        } else {
            handler = disp->dispatcher_trap;
            param = vec;
        }
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
    if(handler == 0) {
        printk(LOG_WARN, "no suitable handler for this type of fault, "
               "making domain unrunnable\n");
        scheduler_remove(dcb_current);
        dispatch(schedule());
    } else {
        cpu_save_area[X86_SAVE_RIP] = handler;
        cpu_save_area[X86_SAVE_EFLAGS] = USER_EFLAGS;
    }

    /* XXX: get GCC to load up the argument registers before returning */
    register uintptr_t arg0 __asm ("%rdi") = disp->udisp;
    register uintptr_t arg1 __asm ("%rsi") = param;
    register uintptr_t arg2 __asm ("%rdx") = error;
    register uintptr_t arg3 __asm ("%rcx") = rip;
    __asm volatile("" :: "r" (arg0), "r" (arg1), "r" (arg2), "r" (arg3));
}

int mycnt_incr = CONFIG_TIMESLICE;

static void
update_kernel_now(void)
{
    uint64_t tsc_now = rdtsc();
    #ifdef CONFIG_ONESHOT_TIMER
    uint64_t ticks = tsc_now - tsc_lasttime;
    kernel_now += ticks / timing_get_tsc_per_ms();
    #else // !CONFIG_ONESHOT_TIMER
    // maintain compatibility with old behaviour. Not sure if it is
    // actually needed. -AKK
    //
    // Ignore timeslice if it happens too closely (less than half
    // of the TSC ticks that are supposed to pass) to the last.
    // In that case we have just synced timers and see a spurious
    // APIC timer interrupt.
    if(tsc_now - tsc_lasttime >
       (kernel_timeslice * timing_get_tsc_per_ms()) / 2) {
        kernel_now += kernel_timeslice;
    }
    #endif // CONFIG_ONESHOT_TIMER
    tsc_lasttime = tsc_now;
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

#if TRACE_ONLY_LLNET
    trace_event(TRACE_SUBSYS_LLNET, TRACE_EVENT_LLNET_IRQ, vector);
#endif // TRACE_ONLY_LLNET

    // APIC timer interrupt: handle in kernel and reschedule
    if (vector == APIC_TIMER_INTERRUPT_VECTOR) {
        apic_eoi();
        static int mycnt = 0;
        mycnt += mycnt_incr;
        if(mycnt == 80) {
            assert(kernel_ticks_enabled);
            update_kernel_now();
            trace_event(TRACE_SUBSYS_KERNEL, TRACE_EVENT_KERNEL_TIMER, kernel_now);
            wakeup_check(kernel_now);
            mycnt = 0;
        }
    /* } else if (vector == APIC_PERFORMANCE_INTERRUPT_VECTOR) { */
        // Handle performance counter overflow
        // Reset counters
        extern struct capability perfmon_callback_ep;
        if(perfmon_callback_ep.type != ObjType_Null) {
            perfmon_measure_reset();
        }
        extern uint8_t perfmon_event;
        if(dcb_current!=NULL && perfmon_event != 0 && perfmon_callback_ep.type != ObjType_Null) {
            // Get faulting instruction pointer
            struct registers_x86_64 *disp_save_area = dcb_current->disabled ?
                dispatcher_get_disabled_save_area(dcb_current->disp) :
                dispatcher_get_enabled_save_area(dcb_current->disp);
            struct dispatcher_shared_generic *disp =
                get_dispatcher_shared_generic(dcb_current->disp);

            // Setup data structure for LMP transfer to user level handler
            struct perfmon_overflow_data data = {
                .ip = disp_save_area->rip
            };
            strncpy(data.name, disp->name, PERFMON_DISP_NAME_LEN);

            /* if(!strncmp(disp->name, "memcache", PERFMON_DISP_NAME_LEN)) { */
                // Call overflow handler represented by endpoint
                errval_t err;
                size_t payload_len = sizeof(struct perfmon_overflow_data)/
                    sizeof(uintptr_t)+1;
                err = lmp_deliver_payload(&perfmon_callback_ep,
                                          NULL,
                                          (uintptr_t*) &data,
                                          payload_len,
                                          false);

                // Make sure delivery was okay. SYS_ERR_LMP_BUF_OVERFLOW is okay for now
                /* assert(err_is_ok(err) || err_no(err)==SYS_ERR_LMP_BUF_OVERFLOW); */
                assert(err_is_ok(err));
            /* } */

        /* } else { */
        /*     // This should never happen, as interrupts are disabled in kernel */
        /*     printf("Performance counter overflow interrupt from " */
        /*            "apic in kernel level\n"); */
            if(mycnt != 0) {
                return;
            }
        }
        /* apic_eoi(); */
    } else if (vector == APIC_ERROR_INTERRUPT_VECTOR) {
        printk(LOG_ERR, "APIC error interrupt fired!\n");
        xapic_esr_t esr = apic_get_esr();
        char str[256];
        xapic_esr_prtval(str, 256, esr);
        printf("%s\n", str);
        apic_eoi();
    } else if (vector == APIC_INTER_CORE_VECTOR) {
        apic_eoi();
        ipi_handle_notify();
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
static __attribute__ ((used, noreturn)) void
generic_handle_irq(int vector,
                   uintptr_t * NONNULL COUNT(X86_SAVE_AREA_SIZE) cpu_save_area,
                   struct registers_x86_64 *disp_save_area)
{
    assert(dcb_current->disp_cte.cap.type == ObjType_Frame);
    dispatcher_handle_t handle = dcb_current->disp;
    uint64_t rip = cpu_save_area[X86_SAVE_RIP];
    assert(vector < NIDT && vector >= NEXCEPTIONS);

    // Copy CPU-saved registers to dispatcher save area
    copy_cpu_frame_to_dispatcher(cpu_save_area, disp_save_area);

    /* sanity-check that the trap handler saved in the right place,
     * and update disabled flag in DCB */
    if (disp_save_area == dispatcher_get_disabled_save_area(handle)) {
        assert(dispatcher_is_disabled_ip(handle, rip));
        dcb_current->disabled = true;
    } else {
        assert(disp_save_area == dispatcher_get_enabled_save_area(handle));
        assert(!dispatcher_is_disabled_ip(handle, rip));
        dcb_current->disabled = false;
    }

    handle_irq(vector);
    resume(disp_save_area);
}

/* Utility function for code below; initialises a gate_descriptor */
static void setgd(struct gate_descriptor *gd, void (* handler)(void),
                  int ist, int type, int dpl, int selector)
{
    memset(gd, 0, sizeof(struct gate_descriptor));
    gd->gd_looffset = (uintptr_t)handler & ((1UL << 16) - 1);
    gd->gd_hioffset = (uintptr_t)handler >> 16;
    gd->gd_selector = selector;
    gd->gd_ist = ist;
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
        .rd_base = (uint64_t)&idt
    };
    int i;

    // reset IDT
    memset((void *)&idt, 0, NIDT * sizeof(idt[0]));

    // initialize IDT with default generic handlers
    for (i = 0; i < NIDT; i++)
        setgd(&idt[i], hwexc_666, 0, SDT_SYSIGT, SEL_KPL,
              GSEL(KCODE_SEL, SEL_KPL));

    /* Setup exception handlers */
    setgd(&idt[0], hwexc_0, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[1], hwexc_1, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[2], hwexc_2, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[3], hwexc_3, 0, SDT_SYSIGT, SEL_UPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[4], hwexc_4, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[5], hwexc_5, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[6], hwexc_6, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[7], hwexc_7, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[8], hwexc_8, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[9], hwexc_9, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[10], hwexc_10, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[11], hwexc_11, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[12], hwexc_12, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[13], hwexc_13, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[14], hwexc_14, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupt 15 is undefined
    setgd(&idt[16], hwexc_16, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[17], hwexc_17, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[18], hwexc_18, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[19], hwexc_19, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    // Interrupts 20 - 31 are reserved

    /* Setup classic PIC interrupt handlers */
    setgd(&idt[32], hwirq_32, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[33], hwirq_33, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[34], hwirq_34, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[35], hwirq_35, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[36], hwirq_36, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[37], hwirq_37, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[38], hwirq_38, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[39], hwirq_39, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[40], hwirq_40, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[41], hwirq_41, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[42], hwirq_42, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[43], hwirq_43, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[44], hwirq_44, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[45], hwirq_45, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[46], hwirq_46, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[47], hwirq_47, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // Setup generic interrupt handlers
    setgd(&idt[48], hwirq_48, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[49], hwirq_49, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[50], hwirq_50, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[50], hwirq_50, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[51], hwirq_51, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[52], hwirq_52, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[53], hwirq_53, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[54], hwirq_54, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[55], hwirq_55, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[56], hwirq_56, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[57], hwirq_57, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[58], hwirq_58, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[59], hwirq_59, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[60], hwirq_60, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[61], hwirq_61, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // XXX Interrupts used for TRACE IPIs
    setgd(&idt[62], hwirq_62, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[63], hwirq_63, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    // Setup local APIC interrupt handlers
    setgd(&idt[249], hwirq_249, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[250], hwirq_250, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[251], hwirq_251, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[252], hwirq_252, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[253], hwirq_253, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));
    setgd(&idt[254], hwirq_254, 0, SDT_SYSIGT, SEL_KPL, GSEL(KCODE_SEL, SEL_KPL));

    /* Load IDT register */
    __asm volatile("lidt %0" :: "m" (region));
}
