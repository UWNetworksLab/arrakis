/**
 * \file
 * \brief x86-64 execution and miscellany
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <init.h>
#include <barrelfish_kpi/cpu.h>
#include <barrelfish_kpi/cpu_arch.h>
#include <exec.h>
#include <irq.h>
#include <x86.h>
#include <dispatch.h>

/**
 * \brief Reboots the system.
 *
 * This function tries hard not to return.
 */
void reboot(void)
{
    struct region_descriptor region = {
        .rd_limit = 0,
        .rd_base = 0
    };

    printk(LOG_NOTE, "Rebooting...\n");

    // try PCI reset register
    uint8_t val = inb(0xcf9) & ~0x6;
    val |= 0x2; // hard reset mode
    outb(0xcf9, val);
    val |= 0x4; // do the reset!
    outb(0xcf9, val);

    // try to reboot using keyboard controller hack (this works on QEMU)
    printk(LOG_NOTE, "PCI reset failed, trying keyboard controller\n");
    // try 10 times!
    for (int i = 0; i < 10; i++) {
        // toggle reset line
        outb(0x64, 0xfe);
    }

    // Otherwise load invalid IDT and cause illegal opcode for triple fault
    printk(LOG_NOTE, "Keyboard controller reset failed, trying triple fault\n");
    __asm volatile("lidt        %[region]       \n\t"
                   "ud2                         \n\t"
                   : /* No output */
                   : [region] "m" (region)
                   );

    halt(); // trick GCC into thinking we don't return
}

/**
 * \brief Triggers a debugger breakpoint.
 */
void breakpoint(void)
{
    if(idt_initialized) {
        hw_breakpoint();
    } else {
        printk(LOG_PANIC,
               "Cannot trap into debugger -- Interrupts not set up yet!\n");
    }
}

/**
 * \brief Go to user-space at entry point 'entry'.
 *
 * This function goes to user-space and starts executing the program at
 * its entry point at virtual address 'entry'.
 *
 * \param entry Entry point address of program to execute.
 */
void __attribute__ ((noreturn))
execute(lvaddr_t entry)
{
    // FIXME: make argument
    uintptr_t arg = get_dispatcher_shared_generic(dcb_current->disp)->udisp;

    __asm volatile ("pushl       %[ss]                  \n\t"
                    "pushl       $0                     \n\t"   // ESP
                    "pushl       %[eflags]              \n\t"
                    "pushl       %[cs]                  \n\t"
                    "pushl       %[entry]               \n\t"   // EIP
                    "movl        $0, %%eax              \n\t"
                    "movl        $0, %%ebx              \n\t"
                    "movl        $0, %%ecx              \n\t"
                    "movl        $0, %%edx              \n\t"
                    "movl        $0, %%esi              \n\t"
                    "movl        $0, %%ebp              \n\t"
                    "mov         %%dx, %%fs             \n\t"
                    "mov         %%dx, %%gs             \n\t"
                    "iretl                              \n\t"
                    : /* No output */
                    :
                    [ss] "i" (GSEL(USTACK_SEL, SEL_UPL)),
                    [cs] "i" (GSEL(UCODE_SEL, SEL_UPL)),
                    [eflags] "r" (USER_EFLAGS),
                    [disp] "D" (arg),
                    [entry] "r" (entry)
                    );

    // Trick GCC to believe us not to return
    halt();
}

/**
 * \brief Resume the given user-space snapshot.
 *
 * This function resumes user-space execution by restoring the CPU
 * registers with the ones given in the array, pointed to by 'regs'.
 */
void __attribute__ ((noreturn)) resume(arch_registers_state_t *state)
{
    struct registers_x86_32 *regs = state;

    __asm volatile ("pushl      %[ss]                   \n\t"
                    "pushl       7*4(%[regs])           \n\t"   // ESP
                    "pushl      %[eflags]               \n\t"
                    "pushl      %[cs]                   \n\t"
                    "pushl       8*4(%[regs])           \n\t"   // EIP
                    "mov         %[fs], %%fs            \n\t"
                    "mov         %[gs], %%gs            \n\t"
                    "movl        1*4(%[regs]), %%ebx    \n\t"
                    "movl        2*4(%[regs]), %%ecx    \n\t"
                    "movl        3*4(%[regs]), %%edx    \n\t"
                    "movl        4*4(%[regs]), %%esi    \n\t"
                    "movl        5*4(%[regs]), %%edi    \n\t"
                    "movl        6*4(%[regs]), %%ebp    \n\t"
                    "movl        0*4(%[regs]), %%eax    \n\t"   // EAX was base register
                    "iretl                              \n\t"
                    : /* No output */
                    :
                    [regs] "a" (regs),
                    [ss] "i" (GSEL(USTACK_SEL, SEL_UPL)),
                    [cs] "i" (GSEL(UCODE_SEL, SEL_UPL)),
                    [fs] "m" (regs->fs),
                    [gs] "m" (regs->gs),
                    [eflags] "r" ((regs->eflags & USER_EFLAGS_MASK)
                                  | USER_EFLAGS)
                    );

    // Trick GCC to believe us not to return
    halt();
}

#if defined(__scc__) && defined(NO_INTERRUPT)
#       include <rck.h>
#       include <dispatch.h>
#endif

/**
 * \brief Halt processor until an interrupt arrives
 *
 * For use in the idle loop when nothing is runnable.
 */
void __attribute__ ((noreturn)) wait_for_interrupt(void)
{
#if defined(__scc__) && defined(NO_INTERRUPT)
#       error Revisit for new scheduling
    for(;;) {
        rck_handle_notification();

        struct dcb *next = schedule();
        if(next) {
            dispatch(next);
        }
    }

#else

    __asm volatile("mov %[x86_32_kernel_stack], %%esp\n\t"
                   "addl %[stack_size], %%esp\n\t"
                   "sti                 \n\t"
                   // The instruction right after STI is still in interrupt
                   // shadow. To avoid unecessary calls to HLT we insert a nop
                   // to make sure pending interrupts are handeled immediately.
                   "nop                 \n\t"
                   "hlt                 \n\t"
                   ::
                   [x86_32_kernel_stack] "r" (&x86_32_kernel_stack),
                   [stack_size] "i" (X86_32_KERNEL_STACK_SIZE)
                   : "esp" );
#endif
    panic("hlt should not return");
}

/**
 * \brief Use MONITOR/MWAIT to block until a given word changes
 *
 * \param base      Virtual address of 64-bit word to monitor
 * \param lastval   Previous value of word
 * \param extensions Processor-specific extensions (zero for defaults)
 * \param hints     Processor-specific hints (zero for defaults)
 *
 * Returns when the 64-bit word at base is not equal to lastval.
 */
// TODO XXX: Where should this propotype live?
void monitor_mwait(lvaddr_t base, uint64_t lastval, uint32_t extensions,
                   uint32_t hints);

void monitor_mwait(lvaddr_t base, uint64_t lastval, uint32_t extensions,
                   uint32_t hints)
{
    volatile uint64_t *val = (uint64_t *)base;

    assert(extensions == 0);
    assert(hints == 0);

    while(*val == lastval) {
        monitor(base, extensions, hints);
        if(*val != lastval) {
            return;
        }
        mwait(hints, extensions);
    }
}
