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
#include <target/x86_64/barrelfish_kpi/cpu_target.h>

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

    /*
     * Go to user-space using SYSRETQ -- the Q is very important, so operand
     * size is 64-bit. Otherwise we return to compatibility mode.
     *
     * We set the startup contents of the RFLAGS register into R11. RCX is
     * set to the entry point address of the user-space program. All other
     * general-purpose registers are zeroed.
     */
    __asm volatile ("movq       %[flags], %%r11         \n\t" 
                    "movq       $0, %%rsi               \n\t"
                    "movq       $0, %%rdx               \n\t"
                    "movq       $0, %%r8                \n\t"
                    "movq       $0, %%r9                \n\t"
                    "movq       $0, %%r10               \n\t"
                    "movq       $0, %%r12               \n\t"
                    "movq       $0, %%r13               \n\t"
                    "movq       $0, %%r14               \n\t"
                    "movq       $0, %%r15               \n\t"
                    "movq       $0, %%rax               \n\t"
                    "movq       $0, %%rbx               \n\t"
                    "movq       $0, %%rbp               \n\t"
                    "movq       $0, %%rsp               \n\t"
                    "mov        %%dx, %%fs              \n\t"
                    "mov        %%dx, %%gs              \n\t"
                    "sysretq                            \n\t"
                    : /* No output */
                    :
                    [entry] "c" (entry),
                    [disp] "D" (arg),
                    [flags] "i" (USER_RFLAGS)
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
    struct registers_x86_64 *regs = state;
    __asm volatile ("pushq      %[ss]                   \n\t"
                    "pushq       7*8(%[regs])           \n\t"   // RSP
                    "pushq      %[rflags]               \n\t"
                    "pushq      %[cs]                   \n\t"
                    "pushq      16*8(%[regs])           \n\t"   // RIP
                    "mov         %[fs], %%fs            \n\t"
                    "mov         %[gs], %%gs            \n\t"
                    "movq        0*8(%[regs]), %%rax    \n\t"
                    "movq        2*8(%[regs]), %%rcx    \n\t"
                    "movq        3*8(%[regs]), %%rdx    \n\t"
                    "movq        4*8(%[regs]), %%rsi    \n\t"
                    "movq        5*8(%[regs]), %%rdi    \n\t"
                    "movq        6*8(%[regs]), %%rbp    \n\t"
                    "movq        8*8(%[regs]), %%r8     \n\t"
                    "movq        9*8(%[regs]), %%r9     \n\t"
                    "movq       10*8(%[regs]), %%r10    \n\t"
                    "movq       11*8(%[regs]), %%r11    \n\t"
                    "movq       12*8(%[regs]), %%r12    \n\t"
                    "movq       13*8(%[regs]), %%r13    \n\t"
                    "movq       14*8(%[regs]), %%r14    \n\t"
                    "movq       15*8(%[regs]), %%r15    \n\t"
                    "movq        1*8(%[regs]), %%rbx    \n\t"   // RBX was base register
                    "iretq                              \n\t"
                    : /* No output */
                    :
                    [regs] "b" (regs),
                    [ss] "i" (GSEL(USTACK_SEL, SEL_UPL)),
                    [cs] "i" (GSEL(UCODE_SEL, SEL_UPL)),
                    [fs] "m" (regs->fs),
                    [gs] "m" (regs->gs),
                    [rflags] "r" ((regs->eflags & USER_RFLAGS_MASK)
                                  | USER_RFLAGS)
                    );

    // Trick GCC to believe us not to return
    halt();
}

/**
 * \brief Halt processor until an interrupt arrives
 *
 * For use in the idle loop when nothing is runnable.
 */
void __attribute__ ((noreturn)) wait_for_interrupt(void)
{
    __asm volatile("lea x86_64_kernel_stack(%%rip), %%rsp\n\t"
                   "addq %[stack_size], %%rsp\n\t"
                   "sti                 \n\t"
                   // The instruction right after STI is still in interrupt
                   // shadow. To avoid unecessary calls to HLT we insert a nop
                   // to make sure pending interrupts are handeled immediately.
                   "nop                 \n\t"
                   "hlt                 \n\t"
                   :: [stack_size] "i" (X86_64_KERNEL_STACK_SIZE) : "rsp" );
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

/// Remember current LDT pointer, so we can avoid reloading it
lvaddr_t current_ldt_base = -1;
size_t current_ldt_npages;

void maybe_reload_ldt(struct dcb *dcb, bool force_reload)
{
    struct dispatcher_shared_x86_64 *disp =
        get_dispatcher_shared_x86_64(dcb->disp);

    /* Read fields from user dispatcher once for consistency */
    lvaddr_t ldt_base = disp->ldt_base;
    size_t ldt_npages = disp->ldt_npages;

    /* optimize out if this is the same as the previous LDT */
    if (!force_reload && ldt_base == current_ldt_base
        && ldt_npages == current_ldt_npages) {
        return;
    }

    uint16_t selector = 0;

    if (ldt_base != 0 && ldt_npages != 0) {
        extern union segment_descriptor *ldt_descriptor;
        ldt_descriptor[0].sys_lo.lo_base = ldt_base & ((1ul << 24) - 1);
        ldt_descriptor[0].sys_lo.hi_base = (ldt_base >> 24) & 0xff;
        ldt_descriptor[1].sys_hi.base = ldt_base >> 32;
        assert(ldt_descriptor[0].sys_lo.granularity != 0);
        ldt_descriptor[0].sys_lo.lo_limit = ldt_npages;

        selector = GSEL(LDT_LO_SEL, SEL_UPL);
    }

    __asm volatile("lldt %%ax"
                   : /* No output */
                   : "a" (selector));

    current_ldt_base = ldt_base;
    current_ldt_npages = ldt_npages;
}
