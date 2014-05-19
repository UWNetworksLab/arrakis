/** \file
 * \brief x86-specific parts of in-kernel GDB stub.
 *
 * This file implements x86 architecture support for the kernel-side GDB stubs.
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <paging_kernel_arch.h>
#include <gdb_stub.h>
#include <arch_gdb_stub.h>
#include <barrelfish_kpi/cpu.h>

/** \brief GDB register save area / frame.
 *
 * Stores pointer to current save frame used by GDB. Used to read/modify
 * register contents, and reloaded when program execution resumes. */
uintptr_t *gdb_arch_registers;

/** \brief Separate stack area for the stub to run on */
static uintptr_t gdb_stack[X86_64_KERNEL_STACK_SIZE/sizeof(uintptr_t)];
/** \brief Pointer to top of GDB stack area. */
uintptr_t * SNT gdb_stack_top = &gdb_stack[X86_64_KERNEL_STACK_SIZE/sizeof(uintptr_t)];

/** \brief Converts exception vector to signal number.
 *
 * This function takes an x86 exception vector and attempts to
 * translate this number into a Unix-compatible signal value.
 */
static int exception_to_signal(int vector)
{
    switch (vector) {
        case 0:     return 8;   // divide by zero
        case 1:     return 5;   // debug exception
        case 3:     return 5;   // breakpoint
        case 4:     return 16;  // into instruction (overflow)
        case 5:     return 16;  // bound instruction
        case 6:     return 4;   // Invalid opcode
        case 7:     return 8;   // coprocessor not available
        case 8:     return 7;   // double fault
        case 9:     return 11;  // coprocessor segment overrun
        case 10:    return 11;  // Invalid TSS
        case 11:    return 11;  // Segment not present
        case 12:    return 11;  // stack exception
        case 13:    return 11;  // general protection
        case 14:    return 11;  // page fault
        case 16:    return 7;   // coprocessor error
        default:    return 7;   // "software generated"
    }
}

__asm__ (
    ".global gdb_handle_exception       \n"
    "gdb_handle_exception:              \n"
    "mov gdb_stack_top(%rip), %rsp      \n"
    "jmp gdb_handle_exception_onstack   \n"
);

/** \brief Entry point for an exception; we are now on our own stack.
 *
 * This function sets up the GDB-format register save frame, constructs the
 * initial message to the remote GDB and calls into the generic debugger entry
 * point.
 */
void gdb_handle_exception_onstack(int vector, uintptr_t * NONNULL
        COUNT(GDB_X86_64_NUM_REGS) save_area) __attribute__((noreturn));
void gdb_handle_exception_onstack(int vector, uintptr_t * NONNULL
        COUNT(GDB_X86_64_NUM_REGS) save_area)
{
    /* sanity check that we didn't trap inside the debugger...
     * if we did, we're definitely hosed now! */
    if (save_area[GDB_X86_64_RSP_REG] >= (lvaddr_t)&gdb_stack &&
        save_area[GDB_X86_64_RSP_REG] <= (lvaddr_t)gdb_stack_top) {
        panic("Nested exception within GDB stub");
    }
    
    /* were we in user mode at the time of the trap? */
    if ((save_area[GDB_X86_64_CS_REG] & 0x3) != 0) {
        printk(LOG_NOTE,
               "Entered from user-mode (at least, CPL in CS is non-zero)\n");

    /* while we're checking the stack pointer, sanity check that it's
     * within the normal kernel stack region */
    } else if (save_area[GDB_X86_64_RSP_REG] < (lvaddr_t)&x86_64_kernel_stack ||
              save_area[GDB_X86_64_RSP_REG] > (lvaddr_t)&x86_64_kernel_stack +
               X86_64_KERNEL_STACK_SIZE) {
        printk(LOG_WARN, "BIG FAT WARNING: kernel stack pointer (0x%lx) is "
               "invalid!\n", save_area[GDB_X86_64_RSP_REG]);
        printk(LOG_WARN, "Boldly attempting to continue into GDB anyway...\n");
    }

    char buffer[64];
    int signal = exception_to_signal(vector);

#if 0 // this is broken, because the register data needs to be in LE byte order
    // T <signal> RSP:<stack ptr>; RIP:<inst ptr>;
    int r = snprintf(buffer, sizeof(buffer), "T%02hhx%x:%lx;%x:%lx;", signal,
                     RSP_REG, save_area[RSP_REG],
                     RIP_REG, save_area[RIP_REG]);
#else
    int r = snprintf(buffer, sizeof(buffer), "S%02hhx", signal);
#endif
    assert(r < sizeof(buffer));
    if (r >= sizeof(buffer)) {
        // ensure termination in the case of overflow
        buffer[sizeof(buffer) - 1] = '\0';
    }

    gdb_arch_registers = save_area;
    gdb_stub_entry(signal, buffer);
}

/** \brief Get the value of a single register in the frame.
 * \param regnum register number (as defined by the #gdb_register_nums enum)
 * \param value pointer to location in which to return current value
 * \return Zero on success, nonzero on failure (invalid regnum).
 */
int gdb_arch_get_register(int regnum, uintptr_t *value)
{
    if (regnum < 0 || regnum >= GDB_X86_64_NUM_REGS) {
        return -1;
    }

    *value = gdb_arch_registers[regnum];
    return 0;
}

/** \brief Set the value of a single register in the frame.
 * \param regnum register number (as defined by the #gdb_register_nums enum)
 * \param value new value
 * \return Zero on success, nonzero on failure (invalid regnum).
 */
int gdb_arch_set_register(int regnum, uintptr_t value)
{
    if (regnum < 0 || regnum >= GDB_X86_64_NUM_REGS) {
        return -1;
    }

    gdb_arch_registers[regnum] = value;
    return 0;
}

/** \brief Resume execution.
 *
 * Resumes execution with the CPU state stored in the #gdb_arch_registers frame.
 */
void gdb_resume(void) __attribute__((noreturn));

__asm__ (
    ".global gdb_resume                 \n"
    "gdb_resume:                        \n"
    /* load address (in PIC manner) of register frame */
    "movq gdb_arch_registers(%rip), %r15\n"
    /* setup stack for iretq below */
    "movq (19*8)(%r15), %rax            \n" // SS
    "pushq %rax                         \n"
    "movq (7*8)(%r15), %rax             \n" // RSP
    "pushq %rax                         \n"
    "movq (17*8)(%r15), %rax            \n" // EFLAGS
    "pushq %rax                         \n"
    "movq (18*8)(%r15), %rax            \n" // CS
    "pushq %rax                         \n"
    "movq (16*8)(%r15), %rax            \n" // EIP
    "pushq %rax                         \n"
    /* load remaining registers directly */
    "movq (0*8)(%r15), %rax             \n"
    "movq (1*8)(%r15), %rbx             \n"
    "movq (2*8)(%r15), %rcx             \n"
    "movq (3*8)(%r15), %rdx             \n"
    "movq (4*8)(%r15), %rsi             \n"
    "movq (5*8)(%r15), %rdi             \n"
    "movq (6*8)(%r15), %rbp             \n"
    "movq (8*8)(%r15), %r8              \n"
    "movq (9*8)(%r15), %r9              \n"
    "movq (10*8)(%r15), %r10            \n"
    "movq (11*8)(%r15), %r11            \n"
    "movq (12*8)(%r15), %r12            \n"
    "movq (13*8)(%r15), %r13            \n"
    "movq (14*8)(%r15), %r14            \n"
    "movq (15*8)(%r15), %r15            \n"
    /* return! */
    "iretq                              \n"
);

/** \brief Resume program execution.
 * \param addr Address to resume at, or 0 to continue at last address.
 */
void gdb_arch_continue(lvaddr_t addr)
{
    if (addr != 0) {
        gdb_arch_registers[GDB_X86_64_RIP_REG] = addr;
    }

    /* clear the trace bit */
    gdb_arch_registers[GDB_X86_64_EFLAGS_REG] &= ~0x100; // XXX

    gdb_resume(); /* doesn't return */
}

/** \brief Single-step program execution.
 * \param addr Address to resume at, or 0 to continue at last address.
 */
void gdb_arch_single_step(lvaddr_t addr)
{
    if (addr != 0) {
        gdb_arch_registers[GDB_X86_64_RIP_REG] = addr;
    }

    /* set the trace bit for single-step */
    gdb_arch_registers[GDB_X86_64_EFLAGS_REG] |= 0x100; // XXX

    gdb_resume(); /* doesn't return */
}

/** \brief Ensures that the page containing addr is mapped.
 * \return Zero on success, negative on failure.
 */
static int ensure_mapping(lvaddr_t addr)
{
    static lpaddr_t lastaddr;

    /* check if address is in kernel image */
    if (addr >= (lvaddr_t)&_start_kernel && addr < (lvaddr_t)&_end_kernel) {
        return 0;
    }

    /* if address is outside "physical" memory region, fail the access */
    if (addr < X86_64_MEMORY_OFFSET) {
        return -1;
    }

    /* we now know we have a valid "physical memory" region address */
    lpaddr_t paddr = mem_to_local_phys(addr);
    paddr -= paddr & X86_64_MEM_PAGE_MASK; // page-align

    /* quick and dirty optimisation: if this address is on the same page as
     * the last time we were called, return immediately */
    if (lastaddr == paddr && lastaddr != 0) {
        return 0;
    }

    int r = paging_x86_64_map_memory(paddr, X86_64_MEM_PAGE_SIZE);
    if (r < 0) {
        return r;
    }

    lastaddr = paddr;
    return 0;
}

/** \brief Writes a byte to an arbitrary address in kernel memory.
 * \return Zero on success, nonzero on error (invalid address)
 */
int gdb_arch_write_byte(uint8_t *addr, uint8_t val)
{
    int r = ensure_mapping((lvaddr_t)addr);
    if (r < 0) {
        return r;
    }
    
    *addr = val;
    return 0;
}

/** \brief Reads a byte from an arbitrary address in kernel memory.
 * \return Zero on success, nonzero on error (invalid address)
 */
int gdb_arch_read_byte(uint8_t *addr, uint8_t *val)
{
    int r = ensure_mapping((lvaddr_t)addr);
    if (r < 0) {
        return r;
    }
    
    *val = *addr;
    return 0;
}
