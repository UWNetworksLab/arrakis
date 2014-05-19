/** \file
 * \brief x86-specific parts of in-kernel GDB stub.
 *
 * This file implements x86 architecture support for the kernel-side GDB stubs.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
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

/** \brief GDB register save area / frame.
 *
 * Stores pointer to current save frame used by GDB. Used to read/modify
 * register contents, and reloaded when program execution resumes. */
uintptr_t *gdb_arch_registers;

/** \brief Separate stack area for the stub to run on */
static uintptr_t gdb_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)];
/** \brief Pointer to top of GDB stack area. */
uintptr_t * SNT gdb_stack_top = &gdb_stack[KERNEL_STACK_SIZE/sizeof(uintptr_t)];

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

/** \brief Entry point for an exception; we are now on our own stack.
 *
 * This function sets up the GDB-format register save frame, constructs the
 * initial message to the remote GDB and calls into the generic debugger entry
 * point.
 */
void gdb_handle_exception_onstack(int vector, uintptr_t * NONNULL
        COUNT(NUM_REGS) save_area) __attribute__((noreturn));
void gdb_handle_exception_onstack(int vector, uintptr_t * NONNULL
        COUNT(NUM_REGS) save_area)
{
}

/** \brief Get the value of a single register in the frame.
 * \param regnum register number (as defined by the #gdb_register_nums enum)
 * \param value pointer to location in which to return current value
 * \return Zero on success, nonzero on failure (invalid regnum).
 */
int gdb_arch_get_register(int regnum, uintptr_t *value)
{
    if (regnum < 0 || regnum >= ARCH_NUMREGS) {
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
    if (regnum < 0 || regnum >= ARCH_NUMREGS) {
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

/** \brief Resume program execution.
 * \param addr Address to resume at, or 0 to continue at last address.
 */
void gdb_arch_continue(vaddr_t addr)
{
    if (addr != 0) {
        gdb_arch_registers[RIP_REG] = addr;
    }

    /* clear the trace bit */
    gdb_arch_registers[EFLAGS_REG] &= ~0x100; // XXX

    gdb_resume(); /* doesn't return */
}

/** \brief Single-step program execution.
 * \param addr Address to resume at, or 0 to continue at last address.
 */
void gdb_arch_single_step(vaddr_t addr)
{
    if (addr != 0) {
        gdb_arch_registers[RIP_REG] = addr;
    }

    /* set the trace bit for single-step */
    gdb_arch_registers[EFLAGS_REG] |= 0x100; // XXX

    gdb_resume(); /* doesn't return */
}

/** \brief Ensures that the page containing addr is mapped.
 * \return Zero on success, negative on failure.
 */
static int ensure_mapping(vaddr_t addr)
{
#if FALSE
    static paddr_t lastaddr;

    /* check if address is in kernel image */
    if (addr >= KERNEL_OFFSET && addr < (vaddr_t)&_end_kernel) {
        return 0;
    }

    /* if address is outside "physical" memory region, fail the access */
    if (addr < MEMORY_OFFSET || addr >= KERNEL_OFFSET) {
        return -1;
    }

    /* we now know we have a valid "physical memory" region address */
    paddr_t paddr = mem_to_phys(addr);
    paddr -= paddr & MEM_PAGE_MASK; // page-align

    /* quick and dirty optimisation: if this address is on the same page as
     * the last time we were called, return immediately */
    if (lastaddr == paddr && lastaddr != 0) {
        return 0;
    }

    int r = paging_map_memory(paddr, MEM_PAGE_SIZE);
    if (r < 0) {
        return r;
    }

    lastaddr = paddr;
#endif /* FALSE */
    return 0;
}

/** \brief Writes a byte to an arbitrary address in kernel memory.
 * \return Zero on success, nonzero on error (invalid address)
 */
int gdb_arch_write_byte(uint8_t *addr, uint8_t val)
{
    int r = ensure_mapping((vaddr_t)addr);
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
    int r = ensure_mapping((vaddr_t)addr);
    if (r < 0) {
        return r;
    }

    *val = *addr;
    return 0;
}
