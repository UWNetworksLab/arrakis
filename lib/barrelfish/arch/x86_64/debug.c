/**
 * \file
 * \brief Arch specific debugging functions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/debug.h>
#include <barrelfish/dispatch.h>
#include <if/monitor_defs.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#define NR_OF_DISPLAYED_RET_ADDRS   10

/**
 * \brief Dump out various memory regions and a partial backtrace.
 *
 * Mainly for debugging traps and faults in the dispatcher handlers.
 */
void debug_dump(arch_registers_state_t *archregs)
{
    struct registers_x86_64 *regs = archregs;

    debug_printf("Dumping stack (0x%lx)...\n", regs->rsp);
    debug_dump_mem_around_addr(regs->rsp);
    // debug_printf("Dumping code (0x%lx)...\n", regs->rip);
    // debug_dump_mem_around_addr(regs->rip);
    // debug_printf("Dumping memory around rbp (0x%lx)\n", regs->rbp);
    // debug_dump_mem_around_addr(regs->rbp);
}

static void debug_call_chain_rbp(uintptr_t bp)
{
    uintptr_t ret_addr;
    uintptr_t user_rbp = bp;

    for (int it = 0; it < NR_OF_DISPLAYED_RET_ADDRS; it++) {
        if (user_rbp < BASE_PAGE_SIZE || (user_rbp % sizeof(uintptr_t)) != 0) {
            break;
        }
        // get return address
        ret_addr = *(uintptr_t *)(user_rbp + sizeof(uintptr_t));
        debug_printf("return address = 0x%" PRIxPTR "\n", ret_addr);
        // get next RBP
        user_rbp = *(uintptr_t *)user_rbp;
    }
}

void debug_call_chain(arch_registers_state_t *archregs)
{
    debug_call_chain_rbp(archregs->rbp);
}

/**
 * \brief Print out the registers in a dispatcher save area, for trap handlers.
 */
void debug_print_save_area(arch_registers_state_t *state)
{
//#define P(x) debug_printf("%16lx "#x"\n", (uintptr_t)state->x);
#define P(x) printf("%16lx "#x"\n", (uintptr_t)state->x);

    P(rip);
    P(rsp);
    P(rbp);
    P(rax);
    P(rbx);
    P(rcx);
    P(rdx);
    P(rdi);
    P(rsi);
    P(r8);
    P(r9);
    P(r10);
    P(r11);
    P(r12);
    P(r13);
    P(r14);
    P(r15);
    P(eflags);
    P(fs);
    P(gs);

#undef P
}

void debug_return_addresses(void)
{
    printf("return address = %p\n", __builtin_return_address(0));
    printf("return address = %p\n", __builtin_return_address(1));
    printf("return address = %p\n", __builtin_return_address(2));
    printf("return address = %p\n", __builtin_return_address(3));
    printf("return address = %p\n", __builtin_return_address(4));
}
