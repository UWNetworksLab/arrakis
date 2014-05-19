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
    struct registers_x86_32 *regs = archregs;

    debug_printf("Dumping stack (0x%" PRIx32 ")...\n", regs->esp);
    debug_dump_mem_around_addr(regs->esp);
    // debug_printf("Dumping code (0x%x)...\n", regs->eip);
    // debug_dump_mem_around_addr(regs->eip);
    // debug_printf("Dumping memory around ebp (0x%x)\n", regs->ebp);
    // debug_dump_mem_around_addr(regs->ebp);
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
    debug_call_chain_rbp(archregs->ebp);
}

/**
 * \brief Print out the registers in a dispatcher save area, for trap handlers.
 */
void debug_print_save_area(arch_registers_state_t *state)
{
    uintptr_t *regs = (uintptr_t*)state;
    const char *reg_names[]={"eax", "ebx", "ecx", "edx", "esi", "edi", "ebp",
                             "esp", "eip", "eflags", "cs", "ss"};
    for (int i = 0; i < X86_32_NUM_REGS; i++) {
        debug_printf("%02u: %016" PRIxPTR " %s\n", i, regs[i], reg_names[i]);
    }
}

void debug_return_addresses(void)
{
    debug_printf("return address = %p\n", __builtin_return_address(0));
    debug_printf("return address = %p\n", __builtin_return_address(1));
    debug_printf("return address = %p\n", __builtin_return_address(2));
    debug_printf("return address = %p\n", __builtin_return_address(3));
    debug_printf("return address = %p\n", __builtin_return_address(4));
}

void debug_print_fpu_state(struct registers_fpu_x86_32 *fpustate)
{
    uint8_t *regs8 = fpustate->registers;
    regs8 += 16 - ((uintptr_t)regs8 % 16);

    uint16_t *regs16 = (void *)regs8;
    uint32_t *regs32 = (void *)regs8;
    uint64_t *regs64 = (void *)regs8;

    debug_printf("FCW %x\n", regs16[0]);
    debug_printf("FSW %x\n", regs16[1]);
    debug_printf("FTW %x\n", regs8[4]);
    debug_printf("FOP %x\n", regs16[3]);
    debug_printf("IP  %" PRIx32 "\n", regs32[3]);
    debug_printf("CS  %x\n", regs16[6]);
    debug_printf("DP  %" PRIx32 "\n", regs32[5]);
    debug_printf("DS  %x\n", regs16[10]);
    debug_printf("MXCSR %" PRIx32 "\n", regs32[7]);
    debug_printf("MXCSR mask %" PRIx32 "\n", regs32[8]);

    for (int i = 0; i < 7; i++) {
        debug_printf("ST%d/MM%d %llx:%llx\n", i, i,
                     regs64[4 + i * 2 + 1] & 0xffff,
                     regs64[4 + i * 2]);
    }

    for (int i = 0; i < 7; i++) {
        debug_printf("XMM%d %llx:%llx\n", i,
                     regs64[20 + i * 2 + 1],
                     regs64[20 + i * 2]);
    }
}
