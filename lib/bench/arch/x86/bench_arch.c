/**
 * \file
 * \brief Bench library initialization.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <bench/bench_arch.h>
#include <barrelfish/sys_debug.h>

bool rdtscp_flag = false;
static uint64_t tscperms;

/**
 * \brief Check if rdtscp instruction is supported
 */
void bench_arch_init(void)
{
    uint32_t eax, ebx, ecx, edx;
    // Check for rdtscp instruction
    cpuid(0x80000001, &eax, &ebx, &ecx, &edx);
    if ((edx >> 27) & 1) {
        rdtscp_flag = true;
    } else {
        rdtscp_flag = false;
    }

    errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));
}

uint64_t bench_tsc_to_ms(cycles_t tsc)
{
    return tsc / tscperms;
}

uint64_t bench_tsc_to_us(cycles_t tsc);
uint64_t bench_tsc_to_us(cycles_t tsc){
	return tsc / (tscperms/1000);
}
