/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <stdio.h>
#include <arch/x86/debug.h>
#include <paging_kernel_arch.h>

union lin_addr {
    uint32_t raw;
    struct {
        uint32_t  offset       :12;
        uint32_t  ptable       :9;
        uint32_t  pdir         :9;
        uint32_t  pdpt         :2;
    } d;
};

void debug_vaddr_identify(lvaddr_t debug_pdpte, lvaddr_t vaddr)
{
    panic("NYI");
}
