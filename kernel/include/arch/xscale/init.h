/**
 * \file
 * \brief ARM architecture initialization
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INIT_H
#define INIT_H

#ifndef __ASSEMBLER__

struct atag;

void arch_init(uint32_t board_id, struct atag *atag_paddr,
               lvaddr_t elf_image, lvaddr_t phys_alloc_top)
    __attribute__((noreturn));

struct phys_mmap;
void arm_kernel_startup(struct phys_mmap* phys_mmap,
                        lpaddr_t initrd_pa,
                        size_t   initrd_bytes)
    __attribute__((noreturn));

#endif // __ASSEMBLER__

#endif // INIT_H
