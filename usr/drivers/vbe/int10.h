/**
 * \file
 * \brief Video BIOS int 10h interface.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INT10_H
#define INT10_H

#define REALMODE_MEM_SIZE       (1024 * 1024)

struct int10_regs {
    uint32_t eax;
    uint32_t ebx;
    uint32_t ecx;
    uint32_t edx;
    uint32_t esi;
    uint32_t edi;
    uint32_t ebp;
    uint16_t es;
    uint32_t eflags;
};

void int10(struct int10_regs *regs);
void int10_init(void *mem);

#endif
