/**
 * \file
 * \brief x86-64 architecture initialization
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INIT_H
#define INIT_H

/// Magic value passed when kernel is booted by itself (and not bootloader)
#define KERNEL_BOOT_MAGIC       0x33e1f154

#ifndef __ASSEMBLER__

extern bool idt_initialized;

void arch_init(uint64_t magic, void *pointer) __attribute__ ((noreturn));

/**
 * Fast system call entry point (in Assembler).
 */
extern void syscall_entry(void);

#endif

#endif
