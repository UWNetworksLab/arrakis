/**
 * \file
 * \brief Relay header for multiboot structures and kernel-specific
 * function definitions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_MULTIBOOT_H
#define KERNEL_MULTIBOOT_H

#include <multiboot.h>

/**
 * Convert a 32bit address from the Multiboot header to a native virtual
 * address as a char pointer.
 */
#define MBADDR_ASSTRING(vaddr)  (char * NTS)TC((uintptr_t)(local_phys_to_mem(vaddr)))

void multiboot_info_print(struct multiboot_info *mb);
struct multiboot_modinfo *multiboot_find_module(const char *basename);
uintptr_t multiboot_end_addr(struct multiboot_info *mi);

#endif
