/**
 * \file
 * \brief Kernel debugging functions
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KERNEL_DEBUG_H
#define KERNEL_DEBUG_H

void debug_vaddr_identify(lvaddr_t pml4, lvaddr_t vaddr);

#endif //KERNEL_DEBUG_H
