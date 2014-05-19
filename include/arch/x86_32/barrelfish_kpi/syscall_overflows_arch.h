/**
 * \file
 * \brief structures used to deal with overflows to the syscall arguments
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_KPI_SYSCALL_OVERFLOW_H
#define ARCH_X86_32_BARRELFISH_KPI_SYSCALL_OVERFLOW_H

struct remote_retype_syscall_overflow {
    capaddr_t rootcap_addr;
    uint8_t rootcap_vbits;
};

#endif
