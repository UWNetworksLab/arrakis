/**
 * \file
 * \brief Arch specific LMP declarations
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_64_BARRELFISH_KPI_LMP_H
#define ARCH_X86_64_BARRELFISH_KPI_LMP_H

/**
 * \brief Maximum total length of LMP and LRPC messages (payload)
 *
 * Determined by number of registers available to transfer messages.
 */
#define LMP_MSG_LENGTH          10
#define LRPC_MSG_LENGTH         4

#endif // ARCH_X86_64_BARRELFISH_KPI_LMP_H
