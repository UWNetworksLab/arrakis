/**
 * \file
 * \brief Unidirectional bulk data transfer via shared memory
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_BULK_TRANSFER_H
#define ARCH_BULK_TRANSFER_H

static inline void bulk_arch_prepare_send(void *mem, size_t size)
{
    // No-op
}

static inline void bulk_arch_prepare_recv(void *mem, size_t size)
{
    // No-op
}

#endif
