/**
 * \file
 * \brief User space memory access functions.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <useraccess.h>

/**
 * Check the validity of the user space buffer.
 *
 * \param type   Type of access to check: ACCESS_WRITE or ACCESS_READ.
 * \param buffer Pointer to beginning of buffer.
 * \param size   Size of buffer.
 */
bool access_ok(uint8_t type, lvaddr_t buffer, size_t size)
{
    // FIXME: Implement!
    return true;
}
