/**
 * \file
 * \brief High-level capability helpers
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CAPABILITIES_ARCH_X86_H
#define CAPABILITIES_ARCH_X86_H

/**
 * \brief Invoke an IO capability to read a byte
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Pointer to returned value
 */
static inline errval_t
iocap_in8(struct capref iocap, uint16_t port, uint8_t *data)
{
    uint32_t tmp = 0;
    errval_t err = invoke_iocap_in(iocap, IOCmd_Inb, port, &tmp);
    if (err_is_ok(err)) {
        *data = tmp;
    }
    return err;
}

/**
 * \brief Invoke an IO capability to read a 16-bit value
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Pointer to returned value
 *
 * \return Error code
 */
static inline errval_t
iocap_in16(struct capref iocap, uint16_t port, uint16_t *data)
{
    uint32_t tmp = 0;
    errval_t err = invoke_iocap_in(iocap, IOCmd_Inw, port, &tmp);
    if (err_is_ok(err)) {
        *data = tmp;
    }
    return err;
}

/**
 * \brief Invoke an IO capability to read a 32-bit value
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Pointer to returned value
 */
static inline errval_t
iocap_in32(struct capref iocap, uint16_t port, uint32_t *data)
{
    return invoke_iocap_in(iocap, IOCmd_Ind, port, data);
}

/**
 * \brief Invoke an IO capability to write a byte
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Output data
 */
static inline errval_t
iocap_out8(struct capref iocap, uint16_t port, uint8_t data)
{
    return invoke_iocap_out(iocap, IOCmd_Outb, port, data);
}

/**
 * \brief Invoke an IO capability to write a 16-bit value
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Output data
 */
static inline errval_t
iocap_out16(struct capref iocap, uint16_t port, uint16_t data)
{
    return invoke_iocap_out(iocap, IOCmd_Outw, port, data);
}

/**
 * \brief Invoke an IO capability to write a 32-bit value
 *
 * \param iocap IO capability
 * \param port  IO port number
 * \param data  Output data
 */
static inline errval_t
iocap_out32(struct capref iocap, uint16_t port, uint32_t data)
{
    return invoke_iocap_out(iocap, IOCmd_Outd, port, data);
}

#endif
