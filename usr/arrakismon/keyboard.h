/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef KEYBOARD_H
#define KEYBOARD_H

#include <stdint.h>
#include "guest.h"
#include "x86.h"

struct keyboard {
    struct guest *guest;
    uint8_t status;             ///< The status register of this controller
    uint8_t write_cmd;          ///< Pending write command to the data reg
};

#define KEYBOARD_STATUS_OUT_DATA            (1 << 0)
#define KEYBOARD_STATUS_IN_DATA             (1 << 1)
#define KEYBOARD_STATUS_SYSTEM_FLAG         (1 << 2)
#define KEYBOARD_STATUS_IN_CMD              (1 << 3)
#define KEYBOARD_STATUS_ENABLED             (1 << 4)
#define KEYBOARD_STATUS_TR_TIMEOUT          (1 << 5)
#define KEYBOARD_STATUS_RC_TIMEOUT          (1 << 6)
#define KEYBOARD_STATUS_PARITY              (1 << 7)

struct keyboard *keyboard_new (struct guest *g);
int keyboard_handle_pio_read (struct keyboard *k, uint16_t port,
                              enum opsize size, uint32_t *val);
int keyboard_handle_pio_write (struct keyboard *k, uint16_t port,
                               enum opsize size, uint32_t val);
int keyboard_handle_int16 (struct keyboard *k, struct guest *g);

#endif // KEYBOARD_H
