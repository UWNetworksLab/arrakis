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

#ifndef CONSOLE_H
#define CONSOLE_H

#include <stdint.h>
#include <guest.h>

struct console {
    uint8_t cursor_pos_x, cursor_pos_y;
};

struct console * console_new (void);
int console_handle_int10 (struct console *c, struct guest *g);

#endif // CONSOLE_H
