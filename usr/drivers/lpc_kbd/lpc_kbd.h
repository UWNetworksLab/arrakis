/**
 * \file
 * \brief Legacy keyboard driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_KBD_H
#define LPC_KBD_H

// these are implemented in lpc_kbd.c
int drv_init(void);
void send_mouse_cmd(uint8_t cmd);

// XXX: these are actually callouts to other code
void key_event(uint8_t scancode, bool extended);
void mouse_event(int32_t xdelta, int32_t ydelta,
                 bool left, bool middle, bool right);
void mouse_data(uint8_t val);
void mouse_init(void);

#endif
