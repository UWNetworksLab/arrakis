/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_KEYBOARD_SERVICE_H_
#define USB_KEYBOARD_SERVICE_H_


errval_t usb_keyboard_service_init(void);
void key_event(uint8_t scancode, bool extended);

#endif /* USB_KEYBOARD_SERVICE_H_ */
