/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_MANAGER_CLIENT_H_
#define USB_MANAGER_CLIENT_H_

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>
#include <if/usb_driver_defs.h>

extern iref_t usb_manager_iref;
extern struct usb_manager_rpc_client usb_manager;

void usb_driver_rx_detach_notify(struct usb_driver_binding *b);
void usb_driver_rx_done_notify(struct usb_driver_binding *b,
        uint32_t tid, uint32_t error, uint8_t *data, size_t length);

#endif /* USB_MANAGER_CLIENT_H_ */
