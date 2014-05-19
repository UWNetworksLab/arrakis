/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef USB_MANAGER_SERVICE_H_
#define USB_MANAGER_SERVICE_H_

#include <thc/thc.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_thc.h>


errval_t usb_manager_service_init(void);
void usb_manager_service_start(void);
#endif /* USB_MANAGER_SERVICE_H_ */
