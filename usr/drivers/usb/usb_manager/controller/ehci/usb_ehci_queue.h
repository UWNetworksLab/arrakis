/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef USB_EHCI_QUEUE_H_
#define USB_EHCI_QUEUE_H_

#include "usb_ehci.h"

struct usb_xfer;

void usb_ehci_enqueue_xfer_intrq(struct usb_xfer *xfer);


usb_ehci_sitd_t *usb_ehci_enq_fs_td(usb_ehci_sitd_t *std, usb_ehci_sitd_t *last);
usb_ehci_sitd_t *usb_ehci_deq_fs_td(usb_ehci_sitd_t *std, usb_ehci_sitd_t *last);

usb_ehci_itd_t *usb_ehci_enq_hs_td(usb_ehci_itd_t *std, usb_ehci_itd_t *last);
usb_ehci_itd_t *usb_ehci_deq_hs_td(usb_ehci_itd_t *std, usb_ehci_itd_t *last);

usb_ehci_qh_t *usb_ehci_enq_qh(usb_ehci_qh_t *qh, usb_ehci_qh_t *last);
usb_ehci_qh_t *usb_ehci_deq_qh(usb_ehci_qh_t *qh, usb_ehci_qh_t *last);

#endif /* USB_EHCI_QUEUE_H_ */
