/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef USB_EHCI_MEMORY_H_
#define USB_EHCI_MEMORY_H_


typedef enum {
   USB_EHCI_DS_32BIT,
   USB_EHCI_DS_64BIT
} usb_ds_size_t;

void usb_ehci_set_datastruct_size(usb_ds_size_t size);
void usb_ehci_print_datastruct_sizes(void);


struct usb_ehci_qh *usb_ehci_qh_alloc(void);
void usb_ehci_qh_free(struct usb_ehci_qh *qh);

struct usb_ehci_qtd *usb_ehci_qtd_alloc(void);
void usb_ehci_qtd_free(struct usb_ehci_qtd *qtd);

struct usb_ehci_sitd *usb_ehci_sitd_alloc(void);
void usb_ehci_sitd_free(struct usb_ehci_sitd *sitd);

struct usb_ehci_itd *usb_ehci_itd_alloc(void);
void usb_ehci_itd_free(struct usb_ehci_itd *itd);

usb_paddr_t usb_ehci_buffer_page_alloc(void);
void usb_ehci_buffer_page_free(usb_paddr_t buf);

void usb_ehci_pframes_alloc(usb_ehci_hc_t *hc);


#endif /* USB_EHCI_MEMORY_H_ */
