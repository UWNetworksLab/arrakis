/**
 * \brief this file contains the initialization code for the generic
 *        host controller
 */

/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <barrelfish/barrelfish.h>

#include <usb/usb.h>

#include <usb_controller.h>
#include <usb_device.h>

#include "controller/ohci/usb_ohci.h"
#include "controller/uhci/usb_uhci.h"
#include "controller/ehci/usb_ehci.h"
#include "controller/xhci/usb_xhci.h"

/// list of currently available host controllers
static usb_host_controller_t *host_controllers = NULL;

/**
 * \brief initializes the USB host controller
 *
 * \param hc pointer ot the generic host controller
 * \param version the version [{O, U, E, X}HCI]
 * \param controller_base the base address of the controller
 */
usb_error_t usb_hc_init(usb_host_controller_t *hc, usb_hc_version_t version,
        uintptr_t controller_base)
{
    if (hc == NULL) {
        return (USB_ERR_NOMEM);
    }

    /* initialize the done queue and the interrupt queue of the host controller*/
    hc->done_queue.head.first = NULL;
    hc->done_queue.head.last_next = &(hc->done_queue.head.first);

    hc->intr_queue.head.first = NULL;
    hc->intr_queue.head.last_next = &(hc->intr_queue.head.first);

    usb_error_t err = USB_ERR_INVAL;
    void *controller;

    /* initalize the specific host controller */
    switch (version) {
        case USB_UHCI:
            USER_PANIC("UHCI controller currently not supported\n");
            break;
        case USB_OHCI:
            /* XXX: some functionality is implemented, but not tested... */
            controller = malloc(sizeof(usb_ohci_hc_t));
            if (controller == NULL) {
                return (USB_ERR_NOMEM);
            }
            memset(controller, 0, sizeof(usb_ohci_hc_t));
            hc->hc_type = USB_OHCI;
            hc->hc_control = controller;
            ((usb_ohci_hc_t*) controller)->controller = hc;
            err = usb_ohci_init((usb_ohci_hc_t*) controller,
                    (uintptr_t) controller_base);
            break;
        case USB_EHCI:
            /* allocate the ehci controller */
            controller = malloc(sizeof(usb_ehci_hc_t));

            if (controller == NULL) {
                return (USB_ERR_NOMEM);
            }
            memset(controller, 0, sizeof(usb_ehci_hc_t));

            /* associate it with the generic host controller */
            hc->hc_type = USB_EHCI;
            hc->hc_control = controller;
            ((usb_ehci_hc_t*) controller)->controller = hc;

            /* set the interrupt handler function */
            hc->handle_intr = usb_ehci_interrupt;

            /* initalize the controller */
            err = usb_ehci_init((usb_ehci_hc_t*) controller, controller_base);
            break;
        case USB_XHCI:
            USER_PANIC("XHCI controller currently not supported\n");
            break;
        default:
            return (USB_ERR_INVAL);
            break;

    }

    /* on success: link the new controller into the host controller list */
    if (err == USB_ERR_OK) {
        if (host_controllers == NULL) {
            hc->next = NULL;
            hc->prev_next = NULL;
            host_controllers = hc;
        } else {
            hc->next = host_controllers;
            host_controllers->prev_next = &(hc->next);
            host_controllers = hc;
        }
        hc->initialized = 1;

        return (USB_ERR_OK);
    }

    return (err);

}

/**
 *  \brief interrupt handler function for host controller interrupts
 *
 *  \param arg currently null
 *
 *  This function is called on every interrupt. The interrupt is then forwared
 *  to the specific interrupt handler functions of the respective HCs
 */
void usb_hc_intr_handler(void *arg)
{
    usb_host_controller_t *hc = host_controllers;

    while (hc != NULL) {
        if (hc->handle_intr != NULL) {
            (hc->handle_intr)(hc);
        }
        hc = hc->next;
    }
}
