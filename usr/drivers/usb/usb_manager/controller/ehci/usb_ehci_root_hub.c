/**
 * \brief this file contains the emulation code for the root hub
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

#include "ehci_device.h"

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>
#include <usb/usb_request.h>

#include <usb_device.h>
#include <usb_controller.h>
#include <usb_hub.h>
#include "usb_ehci.h"
#include "usb_ehci_root_hub.h"


/*
 * --------------------------------------------------------------------------
 * local variables defining the descriptors of the root hub
 */
static const struct usb_device_descriptor rh_dev_desc = {
    .bLength = sizeof(struct usb_device_descriptor),
    .bDescriptorType = USB_DESCRIPTOR_TYPE_DEVICE,
    .bcdUSB = 0x0200,
    .bDeviceClass = USB_HUB_CLASS_CODE,
    .bDeviceSubClass = USB_HUB_SUBCLASS_CODE,
    .bDeviceProtocol = USB_HUB_PROTOCOL_HSHUBSTT,
    .bMaxPacketSize0 = 64,
    .idVendor = 0,
    .idProduct = 0,
    .bcdDevice = 0x0100,
    .iManufacturer = 1,
    .iProduct = 2,
    .iSerialNumber = 0,
    .bNumConfigurations = 1,
};

static const struct usb_device_qualifier_descriptor rh_qual_desc = {
    .bLength = sizeof(struct usb_device_qualifier_descriptor),
    .bDescriptorType = USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER,
    .bcdUSB = 0x0200,
    .bDeviceClass = USB_HUB_CLASS_CODE,
    .bDeviceSubClass = USB_HUB_SUBCLASS_CODE,
    .bDeviceProtocol = USB_HUB_PROTOCOL_FSHUB,
    .bMaxPacketSize0 = 0,
    .bNumConfigurations = 0,
    .bReserved = 0,
};

static const struct usb_ehci_config_descriptor rh_cfg_desc = {
    .config = {
        .bLength = sizeof(struct usb_config_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_CONFIG,
        .wTotalLength = sizeof(rh_cfg_desc),
        .bNumInterfaces = 1,
        .bConfigurationValue = 1,
        .iConfiguration = 0,
        .bmAttributes = USB_CONFIG_SELF_POWERED,
        .bMaxPower = 0,
    },
    .iface = {
        .bLength = sizeof(struct usb_interface_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_INTERFACE,
        .bNumEndpoints = 1,
        .bInterfaceClass = USB_HUB_IFACE_CLASS_CODE,
        .bInterfaceSubClass = USB_HUB_IFACE_SUBCLASS_CODE,
        .bInterfaceProtocol = 0,
    },
    .endpoint = {
        .bLength = sizeof(struct usb_endpoint_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_ENDPOINT,
        .bEndpointAddress = {
            USB_ENDPOINT_DIRECTION_IN,
            0,
            1
        },
        .bmAttributes = {
            0,
            0,
            0,
            USB_ENDPOINT_TYPE_INTR
        },
        .wMaxPacketSize = (8 << 8),
        .bInterval = 255,
    },
};

static const struct usb_hub_descriptor rh_desc = {
    .bDescLength = 0,
    .bDescriptorType = USB_DESCRIPTOR_TYPE_HUB,
    .bNbrPorts = 0,
    .wHubCharacteristics = {
        0,
        0,
        0,
        0,
        0,
        0
    },
    .bPwrOn2PwrGood = 0,
    .bHubContrCurrent = 0,
    .bDeviceRemovable = {
        0
    }
};

/*
 * --------------------------------------------------------------------------
 * functions emulating the root hub requests
 */


/**
 * \brief this function is called when a port change detected interrupt is risen
 *
 * \param hc the host controller which got the interrupt
 */
void usb_ehci_roothub_interrupt(usb_ehci_hc_t *hc)
{
    USB_DEBUG_TR_ENTER;

    for (uint16_t i = 0; i < hc->rh_num_ports; i++) {
        ehci_portsc_t ps = ehci_portsc_rd(&hc->ehci_base, i);
        /* clear out the change bits */
        if (ehci_portsc_occ_extract(ps) || ehci_portsc_pec_extract(ps)
                || ehci_portsc_csc_extract(ps)) {
            USB_DEBUG_HC("roothub_interrupt: port %i has changed\n", i+1);
        }
    }
    /* defer the handing to the hub driver */
    usb_hub_root_interrupt(hc->controller);
}

#define C(req, recipent, dir) ((req) | ((recipent)<<8) | ((dir)<<16))

/**
 * \brief emulates the execution of the requests on the root hub
 *
 * \param device the roothub device
 * \param req the request to execute
 * \param ret_data the data returned when a read request was executed
 * \param ret_length the number of bytes in the returned data
 */
usb_error_t usb_ehci_roothub_exec(struct usb_device *device,
        struct usb_device_request *req, const void **ret_data,
        uint16_t *ret_length)
{
    USB_DEBUG_TR_ENTER;
    usb_ehci_hc_t *hc = (usb_ehci_hc_t *) device->controller->hc_control;
    const char *str;
    const void *data = (const void *) &hc->rh_desc;
    uint16_t data_length = 0;


    switch (C(req->bRequest, req->bType.recipient, req->bType.direction)) {
        /* clear feature requests */
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_WRITE):
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_INTERFACE, USB_REQUEST_WRITE):
        case C(USB_REQUEST_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_ENDPOINT, USB_REQUEST_WRITE):
            /* no-op: don't handle write requests */
            break;

        /* get configuration request */
        case C(USB_REQUEST_GET_CONFIG, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_READ):
            data_length = 1;
            hc->rh_desc.temp[0] = hc->rh_device_config;
            break;

        /* get descriptor request */
        case C(USB_REQUEST_GET_DESCRIPTOR, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_READ):
            switch (req->wValue >> 8) {
                /*
                 * the only the string type has an id.. the others
                 * result in an IO error if there is an id set.
                 */
                case USB_DESCRIPTOR_TYPE_DEVICE:
                case USB_DESCRIPTOR_TYPE_HUB:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    if (req->bType.type != USB_REQUEST_TYPE_CLASS) {
                        data_length = sizeof(rh_dev_desc);
                        data = (const void *) &rh_dev_desc;
                        break;
                    }
                    /* handling class specific request  */
                    hc->rh_desc.hub_desc = rh_desc;
                    hc->rh_desc.hub_desc.bNbrPorts = hc
                            ->rh_num_ports;
                    hc->rh_desc.hub_desc.wHubCharacteristics
                            .port_indicator = ehci_hcsparams_p_indicator_rdf(
                            &hc->ehci_base);
                    hc->rh_desc.hub_desc.wHubCharacteristics
                            .power_mode = ehci_hcsparams_ppc_rdf(&hc->ehci_base);
                    hc->rh_desc.hub_desc.bPwrOn2PwrGood = 200;
                    hc->rh_desc.hub_desc.bDescLength = 8
                            + ((hc->rh_num_ports + 7) / 8);
                    data_length = hc->rh_desc.hub_desc.bDescLength;
                    break;
                case USB_DESCRIPTOR_TYPE_DEVICE_QUALIFIER:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    data_length = sizeof(rh_qual_desc);
                    data = (const void *) &rh_qual_desc;
                    break;

                case USB_DESCRIPTOR_TYPE_CONFIG:
                    if ((req->wValue & 0xFF) != 0) {
                        return (USB_ERR_IOERROR);
                    }
                    data_length = sizeof(rh_cfg_desc);
                    data = (const void *) &rh_cfg_desc;
                    break;

                case USB_DESCRIPTOR_TYPE_STRING:
                    switch (req->wValue & 0xFF) {
                        case 0:
                            str = "\001";
                            break;
                        case 1:
                            str = hc->rh_vendor;
                            break;
                        case 2:
                            str = "EHCI root hub";
                            break;
                        default:
                            str = "";
                            break;
                    }
                    /*
                     * TODO: MAKE STRING DESCRIPTOR
                     * len = ...
                     * store in hub_desc.tmp
                     */
                    break;
                default:
                    debug_printf("GET_DESC ->IOERR\n");
                    return (USB_ERR_IOERROR);
                    break;
            }
            break;

        /* get interface requests */
        case C(USB_REQUEST_GET_INTERFACE, USB_REQUEST_RECIPIENT_INTERFACE, USB_REQUEST_READ):
            /* we don't have an alternative interface */
            data_length = 1;
            hc->rh_desc.temp[0] = 0;
            break;

        /* get status request - device */
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_READ):
            if (req->bType.type != USB_REQUEST_TYPE_CLASS) {
                data_length = 2;
                hc->rh_desc.status.wStatus = USB_STATUS_SELF_POWERED;
                break;
            }
            data_length = 16;
            memset(hc->rh_desc.temp, 0, 16);

            break;

        /* get status request - interface or endpoint */
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_INTERFACE, USB_REQUEST_READ):
        case C(USB_REQUEST_GET_STATUS, USB_REQUEST_RECIPIENT_ENDPOINT, USB_REQUEST_READ):
            data_length = 2;
            hc->rh_desc.status.wStatus = 0;
            break;

        /* set address request */
        case C(USB_REQUEST_SET_ADDRESS, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_WRITE):
            if (req->wValue >= USB_EHCI_MAX_DEVICES) {
                return (USB_ERR_IOERROR);
            }
            hc->rh_device_address = req->wValue;
            break;

        /* set configuration request */
        case C(USB_REQUEST_SET_CONFIG, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_WRITE):
            if ((req->wValue != 0) && (req->wValue != 1)) {
                return (USB_ERR_IOERROR);
            }
            hc->rh_device_config = req->wValue;
            break;

        /* set descriptor request */
        case C(USB_REQUEST_SET_DESCRIPTOR, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_WRITE):
            /* do not allow to change the descriptor */
            break;

        /* set feature request */
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_DEVICE, USB_REQUEST_WRITE):
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_INTERFACE, USB_REQUEST_WRITE):
        case C(USB_REQUEST_SET_FEATURE, USB_REQUEST_RECIPIENT_ENDPOINT, USB_REQUEST_WRITE):
            /* setting a feature results in IO error */
            return (USB_ERR_IOERROR);
            break;

        /* set interface request */
        case C(USB_REQUEST_SET_INTERFACE, USB_REQUEST_RECIPIENT_INTERFACE, USB_REQUEST_WRITE):
            break;

        /* set synch frame request */
        case C(USB_REQUEST_SYNCH_FRAME, USB_REQUEST_RECIPIENT_ENDPOINT, USB_REQUEST_WRITE):
            break;

        /*
         * handling hub class specific requests
         */

        /* clear hub feature request */
        case C(USB_HUB_REQ_CLEAR_FEATURE, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_WRITE):
            if ((req->wIndex < 1) || (req->wLength > hc->rh_num_ports)) {
                /* invalid port nuber  */
                return (USB_ERR_IOERROR);
            }
            /* mackerel is zero based */
            req->wIndex--;

            switch (req->wValue) {
                case USB_HUB_FEATURE_PORT_ENABLE:
                    ehci_portsc_ped_wrf(&hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_PORT_SUSPEND:
                    if (ehci_portsc_sus_rdf(&hc->ehci_base, req->wIndex)
                            && (!ehci_portsc_fpr_rdf(&hc->ehci_base, req->wIndex))) {
                        ehci_portsc_fpr_wrf(&hc->ehci_base, req->wIndex, 1);
                    }
                    USB_WAIT(20);

                    ehci_portsc_sus_wrf(&hc->ehci_base, req->wIndex, 0);
                    ehci_portsc_fpr_wrf(&hc->ehci_base, req->wIndex, 0);
                    ehci_portsc_ls_wrf(&hc->ehci_base, req->wIndex, 0x3);
                    USB_WAIT(4);
                    break;
                case USB_HUB_FEATURE_PORT_POWER:
                    ehci_portsc_pp_wrf(&hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_PORT_TEST:
                    /* clear port test */
                    break;

                case USB_HUB_FEATURE_PORT_INDICATOR:
                    ehci_portsc_pic_wrf(&hc->ehci_base, req->wIndex, 0);
                    break;
                case USB_HUB_FEATURE_C_PORT_CONNECTION:
                    ehci_portsc_csc_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;

                case USB_HUB_FEATURE_C_PORT_ENABLE:
                    ehci_portsc_pec_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_SUSPEND:
                    ehci_portsc_sus_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
                    ehci_portsc_occ_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_C_PORT_RESET:
                    hc->rh_reset = 0;
                    break;
                default:
                    return (USB_ERR_IOERROR);
                    break;
            }
            break;


        /* get hub status request */
        case C(USB_HUB_REQ_GET_STATUS, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_READ):
            if ((req->wIndex < 1) || (req->wIndex > hc->rh_num_ports)) {
                /* invalid port number  */
                debug_printf("ehci: root_hub_exec: invalid port number %i\n",
                        req->wIndex);
                return (USB_ERR_IOERROR);
            }
            data_length = sizeof(hc->rh_desc.port_status);
            struct usb_hub_port_status *ps = &(hc->rh_desc
                    .port_status);
            memset(ps, 0, sizeof(*ps));
            /* subtract one, mackerel is zero based */
            ehci_portsc_t ehci_ps = ehci_portsc_rd(&hc->ehci_base,
                    req->wIndex - 1);

            ps->wPortStatus.enabled = ehci_portsc_ped_extract(ehci_ps);
            ps->wPortStatus.indicator = ehci_portsc_pic_extract(ehci_ps) > 0;
            ps->wPortStatus.test_mode = ehci_portsc_ptc_extract(ehci_ps) > 0;

            if (ehci_portsc_ls_extract(ehci_ps) == 0x1) {
                /* low speed device */
                USB_DEBUG("port (%u) has low speed device\n", req->wIndex);
                ps->wPortStatus.is_ls = 1;
                ps->wPortStatus.is_hs = 0;
            } else {
                ps->wPortStatus.is_hs = 1;
                ps->wPortStatus.is_ls = 0;
            }

            ps->wPortStatus.power_state = ehci_portsc_pp_extract(ehci_ps);
            ps->wPortStatus.reset = ehci_portsc_pr_extract(ehci_ps);
            ps->wPortStatus.over_current = ehci_portsc_oca_extract(ehci_ps);
            if (ehci_portsc_sus_extract(ehci_ps)
                    && !(ehci_portsc_fpr_extract(ehci_ps))) {
                ps->wPortStatus.suspend = 1;
            }
            ps->wPortStatus.connection = ehci_portsc_ccs_extract(ehci_ps);

            ps->wPortChange.is_reset = hc->rh_reset;
            ps->wPortChange.over_current = ehci_portsc_occ_extract(ehci_ps);
            ps->wPortChange.resumed = ehci_portsc_fpr_extract(ehci_ps);
            ps->wPortChange.disabled = ehci_portsc_pec_extract(ehci_ps);
            ps->wPortChange.connect = ehci_portsc_csc_extract(ehci_ps);

            break;

        /* set hub feature request */
        case C(USB_HUB_REQ_SET_FEATURE, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_WRITE):
            if ((req->wIndex < 1) || (req->wIndex > hc->rh_num_ports)) {
                /* invalid port number  */
                return (USB_ERR_IOERROR);
            }
            /* mackerel is zero based */
            req->wIndex--;
            switch (req->wValue) {
                case USB_HUB_FEATURE_PORT_ENABLE:
                    ehci_portsc_ped_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_SUSPEND:
                    ehci_portsc_sus_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_RESET:
                    if (ehci_portsc_ls_rdf(&hc->ehci_base, req->wIndex) == 0x1) {
                        /* low speed device */
                        usb_ehci_roothub_port_disown(hc, req->wIndex);
                        break;
                    }
                    /* initiate reset sequence */
                    ehci_portsc_pr_wrf(&hc->ehci_base, req->wIndex, 1);
                    USB_WAIT(200);

                    /* clear the reset */
                    ehci_portsc_pr_wrf(&hc->ehci_base, req->wIndex, 0);
                    USB_WAIT(200);

                    if (ehci_portsc_pr_rdf(&hc->ehci_base, req->wIndex)) {
                        debug_printf("exec: timeout while resetting port\n");
                        return (USB_ERR_TIMEOUT);
                    }

                    if (!ehci_portsc_ped_rdf(&hc->ehci_base, req->wIndex)) {
                        /*
                         * TODO: DISOWNING PORTS...
                         * if (hc->flags.tt_present) {
                            usb_ehci_roothub_port_disown(hc, req->wIndex, 0);
                        }*/
                    }
                    hc->rh_reset = 1;

                    break;
                case USB_HUB_FEATURE_PORT_POWER:
                    ehci_portsc_pp_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                case USB_HUB_FEATURE_PORT_TEST:
                    break;
                case USB_HUB_FEATURE_PORT_INDICATOR:
                    ehci_portsc_pic_wrf(&hc->ehci_base, req->wIndex, 1);
                    break;
                default:
                    return (USB_ERR_IOERROR);
            }
            break;

        /* transaction translator requests */
        case C(USB_HUB_REQ_CLEAR_TT_BUFFER, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_WRITE):
        case C(USB_HUB_REQ_RESET_TT, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_WRITE):
        case C(USB_HUB_REQ_STOP_TT, USB_REQUEST_RECIPIENT_OTHER, USB_REQUEST_WRITE):
            /* root hub does not have a transaction translator */
            break;
        default:
            return (USB_ERR_IOERROR);
    }

    if (ret_length) {
        *ret_length = data_length;
    }

    if (ret_data) {

        *ret_data = data;
    }

    return (USB_ERR_OK);
}


/*
 * \brief this function hands over the port to the companion controller
 *
 * \param hc the host controller of the root hub
 * \param portno the port number to disown
 */
void usb_ehci_roothub_port_disown(usb_ehci_hc_t *hc, uint16_t portno)
{
    if (portno > ehci_hcsparams_n_ports_rdf(&hc->ehci_base)) {
        debug_printf("ERROR: port does not exist! \n");
        return;
    }

    assert(portno > 0);
    /* mackerel is zero based */
    portno--;

    ehci_portsc_po_wrf(&hc->ehci_base, portno, 1);

}

