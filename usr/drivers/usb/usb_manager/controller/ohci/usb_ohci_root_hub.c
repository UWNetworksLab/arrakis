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

#include "ohci_device.h"

#include <usb/usb.h>
#include <usb/usb_descriptor.h>
#include <usb/usb_error.h>
#include <usb/usb_request.h>
#include <usb_device.h>
#include <usb_hub.h>
#include <usb_controller.h>
#include "usb_ohci.h"
#include "usb_ohci_root_hub.h"


static const struct usb_device_descriptor usb_ohci_root_hub_device_desc = {
        .bLength = sizeof(struct usb_device_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_DEVICE,
        .bcdUSB = 0x0100,
        .bDeviceClass = USB_HUB_CLASS_CODE,
        .bDeviceSubClass = USB_HUB_SUBCLASS_CODE,
        .bDeviceProtocol = USB_HUB_PROTOCOL_FSHUB,
        .bMaxPacketSize0 = 64,
        .idVendor = 0,
        .idProduct = 0,
        .bcdDevice = 0x0100,
        .iManufacturer = 1,
        .iProduct = 2,
        .iSerialNumber = 0,
        .bNumConfigurations = 1,
};

static const struct usb_ohci_config_desc usb_ohci_root_hub_config_desc = {
.config_desc = {
        .bLength = sizeof(struct usb_config_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_CONFIG,
        .wTotalLength = sizeof(usb_ohci_root_hub_config_desc),
        .bNumInterfaces = 1,
        .bConfigurationValue = 1,
        .iConfiguration = 0,
        .bmAttributes = USB_CONFIG_SELF_POWERED,
        .bMaxPower = 0,
}, .iface_desc = {
.bLength = sizeof(struct usb_interface_descriptor), .bDescriptorType =
        USB_DESCRIPTOR_TYPE_INTERFACE, .bNumEndpoints = 1, .bInterfaceClass =
        USB_HUB_IFACE_CLASS_CODE, .bInterfaceSubClass =
        USB_HUB_IFACE_SUBCLASS_CODE, .bInterfaceProtocol = 0,
}, .ep_desc = {
        .bLength = sizeof(struct usb_endpoint_descriptor),
        .bDescriptorType = USB_DESCRIPTOR_TYPE_ENDPOINT,
        .bEndpointAddress = {USB_ENDPOINT_DIRECTION_IN,0,1},
        .bmAttributes = {0,0,0,USB_ENDPOINT_TYPE_INTR},
        .wMaxPacketSize = 32,
        .bInterval = 255,
},
};

static const struct usb_hub_descriptor usb_ohci_root_hub_desc = {
        .bDescLength = 0,
        .bDescriptorType = USB_DESCRIPTOR_TYPE_HUB,
        .bNbrPorts = 0,
        .wHubCharacteristics = {0, 0, 0, 0, 0, 0},
        .bPwrOn2PwrGood = 0,
        .bHubContrCurrent = 0,
        .bDeviceRemovable =     {0}
};

/**
 * \brief   this function clears the old interrupt data and reads the
 *          status from the ports
 *
 * \param   hc  the host controller
 */
void usb_ohci_root_hub_interrupt(usb_ohci_hc_t *hc)
{
    /* clear old interrupt data */
    memset(hc->root_hub_intr_data, 0, sizeof(hc->root_hub_intr_data));

    /* get the root hub status */
    //ohci_rh_status_t hstatus = ohci_rh_status_rd(hc->ohci_base);


    /* get the number of ports */
    uint16_t num_ports = hc->root_hub_num_ports + 1;
    if (num_ports > (8 * sizeof(hc->root_hub_intr_data))) {
        num_ports = (8 * sizeof(hc->root_hub_intr_data));
    }

    //char buf[4048];
        //     ohci_rh_portstat_pr(buf, 4047, hc->ohci_base);
          //  printf(buf);
    ohci_rh_portstat_t ps;
    /* set the bits in the interrupt data field */
    for (uint16_t i = 0; i < num_ports; i++) {

        ps = ohci_rh_portstat_rd(hc->ohci_base, i);
        if (ps >> 16) {
            /* port i has changed */
            USB_DEBUG("port %u, has changed..\n", i);
            hc->root_hub_intr_data[i / 8] |= (1 << (i % 8));
        }
    }

    /* TODO: Handle the hub interrupts */
   // assert(!"NYI: Root hub interrupt handling");
    //uhub_root_intr(&sc->sc_bus, sc->sc_hub_idata, sizeof(sc->sc_hub_idata));
}

/*
 * \brief   this function enables the status change interrupt
 *          of the root hub and updates the enabled_intrs field
 *          of the host controller.
 *
 * \param   hc  the host controller
 */
static void usb_ohci_root_hub_sc_intr_enable(usb_ohci_hc_t *hc)
{
    /* update the enabled interrupt field in our hc */
    ohci_interrupt_t ie = ohci_intenable_rd(hc->ohci_base);
    ohci_interrupt_rhsc_insert(ie, 1);
    hc->enabled_intrs = ie;

    /* enable the interrupt */
    ohci_intenable_rhsc_wrf(hc->ohci_base, 1);

    /* acknowledge any RHSC interrupts */
    ohci_intstatus_rhsc_wrf(hc->ohci_base, 1);

    usb_ohci_root_hub_interrupt(hc);
}

/**
 *  \brief  this function emulates the USB root hub device by software
 *
 *  \param  device      the USB device we want to issue the request for
 *  \param  req         the USB request to execute
 *  \param  ret_data    pointer to the returned data
 *  \param  ret_length  the returned length
 */
usb_error_t usb_ohci_roothub_exec(struct usb_device *device,
        struct usb_device_request *req, const void **ret_data,
        uint16_t *ret_length)
{
    usb_ohci_hc_t *hc = (usb_ohci_hc_t *) device->controller->hc_control;

    const void *data = (const void *) hc->root_hub_desc.temp;
    uint16_t length = 0;

    uint16_t req_value = req->wValue;
    uint16_t req_index = req->wIndex;

    const char *str_ptr;

    /*
     * execute the request for the USB root hub
     */
    switch (req->bRequest) {

        case USB_REQUEST_CLEAR_FEATURE:
            /*
             * ClearFeature() Request
             *
             * The only clear feature request is the clear port feature
             * for an existing port 1..num_ports
             */
            if (req->bType.recipient == USB_REQUEST_RECIPIENT_OTHER) {
                if ((req_index < 1) || (req_index > hc->root_hub_num_ports)) {
                    *ret_length = length;
                    *ret_data = data;
                    return USB_ERR_IOERROR;
                }
                // get the current value of the register
                ohci_rh_portstat_t ps = ohci_rh_portstat_rd(hc->ohci_base,
                        req_index);

                switch (req_value) {
                    case USB_HUB_FEATURE_PORT_ENABLE:
                        ohci_rh_portstat_ccs_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_PORT_SUSPEND:
                        ohci_rh_portstat_poci_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_PORT_POWER:
                        ohci_rh_portstat_lsda_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_C_PORT_CONNECTION:
                        ohci_rh_portstat_csc_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_C_PORT_ENABLE:
                        ohci_rh_portstat_pesc_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_C_PORT_SUSPEND:
                        ohci_rh_portstat_pssc_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
                        ohci_rh_portstat_ocic_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_C_PORT_RESET:
                        ohci_rh_portstat_prsc_insert(ps, 1);
                        break;
                    default:
                        *ret_length = length;
                        *ret_data = data;
                        return USB_ERR_IOERROR;
                }
                // write the value to the register
                ohci_rh_portstat_rawwr(hc->ohci_base, req_index, ps);

                switch (req_value) {
                    case USB_HUB_FEATURE_C_PORT_CONNECTION:
                    case USB_HUB_FEATURE_C_PORT_ENABLE:
                    case USB_HUB_FEATURE_C_PORT_SUSPEND:
                    case USB_HUB_FEATURE_C_PORT_OVER_CURRENT:
                    case USB_HUB_FEATURE_C_PORT_RESET:
                        ps = ohci_rh_portstat_rd(hc->ohci_base, req_index);
                        if ((ps >> 16) == 0) {
                            usb_ohci_root_hub_sc_intr_enable(hc);
                        }
                        break;
                    default:
                        break;
                }
            }
            break;

        case USB_REQUEST_GET_CONFIG:
            /*
             * GetConfiguration() Request
             */
            if ((req->bType.recipient == USB_REQUEST_RECIPIENT_DEVICE)
                    && req->bType.direction == USB_REQUEST_READ) {
                length = 1;
                hc->root_hub_desc.temp[0] = hc->root_hub_config;
            }
            break;

        case USB_REQUEST_GET_DESCRIPTOR:
            /*
             * GetDescriptor() Request
             *
             * There are 3 different kind of standard descriptors we can
             * return: Device, Interface and String
             *
             * and also a class specific request
             */
            if ((req->bType.recipient == USB_REQUEST_RECIPIENT_DEVICE)
                    && req->bType.direction == USB_REQUEST_READ) {

                /* hub class specific request */
                if (req->bType.type == USB_REQUEST_TYPE_CLASS) {
                    if ((req_value & 0xFF) != 0) {
                        *ret_length = length;
                        *ret_data = data;
                        return USB_ERR_IOERROR;
                    }
                    ohci_rh_descra_t cda;
                    cda = ohci_rh_descra_rd(hc->ohci_base);

                    // get the standard hub descriptor to fill in data
                    hc->root_hub_desc.hub_descriptor = usb_ohci_root_hub_desc;

                    struct usb_hub_descriptor *hub = &(hc->root_hub_desc
                            .hub_descriptor);
                    hub->bNbrPorts = hc->root_hub_num_ports;

                    hub->wHubCharacteristics.power_mode =
                            ohci_rh_descra_nps_extract(cda);
                    hub->bPwrOn2PwrGood = ohci_rh_descra_potpgt_extract(cda);

                    /* update device removable stats */
                    ohci_rh_descra_t cdb;
                    cdb = ohci_rh_descrb_rd(hc->ohci_base);
                    for (uint16_t i = 0; i < hc->root_hub_num_ports; i++) {
                        if (cdb & 1) {
                            hub->bDeviceRemovable[i / 8] |= (1 << (i % 8));
                        }
                        i >>= 1;
                    }
                    hub->bDescLength = 8 + ((hc->root_hub_num_ports + 7 / 8));
                    length = hub->bDescLength;
                    break;
                }
                /* standard usb request */
                switch (req_value >> 8) {
                    case USB_DESCRIPTOR_TYPE_DEVICE:
                        if ((req_value & 0xFF) != 0) {
                            *ret_length = length;
                            *ret_data = data;
                            return USB_ERR_IOERROR;
                        }
                        length = sizeof(usb_ohci_root_hub_device_desc);
                        data = (const void *) &usb_ohci_root_hub_device_desc;
                        break;
                    case USB_DESCRIPTOR_TYPE_CONFIG:
                        if ((req_value & 0xFF) != 0) {
                            *ret_length = length;
                            *ret_data = data;
                            return USB_ERR_IOERROR;
                        }
                        length = sizeof(usb_ohci_root_hub_config_desc);
                        data = (const void *) &usb_ohci_root_hub_config_desc;
                        break;
                    case USB_DESCRIPTOR_TYPE_STRING:
                        switch (req_value & 0xFF) {
                            case 0:
                                str_ptr = "\001";
                                break;
                            case 1:
                                break;
                                str_ptr = hc->vendor;
                            case 2:
                                break;
                                str_ptr = "OHCI root HUB";
                            default:
                                str_ptr = "";
                                break;
                        }
                        /* TODO: Make string descriptor */
                        assert(!"NYI: Make string descriptor");
                        break;
                    default:
                        *ret_length = length;
                        *ret_data = data;
                        return USB_ERR_IOERROR;
                        break;
                }
            }
            break;

        case USB_REQUEST_GET_INTERFACE:
            /*
             * GetInterface() Request
             *
             * Root hub has just one interface and no alternative one
             */
            if ((req->bType.recipient == USB_REQUEST_RECIPIENT_DEVICE)
                    && req->bType.direction == USB_REQUEST_READ) {
                length = 1;
                hc->root_hub_desc.temp[0] = 0;
            }
            break;

        case USB_REQUEST_GET_STATUS:
            /*
             * GetStatus() Request
             */
            if (req->bType.direction == USB_REQUEST_WRITE) {
                *ret_data = data;
                *ret_length = length;
                return USB_ERR_IOERROR;
            }

            if (req->bType.type == USB_REQUEST_TYPE_CLASS) {
                if (req->bType.recipient == USB_REQUEST_RECIPIENT_DEVICE) {
                    length = 16;
                    memset(hc->root_hub_desc.temp, 0, 16);
                    break;
                } else if (req->bType.recipient == USB_REQUEST_RECIPIENT_OTHER) {
                    /* get port status */
                    if ((req_index < 1) || req_index > hc->root_hub_num_ports) {
                        // invalid port number;
                        *ret_length = length;
                        *ret_data = data;
                        return USB_ERR_IOERROR;
                    }
                    ohci_rh_portstat_t ps = ohci_rh_portstat_rawrd(
                            hc->ohci_base, req_index);
                    memcpy(&hc->root_hub_desc.port_status.wPortChange, &ps, 2);
                    ps <<= 16;
                    memcpy(&hc->root_hub_desc.port_status.wPortStatus, &ps, 2);
                    //hc->root_hub_desc.port_status.wPortChange = (ps >> 16);
                    //hc->root_hub_desc.port_status.wPortStatus = (ps & 0xFFFF);
                    length = sizeof(hc->root_hub_desc.port_status);
                    break;
                }
            }

            if ((req->bType.recipient == USB_REQUEST_RECIPIENT_DEVICE)) {
                length = 2;
                hc->root_hub_desc.status.wStatus = USB_STATUS_SELF_POWERED;
            } else {
                length = 2;
                hc->root_hub_desc.status.wStatus = 0;
            }

            break;

        case USB_REQUEST_SET_ADDRESS:
            /*
             * SetAddress() Request
             *
             * we can set the address of the root hub if it is
             * withint the maximum devices range
             */
            if (req_value > USB_OHCI_MAX_DEVICES) {
                *ret_length = length;
                *ret_data = data;
                return USB_ERR_IOERROR;
            }
            hc->root_hub_address = req_value;
            break;

        case USB_REQUEST_SET_CONFIG:
            /*
             * SetConfiguration() Request
             *
             * We have only two two options (0, 1) that we can set
             */
            if (req_value > 1) {
                *ret_length = length;
                *ret_data = data;
                return USB_ERR_IOERROR;
            }
            hc->root_hub_config = req_value;
            break;

        case USB_REQUEST_SET_DESCRIPTOR:
            /*
             * SetDescriptor() Request
             *
             * We do not allow to change the USB root hub descriptor
             * so this is a no-op for standard requests
             *
             * but is is an error for hub class requests
             */
            if (req->bType.type == USB_REQUEST_TYPE_CLASS) {
                *ret_length = length;
                *ret_data = data;
                return USB_ERR_IOERROR;
            }
            break;

        case USB_REQUEST_SET_FEATURE:
            /*
             * SetFeature() Request
             *
             * Setting a feature on the root hub is an error,
             * so we reply with USB_ERR_IOERROR
             */
            if (req->bType.type != USB_REQUEST_TYPE_CLASS) {
                *ret_length = length;
                *ret_data = data;
                return USB_ERR_IOERROR;
                break;
            }
            /* handling of hub cpass specific port request */

            if (req->bType.recipient == USB_REQUEST_RECIPIENT_OTHER) {
                if ((req_index < 1) || (req_index > hc->root_hub_num_ports)) {
                    *ret_length = length;
                    *ret_data = data;
                    return USB_ERR_IOERROR;
                    break;
                }
                ohci_rh_portstat_t ps = ohci_rh_portstat_rawrd(hc->ohci_base,
                        req_index);
                switch (req_value) {
                    case USB_HUB_FEATURE_PORT_ENABLE:
                        ohci_rh_portstat_pes_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_PORT_SUSPEND:
                        ohci_rh_portstat_pss_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_PORT_RESET:
                        ohci_rh_portstat_prs_insert(ps, 1);
                        break;
                    case USB_HUB_FEATURE_PORT_POWER:
                        ohci_rh_portstat_pps_insert(ps, 1);
                        break;
                    default:
                        *ret_length = length;
                        *ret_data = data;
                        return USB_ERR_IOERROR;
                        break;
                }
                ohci_rh_portstat_rawwr(hc->ohci_base, req_index, ps);
                /*
                 * handle the reset of the port
                 */
                if (req_value == USB_HUB_FEATURE_PORT_RESET) {
                    for (uint32_t i = 0;; i++) {
                        if (ohci_rh_portstat_prs_rdf(hc->ohci_base,
                                req_index)) {
                            /*
                             * bit has not been cleared, that means the
                             * reset is not completed yet
                             */
                            /* TODO: WAIT SOME TIME */
                            if (i > 12) {
                                *ret_data = data;
                                *ret_length = length;
                                return USB_ERR_TIMEOUT;
                            }
                        } else {
                            // reset complete
                            break;
                        }
                    }
                }
            }
            break;

        case USB_REQUEST_SET_INTERFACE:
            /*
             * SetInterface() Request
             *
             * we have just one interface, so this is a no-op
             */
            break;

        case USB_REQUEST_SYNCH_FRAME:
            /*
             * SetSynchFrame() Request
             *
             * No-op
             */
            break;
        default:
            *ret_length = length;
            *ret_data = data;
            return USB_ERR_IOERROR;
            break;
    }
    *ret_length = length;
    *ret_data = data;
    return USB_ERR_OK;
}

