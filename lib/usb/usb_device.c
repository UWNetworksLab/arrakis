/**
 * \brief   this file contains functions to manipulate the usb device
 *
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
#include <barrelfish/barrelfish.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/usb_device.h>
#include <usb/usb_xfer.h>
#include <usb/usb_parse.h>

#include "usb_manager_client.h"

usb_device_t device;

static uint8_t is_init = 0;

/**
 * \brief this function frees an allocated configuration tree i.e. the allocated
 *        interfaces and endpoints
 *
 * \param dev the device for which the configuration should be freed
 */
static void usb_device_cfg_free(struct usb_device *dev)
{
    if (dev->ifaces != NULL) {
        free(dev->ifaces);
    }

    if (dev->endpoints != NULL) {
        free(dev->endpoints);
    }
    dev->ifaces = NULL;
    dev->iface_max = 0;
    dev->endpoints = NULL;
    dev->ep_max = 0;
}


/**
 * \brief this function initializes an endpoint structure
 *
 * \param iface_index the index of the interface
 * \param desc the endpoint descriptor used as data source
 * \param ep the endpoint to initalize
 */
static void usb_device_init_endpoint(uint8_t iface_index,
        struct usb_endpoint_descriptor *desc,
        struct usb_endpoint *ep)
{
    USB_DEBUG_TR_ENTER;
    ep->ep_direction = desc->bEndpointAddress.direction;
    ep->ep_number = desc->bEndpointAddress.ep_number;
    ep->ep_type = desc->bmAttributes.xfer_type;
    ep->ep_usage = desc->bmAttributes.usage_type;
    ep->ep_sync =  desc->bmAttributes.sync_type;
    ep->iface_index = iface_index;
}

/**
 * \brief processes a configuration descriptor and allocates the interface
 *        and endpoint structures
 *
 * \param dev the device to process the configuration
 * \param iface the interface number
 * \param init flag indicating if allocation or initialization
 * \param config the configuration descriptor
 */
static usb_error_t usb_device_cfg_process(struct usb_device *dev, uint8_t iface,
        uint8_t init, struct usb_config_descriptor *config)
{
    USB_DEBUG_TR_ENTER;

    struct usb_iface_parse_state iface_ps;
    struct usb_endpoint *ep;
    uint8_t ep_max, ep_current;
    uint8_t alt_index = 0;
    usb_error_t err = USB_ERR_OK;

    /* this is an parameter override, when setting an alternate interface */
    if (iface != USB_INTERFACE_INDEX_ANY) {
        alt_index = init;
        init = 2;
    }


    if (init) {
        /* check if an endpoint reset is needed */
        ep = dev->endpoints;
        ep_max = dev->ep_max;

        while (ep_max--) {
            if ((iface == USB_INTERFACE_INDEX_ANY)
                    || (iface == ep->iface_index)) {
                    /* perform EP reset */
                    memset(ep, 0, sizeof(*ep));
                    ep->iface_index = USB_INTERFACE_INDEX_ANY;

            }
            ep++;
        }
        if (err != USB_ERR_OK) {
            return (err);
        }
    }

    /* reset the parse state */
    memset(&iface_ps, 0, sizeof(iface_ps));
    ep_current = 0;
    ep_max = 0;
    uint8_t ep_tmp = 0;

    struct usb_interface_descriptor *idesc;
    struct usb_interface *interface;
    struct usb_endpoint_descriptor *edesc;
    uint8_t iface_index = 0;

    /* loop over all interfaces of this configuration descirptor */
    while ((idesc = usb_parse_next_iface(config, &iface_ps))) {

        if (iface_ps.iface_index == 32) {
            /* the maximium ifaces */
            break;
        }

        /* get the next interface */
        interface = dev->ifaces + iface_ps.iface_index;

        uint8_t do_init = 0;

        if (init) {
            /* check if we have to initialize the iface or endpoints */
            if ((iface_index != USB_INTERFACE_INDEX_ANY)
                    && (iface_index != iface_ps.iface_index)) {
                do_init = 0;
            } else if (alt_index != iface_ps.iface_index_alt) {
                do_init = 0;
            } else {
                do_init = 1;
            }
        }

        if (iface_ps.iface_index_alt == 0) {
            ep_current = ep_max;
        }

        if (do_init) {
            /* initialize the interface */
            assert(interface != NULL);
            interface->iface_number  = idesc->bInterfaceNumber;
            interface->iface_class  = idesc->bInterfaceClass;
            interface->iface_subclass  = idesc->bInterfaceSubClass;
            interface->iface_protocol  = idesc->bInterfaceProtocol;
            interface->num_endpoints  = idesc->bNumEndpoints;
            interface->config  = config->bConfigurationValue;
            interface->parent_iface_index = USB_INTERFACE_INDEX_ANY;
            interface->alt_setting = alt_index;
        }

        /*
         * get the address of the endpoint descriptor
         * the parse_next function will look one descriptor after this
         * so we have to initialize it with the interface descriptor
         * */
        edesc = (struct usb_endpoint_descriptor *) idesc;

        ep_tmp = ep_current;

        /* loop over all endpoints of this interface descriptor */
        while ((edesc = usb_parse_next_edesc(config, edesc))) {

            if (ep_tmp == 32) {
                /* maximum endpoints */
                break;
            }

            /* get the endpoint */
            ep = dev->endpoints + ep_tmp;

            if (do_init) {
                /* initialize the endpoint */
                usb_device_init_endpoint(iface_ps.iface_index, edesc, ep);
                ep->iface = interface;
            }
            ep_tmp++;

            if (ep_max < ep_tmp) {
                ep_max = ep_tmp;
            }
            /* the last endpoint descriptor will be stored at the iface again */
            idesc = (struct usb_interface_descriptor *) edesc;
        }

    }

    if (!init) {
        /*
         * we know how many interfaces / endpoints we have to allocate so
         * get some memory for this
         */
        dev->iface_max = iface_ps.iface_index;
        dev->ifaces = NULL;

        if (dev->iface_max != 0) {
            dev->ifaces = malloc(sizeof(*interface) * dev->iface_max);
            if (dev->ifaces == NULL) {
                usb_device_cfg_free(dev);
                return (USB_ERR_NOMEM);
            }
        }

        if (ep_max != 0) {
            dev->endpoints = malloc(sizeof(*ep) * ep_max);
            if (dev->endpoints == NULL) {
                usb_device_cfg_free(dev);
                return (USB_ERR_NOMEM);
            }
            dev->ep_max = ep_max;
        }
    }

    return (USB_ERR_OK);
}




void usb_device_init(void *desc)
{
    memset(&device, 0, sizeof(device));

    struct usb_device_descriptor *ddesc = (struct usb_device_descriptor*) desc;
    struct usb_config_descriptor *config = (struct usb_config_descriptor *) (ddesc + 1);

    device.config_desc = config;
    device.dev_class = ddesc->bDeviceClass;
    device.dev_subclass  = ddesc->bDeviceSubClass;
    device.dev_protocol  = ddesc->bDeviceProtocol;
    device.vendor  = ddesc->idVendor;
    device.product  = ddesc->idProduct;
    device.version  = ddesc->bcdDevice;
    device.num_config = ddesc->bNumConfigurations;
    device.current_config = config->bConfigurationValue;

    usb_device_cfg_process(&device, USB_INTERFACE_INDEX_ANY, 0, config);
    usb_device_cfg_process(&device, USB_INTERFACE_INDEX_ANY, 1, config);

    is_init = 1;
}


/**
 * \brief this function returns the interface count of the current configuration
 *
 * \param ret_count the number of interfaces
 */
usb_error_t usb_device_get_iface_count(uint8_t *ret_count)
{
    if (!is_init) {
        return (USB_ERR_NOT_CONFIGURED);
    }
    *ret_count = device.config_desc->bNumInterfaces;
    return (USB_ERR_OK);

}

/**
 * \brief this function returns the speed of the device
 *
 * \param ret_speed the speed identifier
 */
usb_error_t usb_device_get_speed(usb_speed_t *ret_speed)
{
    if (!is_init) {
        return (USB_ERR_NOT_CONFIGURED);
    }
    assert(!"NYI: Getting the device speed.");
    return (USB_ERR_OK);
}


/**
 * \brief suspend the USB device on the bus
 */
usb_error_t usb_device_suspend(void)
{
    assert(!"NYI: Suspending a device");
    return (USB_ERR_OK);
}

/**
 * \brief resumes the USB device on the bus
 */
usb_error_t usb_device_resume(void)
{
    assert(!"NYI: Resuming a device");
    return (USB_ERR_OK);
}

/**
 * \brief returns the number of available configurations
 */
uint8_t usb_device_get_num_config(void) {
    assert(is_init);
    return (device.num_config);
}

/**
 * \brief returns the configuration descriptor of the device
 */
struct usb_config_descriptor *usb_device_get_cfg_desc(void)
{
    return (device.config_desc);
}

/**
 * \brief returns the interface of a given interface number
 *
 * \param iface the interface number to look for
 *
 * \return interface if one is found, NULL otherwise
 */
struct usb_interface *usb_device_get_iface(uint8_t iface)
{
    struct usb_interface *interface, *ret = NULL;

    /* loop over all interfaces and return the one with the matching number */
    for (uint8_t i = 0; i < device.iface_max; i++) {
        interface = (device.ifaces + i);
        if (interface->iface_number == iface) {
            ret = interface;
            break;
        }
    }
    return (ret);
}

