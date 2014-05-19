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
#include <usb/usb_parse.h>

#include <if/usb_driver_defs.h>

#include <usb_controller.h>
#include <usb_device.h>
#include <usb_request.h>
#include <usb_pipe.h>
#include <usb_interface.h>
#include <usb_hub.h>
#include <usb_transfer.h>
#include <usb_endpoint.h>

/**
 * \brief frees up the allocated interface and enpoint structures for the
 *        current configuration
 *
 * \param dev the usb device to free the structures
 *
 * NOTE: changing the configuration usually involves freeing up the old dat
 */
static void usb_device_free_config(struct usb_device *device)
{
    USB_DEBUG_TR_ENTER;

    /* free the allocated array structures, if set */
    if (device->ifaces != NULL) {
        free(device->ifaces);
    }

    if (device->endpoints != NULL) {
        free(device->endpoints);
    }

    /* update the pointers */
    device->ep_clear_stall = NULL;
    device->ifaces = NULL;
    device->iface_max = 0;
    device->endpoints = NULL;
    device->ep_max = 0;
}

/**
 * \brief this functions parses the configuration descriptor and allocates
 *        the interface and endpoint structures for this configuration
 *
 * \param device the device to process the configuraiton
 * \param iface the interface to initialze mostly USB_INTERFACE_ANY
 * \þaram init  0=do allocation, 1=do initialization
 *
 * NOTE: usually you have to call first with init=0 to allocate the structures
 *       then call with init=1 to initialize the allocated structures
 */
static usb_error_t usb_device_parse_config(struct usb_device *device,
        uint8_t iface, uint8_t init)
{
    USB_DEBUG_TR_ENTER;

    struct usb_iface_parse_state iface_ps;

    /* variabels for tracking the endpoints */
    struct usb_endpoint *ep;
    uint8_t ep_max, ep_current;

    /* the alternative interface index */
    uint8_t alt_index = 0;

    usb_error_t err = USB_ERR_OK;

    if (iface != USB_INTERFACE_INDEX_ANY) {
        /*
         * this is some kind of parameter overloading, if we set the alternate
         * index to a specific value
         */
        alt_index = init;
        init = 2;
    }

    if (init) {
        /*
         * performa a reset on the allocated endpoints and check if they are
         * still used or not.
         */
        ep = device->endpoints;
        ep_max = device->ep_max;

        while (ep_max--) {
            if ((iface == USB_INTERFACE_INDEX_ANY)
                    || (iface == ep->iface_index)) {
                /* just check if the iface_index matches */
                if (ep->ref_allocation != 0) {
                    err = USB_ERR_IN_USE;
                } else {
                    /* perform the actual resetting */
                    memset(ep, 0, sizeof(*ep));
                    ep->iface_index = USB_INTERFACE_INDEX_ANY;
                }
            }
            ep++;
        }
        if (err != USB_ERR_OK) {
            return (err);
        }
    }

    /* reset the interface parsing state */
    memset(&iface_ps, 0, sizeof(iface_ps));

    ep_current = 0;
    ep_max = 0;
    uint8_t ep_tmp = 0;

    struct usb_interface_descriptor *idesc;
    struct usb_interface *interface;
    struct usb_endpoint_descriptor *edesc;
    uint8_t iface_index = 0;

    /*
     * The config descriptor has the following structure
     *    + interface descriptor 1
     *      + endpoint descriptor 1
     *      + endpoint descriptor 2
     *    + interface descriptor 2
     *      + endpoint descriptor 1
     *      + endpoint descriptor 2
     *
     * we first have to get the next interface and then all the belonging
     * endpoints
     */
    while ((idesc = usb_parse_next_iface(device->config_desc, &iface_ps))) {

        if (iface_ps.iface_index == 32) {
            /*
             * there is a maximum of 32 interfaces, since the interface number
             * is just 4 bits.
             */
            break;
        }

        /* get next interface location */
        interface = device->ifaces + iface_ps.iface_index;

        uint8_t do_init = 0;

        if (init) {
            /*
             * if a specific interface other than USB_INTERFACE_INDEX_ANY is
             * given, then we just want to initialize this given interface
             */
            if ((iface_index != USB_INTERFACE_INDEX_ANY)
                    && (iface_index != iface_ps.iface_index)) {
                do_init = 0;
            } else if (alt_index != iface_ps.iface_index_alt) {
                do_init = 0;
            } else {
                do_init = 1;
            }
        }

        /* update the current endpoint count in case of an alternative iface*/
        if (iface_ps.iface_index_alt == 0) {
            ep_current = ep_max;
        }

        /* perform the initialization of the interface structure */
        if (do_init) {
            assert(interface != NULL);

            interface->descriptor = idesc;
            interface->parent_iface_index = USB_INTERFACE_INDEX_ANY;
            interface->alt_setting = alt_index;
        }

        /*
         * the endpoint descriptors are located in the configuration descriptor
         * right after the interface descriptor. so we can use the address
         * of the current interface descriptor as the starting point for
         * looking for the first endpoint descriptor of this interface
         */
        edesc = (struct usb_endpoint_descriptor *) idesc;

        ep_tmp = ep_current;

        while ((edesc = usb_parse_next_edesc(device->config_desc, edesc))) {

            if (ep_tmp == 32) {
                /*
                 * there are maximum 32 endpoints within an interface since the
                 * endpoint number is just 4 bits long
                 */
                break;
            }

            /* get the location of the next endpoint */
            ep = device->endpoints + ep_tmp;

            if (do_init) {
                /* initialize it if the memory is allocated */
                usb_endpoint_init(device, iface_ps.iface_index, edesc, ep);
            }

            ep_tmp++;

            if (ep_max < ep_tmp) {
                /* update the current maximum found endpoints */
                ep_max = ep_tmp;
            }

            /*
             * update the interface descriptor pointer.
             * NOTE: usb_parse_next_edesc returns null if there is no more
             *       endpoint descriptor, so we have to do the update
             *       within the loop to avoid null pointers
             */
            idesc = (struct usb_interface_descriptor *) edesc;
        } /* parse next ep descriptor */
    } /* parse next iface descriptor */

    if (!init) {
        /*
         * allocate memory for the interface and endpoint structures according
         * to the values of the number of found endpoints / interfaces
         * in the code above.
         */
        device->iface_max = iface_ps.iface_index;
        device->ifaces = NULL;
        if (device->iface_max != 0) {
            device->ifaces = malloc(sizeof(*interface) * device->iface_max);
            if (device->ifaces == NULL) {
                usb_device_free_config(device);
                return (USB_ERR_NOMEM);
            }
        }

        if (ep_max != 0) {
            device->endpoints = malloc(sizeof(*ep) * ep_max);
            if (device->endpoints == NULL) {
                usb_device_free_config(device);
                return (USB_ERR_NOMEM);
            }
            device->ep_max = ep_max;
            /* XXX: clearing stall currently not supported */
            device->ep_clear_stall = NULL;
        }
    }

    return (USB_ERR_OK);
}

/**
 * \brief initializes the device descriptor by executing the request on the
 *        usb device
 *
 * \param device the device to initialize the device descriptor
 */
static usb_error_t usb_device_initialize_descriptor(struct usb_device *device)
{
    USB_DEBUG_TR_ENTER;

    usb_error_t err;

    switch (device->speed) {
        case USB_SPEED_LOW:
        case USB_SPEED_FULL:
            /*
             * full and low speed device have a maximum packet size of 8 bytes
             * so we have to treat them a little bit special here
             */
            err = usb_req_get_descriptor(device, NULL, &device->device_desc, 8,
                    8, 0, USB_DESCRIPTOR_TYPE_DEVICE, 0, 0);

            if (err != USB_ERR_OK) {
                USB_DEBUG("ERROR: Failed to get device descriptor\n");
                USB_DEBUG_TR_RETURN;
                return (err);
            }
            break;
        default:
            /*
             * for high and super speed devices we have a maximum packet size
             * of at least 64 bytes
             */
            break;
    }

    err = usb_req_get_device_descriptor(device, &device->device_desc);

    if (err != USB_ERR_OK) {
        /* retry once more */
        USB_DEBUG("NOTICE: getting descriptor failed. retry.\n");
        err = usb_req_get_device_descriptor(device, &device->device_desc);
    }

    return (err);
}

/**
 * \brief changes the current configuration of the device
 *
 * \param device the device to update the configuration
 * \param config the configuration value to set
 *
 * NOTE: This updates also the configuration descriptor and parses it
 */
usb_error_t usb_device_set_configuration(struct usb_device *device,
        uint8_t config)
{
    USB_DEBUG_TR_ENTER;

    if (config > device->device_desc.bNumConfigurations) {
        USB_DEBUG("WARNING: setting configuration bigger than num config\n");
        return (USB_ERR_INVAL);
    }

    /* free the old parsed configuration first i.e. the endpoints and ifaces */
    usb_device_free_config(device);

    /* free old config descriptor */
    if (device->config_desc) {
        free(device->config_desc);
        device->config_desc = NULL;
    }

    /*
     * if the configuration to be set is USB_CONFIGURATION_UNCONFIGURED
     * then we don't process further and change the device state to
     * ADDRESSED.
     */
    if (config == USB_CONFIGURATION_UNCONFIGURED) {
        device->config_index = USB_CONFIGURATION_UNCONFIGURED;
        device->config_number = 0;
        if (device->state == USB_DEVICE_STATE_CONFIGURED) {
            device->state = USB_DEVICE_STATE_ADDRESSED;
        }
        return (USB_ERR_OK);
    }

    usb_error_t err;
    struct usb_config_descriptor *cdesc;

    /* get the new configuration descriptor belonging to configuration config */
    err = usb_req_get_config_descriptor(device, &cdesc, config);

    if (err) {
        USB_DEBUG("ERROR: getting configuration failed.\n");
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    assert(cdesc != NULL);

    /* update the configuration descriptor of the device */
    device->config_desc = cdesc;

    /*
     * TODO: Check if the devices has its own power source or if it takes
     * the power from the bus. This is important for attached hubs since
     * it its not allowed to attach multiple bus powered hubs in series
     */
    device->flags.self_powered = 1;

    /* update the device state and set the configuration values */
    device->config_index = config;
    device->config_number = cdesc->bConfigurationValue;
    device->state = USB_DEVICE_STATE_CONFIGURED;

    /* set the actual configuration value */
    err = usb_req_set_config(device, cdesc->bConfigurationValue);

    if (err != USB_ERR_OK) {
        USB_DEBUG("ERROR: usb_req_set_config failed.\n");
        USB_DEBUG_TR_RETURN;
        return (err);
    }

    /* parse the configuration to allocate new interfaces and endpoints for
     * this device and initialize them
     */
    err = usb_device_parse_config(device, USB_INTERFACE_INDEX_ANY, 0);
    if (err) {
        usb_device_free_config(device);
        USB_DEBUG("WARNING: allocating ifaces and endpoints failed\n");
        return (err);
    }

    err = usb_device_parse_config(device, USB_INTERFACE_INDEX_ANY, 1);
    if (err) {
        usb_device_free_config(device);
        USB_DEBUG("WARNING: initialization of ifaces and endpoints failed\n");
        return (err);
    }

    USB_DEBUG_TR_RETURN;

    return (USB_ERR_OK);
}

static void usb_device_setup_strings(struct usb_device *device)
{
    char buf[255];

    usb_req_get_string(device, buf, 255, device->device_desc.iSerialNumber);
    device->serial_number = strdup(buf);

    usb_req_get_string(device, buf, 255, device->device_desc.iManufacturer);
    if (buf[0] == 0) {
        snprintf(buf, 255, "vendor 0x%04x", device->device_desc.idVendor);
    }
    device->manifacturer = strdup(buf);

    usb_req_get_string(device, buf, 255, device->device_desc.iProduct);
    if (buf[0] == 0) {
        snprintf(buf, 255, "product 0x%04x", device->device_desc.idProduct);
        device->product = strdup(buf);
    }
    device->product = strdup(buf);

}

/**
 * \brief allocates a newly discovered device and initializes it
 *
 * \param hc the host controller of the newly discovered device
 * \param parent_hub the parent hub of the device
 * \parem depth the depth of the device in the usb topology
 * \param portindex the port index of the parent hub
 * \param portno the port number the device is connected to
 * \param speed speed information
 * \þaram mode the mode of the device. Currently just USB_MODE_HOST
 *
 * \return pointer to a usb device on success
 *         NULL on failure
 */
struct usb_device *usb_device_alloc(struct usb_host_controller *hc,
        struct usb_device *parent_hub, uint8_t depth, uint8_t portindex,
        uint8_t portno, usb_speed_t speed, usb_mode_t mode)
{
    USB_DEBUG_TR_ENTER;
    usb_error_t err;

    /*
     * find and empty device index / address we can use, starting at the
     * root hub device address which is 1 per default.
     */
    uint8_t device_index = USB_ROOTHUB_ADDRESS;
    while (device_index < hc->devices_max) {
        if (hc->devices[device_index] == NULL) {
            break;
        }
        device_index++;
    }

    if (device_index == hc->devices_max) {
        /* no free space on the bus */
        USB_DEBUG("WARNING: No free device index!\n");
        return (NULL);
    }

    /* the depth of the usb devices must not exceed 7 */
    if (depth > 7) {
        USB_DEBUG("ERROR: Invalid device depth.\n");
        return (NULL);
    }

    struct usb_device *device = malloc(sizeof(struct usb_device));
    if (device == NULL) {
        USB_DEBUG("ERROR: no free mem.\n");
        return (NULL);
    }

    /* make sure that everything has an expected value */
    memset(device, 0, sizeof(struct usb_device));

    /* counter for xfer IDs belonging to this device */
    device->xfer_id = 1;

    /* initialize the device with the supplied values */
    device->parent_hub = parent_hub;
    device->hub_port_index = portindex;
    device->hub_port_number = portno;
    device->depth = depth;
    device->controller = hc;

    /* all new devices have 0 als default start address */
    device->device_address = 0;

    /* setup default endpoint descriptor */
    device->ctrl_ep_desc.bLength = sizeof(device->ctrl_ep_desc);
    device->ctrl_ep_desc.bDescriptorType = USB_DESCRIPTOR_TYPE_ENDPOINT;

    /*
     * by setting the maximum packet size of the control endpoint to 8 bytes
     * we ensure that the initial transactions can be executed by any device
     */
    device->ctrl_ep_desc.wMaxPacketSize = 8;
    device->device_desc.bMaxPacketSize0 = 8;

    device->speed = speed;
    device->flags.usb_mode = mode;

    /*
     * find the parent high speed hub. This is needed for the transaction
     * translator of the high speed hub. This may not always succeed, since
     * the root hub has no parent hub and OHCI/UHCI only supportes FULL/LOW
     * speed devices.
     */
    struct usb_device *hub = device->parent_hub;
    struct usb_device *adev = device;

    while (hub) {
        if (hub->speed == USB_SPEED_HIGH) {
            device->hs_hub_address = hub->device_address;
            device->parent_hs_hub = hub;
            device->hs_hub_port_number = adev->hub_port_number;
            break;
        }
        adev = hub;
        hub = hub->parent_hub;
    }

    /* initialize the control endpoint */
    usb_endpoint_init(device, 0, &device->ctrl_ep_desc, &device->ctrl_ep);

    device->device_index = device_index;

    /* the devices is now attached and thus in the powered state */
    device->state = USB_DEVICE_STATE_POWERED;

    /* set the device address */
    err = usb_req_set_address(device, device_index);

    if (err) {
        debug_printf("set address failed (ignored)\n");
        return (NULL);
    }

    /* wait till the address has settled */
    USB_WAIT(USB_DELAY_SET_ADDRESS*5);

    /*
     * this check has to be done, since there may be a controller specific
     * set address function that does this already for us, however this may
     * not be the case, thus we have to check and do it ourself.
     */
    if (device->device_address == 0) {
        USB_DEBUG_DEV("setting device address to %i\n", device_index);
        device->device_address = device_index;
    }

    /* update the devices state to addressed */
    device->state = USB_DEVICE_STATE_ADDRESSED;

    /* initialize the device descriptor */
    err = usb_device_initialize_descriptor(device);

    assert(device->device_desc.bDescriptorType == USB_DESCRIPTOR_TYPE_DEVICE);

    char buf[255];
    memset(buf, 0, sizeof(buf));

    /* try to read the string descriptors if any */
    if (device->device_desc.iManufacturer || device->device_desc.iProduct
            || device->device_desc.iSerialNumber) {
        err = usb_req_get_string_desc(device, buf, 4, 0, 0);
    } else {
        /* there are no string descriptors.. setting to invalid */
        err = USB_ERR_INVAL;
    }

    /* if there are any string descriptors try to get the language id */
    if (err != USB_ERR_OK || buf[0] < 4) {
        device->flags.no_strings = 1;
    } else {
        uint16_t langid = 0;

        buf[0] &= ~1;
        uint32_t i;
        for (i = 2; i < buf[0]; i += 2) {
            langid = (uint16_t) *(buf + i);
            if ((langid & 0x00FF) == 0x0009) {
                break;
            }
        }
        if (i >= buf[0]) {
            langid = (uint16_t) *(buf + 2);
        }
        device->language_id = langid;
    }

    /* setup the string descriptors by reading out the string descriptors */
    usb_device_setup_strings(device);

    /* TODO: check for the real power needs */
    device->power_needed = USB_POWER_MIN;

    /* finally set the configuration */
    err = usb_device_set_configuration(device, 0);

    if (err != USB_ERR_OK) {
        if (device->device_desc.bNumConfigurations != 0) {
            debug_printf("WARNING: Set configuration failed.\n");

            USB_DEBUG("WARNING: getting descriptor failed. "
            "Try to re-enumerate\n");
            /* TODO: err = usb_req_re_enumerate(dev, NULL); */
            assert(!"NYI: re-enumeration\n");
            err = usb_device_set_configuration(device, 0);
        }
    }

    if (err != USB_ERR_OK) {
        usb_device_free(device, 0);
        return (NULL);
    }

    /* set the device index on the port of the parent hub */
    if (parent_hub) {
        (parent_hub->hub->ports + portindex)->device_index = device_index;
    }

    if (device_index != 0) {
        hc->devices[device_index] = device;
    }

    USB_DEBUG_TR_RETURN;

    return (device);
}

/// state variable for the device detached
static volatile uint8_t usb_device_detached = 0;

/**
 * \brief call back function when the detach notification is sent sucessfully
 *
 * \brief a pointer to the detached devices
 */
static void usb_device_detach_cb(void *a)
{
    struct usb_device *st = a;
    free(st->usb_driver_binding);
    free(st->usb_manager_binding);
    st->usb_driver_binding = NULL;
    st->usb_manager_binding = NULL;
    usb_device_detached = 1;
}

/**
 * \brief sends the detach notification
 *
 * \param a pointer to the detached device
 */
static void usb_device_detach_notify(void *a)
{

    struct usb_device *st = a;
    errval_t err;

    struct event_closure txcont = MKCONT(usb_device_detach_cb, st);

    err = usb_driver_device_detach_notify__tx(st->usb_driver_binding, txcont);

    if (err_is_fail(err)) {
        if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {
            txcont = MKCONT(usb_device_detach_notify, st);
            struct waitset *ws = get_default_waitset();
            err = st->usb_driver_binding->register_send(st->usb_driver_binding,
                    ws, txcont);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "error register_send on binding failed!\n");
            }
        } else {
            DEBUG_ERR(err, "error _retry(): sending response!\n");\
            free(st->usb_driver_binding);
            free(st->usb_manager_binding);
            st->usb_driver_binding = NULL;
            st->usb_manager_binding = NULL;
        }
    }
}

/**
 * \brief this function frees the allocated memory for a device
 */
void usb_device_free(struct usb_device * device, uint8_t flag)
{
    usb_device_detached = 1;
    if (device->hub != NULL) {
        debug_printf("Device was a hub device: TODO: recurse on other..");

    } else {
        if (device->usb_driver_binding != NULL) {
            usb_device_detached = 0;
            usb_device_detach_notify(device);
        }
    }

    uint8_t timeout = 0;
    while (!usb_device_detached) {
        USB_WAIT(10);
        if (timeout > 5) {
            break;
        }
        timeout++;
    }

    struct usb_xfer *xfer = device->xfers;
    struct usb_xfer *xfer_next;

    while (xfer != NULL) {
        xfer_next = xfer->device_xfers_next;
        usb_transfer_unsetup(&xfer, 1);
        xfer = xfer_next;
    }

    device->controller->devices[device->device_index] = NULL;

    if (device->config_desc)
        free(device->config_desc);
    if (device->ifaces)
        free(device->ifaces);
    if (device->endpoints)
        free(device->endpoints);
    if (device->serial_number)
        free(device->serial_number);
    if (device->manifacturer)
        free(device->manifacturer);
    if (device->product)
        free(device->product);
    if (device->ep_clear_stall)
        free(device->ep_clear_stall);
    free(device);
}
