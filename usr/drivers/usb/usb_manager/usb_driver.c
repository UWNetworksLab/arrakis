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
#include <barrelfish/spawn_client.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/class/usb_hid.h>

#include <usb_device.h>
#include <usb_driver.h>
#include <usb_hub.h>
#include <usb_interface.h>

/// list containing usb devices that are currently pending for driver spawn */
static struct usb_device *devices_pending = NULL;

/// the currently spawned device waiting for driver connection
static struct usb_device *device_process = NULL;

/**
 * \brief executes the query to find the driver binary path
 *
 * \param class    the USB class of the device
 * \param subclass the USB subclass of the device
 * \param protocol the protocol ID of the device
 * \param vendor   the vendor ID of the device
 * \param product  the product ID of the device
 *
 * \return the path of the binary on success
 *         NULL if there is no matchting binary
 *
 * TODO: the query may be deferred to the SKB to get the path of the binary
 *
 * XXX: This function is a stub, serving only the usb_keyboard at the moment
 */
static char *usb_driver_query(uint8_t class, uint8_t subclass, uint8_t protocol,
        uint16_t vendor, uint16_t product)
{
    char *path = NULL;
    switch (class) {
        case USB_DEVICE_CLASS_COMPOSITE:
            USB_DEBUG_DRIVER("Device is a composite device\n");
            break;
        case USB_DEVICE_CLASS_AUDIO:
            USB_DEBUG_DRIVER("Device is a audio device\n");
            break;
        case USB_DEVICE_CLASS_COMM:
            USB_DEBUG_DRIVER("Device is a communication device\n");
            break;
        case USB_DEVICE_CLASS_HID:
            if (subclass == USB_HID_SUBCLASS_CODE_BOOT) {
                /* boot interface class */
            }

            switch (protocol) {
                case USB_HID_PROTOCOL_NONE:
                    USB_DEBUG_DRIVER("HID device with no ptorocol\n");
                    break;
                case USB_HID_PROTOCOL_KEYBOARD:
                    USB_DEBUG_DRIVER("HID device: Keyboard\n");
                    path = "/armv7/sbin/usb_keyboard";
                    break;
                case USB_HID_PROTOCOL_MOUSE:
                    USB_DEBUG_DRIVER("HID device: Mouse\n");
                    break;
                default:
                    USB_DEBUG_DRIVER("HID device with reserved protocol\n");
                    break;
            }
            break;
        case USB_DEVICE_CLASS_PHYSICAL:
            USB_DEBUG_DRIVER("Device is a physical device\n");
            break;
        case USB_DEVICE_CLASS_IMAGE:
            USB_DEBUG_DRIVER("Device is a imaging device\n");
            break;
        case USB_DEVICE_CLASS_PRINTER:
            USB_DEBUG_DRIVER("Device is a printing device\n");
            break;
        case USB_DEVICE_CLASS_MSD:
            USB_DEBUG_DRIVER("Device is a mass storage device\n");
            break;
        case USB_DEVICE_CLASS_HUB:
            USB_DEBUG_DRIVER("Device is a hub device\n");
            break;
        case USB_DEVICE_CLASS_CDC:
            USB_DEBUG_DRIVER("Device is a cdc device\n");
            break;
        case USB_DEVICE_CLASS_SMARTCARD:
            USB_DEBUG_DRIVER("Device is a smartcard device\n");
            break;
        case USB_DEVICE_CLASS_SECURITY:
            USB_DEBUG_DRIVER("Device is a security device\n");
            break;
        case USB_DEVICE_CLASS_VIDEO:
            USB_DEBUG_DRIVER("Device is a video device\n");
            break;
        case USB_DEVICE_CLASS_HEALTH:
            USB_DEBUG_DRIVER("Device is a health device\n");
            break;
        case USB_DEVICE_CLASS_AV:
            USB_DEBUG_DRIVER("Device is a audio/video device\n");
            break;
        case USB_DEVICE_CLASS_DIAG:
            USB_DEBUG_DRIVER("Device is a diag device\n");
            break;
        case USB_DEVICE_CLASS_WIFI:
            USB_DEBUG_DRIVER("Device is a wifi device\n");
            break;
        case USB_DEVICE_CLASS_MISC:
            USB_DEBUG_DRIVER("Device is a misc device\n");
            break;
        case USB_DEVICE_CLASS_APPL:
            USB_DEBUG_DRIVER("Device is a application specific device\n");
            break;
        case USB_DEVICE_CLASS_VENDOR:
            USB_DEBUG_DRIVER("Device is a vendor specific device\n");
            break;
        default:
            USB_DEBUG("WARNING: Unknown device class!");
            return (NULL);
    }

    return (path);
}

/**
 * \brief wrapper function for lookups using the information of the interface
 *        to query the driver path
 *
 * \param device the device we want to query
 *
 * \return path to the driver binary on success
 *         NULL if there is no binary
 */
static char *usb_driver_lookup_iface(struct usb_device *device)
{
    USB_DEBUG_TR_ENTER;

    struct usb_interface *iface = device->ifaces;
    struct usb_interface_descriptor *idesc;

    char *path = NULL;

    uint32_t i = 0;

    /* loop over all interfaces of this device */
    while ((iface != NULL) && (i < device->iface_max)) {

        /* get the interface descriptor */
        idesc = iface->descriptor;
        if (idesc == NULL) {
            iface++;
            i++;
            continue;
        }

        /* query using the interface information */
        path = usb_driver_query(idesc->bInterfaceClass,
                idesc->bInterfaceSubClass, idesc->bInterfaceProtocol,
                device->device_desc.idVendor, device->device_desc.idProduct);

        /* if there is a path, stop. Continue with next otherwise */
        if (path != NULL) {
            break;
        }
        iface++;
        i++;
    }

    return (path);
}

/**
 * \brief wrapper function for looking up driver path for communication devices
 *
 * \param device the device to lookup
 *
 * \return path of the driver binary on success
 *         NULL if there is no matching binary
 */
static char *usb_driver_lookup_comm(struct usb_device *device)
{
    return (usb_driver_query(device->device_desc.bDeviceClass, 0, 0,
            device->device_desc.idVendor, device->device_desc.idProduct));
}

/**
 * \brief wrapper function for looking up driver path for diagnostic devices
 *
 * \param device the device to lookup
 *
 * \return path of the driver binary on success
 *         NULL if there is no matching binary
 */
static char *usb_driver_lookup_diag(struct usb_device *device)
{
    return (NULL);
}

/**
 * \brief wrapper function for looking up driver path for misc devices
 *
 * \param device the device to lookup
 *
 * \return path of the driver binary on success
 *         NULL if there is no matching binary
 */
static char *usb_driver_lookup_misc(struct usb_device *device)
{
    return (NULL);
}

/**
 * \brief wrapper function for looking up driver path for vendor devices
 *
 * \param device the device to lookup
 *
 * \return path of the driver binary on success
 *         NULL if there is no matching binary
 */
static char *usb_driver_lookup_vendor(struct usb_device *device)
{
    return (usb_driver_lookup_iface(device));
}

/**
 * \brief generic driver lookup wrapper function that deferres the lookup
 *        to the specific lookup function according to the USB IF
 *
 * \param device the device to lookup
 *
 * \return path of the driver binary on success
 *         NULL if there is no matching binary
 */
static char *usb_driver_lookup(struct usb_device *dev)
{
    USB_DEBUG_TR_ENTER;

    switch (dev->device_desc.bDeviceClass) {
        case USB_DEVICE_CLASS_HUB:
            USB_DEBUG("tried to start external hub driver...\n");
            return (NULL);

        case USB_DEVICE_CLASS_COMM:
            USB_DEBUG("communication device found...\n");
            return (usb_driver_lookup_comm(dev));
            break;
        case USB_DEVICE_CLASS_DIAG:
            USB_DEBUG("diag device found...\n");
            return (usb_driver_lookup_diag(dev));
            break;
        case USB_DEVICE_CLASS_MISC:
            USB_DEBUG("misc device found...\n");
            return (usb_driver_lookup_misc(dev));
            break;
        case USB_DEVICE_CLASS_VENDOR:
            return (usb_driver_lookup_vendor(dev));
            break;
    }

    /*
     * All other device classes have to be looked up using the
     * interface descriptor
     */
    return (usb_driver_lookup_iface(dev));
}

/**
 * \brief spawns the driver domain for the currently processing device
 *
 */
static void usb_driver_spawn(void)
{
    USB_DEBUG_TR_ENTER;

    /*
     * can only handle one device spawn at a time
     */
    if (device_process != NULL) {
        return;
    }

    /*
     * there are no devices left
     */
    if (devices_pending == NULL) {
        return;
    }

    /* get the next device */
    device_process = devices_pending;
    devices_pending = device_process->next_pending;

    assert(device_process != NULL);
    assert(device_process->path != NULL);

    domainid_t new_domain = -1;

    char *argv[1] = {
        [0] = NULL,
    };

    /* spawn the program */
    errval_t err = spawn_program(0, device_process->path, argv, NULL, 0,
            &new_domain);

    if (err_is_fail(err)) {
        DEBUG_ERR(err,
                "failed to spawn %s ", device_process->path);
        device_process = NULL;
    } else {
        USB_DEBUG_DRIVER("driver %s spawned\n", device_process->path);
    }
}

/**
 * \brief inserts a device into the pending list for later processing
 *
 * \param device the usb device to insert
 */
static void usb_driver_insert_pending(struct usb_device *device)
{
    device->next_pending = devices_pending;
    devices_pending = device;
}


/**
 * \brief associate the currently processing usb device with the respective
 *        flounder bindings and set the desired configuration
 *
 * \param bind the usb manager binding
 * \param driver the usb driver binding
 * \param config the configuration to set
 */
void usb_driver_connected(struct usb_manager_binding *bind,
        struct usb_driver_binding *driver, uint16_t config)
{

    usb_error_t err = USB_ERR_OK;

    assert(device_process != NULL);

    /* compare the desired configuration number and update if they differ*/
    if (device_process->config_number != config) {
        USB_DEBUG_DRIVER("Updating configuration to %u\n", config);

        err = usb_device_set_configuration(device_process, config);
        if (err != USB_ERR_OK) {
            bind->st = NULL;
            return;
        }
    }

    /* set the bindings in both ways */

    device_process->usb_manager_binding = bind;
    device_process->usb_driver_binding = driver;

    bind->st = device_process;

    /* set the current device to be processed */
    device_process = NULL;

    /* spawn possible next driver */
    usb_driver_spawn();
}

/**
 * \brief initiates the spawning of the usb driver binary.
 *
 * \param device the device to start the driver
 *
 * NOTE: hub devices are configured using the internal driver
 */
void usb_driver_start(struct usb_device *device)
{
    USB_DEBUG_TR_ENTER;

    usb_error_t err;

    /* hub devices are initialized using the internal hub driver */
    if (device->device_desc.bDeviceClass == USB_HUB_CLASS_CODE
            && device->device_desc.bDeviceSubClass == USB_HUB_SUBCLASS_CODE) {

        USB_DEBUG_DRIVER("Hub device. [%s, %s]\n", device->manifacturer, device->product);
        USB_DEBUG_DRIVER("Starting internal driver...\n");

        err = usb_hub_init(device);

        if (err != USB_ERR_OK) {
            USB_DEBUG("ERROR: Could not initialize the hub device!");
        }
        return;
    }

    /* otherwise: look up driver binary */
    char *path = usb_driver_lookup(device);
    USB_DEBUG_DRIVER("Vendor: [%s], Product: [%s]\n", device->manifacturer, device->product);

    if (path == NULL) {
        USB_DEBUG("WARNING: no suitable usb device driver found!\n");
        return;
    }

    /* set the binary path */
    device->path = path;

    /* insert it into the pending list */
    usb_driver_insert_pending(device);

    /* spawn it eventually */
    usb_driver_spawn();
}

