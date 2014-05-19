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

#include <usb/usb.h>
#include <usb/usb_parse.h>


/**
 * \brief use this function to iterate over the USB configuration descriptor
 *
 * \param cd    the configuration descriptor to iterate over
 * \param desc  the descriptor to start from
 *
 * \return  NULL: there iare no mor descriptors
 *          Else: the next descriptor after "desc"
 */
struct usb_descriptor *usb_parse_next_descriptor(
        struct usb_config_descriptor *cd, struct usb_descriptor *_desc)
{
    USB_DEBUG_TR_ENTER;
    uint8_t *desc_next;
    uint8_t *start;
    uint8_t *end;
    uint8_t *desc;

    /* be NULL safe */
    if (cd == NULL)
        return (NULL);

    /* We assume that the "wTotalLength" has been checked. */
    start = (uint8_t *) cd;
    end = start + cd->wTotalLength;
    desc = (uint8_t *) _desc;

    /* Get start of next USB descriptor. */
    if (desc == NULL)
        desc = start;
    else
        desc = desc + desc[0];

    /* Check that the next USB descriptor is within the range. */
    if ((desc < start) || (desc >= end))
        return (NULL); /* out of range, or EOD */

    /* Check that the second next USB descriptor is within range. */
    desc_next = desc + desc[0];
    if ((desc_next < start) || (desc_next > end))
        return (NULL); /* out of range */

    /* Check minimum descriptor length. */
    if (desc[0] < 3)
        return (NULL); /* too short descriptor */

    /* Return start of next descriptor. */
    return ((struct usb_descriptor *) desc);
}

/**
 * \brief this function will lookup the next interface descriptor in an
 *        configuration descriptor
 *
 * \param cd  the configuration descriptor
 * \param ps  the parse state which interface descriptor will be next
 *
 * \return  NULL: there are no more descriptors
 *          Else: the next interface descriptor
 */
struct usb_interface_descriptor *usb_parse_next_iface(
        struct usb_config_descriptor *cd, struct usb_iface_parse_state *ps)
{
    USB_DEBUG_TR_ENTER;

    struct usb_interface_descriptor *id;
    uint8_t new_iface;

    /* retrieve current descriptor */
    id = (struct usb_interface_descriptor *) ps->desc;

    /* default is to start a new interface */
    new_iface = 1;

    while (1) {
        /*
         * loop over the remaining descriptors and check if it is of type
         * interface descriptor
         */
        id = (struct usb_interface_descriptor *) usb_parse_next_descriptor(cd,
                (struct usb_descriptor *) id);

        if (id == NULL) {
            /* no more left, stop */
            break;
        }

        /* compare descriptor type */
        if ((id->bDescriptorType == USB_DESCRIPTOR_TYPE_INTERFACE)
                && (id->bLength >= sizeof(*id))) {
            if (ps->iface_no_last == id->bInterfaceNumber)
                new_iface = 0;
            ps->iface_no_last = id->bInterfaceNumber;
            break;
        }
    }

    if (ps->desc == NULL) {
        /* first time */
    } else if (new_iface) {
        /* new interface */
        ps->iface_index++;
        ps->iface_index_alt = 0;
    } else {
        /* new alternate interface */
        ps->iface_index_alt++;
    }

    /* store and return current descriptor */
    ps->desc = (struct usb_descriptor *) id;
    return (id);
}


/**
 * \brief this function will lookup the next endpoint descriptor in an
 *        configuration descriptor
 *
 * \param cd  the configuration descriptor
 * \param ped  a pointer to an interface descriptor
 *
 * \return  NULL: there are no more descriptors
 *          Else: the next descriptor after "desc"
 */
struct usb_endpoint_descriptor *usb_parse_next_edesc(
        struct usb_config_descriptor *cd, struct usb_endpoint_descriptor *ped)
{
    USB_DEBUG_TR_ENTER;
    struct usb_descriptor *desc;

    desc = ((struct usb_descriptor *) ped);

    /* loop over the descritpors */
    while ((desc = usb_parse_next_descriptor(cd, desc))) {
        /* if the type is configuration we are finish, no more descriptors */
        if (desc->bDescriptorType == USB_DESCRIPTOR_TYPE_CONFIG) {
            break;
        }
        /* this is an endpoint descriptor, check for the size */
        if (desc->bDescriptorType == USB_DESCRIPTOR_TYPE_ENDPOINT) {
            if (desc->bLength < sizeof(*ped)) {
                /* endpoint descriptor is invalid */
                break;
            }
            return ((struct usb_endpoint_descriptor *) desc);
        }
    }
    return (NULL);
}
