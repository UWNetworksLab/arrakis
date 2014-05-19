/*
 * \brief This file contains class specific functions for human interface
 *        devices such as keyboard and mouse
 *
 *        USB Device Class 0x03
 * ===========================================================================
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
#include <usb/usb_request.h>
#include <usb/usb_device.h>
#include <usb/usb_parse.h>
#include <usb/class/usb_hid.h>

/**
 * \brief   clears out the local data of a HID item descriptor
 */
static void usb_hid_item_clear_local(struct usb_hid_item *c)
{
    c->loc.count = 0;
    c->loc.size = 0;
    c->usage = 0;
    c->usage_minimum = 0;
    c->usage_maximum = 0;
    c->designator_index = 0;
    c->designator_minimum = 0;
    c->designator_maximum = 0;
    c->string_index = 0;
    c->string_minimum = 0;
    c->string_maximum = 0;
    c->set_delimiter = 0;
}

/**
 * \brief this function switches the report ID of an hid item with the
 *        new value
 *
 * \param s the current HID data
 * \param c the current HID item
 * \param next_rID the report id to set
 */
static void usb_hid_switch_reportid(struct usb_hid_data *s,
        struct usb_hid_item *c, int32_t next_rID)
{
    uint8_t i;

    /* check for same report ID - optimize */

    if (c->report_ID == next_rID)
        return;

    /* save current position for current rID */

    if (c->report_ID == 0) {
        i = 0;
    } else {
        for (i = 1; i != USB_HID_MAXID; i++) {
            if (s->last_pos[i].rid == c->report_ID)
                break;
            if (s->last_pos[i].rid == 0)
                break;
        }
    }
    if (i != USB_HID_MAXID) {
        s->last_pos[i].rid = c->report_ID;
        s->last_pos[i].position = c->loc.position;
    }

    /* store next report ID */

    c->report_ID = next_rID;

    /* lookup last position for next rID */

    if (next_rID == 0) {
        i = 0;
    } else {
        for (i = 1; i != USB_HID_MAXID; i++) {
            if (s->last_pos[i].rid == next_rID)
                break;
            if (s->last_pos[i].rid == 0)
                break;
        }
    }
    if (i != USB_HID_MAXID) {
        s->last_pos[i].rid = next_rID;
        c->loc.position = s->last_pos[i].position;
    } else {
        debug_printf("Out of RID entries, position is set to zero!\n");
        c->loc.position = 0;
    }
}

/**
 * \brief   allocates a new usb_hid_data structure for parsing the USB HID
 *          reports
 *
 * \param   d       initial data pointer
 * \param   len     length of the data
 * \param   kindset the kindset
 *
 * \return  pointer to the usb_hid_data structure on sucess
 *          NULL on failure
 */
struct usb_hid_data *usb_hid_start_parse(const void *d, uint32_t len,
        int32_t kindset)
{
    struct usb_hid_data *s;

    if ((kindset - 1) & kindset) {
        debug_printf("WARNING: Only one bit can be set in the kindset\n");
        return (NULL);
    }

    s = malloc(sizeof(*s));
    memset(s, 0, sizeof(*s));
    s->start = s->p = d;
    s->end = ((const uint8_t *) d) + len;
    s->kindset = kindset;
    return (s);
}

/**
 * \brief   frees up the usb_hid_data structure from parsiDPRINTFNng the report
 *          descriptor
 *
 * \param   s   the usb_hid_data structure to be freed
 */
void usb_hid_end_parse(struct usb_hid_data *s)
{
    if (s == NULL)
        return;

    free(s);
}

/**
 * \brief   gets the next byte when parsing the USB HID report descriptor
 *
 * \param   s       pointer to the parsing state
 * \param   wSize   bytes to advance the data pointer
 *
 * \return  the next byte on success
 *          0 on failure
 */
static uint8_t usb_hid_get_byte(struct usb_hid_data *s, const uint16_t wSize)
{
    const uint8_t *ptr;
    uint8_t retval;

    ptr = s->p;

    /* check if end is reached */
    if (ptr == s->end)
        return (0);

    /* read out a byte */
    retval = *ptr;

    /* check if data pointer can be advanced by "wSize" bytes */
    if ((s->end - ptr) < wSize)
        ptr = s->end;
    else
        ptr += wSize;

    /* update pointer */
    s->p = ptr;

    return (retval);
}

/**
 * \brief   this function gets the the next usb_hid_item from the report
 *          descriptor according to the current parsing state
 *
 * \param   s   the current parsing state
 * \param   h   pointer where to store the returned data
 *
 * \return  0: on failure
 *          1: on success
 */
int32_t usb_hid_get_item(struct usb_hid_data *s, struct usb_hid_item *h)
{
    struct usb_hid_item *c;
    uint32_t bTag, bType, bSize;
    uint32_t oldpos;
    int32_t mask;
    int32_t dval;

    if (s == NULL)
        return (0);

    c = &s->cur[s->pushlevel];

    top:
    /* check if there is an array of items */
    if (s->icount < s->ncount) {
        /* get current usage */
        if (s->iusage < s->nusage) {
            dval = s->usages_min[s->iusage] + s->ousage;
            c->usage = dval;
            s->usage_last = dval;
            if (dval == s->usages_max[s->iusage]) {
                s->iusage++;
                s->ousage = 0;
            } else {
                s->ousage++;
            }
        } else {
            dval = s->usage_last;
        }
        s->icount++;
        /*
         * Only copy HID item, increment position and return
         * if correct kindset!
         */
        if (s->kindset & (1 << c->kind)) {
            *h = *c;
            c->loc.position += c->loc.size * c->loc.count;
            return (1);
        }
    }

    /* reset state variables */
    s->icount = 0;
    s->ncount = 0;
    s->iusage = 0;
    s->nusage = 0;
    s->susage = 0;
    s->ousage = 0;
    usb_hid_item_clear_local(c);

    /* get next item */
    while (s->p != s->end) {

        bSize = usb_hid_get_byte(s, 1);
        if (bSize == 0xfe) {
            /* long item */
            bSize = usb_hid_get_byte(s, 1);
            bSize |= usb_hid_get_byte(s, 1) << 8;
            bTag = usb_hid_get_byte(s, 1);
            bType = 0xff; /* XXX what should it be */
        } else {
            /* short item */
            bTag = bSize >> 4;
            bType = (bSize >> 2) & 3;
            bSize &= 3;
            if (bSize == 3)
                bSize = 4;
        }
        switch (bSize) {
            case 0:
                dval = 0;
                mask = 0;
                break;
            case 1:
                dval = (int8_t) usb_hid_get_byte(s, 1);
                mask = 0xFF;
                break;
            case 2:
                dval = usb_hid_get_byte(s, 1);
                dval |= usb_hid_get_byte(s, 1) << 8;
                dval = (int16_t) dval;
                mask = 0xFFFF;
                break;
            case 4:
                dval = usb_hid_get_byte(s, 1);
                dval |= usb_hid_get_byte(s, 1) << 8;
                dval |= usb_hid_get_byte(s, 1) << 16;
                dval |= usb_hid_get_byte(s, 1) << 24;
                mask = 0xFFFFFFFF;
                break;
            default:
                dval = usb_hid_get_byte(s, bSize);
                USB_DEBUG("bad length %u (data=0x%02x)\n", bSize, dval);
                continue;
        }

        switch (bType) {
            case 0: /* Main */
                switch (bTag) {
                    case 8: /* Input */
                        c->kind = USB_HID_KIND_INPUT;
                        c->flags = dval;
                        ret: c->loc.count = s->loc_count;
                        c->loc.size = s->loc_size;

                        if (c->flags & USB_HID_IO_VARIABLE) {
                            /* range check usage count */
                            if (c->loc.count > 255) {
                                USB_DEBUG("Number of "
                                "items truncated to 255\n");
                                s->ncount = 255;
                            } else
                                s->ncount = c->loc.count;

                            /*
                             * The "top" loop will return
                             * one and one item:
                             */
                            c->loc.count = 1;
                        } else {
                            s->ncount = 1;
                        }
                        goto top;

                    case 9: /* Output */
                        c->kind = USB_HID_KIND_OUTPUT;
                        c->flags = dval;
                        goto ret;
                    case 10: /* Collection */
                        c->kind = USB_HID_KIND_COLLECTION;
                        c->collection = dval;
                        c->collevel++;
                        c->usage = s->usage_last;
                        *h = *c;
                        return (1);
                    case 11: /* Feature */
                        c->kind = USB_HID_KIND_FEATURE;
                        c->flags = dval;
                        goto ret;
                    case 12: /* End collection */
                        c->kind = USB_HID_KIND_ENDCOLLECTION;
                        if (c->collevel == 0) {
                            USB_DEBUG("invalid end collection\n");
                            return (0);
                        }
                        c->collevel--;
                        *h = *c;
                        return (1);
                    default:
                        /* noop */
                        break;
                }
                break;
            case 1: /* Global */
                switch (bTag) {
                    case 0:
                        c->_usage_page = dval << 16;
                        break;
                    case 1:
                        c->logical_minimum = dval;
                        break;
                    case 2:
                        c->logical_maximum = dval;
                        break;
                    case 3:
                        c->physical_minimum = dval;
                        break;
                    case 4:
                        c->physical_maximum = dval;
                        break;
                    case 5:
                        c->unit_exponent = dval;
                        break;
                    case 6:
                        c->unit = dval;
                        break;
                    case 7:
                        /* mask because value is unsigned */
                        s->loc_size = dval & mask;
                        break;
                    case 8:
                        usb_hid_switch_reportid(s, c, dval);
                        break;
                    case 9:
                        /* mask because value is unsigned */
                        s->loc_count = dval & mask;
                        break;
                    case 10: /* Push */
                        s->pushlevel++;
                        if (s->pushlevel < USB_HID_MAXPUSH) {
                            s->cur[s->pushlevel] = *c;
                            /* store size and count */
                            c->loc.size = s->loc_size;
                            c->loc.count = s->loc_count;
                            /* update current item pointer */
                            c = &s->cur[s->pushlevel];
                        } else {
                            USB_DEBUG("Cannot push "
                            "item @ %d\n", s->pushlevel);
                        }
                        break;
                    case 11: /* Pop */
                        s->pushlevel--;
                        if (s->pushlevel < USB_HID_MAXPUSH) {
                            /* preserve position */
                            oldpos = c->loc.position;
                            c = &s->cur[s->pushlevel];
                            /* restore size and count */
                            s->loc_size = c->loc.size;
                            s->loc_count = c->loc.count;
                            /* set default item location */
                            c->loc.position = oldpos;
                            c->loc.size = 0;
                            c->loc.count = 0;
                        } else {
                            USB_DEBUG("Cannot pop "
                            "item @ %d\n", s->pushlevel);
                        }
                        break;
                    default:
                        /* noop */
                        break;
                }
                break;
            case 2: /* Local */
                switch (bTag) {
                    case 0:
                        if (bSize != 4)
                            dval = (dval & mask) | c->_usage_page;

                        /* set last usage, in case of a collection */
                        s->usage_last = dval;

                        if (s->nusage < USB_HID_MAXUSAGE) {
                            s->usages_min[s->nusage] = dval;
                            s->usages_max[s->nusage] = dval;
                            s->nusage++;
                        } else {
                            USB_DEBUG("max usage reached\n");
                        }

                        /* clear any pending usage sets */
                        s->susage = 0;
                        break;
                    case 1:
                        s->susage |= 1;

                        if (bSize != 4)
                            dval = (dval & mask) | c->_usage_page;
                        c->usage_minimum = dval;

                        goto check_set;
                    case 2:
                        s->susage |= 2;

                        if (bSize != 4)
                            dval = (dval & mask) | c->_usage_page;
                        c->usage_maximum = dval;

                        check_set: if (s->susage != 3)
                            break;

                        /* sanity check */
                        if ((s->nusage < USB_HID_MAXUSAGE)
                                && (c->usage_minimum <= c->usage_maximum)) {
                            /* add usage range */
                            s->usages_min[s->nusage] = c->usage_minimum;
                            s->usages_max[s->nusage] = c->usage_maximum;
                            s->nusage++;
                        } else {
                            USB_DEBUG("Usage set dropped\n");
                        }
                        s->susage = 0;
                        break;
                    case 3:
                        c->designator_index = dval;
                        break;
                    case 4:
                        c->designator_minimum = dval;
                        break;
                    case 5:
                        c->designator_maximum = dval;
                        break;
                    case 7:
                        c->string_index = dval;
                        break;
                    case 8:
                        c->string_minimum = dval;
                        break;
                    case 9:
                        c->string_maximum = dval;
                        break;
                    case 10:
                        c->set_delimiter = dval;
                        break;
                    default:
                        /* noop */
                        break;
                }
                break;
            default:
                /* noop */
                break;
        }
    }
    return (0);
}

/**
 * \brief       calculates the size of the USB HID report descriptor
 *
 * \param buf   pointer to the buffer containing the data to parse
 * \param len   the length of the data in the buffer
 * \param k     the kind of the report descriptor we want to get the size of
 * \param id    the id of the descriptor we want to get
 *
 * \return      the length of the report in bytes
 */
int32_t usb_hid_report_size(const void *buf, uint32_t len, enum usb_hid_kind k,
        uint8_t *id)
{
    struct usb_hid_data *d;
    struct usb_hid_item h;
    uint32_t temp;
    uint32_t hpos;
    uint32_t lpos;
    uint8_t any_id;

    any_id = 0;
    hpos = 0;
    lpos = 0xFFFFFFFF;

    for (d = usb_hid_start_parse(buf, len, 1 << k); usb_hid_get_item(d, &h);) {
        if (h.kind == k) {
            /* check for ID-byte presence */
            if ((h.report_ID != 0) && !any_id) {
                if (id != NULL)
                    *id = h.report_ID;
                any_id = 1;
            }
            /* compute minimum */
            if (lpos > h.loc.position)
                lpos = h.loc.position;
            /* compute end position */
            temp = h.loc.position + (h.loc.size * h.loc.count);
            /* compute maximum */
            if (hpos < temp)
                hpos = temp;
        }
    }
    usb_hid_end_parse(d);

    /* safety check - can happen in case of currupt descriptors */
    if (lpos > hpos)
        temp = 0;
    else
        temp = hpos - lpos;

    /* check for ID byte */
    if (any_id)
        temp += 8;
    else if (id != NULL)
        *id = 0;

    /* return length in bytes rounded up */
    return ((temp + 7) / 8);
}

/**
 * \brief
 *
 * \param desc  the pointer to the report descriptor descriptor
 * \param size  the size of the descriptor
 * \param u     the usage
 * \param k     the kind of the report item we want to locate
 * \param index the index of the report with kind k we want to locate
 * \param loc   the returned location
 * \param flags the returned flags
 * \param id    the returned id
 *
 * \return  0: there is no matching element
 *          Else: there is a matching element found at location loc
 */
int32_t usb_hid_locate(const void *desc, uint32_t size, uint32_t usage,
        enum usb_hid_kind k, uint8_t repindex, struct usb_hid_location *loc,
        uint32_t *flags, uint8_t *id)
{
    struct usb_hid_data *d;
    struct usb_hid_item h;

    d = usb_hid_start_parse(desc, size, 1 << k);

    while (usb_hid_get_item(d, &h)) {
        if (h.kind == k && !(h.flags & USB_HID_IO_CONST) && h.usage == usage) {
            if (repindex--)
                continue;
            if (loc != NULL)
                *loc = h.loc;
            if (flags != NULL)
                *flags = h.flags;
            if (id != NULL)
                *id = h.report_ID;
            usb_hid_end_parse(d);
            return (1);
        }
    }
    if (loc != NULL)
        loc->size = 0;
    if (flags != NULL)
        *flags = 0;
    if (id != NULL)
        *id = 0;
    usb_hid_end_parse(d);
    return (0);
}

/**
 * \brief generic function that gets data out of some
 *
 * \param buf       buffer containing the data
 * \param len       the length of the data in the buffer
 * \param loc       the location we want to extract
 * \param is_signed the returned data is signed or not
 *
 * \return  0: on failure
 *          data: on success
 */
static uint32_t usb_hid_get_data_generic(const uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc, int32_t is_signed)
{
    uint32_t hpos = loc->position;
    uint32_t hsize = loc->size;
    uint32_t data;
    uint32_t rpos;
    uint8_t n;

    /* Range check and limit */
    if (hsize == 0)
        return (0);
    if (hsize > 32)
        hsize = 32;

    /* Get data in a safe way */
    data = 0;
    rpos = (hpos / 8);
    n = (hsize + 7) / 8;
    rpos += n;

    while (n--) {
        rpos--;
        if (rpos < len) {
            data |= buf[rpos] << (8 * n);
        }
    }

    /* Correctly shift down data */
    data = (data >> (hpos % 8));
    n = 32 - hsize;

    /* Mask and sign extend in one */
    if (is_signed != 0) {
        data = (int32_t) ((int32_t) data << n) >> n;
    } else {
        data = (uint32_t) ((uint32_t) data << n) >> n;
    }

    USB_DEBUG_HID("Get data generic: pos=%u, size=%u, data=%x\n", hpos, hsize, data);

    return (data);
}

/**
 * \brief   wrapper function for getting the data ouf of a report
 *          the data returned is signed
 *
 * \param buf       buffer containing the data
 * \param len       the length of the data in the buffer
 * \param loc       the location we want to extract
 *
 * \return  0: on failure
 *          data: on sucess
 */
int32_t usb_hid_get_data(const uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc)
{
    return (usb_hid_get_data_generic(buf, len, loc, 1));
}

/**
 * \brief   wrapper function for getting the data ouf of a report
 *          the data returned is unsigned
 *
 * \param buf       buffer containing the data
 * \param len       the length of the data in the buffer
 * \param loc       the location we want to extract
 *
 * \return  0: on failure
 *          data: on sucess
 */uint32_t usb_hid_get_data_unsigned(const uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc)
{
    return (usb_hid_get_data_generic(buf, len, loc, 0));
}

/*
 * \brief inserts a data value into the report descriptor
 *
 * \param buf   buffer containing the report descriptor
 * \param len   the length of the data in the buffer
 * \param loc   the location to put in the value
 * \param value the value to put into the report
 */
void usb_hid_put_data_unsigned(uint8_t *buf, uint32_t len,
        struct usb_hid_location *loc, uint32_t value)
{
    uint32_t hpos = loc->position;
    uint32_t hsize = loc->size;
    uint64_t data;
    uint64_t mask;
    uint32_t rpos;
    uint8_t n;

    /* Range check and limit */
    if (hsize == 0)
        return;
    if (hsize > 32)
        hsize = 32;

    /* Put data in a safe way */
    rpos = (hpos / 8);
    n = (hsize + 7) / 8;
    data = ((uint64_t) value) << (hpos % 8);
    mask = ((1ULL << hsize) - 1ULL) << (hpos % 8);
    rpos += n;
    while (n--) {
        rpos--;
        if (rpos < len) {
            buf[rpos] &= ~(mask >> (8 * n));
            buf[rpos] |= (data >> (8 * n));
        }
    }
}

/*
 * \brief checks if the HID device is a collection or a single function device
 *
 * \param desc  the descriptor to analyize
 * \param size  the size of the descriptor to analize
 * \param usage the usage of the device
 *
 * \return 1: the device is a collection
 *         0: the device is not a collection
 */
int32_t usb_hid_is_collection(const void *desc, uint32_t size, uint32_t usage)
{
    struct usb_hid_data *hd;
    struct usb_hid_item hi;
    int32_t err;

    hd = usb_hid_start_parse(desc, size, USB_HID_KIND_INPUT);
    if (hd == NULL)
        return (0);

    while ((err = usb_hid_get_item(hd, &hi))) {
        if (hi.kind == USB_HID_KIND_COLLECTION && hi.usage == usage)
            break;
    }
    usb_hid_end_parse(hd);
    return (err);
}

/**
 * \brief search for a hid descriptor in a config descriptor between two
 *        interface descriptors
 *
 *
 * \param cd the config descriptor to search
 * \param id the interface descriptor to start searching
 *
 * \return  usb_hid_descriptor on success
 *          NULL if there are moer HID descriptors
 */
struct usb_hid_descriptor *usb_hid_get_descriptor_from_usb(
        struct usb_config_descriptor *cd, struct usb_interface_descriptor *id)
{
    struct usb_descriptor *desc = (void *) id;

    if (desc == NULL) {
        return (NULL);
    }

    while ((desc = usb_parse_next_descriptor(cd, desc))) {
        if ((desc->bDescriptorType == USB_DESCRIPTOR_TYPE_HID)
                && (desc->bLength >= USB_HID_DESCRIPTOR_SIZE(0))) {
            return ((void *) desc);
        }
        if (desc->bDescriptorType == USB_DESCRIPTOR_TYPE_INTERFACE) {
            break;
        }
    }
    return (NULL);
}

/*
 * \brief get the HID descriptor from the usb device using a USB request
 *
 * \param ret_desc the returned descriptor
 * \param ret_size the returned size of the descriptor
 * \param iface the interface number to get the idescriptor of
 */
usb_error_t usb_hid_get_hid_descriptor(struct usb_hid_descriptor **ret_desc,
        uint16_t *ret_size, uint8_t iface)
{
    struct usb_hid_descriptor *hid;

    struct usb_config_descriptor *cfg_desc = usb_device_get_cfg_desc();

    hid = usb_hid_get_descriptor_from_usb(cfg_desc,
            (struct usb_interface_descriptor *) (cfg_desc + 1));

    if (hid == NULL) {
        return (USB_ERR_IOERROR);
    }

    *ret_size = USB_HID_DTYPE_GET_LEN(hid->descriptors);

    if (*ret_size == 0) {
        return (USB_ERR_IOERROR);
    }

    if (*ret_desc == NULL) {
        return (USB_ERR_NOMEM);
    }

    usb_error_t err = usb_hid_get_report_descriptor(ret_desc, *ret_size, iface);

    if (err) {
        *ret_desc = NULL;
        return (err);
    }

    return (USB_ERR_OK);
}

/**
 * \brief gets the report descriptor from the HID device
 *
 * \param d pointer to the report descriptror
 * \param size the expected size of the report descriptor
 * \param iface the interface we want to get the report of
 */
usb_error_t usb_hid_get_report_descriptor(struct usb_hid_descriptor **d,
        uint16_t size, uint8_t iface)
{
    if (d == NULL) {
        return (USB_ERR_INVAL);
    }
    struct usb_device_request req;
    req.bType.direction = USB_REQUEST_READ;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bRequest = USB_HID_REQUEST_GET_DESCRIPTOR;
    req.wValue = USB_DESCRIPTOR_TYPE_REPORT << 8;
    req.wIndex = iface;
    req.wLength = size;

    uint16_t ret_data_length;

    usb_error_t err = usb_do_request_read(&req, &ret_data_length, (void **) d);

    if (err != USB_ERR_OK) {
        *d = NULL;
    }

    if (ret_data_length != size) {
        USB_DEBUG("WARNING: got wrong data size. \n");
        free(*d);
        *d = NULL;
        err = USB_ERR_IOERROR;
    }

    return (err);

}

/**
 * \brief this function sets the idlerate of the HID device
 *
 * \param iface the interface to set the idle rate for
 * \param duration the interval in 2 ms setps
 * \param id the id of the report
 */
usb_error_t usb_hid_set_idle(uint8_t iface, uint8_t duration, uint8_t id)
{
    struct usb_device_request req;

    req.bRequest = USB_HID_REQUEST_SET_IDLE;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.recipient = USB_REQUEST_RECIPIENT_INTERFACE;
    req.bType.type = USB_REQUEST_TYPE_CLASS;

    req.wValue = (duration << 8) | id;
    req.wIndex = iface;
    req.wLength = 0;

    return (usb_do_request(&req));
}
