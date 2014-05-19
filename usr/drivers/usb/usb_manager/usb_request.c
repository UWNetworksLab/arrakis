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
#include <ctype.h>
#include <string.h>
#include <barrelfish/barrelfish.h>

#include <if/usb_manager_defs.h>
#include <if/usb_manager_rpcclient_defs.h>

#include <usb/usb.h>
#include <usb/usb_request.h>

#include <usb_device.h>
#include <usb_controller.h>
#include <usb_request.h>
#include <usb_transfer.h>
#include <usb_memory.h>
#include <usb_xfer.h>

/**
 * \brief   this function handles the USB requests and executes them either
 *          on the device or calls the root hub emulation function
 */
usb_error_t usb_handle_request(struct usb_device *device, uint16_t flags,
        struct usb_device_request *req, struct usb_request_state *req_state,
        void *data, uint16_t *ret_length)
{
    USB_DEBUG_TR_ENTER;

    struct usb_xfer *xfer;
    usb_error_t err = USB_ERR_OK;
    uint16_t length = req->wLength;
    uint16_t actual_length = 0;

    USB_DEBUG_REQ("bmRequestType = %x\n", *((uint8_t *)(&req->bType)));USB_DEBUG_REQ("bRequest  = %x\n", *((uint8_t *)(&req->bRequest)));USB_DEBUG_REQ("wValue = %x\n", *((uint16_t *)(&req->wValue)));USB_DEBUG_REQ("wIndex = %x\n", *((uint16_t *)(&req->wIndex)));USB_DEBUG_REQ("wLength= %x\n", *((uint16_t *)(&req->wLength)));

    /*
     * check if the device is in the correct state to handle requests
     * the device must be at lease in the powered state
     */
    if (device->state < USB_DEVICE_STATE_POWERED) {
        USB_DEBUG("Error: USB Device has not been configured\n");
        if (req_state) {
            req_state->error = USB_ERR_NOT_CONFIGURED;
            req_state->callback(req_state->bind);
        }

        USB_DEBUG_TR_RETURN;
        return (USB_ERR_NOT_CONFIGURED);
    }

    /*
     * reset the length value
     */
    if (ret_length) {
        *ret_length = 0;
    }

    /*
     * the device may be the root hub, so we need to take the root hub
     * execution function for this, the root hub is the device which
     * does not have a parent hub associated
     */
    if (device->parent_hub == NULL) {
        /*
         * cannot write data to the root hub
         */
        if ((req->bType.direction != USB_REQUEST_READ) && (length != 0)) {
            USB_DEBUG("Error: root hub does not support writing of data\n");
            if (req_state) {
                req_state->error = USB_ERR_INVAL;
                req_state->callback(req_state->bind);
            }

            USB_DEBUG_TR_RETURN;
            return (USB_ERR_INVAL);
        }

        const void *ret_desc;
        uint16_t ret_size;
        err = USB_ERR_NO_ROOT_HUB;
        if (device->controller->hcdi_bus_fn->roothub_exec != NULL) {
            err = device->controller->hcdi_bus_fn->roothub_exec(device, req,
                    &ret_desc, &ret_size);
        }
        if (err != USB_ERR_OK) {
            USB_DEBUG(
                    "ERROR: root_hub_exec failed(): %s\n", usb_get_error_string(err));
            if (req_state) {
                req_state->error = err;
                req_state->callback(req_state->bind);
            }USB_DEBUG_TR_RETURN;
            return (err);
        }

        /*
         * we have encountered a short transfer, this may be ok when the flag
         * is set
         */
        if (length > ret_size) {

            if (!(flags & USB_REQUEST_FLAG_IGNORE_SHORT_XFER)) {
                if (req_state) {
                    req_state->error = USB_ERR_SHORT_XFER;
                    req_state->callback(req_state->bind);
                }USB_DEBUG_TR_RETURN;
                return (USB_ERR_SHORT_XFER);
            }

            // short xfers are ok so update the length
            length = ret_size;
        }

        if (ret_length) {
            *ret_length = length;
        }

        /*
         * we have some data that we have to return
         */
        if (length > 0 && data != NULL) {
            memcpy(data, ret_desc, length);
        }
        if (req_state) {
            req_state->error = USB_ERR_OK;
        }

        return (USB_ERR_OK);
    }

    /*
     * we are executing the request on a real device so we have to setup
     * a new USB control transfer on this device
     */

    usb_transfer_setup_ctrl_default(device, req_state);

    xfer = device->ctrl_xfer[0];
    xfer->ed_direction =
            (req->bType.direction == USB_REQUEST_READ) ?
                    USB_ENDPOINT_DIRECTION_IN : USB_ENDPOINT_DIRECTION_OUT;
    if (xfer == NULL) {
        USB_DEBUG("ERROR: No memory for setting up transfers\n");
        return (USB_ERR_NOMEM);
    }

    if (req_state) {
        req_state->xfer = xfer;
    }
    /*
     * we have a xfer so set it up according to the setup
     * and the given flags
     */
    if (flags & USB_REQUEST_FLAG_DELAY_STATUS) {
        xfer->flags.manual_status = 1;
    } else {
        xfer->flags.manual_status = 0;
    }

    if (flags & USB_REQUEST_FLAG_IGNORE_SHORT_XFER) {
        xfer->flags.short_xfer_ok = 1;
    } else {
        xfer->flags.short_xfer_ok = 1;
    }

    xfer->timeout = 1000;   // TODO: TIMEOUT

    /*
     * copy the request into DMA memory
     */
    usb_mem_copy_in(xfer->frame_buffers[0], 0, req,
            sizeof(struct usb_device_request));
    xfer->frame_lengths[0] = sizeof(struct usb_device_request);

    /*
     * loop till we got all requested data
     */
    uint16_t current_data_length;
    while (1) {
        current_data_length = length;
        if (current_data_length > xfer->max_data_length) {
            USB_DEBUG(
                    "NOTICE: current_data_length (%u)> xfer->max_data_length (%u)\n", current_data_length, xfer->max_data_length);
            current_data_length = xfer->max_data_length;
        }
        // set the frame length of the data stage
        xfer->frame_lengths[1] = current_data_length;

        /*
         * we have a data stage, so we have to handle the data read or write
         * in the case of data write, we have to copy the data into the
         * second frame buffer and indicate that we have two frames
         */
        if (current_data_length > 0) {
            if ((req->bType.direction == USB_REQUEST_WRITE)) {
                usb_mem_copy_in(xfer->frame_buffers[1], 0, data,
                        current_data_length);
            }
            xfer->num_frames = 2;
        } else {
            if (xfer->frame_lengths[0] == 0) {
                if (xfer->flags.manual_status) {
                    xfer->flags.manual_status = 0;
                } else {
                    break;
                }
            }
            xfer->num_frames = 1;
        }

        USB_DEBUG_REQ("-------------------- starting transfer\n");
        usb_transfer_start(xfer);

        /* wait till completed... */
        while (!usb_transfer_completed(xfer)) {
            USB_WAIT(10);
            //thread_yield();
        }

        /*
         * transfer is complete, check for error condition
         */
        err = xfer->error;

        if (err != USB_ERR_OK) {
            break;
        }

        /*
         * get the actual number of frames
         */
        if (xfer->actual_frames < 2) {
            actual_length = 0;  // no data stage
        } else {
            actual_length = xfer->frame_lengths[1];
        }

        /*
         * updating variables to catch short packets
         */
        if (current_data_length > actual_length) {
            current_data_length = actual_length;
            length = current_data_length;
        }

        /*
         * copy the data out to buffer if it is a read request
         * and we have some bytes to read
         */
        if ((current_data_length > 0)
                && (req->bType.direction == USB_REQUEST_READ)) {
            usb_mem_copy_out(xfer->frame_buffers[1], 0, data,
                    current_data_length);
        }

        /*
         * update the frame length accordingly
         */
        xfer->frame_lengths[0] = 0;
        length -= current_data_length;

        /*
         * advance buffer pointer
         */
        data += current_data_length;

        if (ret_length) {
            (*ret_length) += current_data_length;
        }

        /*
         * TODO: Timeout
         */

    }

    if (err != USB_ERR_OK) {
        usb_transfer_stop(xfer);
    }

    if (req_state) {
        req_state->error = (usb_error_t) err;
        req_state->callback(req_state);
    }

    USB_DEBUG_TR_RETURN;
    return ((usb_error_t) err);
}

usb_error_t usb_exec_request(struct usb_device *device, uint16_t flags,
        struct usb_device_request *request, void *data, uint16_t *ret_length)
{
    return (usb_handle_request(device, flags, request, NULL, data, ret_length));
}

/*
 * --------------------------------------------------------------------------
 * Flounder Callbacks
 * --------------------------------------------------------------------------
 */

/// define for checking of error codes and retrying
#define USB_TX_REQUEST_ERR(_retry) \
    if (err_is_fail(err)) { \
       if (err_no(err) == FLOUNDER_ERR_TX_BUSY) {\
           USB_DEBUG_IDC("re-sending _retry() \n");\
           txcont = MKCONT(_retry, st);\
           struct waitset *ws = get_default_waitset();\
           err = st->bind->register_send(st->bind, ws, txcont);\
           if (err_is_fail(err)) {\
               DEBUG_ERR(err, "error register_send on binding failed!\n");\
           }\
       } else {\
           DEBUG_ERR(err, "error _retry(): sending response!\n");\
           free_request_state(st);\
       }\
   }\


/**
 * \brief   frees up the request state struct upon completion
 *
 * \param   st  the state to free
 */
static void free_request_state(struct usb_request_state *st)
{
    if (st->data) {
        free(st->data);
    }
    if (st->req) {
        free(st->req);
    }
    free(st);
}

static void usb_tx_request_generic_cb(void *a)
{
    USB_DEBUG_IDC("rusb_tx_request_generic_cb(): successful transmitted\n");
    struct usb_request_state *st = (struct usb_request_state *) a;

    free_request_state(st);
}

/**
 * \brief   wrapper function for handling error state
 */
static void usb_request_send_error(usb_error_t err,
        struct usb_manager_binding *b, void (*callback)(void *a))
{
    struct usb_request_state *rs = malloc(sizeof(struct usb_request_state));

    rs->xfer = NULL;
    rs->bind = b;
    callback(rs);
}

/* ------------------- read request------------------- */

/**
 *
 */
static void usb_tx_request_read_response(void *a)
{
    errval_t err;
    struct usb_request_state *st = (struct usb_request_state *) a;

    USB_DEBUG_IDC("send usb_tx_request_read_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_request_generic_cb, st);

    err = usb_manager_request_read_response__tx(st->bind, txcont, st->data,
            st->data_length, (uint32_t) st->error);

    USB_TX_REQUEST_ERR(usb_tx_request_read_response);
}

/**
 *
 */
void usb_rx_request_read_call(struct usb_manager_binding *binding,
        uint8_t *request, size_t req_length)
{
    USB_DEBUG_TR_ENTER;

    USB_DEBUG_IDC("received usb_rx_request_read_call()\n");

    // check if we have received the correct amount of data
    if (req_length != sizeof(struct usb_device_request)) {
        USB_DEBUG_IDC("received too less data to fullfill the request:\n "
                "request length: expected %i bytes, was %i\n",
                sizeof(struct usb_device_request), req_length);

        usb_request_send_error(USB_ERR_INVAL, binding,
                usb_tx_request_read_response);
    }

    /*
     * execute request and prepare reply
     */
    struct usb_request_state *st = malloc(sizeof(struct usb_request_state));

    struct usb_device *device = (struct usb_device *) binding->st;
    struct usb_device_request *req = (struct usb_device_request *) request;
    st->req = req;
    st->bind = binding;
    st->data_length = 0;
    if (req->wLength > 0) {
        st->data = malloc(req->wLength);
    } else {
        /* XXX: Just allocating some memory, note this may not be enough */
        st->data = malloc(1024);
        req->wLength = 1024;  // setting the maximum data length
    }

    st->callback = usb_tx_request_read_response;

    usb_handle_request(device, 0, req, st, st->data, &st->data_length);
}

/* ------------------- write request -------------------  */

static void usb_tx_request_write_response(void *a)
{
    USB_DEBUG_TR_ENTER;

    errval_t err;
    struct usb_request_state *st = (struct usb_request_state *) a;

    USB_DEBUG_IDC("send usb_tx_request_write_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_request_generic_cb, st);

    err = usb_manager_request_write_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_REQUEST_ERR(usb_tx_request_write_response);
}

void usb_rx_request_write_call(struct usb_manager_binding *binding,
        uint8_t *request, size_t req_length, uint8_t *data, size_t data_length)
{
    USB_DEBUG_TR_ENTER;

    USB_DEBUG_IDC("received usb_rx_request_call() of %i bytes\n", data_length);

    struct usb_device_request *req = (struct usb_device_request *) request;

    /* check if we have received the correct amount of data */
    if ((req_length != sizeof(struct usb_device_request))
            || (req->wLength != data_length)) {
        USB_DEBUG_IDC("ERROR in usb_rx_request_call(): received too less data"
                " to full fill the request:\n "
                "request length: expected %i bytes, was %i\n"
                "data length: expected %i, was %i\n",
                sizeof(struct usb_device_request), req_length, req->wLength,
                data_length);

        usb_request_send_error(USB_ERR_INVAL, binding,
                usb_tx_request_write_response);
    }

    /* execute request and prepare reply */

    struct usb_request_state *st = malloc(sizeof(struct usb_request_state));

    if (st == NULL) {
        USB_DEBUG_IDC("WARNING: usb_rx_request_write_call(): out of memory\b");

        usb_request_send_error(USB_ERR_NOMEM, binding,
                usb_tx_request_write_response);

        return;
    }

    /* fill in the struct */

    st->bind = binding;
    st->req = req;
    /* write requests have no data to return */
    st->data_length = data_length;
    st->data = data;
    st->callback = usb_tx_request_write_response;

    struct usb_device *device = (struct usb_device *) binding->st;

    usb_handle_request(device, 0, req, st, st->data, &st->data_length);

}

/* ------------------- simple request ------------------- */

static void usb_tx_request_response(void *a)
{
    USB_DEBUG_TR_ENTER;

    errval_t err;
    struct usb_request_state *st = (struct usb_request_state *) a;

    USB_DEBUG_IDC("sending usb_tx_request_response()\n");

    struct event_closure txcont = MKCONT(usb_tx_request_generic_cb, st);

    err = usb_manager_request_response__tx(st->bind, txcont,
            (uint32_t) st->error);

    USB_TX_REQUEST_ERR(usb_tx_request_response);
}

void usb_rx_request_call(struct usb_manager_binding *binding, uint8_t *request,
        size_t req_length)
{
    USB_DEBUG_TR_ENTER;

    USB_DEBUG_IDC("received usb_rx_request_call()\n");

    /* check if we have received the correct amount of data */
    if (req_length != sizeof(struct usb_device_request)) {
        USB_DEBUG_IDC("ERROR in usb_rx_request_call(): received too less data"
                " to full fill the request:\n "
                "request length: expected %i bytes, was %i\n",
                sizeof(struct usb_device_request), req_length);

        usb_request_send_error(USB_ERR_INVAL, binding, usb_tx_request_response);

        return;
    }

    /* execute request and prepare reply  */

    struct usb_request_state *st = malloc(sizeof(struct usb_request_state));

    if (st == NULL) {
        USB_DEBUG_IDC("WARNING:usb_rx_request_call(): out of memory\b");

        usb_request_send_error(USB_ERR_NOMEM, binding, usb_tx_request_response);

        return;
    }

    /* fill in the struct */

    st->bind = binding;
    /* simple requests have no data to return */
    st->data_length = 0;
    st->data = NULL;
    st->callback = usb_tx_request_response;

    /* get the device from the binding state */
    struct usb_device *device = (struct usb_device *) binding->st;

    struct usb_device_request *req = (struct usb_device_request *) request;

    usb_handle_request(device, 0, req, st, st->data, &st->data_length);
}

/*
 * ==========================================================================
 *  The following functions are needed to handle the configuration of new
 *  devices
 * ==========================================================================
 */

/**
 * \brief Sets the address of a new device
 */
usb_error_t usb_req_set_address(struct usb_device *dev, uint16_t addr)
{
    USB_DEBUG_TR_ENTER;
    struct usb_device_request req;
    usb_error_t err = USB_ERR_INVAL;

    req.bRequest = USB_REQUEST_SET_ADDRESS;
    req.wValue = addr;
    req.wIndex = 0;
    req.wLength = 0;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.direction = USB_REQUEST_WRITE;

    if (dev->controller->hcdi_bus_fn->set_address != NULL) {
        USB_DEBUG("set_address function set.\n");
        err = (dev->controller->hcdi_bus_fn->set_address)(dev, addr);
    }

    if (err != USB_ERR_INVAL) {
        return (err);
    }

    return (usb_exec_request(dev, 0, &req, NULL, NULL));
}

/**
 * \brief this function executs a get descriptor request on a USB device
 */
usb_error_t usb_req_get_descriptor(struct usb_device *dev,
        uint16_t *actual_length, void *desc, uint16_t min_length,
        uint16_t max_length, uint16_t id, uint8_t type, uint8_t desc_index,
        uint8_t retries)
{
    USB_DEBUG_TR_ENTER;
    struct usb_device_request req;
    usb_error_t err;

    req.bRequest = USB_REQUEST_GET_DESCRIPTOR;
    req.wIndex = id;
    req.bType.direction = USB_REQUEST_READ;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.wValue = ((type) << 8) | desc_index;
    uint16_t ret_length = 0;

    /*
     * XXX: There is actually a bug if min_length < max_length
     * this causes the loop to run forever...
     */
    while (1) {
        if ((min_length < 2) || (max_length < 2)) {
            return (USB_ERR_INVAL);
        }

        req.wLength = min_length;

        err = usb_exec_request(dev, 0, &req, desc, &ret_length);

        if (err != USB_ERR_OK) {
            if (!retries) {
                return (err);
            }
            USB_DEBUG("#### RETRY: usb_req_get_descriptor()... halting.\n");
            while (1)
                ;
            retries--;

            continue;
        }

        uint8_t *buf = desc;
        if (min_length == max_length) {

            if ((buf[0] > min_length) && (actual_length == NULL)) {
                buf[0] = min_length;
            }
            buf[1] = type;
            break;
        }

        if (max_length > buf[0]) {
            max_length = buf[0];
        }

        while (min_length > max_length) {
            buf[--min_length] = 0;
        }

        min_length = max_length;
    }

    if (actual_length != NULL) {
        if (err != USB_ERR_OK) {
            *actual_length = 0;
        } else {
            *actual_length = min_length;
        }
    }

    USB_DEBUG_TR_RETURN;
    return (err);
}

/**
 * \brief this function gets the device descriptor from a device
 */
usb_error_t usb_req_get_device_descriptor(struct usb_device *dev,
        struct usb_device_descriptor *desc)
{
    USB_DEBUG_TR_ENTER;

    usb_error_t err;
    uint16_t act_len = 0;

    err = usb_req_get_descriptor(dev, &act_len, desc, sizeof(*desc),
            sizeof(*desc), 0, USB_DESCRIPTOR_TYPE_DEVICE, 0, 3);

    if (act_len != sizeof(struct usb_device_descriptor)) {
        debug_printf("WARNING: received too less data!");
        assert(err != USB_ERR_OK);
    }
    return (err);
}

/**
 * \brief this function gets the configuration descriptor from a device
 */
usb_error_t usb_req_get_config_descriptor(struct usb_device *dev,
        struct usb_config_descriptor **cdesc, uint8_t cindex)
{
    USB_DEBUG_TR_ENTER;

    usb_error_t err;

    struct usb_config_descriptor cd;

    err = usb_req_get_descriptor(dev, NULL, &cd, sizeof(cd), sizeof(cd), 0,
            USB_DESCRIPTOR_TYPE_CONFIG, cindex, 0);
    if (err) {
        return (err);
    }
    /* Extra sanity checking */
    if (cd.wTotalLength < sizeof(cd)) {
        return (USB_ERR_INVAL);
    }

    /* the configuration descriptor is of arbitrary size so allocate memory */
    *cdesc = malloc(cd.wTotalLength);

    if (*cdesc == NULL) {
        return (USB_ERR_NOMEM);
    }

    err = usb_req_get_descriptor(dev, NULL, *cdesc, cd.wTotalLength,
            cd.wTotalLength, 0, USB_DESCRIPTOR_TYPE_CONFIG, cindex, 3);

    if (err != USB_ERR_OK) {
        free(*cdesc);
        *cdesc = NULL;
        return (err);
    }

    dev->config_desc_size = cd.wTotalLength;

    return (USB_ERR_OK);
}

usb_error_t usb_req_get_string_desc(struct usb_device *dev, void *sdesc,
        uint16_t max_len, uint16_t lang_id, uint8_t string_index)
{
    //return (USB_ERR_IOERROR);
    /* TODO: change min_length = 2 */
    return (usb_req_get_descriptor(dev, NULL, sdesc, 2, max_len, lang_id,
            USB_DESCRIPTOR_TYPE_STRING, string_index, 0));
}

usb_error_t usb_req_get_string(struct usb_device *dev, char *buf, uint16_t len,
        uint8_t string_index)
{

    if (len == 0) {
        return (USB_ERR_OK);
    }

    if (string_index == 0) {
        buf[0] = 0;
        return (USB_ERR_INVAL);
    }

    if (dev->flags.no_strings) {
        buf[0] = 0;
        return (USB_ERR_STALLED);
    }

    usb_error_t err = usb_req_get_string_desc(dev, buf, len, dev->language_id,
            string_index);

    if (err != USB_ERR_OK) {
        buf[0] = 0;
        return (err);
    }
    uint8_t *val = (uint8_t *) buf;
    if (val[0]<2) {
        buf[0] = 0;
        return (USB_ERR_INVAL);
    }
    len--;

    char *str = buf;
    uint8_t swap;
    uint16_t num = (val[0] / 2) - 1;

    if (num > len) {
        num = len;
    }

    val += 2;
    swap = 3;
    uint16_t c;
    for (uint16_t i = 0; i < num; i++) {
        c = (uint16_t) ((val + (2 * i))[0] | ((val + (2 * i))[1] << 8));
        if (((c & 0xff00) == 0) && (swap & 1)) {
            *str = c;
            swap = 1;
        } else if (((c & 0x00FF) == 0) && (swap & 2)) {
            *str = c >> 8;
            swap = 2;
        } else {
            continue;
        }

        if (isalpha((uint8_t)*str) || isdigit((uint8_t)*str) || *str == '-' || *str == '+'
                || *str == ' ' || *str == '.' || *str == ',') {
            str++;
        }
    }
    *str = 0;

    return (USB_ERR_OK);
}

/**
 * \brief updates the configuration with the new configuration value
 */
usb_error_t usb_req_set_config(struct usb_device *dev, uint8_t config)
{
    USB_DEBUG_TR_ENTER;

    struct usb_device_request req;

    req.bRequest = USB_REQUEST_SET_CONFIG;
    req.wIndex = 0;
    req.bType.direction = USB_REQUEST_WRITE;
    req.bType.type = USB_REQUEST_TYPE_STANDARD;
    req.bType.recipient = USB_REQUEST_RECIPIENT_DEVICE;
    req.wValue = config;
    req.wLength = 0;

    return (usb_exec_request(dev, 0, &req, NULL, NULL));

}
