/**
 * \brief this function contains functions to manage USB transfers
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


#include <usb/usb.h>
#include <usb/usb_request.h>
#include <usb/usb_xfer.h>
#include <usb/usb_transfer.h>

#include "usb_manager_client.h"

/*
 * -------------------------------------------------------------------------
 * Internatl transfer state
 * -------------------------------------------------------------------------
 */

/**
 * struct containing information about the existing transfers
 */
struct usb_xfer_state {
    usb_xfer_id_t tid;              ///< the transfer id
    usb_type_t type;                ///< the type of the transfer
    usb_transfer_cb_t *done_cb;     ///< pointer to the callback function
    usb_error_t error;              ///< the error condition of the transfer
    usb_tstate_t state;             ///< the state of the transfer
    struct usb_xfer_state *next;    ///< pointer to the next transfer
    struct usb_xfer_state *prev;    ///< previous transfer
};

/// stores the created transfer
static struct usb_xfer_state *xfers = NULL;


/*
 * -------------------------------------------------------------------------
 * Helper functions for maintaining the queue
 * -------------------------------------------------------------------------
 */

/**
 * \brief   enqueues a xfer state element into the xfer list
 *
 * \param   state   the xfer state to enqueue
 */
static void usb_xfer_state_enq(struct usb_xfer_state *state)
{
    if (xfers == NULL) {
        state->prev = NULL;
        state->next = NULL;
        xfers = state;
        return;
    }

    state->prev = NULL;
    state->next = xfers;
    xfers->prev = state;
    xfers = state;
}

/**
 * \brief   removes a xfer state from the xfers list, this does not
 *          free the element struct
 *
 * \param   state   the state to dequeue from the list
 */
static void usb_xfer_state_deq(struct usb_xfer_state *state)
{
    if (xfers->next == NULL) {
        xfers = NULL;
        return;
    }

    state->prev->next = state->next;
    state->next->prev = state->prev;
}


/**
 * \brief   gets the xfer state struct to a corresponding transfer id
 *
 * \param   tid     the transfer ID
 *
 * \return  pointer to the usb transfer state if matchting tids
 *          NULL otherwise
 */
static struct usb_xfer_state *usb_xfer_get_state(usb_xfer_id_t tid)
{
    struct usb_xfer_state *st = xfers;
    while (st) {
        if (st->tid == tid) {
            break;
        }
        st = st->next;
    }

    if (st->tid != tid) {
        return (NULL);
    }

    return (st);
}


/**
 * \brief this function is part of the USB driver interface and gets called
 *        when an USB transfer is completed.
 */
void usb_driver_rx_done_notify(struct usb_driver_binding *b,
        uint32_t tid, uint32_t error, uint8_t *data, size_t length)
{

    struct usb_xfer_state *st = usb_xfer_get_state(tid);

    if (st == NULL) {
        debug_printf("WARNING: xfer done with unknown tid.. %u\n", tid);
        free(data);
        return;
    }

    st->done_cb(error, data, length);

    /* XXX: this may be better done in the callback, since the data may need
     *      be to be present afterwards...
     */
    free(data);
}


/*
 * -------------------------------------------------------------------------
 * Functions for setup/unsetup of USB transfers
 * -------------------------------------------------------------------------
 */

/**
 * \brief   sets up a new USB control transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_control(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id)
{
    uint32_t ret_tid = 0;
    uint32_t ret_error = 0;

    usb_manager_setup_param_t *params = (usb_manager_setup_param_t *) setup;

    usb_manager.vtbl.transfer_setup(&usb_manager, (uint8_t) USB_TYPE_CTRL,
            *params, &ret_error, &ret_tid);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    struct usb_xfer_state *st = malloc(sizeof(struct usb_xfer_state));

    st->done_cb = done_cb;
    st->error = USB_ERR_OK;
    st->state = USB_TRANSFER_STATE_SETUP;
    st->tid = ret_tid;
    st->type = USB_TYPE_CTRL;

    usb_xfer_state_enq(st);

    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB control transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_isoc(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id)
{
    uint32_t ret_tid = 0;
    uint32_t ret_error = 0;

    usb_manager_setup_param_t *params = (usb_manager_setup_param_t *) setup;

    usb_manager.vtbl.transfer_setup(&usb_manager, (uint8_t) USB_TYPE_ISOC,
            *params, &ret_error, &ret_tid);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    struct usb_xfer_state *st = malloc(sizeof(struct usb_xfer_state));

    st->done_cb = done_cb;
    st->error = USB_ERR_OK;
    st->state = USB_TRANSFER_STATE_SETUP;
    st->tid = ret_tid;
    st->type = USB_TYPE_ISOC;

    usb_xfer_state_enq(st);

    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB bulk transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_bulk(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id)
{
    uint32_t ret_tid = 0;
    uint32_t ret_error = 0;

    usb_manager_setup_param_t *params = (usb_manager_setup_param_t *) setup;

    usb_manager.vtbl.transfer_setup(&usb_manager, (uint8_t) USB_TYPE_BULK,
            *params, &ret_error, &ret_tid);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    struct usb_xfer_state *st = malloc(sizeof(struct usb_xfer_state));

    st->done_cb = done_cb;
    st->error = USB_ERR_OK;
    st->state = USB_TRANSFER_STATE_SETUP;
    st->tid = ret_tid;
    st->type = USB_TYPE_BULK;

    usb_xfer_state_enq(st);

    return (USB_ERR_OK);
}

/**
 * \brief   sets up a new USB interrupt transfer for the associated
 *          device, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   setup   setup parameters
 * \param   done_cb callback when the transfer completes
 * \param   ret_id  returns the ID of the created usb transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_setup_intr(usb_transfer_setup_t *setup,
        usb_transfer_cb_t *done_cb, usb_xfer_id_t *ret_id)
{
    uint32_t ret_tid = 0;
    uint32_t ret_error = 0;

    usb_manager_setup_param_t *params = (usb_manager_setup_param_t *) setup;

    usb_manager.vtbl.transfer_setup(&usb_manager, (uint8_t) USB_TYPE_INTR,
            *params, &ret_error, &ret_tid);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    *ret_id = ret_tid;

    struct usb_xfer_state *st = malloc(sizeof(struct usb_xfer_state));

    st->done_cb = done_cb;
    st->error = USB_ERR_OK;
    st->state = USB_TRANSFER_STATE_SETUP;
    st->tid = ret_tid;
    st->type = USB_TYPE_INTR;

    usb_xfer_state_enq(st);

    return (USB_ERR_OK);
}

/**
 * \brief   destroys the previously created USB transfer and frees up the
 *          used resources, this allocates the resources, but does not start
 *          the execution of the transfer.
 *
 * \param   tid the transfer id of the transfer to unsetup
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_unsetup(usb_xfer_id_t tid)
{
    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        return (USB_ERR_INVAL);
    }
    uint32_t ret_error = 0;

    usb_manager.vtbl.transfer_unsetup(&usb_manager, tid, &ret_error);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    usb_xfer_state_deq(st);

    free(st);

    return (USB_ERR_OK);
}


/*
 * -------------------------------------------------------------------------
 * Functions for transfer controlling
 * -------------------------------------------------------------------------
 */

/**
 * \brief   initiates the execution of an existing USB transfer
 *
 * \param   tid the ID of the transfer to start
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_start(usb_xfer_id_t tid)
{
    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        USB_DEBUG("inavlid transfer id");
        return (USB_ERR_INVAL);
    }


    uint32_t ret_error = 0;

    usb_manager.vtbl.transfer_start(&usb_manager, tid, &ret_error);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    return (USB_ERR_OK);
}

/**
 * \brief   stops the execution of an existing USB transfer, this does not
 *          free up the allocated resources
 *
 * \param   tid the ID of the transfer to stop
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_stop(usb_xfer_id_t tid)
{
    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        return (USB_ERR_INVAL);
    }
    uint32_t ret_error = 0;

    usb_manager.vtbl.transfer_stop(&usb_manager, tid, &ret_error);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    return (USB_ERR_OK);
}

/**
 * \brief   handles the stall condition of a transfer and clears it
 *
 * \param   tid the ID of the stalled transfer
 *
 * \return  USB_ERR_OK on sucess
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_clear_stall(usb_xfer_id_t tid)
{
    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        return (USB_ERR_INVAL);
    }
    uint32_t ret_error = 0;

    usb_manager.vtbl.transfer_clear_stall(&usb_manager, tid, &ret_error);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    return (USB_ERR_OK);
}

/*
 * -------------------------------------------------------------------------
 * Functions for getting the transfer state and status
 * -------------------------------------------------------------------------
 */

/**
 * \brief   gets the state of the USB transfer
 *
 * \param   tid         the ID of the transfer to get the state
 * \param   ret_state   the returned state
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_get_state(usb_xfer_id_t tid, usb_tstate_t *ret_state)
{
    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        return (USB_ERR_INVAL);
    }
    uint32_t ret_error = 0;
    uint32_t idc_ret_state = 0;

    usb_manager.vtbl.transfer_state(&usb_manager, tid, &ret_error,
            &idc_ret_state);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    *ret_state = (usb_tstate_t) idc_ret_state;

    st->state = *ret_state;

    return (USB_ERR_OK);
}

/**
 * \brief   gets the status of the USB transfer
 *
 * \param   tid             the ID of the transfer to get the status
 * \param   ret_actlen      the actual length of the transfer processed
 * \param   ret_length      the total length of the transfer
 * \param   ret_actframes   the actual frames processed
 * \param   ret_numframes   the total frames of the transfer
 *
 * \return  USB_ERR_OK on success
 *          USB_ERR_XX on failure
 */
usb_error_t usb_transfer_get_status(usb_xfer_id_t tid, uint32_t *ret_actlen,
        uint32_t *ret_length, uint32_t *ret_actframes, uint32_t *ret_numframes)
{

    struct usb_xfer_state *st = usb_xfer_get_state(tid);
    if (st == NULL) {
        return (USB_ERR_INVAL);
    }

    uint32_t ret_error = 0;

    usb_manager.vtbl.transfer_status(&usb_manager, tid, &ret_error,
            ret_actlen, ret_length, ret_actframes, ret_numframes);

    if (((usb_error_t) ret_error) != USB_ERR_OK) {
        return ((usb_error_t) ret_error);
    }

    // XXX: store the status in the xfer_state struct?
    return (USB_ERR_OK);

}
