/**
 * \brief this file contains the USB error codes
 */
/*
 * Copyright (c) 2007-2013 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _USB_ERROR_H_
#define _USB_ERROR_H_


typedef enum {
	USB_ERR_OK 					= 0,
	USB_ERR_PENDING_REQUESTS,	/* 1 */
	USB_ERR_NOT_STARTED,		/* 2 */
	USB_ERR_INVAL,				/* 3 */
	USB_ERR_NOMEM,				/* 4 */
	USB_ERR_CANCELLED,			/* 5 */
	USB_ERR_BAD_ADDRESS,		/* 6 */
	USB_ERR_BAD_BUFSIZE,		/* 7 */
	USB_ERR_BAD_FLAG,			/* 8 */
	USB_ERR_NO_CALLBACK,		/* 9 */
	USB_ERR_IN_USE,				/* 10 */
	USB_ERR_NO_ADDR,			/* 11 */
	USB_ERR_NO_PIPE,			/* 12 */
	USB_ERR_ZERO_NFRAMES,		/* 13 */
	USB_ERR_ZERO_MAXP,			/* 14 */
	USB_ERR_SET_ADDR_FAILED,	/* 15 */
	USB_ERR_NO_POWER,			/* 16 */
	USB_ERR_TOO_DEEP,			/* 17 */
	USB_ERR_IOERROR,			/* 18 */
	USB_ERR_NOT_CONFIGURED,		/* 19 */
	USB_ERR_TIMEOUT,			/* 20 */
	USB_ERR_SHORT_XFER,			/* 21 */
	USB_ERR_STALLED,			/* 22 */
	USB_ERR_INTERRUPTED,		/* 23 */
	USB_ERR_DMA_LOAD_FAILED,	/* 24 */
	USB_ERR_BAD_CONTEXT,		/* 25 */
	USB_ERR_NO_ROOT_HUB,		/* 26 */
	USB_ERR_NO_INTR_THREAD,		/* 27 */
	USB_ERR_NOT_LOCKED,			/* 28 */
	USB_ERR_BAD_REQUEST,		/* 29 */
	USB_ERR_IDC,                /* 30 */
	USB_ERR_MAX
} usb_error_t;


const char* usb_get_error_string(usb_error_t errno);

#endif
