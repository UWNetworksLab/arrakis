/*
 * Copyright (c) 2007, 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef WEBSERVER_SESSION_H
#define WEBSERVER_SESSION_H


#include <nfs/nfs.h>

#include "webserver_debug.h"

enum http_state {
    HTTP_STATE_CLOSED,              // dead connection
    HTTP_STATE_NEW,                 // new connection
    HTTP_STATE_REQUEST,             // receiving request
    HTTP_STATE_SENDHEADER,          // sending reply header
    HTTP_STATE_SENDFILE,            // sending file
    HTTP_STATE_CLOSING,             // closing connection
    HTTP_STATE_BACKEND_BUSY         // backend busy
};


struct buff_holder {
    long                r_counter;   /* reference counter */
    void                *data;      /* cached data (file-contents) */
    size_t              len;        /* length of data */
};

struct http_conn {
    int                 request_no;
    enum http_state     state;
    /* request data and length (on heap) */
    char                *request;
    size_t              request_length;
    /* reply header, position and length (static) */
    const char          *header;
    size_t              header_pos, header_length;

    struct buff_holder  *hbuff;      /* reply buffer holder */
    size_t              reply_pos;  /* amount of data sent from reply */

    // Time taken to send reply
    uint64_t            start_ts;
    /* number of send retries */
    int                 retries;
    int                 error; /*flag for internal errors */
    char                *filename;     /* name of the requested file */
    struct tcp_pcb      *pcb;
    void (*callback) (struct http_conn *);
    int                 mark_invalid;     /* is it marked for delete? */
    long                ref_count;
    struct http_conn    *next;     /* for making the linked list */
};


err_t http_fetch_file(const char *name, struct http_conn *cs);
long decrement_http_conn_reference (struct http_conn *cs);
long increment_http_conn_reference (struct http_conn *cs);
#endif // WEBSERVER_SESSION_H
