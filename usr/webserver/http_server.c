/**
 * \file
 * \brief HTTP server
 *
 * \bug All calls to tcp_write currently use TCP_WRITE_FLAG_COPY, causing
 *   the data to be copied to LWIP's internal memory pool. This is necessary,
 *   because we lack the VM support necessary to do a reverse mapping for
 *   arbitrary memory regions.
 */

/*
 * Copyright (c) 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <lwip/tcp.h>
#include <lwip/init.h>
#include <contmng/netbench.h>

#include "http_cache.h"
#include "webserver_network.h"
#include "webserver_debug.h"
#include "webserver_session.h"

#define HTTP_PORT       80

#define CRLF "\r\n"
#define HTTP_HEADER_COMMON "Server: Barrelfish" CRLF
#define HTTP_HEADER_200 "HTTP/1.0 200 OK" CRLF HTTP_HEADER_COMMON
#define HTTP_HEADER_404 "HTTP/1.0 404 Not Found" CRLF HTTP_HEADER_COMMON
#define HTTP_HEADER_500 "HTTP/1.0 500 Internal Server Error" CRLF HTTP_HEADER_COMMON

#define HTTP_MIME_HTML  "Content-type: text/html; charset=utf-8" CRLF
#define HTTP_MIME_GIF   "Content-type: image/gif" CRLF
#define HTTP_MIME_JPG   "Content-type: image/jpeg" CRLF
#define HTTP_MIME_PDF   "Content-type: application/pdf" CRLF
#define HTTP_MIME_TAR   "Content-type: application/x-tar" CRLF
#define HTTP_MIME_GZIP  "Content-type: application/x-gzip" CRLF
#define HTTP_MIME_BZIP2 "Content-type: application/x-bzip2" CRLF
#define HTTP_MIME_OCTET "Content-type: application/octet-stream" CRLF

#define MIN(a,b)        ((a) < (b) ? (a) : (b))



static const char notfound_reply[] =
    HTTP_HEADER_404 HTTP_MIME_HTML CRLF
    "<html>" CRLF
    "<body>" CRLF
    "<h1>404 Not Found</h1>" CRLF
    "<p>The requested URL was not found.</p>" CRLF
    "</body>" CRLF
    "</html>" CRLF;

static const char error_reply[] =
    HTTP_HEADER_500 HTTP_MIME_HTML CRLF
    "<html>" CRLF
    "<body>" CRLF
    "<h1>500 Internal Server Error</h1>" CRLF
    "<p>Bad stuff happened. Damn.</p>" CRLF
    "</body>" CRLF
    "</html>" CRLF;

static const char header_html[] = HTTP_HEADER_200 HTTP_MIME_HTML CRLF;
static const char header_gif[] = HTTP_HEADER_200 HTTP_MIME_GIF CRLF;
static const char header_jpg[] = HTTP_HEADER_200 HTTP_MIME_JPG CRLF;
static const char header_pdf[] = HTTP_HEADER_200 HTTP_MIME_PDF CRLF;
static const char header_bz2[] = HTTP_HEADER_200 HTTP_MIME_BZIP2 CRLF;
static const char header_gz[] = HTTP_HEADER_200 HTTP_MIME_GZIP CRLF;
static const char header_octet[] = HTTP_HEADER_200 HTTP_MIME_OCTET CRLF;

#if 0

#define MAX_DURATIONS   1000

#define INST_BEGIN \
    static uint64_t _dursum = 0, _dur = 0;      \
    uint64_t _begin = rdtsc();

#define INST_END \
    _dursum += rdtsc() - _begin;                \
    _dur++;                                                     \
    if(_dur == MAX_DURATIONS) {                                 \
        DEBUGPRINT("%s: %lu\n", __func__, _dursum / MAX_DURATIONS);     \
        _dur = 0; _dursum = 0;                                      \
    }

#else

#define INST_BEGIN
#define INST_END

#endif

/* GLOBAL STATE */
static int parallel_connections = 0; /* number of connections alive at moment */
static int request_counter = 0;  /* Total no. of requests received till now */
/* above both are for debugging purpose only */



static struct http_conn *http_conn_new(void)
{
    struct http_conn *newconn = malloc(sizeof(struct http_conn));
    assert (newconn != NULL);
    memset (newconn, 0, sizeof(struct http_conn));

    newconn->state = HTTP_STATE_NEW;
    newconn->request_no = request_counter++;

    DEBUGPRINT ("%d: http_conn created [p %d] %lu %lu\n", newconn->request_no,
        parallel_connections, newconn->header_pos, newconn->header_length);
    ++parallel_connections;
    return newconn;
}


static void http_conn_free(struct http_conn *conn)
{
    DEBUGPRINT ("%d: http_conn_free freeing [p %d]\n", conn->request_no,
        parallel_connections);

    if(conn->request != NULL) {
        free(conn->request);
    }
    /* decrementing the reference to buff_holder */
    decrement_buff_holder_ref (conn->hbuff);
    free(conn);
    --parallel_connections;
}

/* increments the reference counter and returns the incremented value */
long increment_http_conn_reference (struct http_conn *cs)
{
    ++cs->ref_count;
    return (cs->ref_count);
} /* end function: increment_http_conn_reference  */

/* This function decrements the references to http_conn
 * and if references reach 0, the memory of struct is released. */
long decrement_http_conn_reference (struct http_conn *cs)
{
    --cs->ref_count;
    if (cs->mark_invalid) {
        /* connection is no longer valid */
        if (cs->ref_count <= 0) {
            /* no one is using the connection */
            /* so, free up the the memory */
            http_conn_free(cs);
            return 0;
        }
    } /* end if : invalid http_conn */
    return cs->ref_count;
} /* end function: decrement_reference */


static void http_conn_invalidate (struct http_conn *conn)
{
    DEBUGPRINT ("%d: http_conn_invalidate\n", conn->request_no);
    conn->mark_invalid = 1;
    decrement_http_conn_reference (conn);
}


static void http_server_err(void *arg, err_t err)
{
    struct http_conn *conn = arg;

    DEBUGPRINT("http_server_err! %p %d\n", arg, err);
    if(conn != NULL) {
        DEBUGPRINT("%d: http_server_err! %p %d\n", conn->request_no, arg, err);
        http_conn_invalidate (conn);
    } else {
        DEBUGPRINT("http_server_err! %p %d\n", arg, err);
    }
}


static void http_server_close(struct tcp_pcb *tpcb, struct http_conn *cs)
{
/*
    printf("%s %s %s %hu.%hu.%hu.%hu in %"PU"\n",
            cs->hbuff->data ? "200" : "404", cs->request, cs->filename,
           ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
           ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip),
            in_seconds(get_time_delta(&cs->start_ts)));
*/
    DEBUGPRINT("%d: http_server_close freeing the connection\n",
        cs->request_no);

    // replace TCP callbacks with NULL
    tcp_arg(tpcb, NULL);
    tcp_sent(tpcb, NULL);
    tcp_recv(tpcb, NULL);
    if (cs != NULL) {
        http_conn_invalidate (cs);
    }
    tcp_close(tpcb);
}

static err_t trysend(struct tcp_pcb *t, const void *data, size_t *len, bool
more)
{
    size_t sendlen = MIN(*len, tcp_sndbuf(t));
    err_t err;

    do {
        err = tcp_write(t, data, sendlen,
                        TCP_WRITE_FLAG_COPY | (more ? TCP_WRITE_FLAG_MORE : 0));
        if (err == ERR_MEM) {
            sendlen /= 2;
            more = true;
        }
    } while (err == ERR_MEM && sendlen > 1);

    if (err == ERR_OK) {
        *len = sendlen;
    }

    return err;
}

static void http_send_data(struct tcp_pcb *tpcb, struct http_conn *conn)
{
    err_t err;
    const void *data;
    size_t len;

    switch (conn->state) {
    case HTTP_STATE_SENDHEADER:
        DEBUGPRINT ("%d: http_send_data: header_pos %lu < header_len %lu\n",
            conn->request_no, conn->header_pos, conn->header_length);
        assert(conn->header_pos < conn->header_length);
        data = &conn->header[conn->header_pos];
        len = conn->header_length - conn->header_pos;
        err = trysend(tpcb, data, &len, (conn->hbuff->data != NULL));
        if (err != ERR_OK) {
            DEBUGPRINT("http_send_data(): Error %d sending header\n", err);
            return; // will retry
        }

        conn->header_pos += len;
        DEBUGPRINT ("%d: http_send_data incr: hdr_pos %lu < hdr_len %lu\n",
                conn->request_no, conn->header_pos, conn->header_length);
        if (conn->header_pos == conn->header_length) {
            conn->state = HTTP_STATE_SENDFILE; // fall through below
        } else {
            break;
        }

    case HTTP_STATE_SENDFILE:
        if (conn->hbuff->data == NULL) {
            conn->state = HTTP_STATE_CLOSING;
            break;
        }
        data = conn->hbuff->data +conn->reply_pos; /* pointer arithmatic */
        len = conn->hbuff->len - conn->reply_pos;
        err = trysend(tpcb, data, &len, false);
        if (err != ERR_OK) {
            DEBUGPRINT("http_send_data(): Error %d sending payload\n", err);
            return; // will retry
        }
        conn->reply_pos += len;
        if (conn->reply_pos == conn->hbuff->len) {
            conn->state = HTTP_STATE_CLOSING;
        }
        break;

    default:
        DEBUGPRINT ("http_send_data(): Wrong state! (%d)\n", conn->state);
        break;
    }
}

/* This function is called periodically from TCP.
 * and is also responsible for taking care of stale connections.
**/
static err_t http_poll(void *arg, struct tcp_pcb *tpcb)
{
    struct http_conn *conn = arg;

    if (conn == NULL && tpcb->state == ESTABLISHED) {
        tcp_abort(tpcb);
        return ERR_ABRT;
    } else if (conn != NULL && (conn->state == HTTP_STATE_SENDHEADER
                                || conn->state == HTTP_STATE_SENDFILE)) {
        if (++conn->retries == 4) {
            tcp_arg(tpcb, NULL);
            DEBUGPRINT ("connection closed, tried too hard\n");
            http_conn_invalidate (conn);
            tcp_abort(tpcb);
            return ERR_ABRT;
        }
        http_send_data(tpcb, conn);
        if (conn->state == HTTP_STATE_CLOSING) {
            DEBUGPRINT ("%d: http_poll closing the connection\n",
                    conn->request_no);
            http_server_close(tpcb, conn);
        } else {
            tcp_output(tpcb);
        }
    } else if (conn != NULL && (conn->state == HTTP_STATE_NEW
                                || conn->state == HTTP_STATE_REQUEST)) {
        /* abort connections that sit open for too long without sending a
request */
        if (++conn->retries == 60) {
            DEBUGPRINT("connection in state %d too long, aborted\n",
                         conn->state);
            DEBUGPRINT("connection in state %d too long, aborted\n",
                        conn->state);

            tcp_arg(tpcb, NULL);
            http_conn_invalidate (conn);
            tcp_abort(tpcb);
            return ERR_ABRT;
        }
    }
    return ERR_OK;
} /* end function: http_poll */

/* called when data is successfully sent */
static err_t http_server_sent(void *arg, struct tcp_pcb *tpcb, u16_t length)
{
    struct http_conn *conn = arg;

    if(conn == NULL) {
        return ERR_OK;
    }

    conn->retries = 0;

    switch(conn->state) {
    case HTTP_STATE_SENDHEADER:
    case HTTP_STATE_SENDFILE:
        // Need to send more data?
        http_send_data(tpcb, conn);
        if (conn->state != HTTP_STATE_CLOSING) {
            tcp_output(tpcb);
            break;
        }

    case HTTP_STATE_CLOSING:
        DEBUGPRINT("%d: http_server_sent closing the connection\n",
                    conn->request_no);
        http_server_close(tpcb, conn);
        break;

    default:
        DEBUGPRINT("http_server_sent(): Wrong state! (%d)\n", conn->state);
        break;
    }

    return ERR_OK;
}

static const void *make_header(const char *uri, size_t *retlen)
{
    /* FIXME: hack to guess MIME type */
    size_t urilen = strlen(uri);
    if (strcmp(uri + urilen - 5, ".html") == 0) {
        *retlen = sizeof(header_html) - 1; // -1 for '\0'
        return header_html;
    } else if (strcmp(uri + urilen - 4, ".gif") == 0) {
        *retlen = sizeof(header_gif) - 1;
        return header_gif;
    } else if (strcmp(uri + urilen - 4, ".jpg") == 0) {
        *retlen = sizeof(header_jpg) - 1;
        return header_jpg;
    } else if (strcmp(uri + urilen - 4, ".pdf") == 0) {
        *retlen = sizeof(header_pdf) - 1;
        return header_pdf;
    } else if (strcmp(uri + urilen - 4, ".bz2") == 0) {
        *retlen = sizeof(header_bz2) - 1;
        return header_bz2;
    } else if (strcmp(uri + urilen - 3, ".gz") == 0) {
        *retlen = sizeof(header_gz) - 1;
        return header_gz;
    } else {
        *retlen = sizeof(header_octet) - 1;
        return header_octet;
    }
}

/* callback function to fetch file
    This function is responsible for sending the fetched file */
static void send_response(struct http_conn *cs)
{

    if (cs->error) {
        DEBUGPRINT ("%d: BIGERROR Sending the response back of size %lu\n",
					cs->request_no, cs->reply_pos);
        DEBUGPRINT("%s %s %s %hu.%hu.%hu.%hu\n", "500",
               cs->request, cs->filename,
               ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
               ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip));

        cs->header = error_reply;
        cs->header_length = sizeof(error_reply) - 1;

    } else {
        DEBUGPRINT ("%d: Sending the response back of size %lu\n",
                cs->request_no, cs->reply_pos);
        DEBUGPRINT("%s %s %s %hu.%hu.%hu.%hu\n", cs->hbuff->data ?
                "200" : "404", cs->request, cs->filename,
               ip4_addr1(&cs->pcb->remote_ip), ip4_addr2(&cs->pcb->remote_ip),
               ip4_addr3(&cs->pcb->remote_ip), ip4_addr4(&cs->pcb->remote_ip));

        if (cs->hbuff->data == NULL) {
            /* not found, send 404 */
            DEBUGPRINT ("%d: making 404 case\n",cs->request_no);
            DEBUGPRINT ("witness: header_pos %lu < header_len %lu\n",
                cs->header_pos, cs->header_length);

            cs->header = notfound_reply;
            cs->header_length = sizeof(notfound_reply) - 1;
        } else {
            /* found, send static header */
            cs->header = make_header(cs->filename, &cs->header_length);
        }
    } /* end else: internal error */

    /* send data */
    cs->state = HTTP_STATE_SENDHEADER;
    cs->retries = 0;
    http_send_data(cs->pcb, cs);

    /* did we send the whole page? */
    if (cs->state == HTTP_STATE_CLOSING) {
        DEBUGPRINT("%d: send_response closing the connection\n",
                cs->request_no);
        http_server_close(cs->pcb, cs);
    } else {
        tcp_sent(cs->pcb, http_server_sent);
        tcp_output(cs->pcb);
    }
} /* end function: send_response */

static err_t http_server_recv(void *arg, struct tcp_pcb *tpcb, struct pbuf *p,
                              err_t err)
{
    struct http_conn *conn = arg;

    DEBUGPRINT("%d, http_server_recv called\n", conn->request_no);
    if (err != ERR_OK) {
        DEBUGPRINT("http_server_recv called with err %d\n", err);
        return ERR_OK;
    }

    // check if connection closed
    if(conn == NULL) {
        return ERR_OK;
    } else if (p == NULL) {
        DEBUGPRINT("%d, closing from http_server_recv\n", conn->request_no);
        http_server_close(tpcb, conn);
        return ERR_OK;
    }

    switch(conn->state) {
    case HTTP_STATE_NEW:
        conn->state = HTTP_STATE_REQUEST;
        // Fall through...

    case HTTP_STATE_REQUEST:
        /* don't send an immediate ack here, do it later with the data */
        tpcb->flags &= ~(TF_ACK_DELAY | TF_ACK_NOW);

        /* accumulate the request data */
        conn->request_length += p->tot_len;
        conn->request = realloc(conn->request, conn->request_length + 1);
        char *d = conn->request + conn->request_length - p->tot_len;

        for(struct pbuf *pb = p; pb != NULL; pb = pb->next) {
            memcpy(d, pb->payload, pb->len);
            tcp_recved(tpcb, pb->len);
            d += pb->len;
        }
        *d = '\0';

        pbuf_free(p);

        // have we seen the end of the request yet?
        if (strstr(conn->request, CRLF CRLF) == NULL) {
            break;
        }

        // ignore everything after the first line
        char *cp = strstr(conn->request, CRLF);
        assert(cp != NULL);
        *cp = '\0';

        // Parse request: break into method and URI
        cp = strchr(conn->request, ' ');
        if (cp == NULL) {
            goto invalid;
        }
        *cp = '\0';
        const char *uri = cp + 1;
        cp = strrchr(uri, ' ');
        if (cp == NULL) {
            goto invalid;
        }
        *cp = '\0';

        if (strcmp(conn->request, "GET") != 0) {
            goto invalid;
        }

        // drop a leading /
        if (uri[0] == '/') {
            uri++;
        }

        // if URI is now empty, look for index.html
        if (uri[0] == '\0') {
            uri = "index.html";
        }

        conn->filename = (char *)uri;
        conn->callback = send_response;
        conn->pcb = tpcb;
        conn->start_ts = rdtsc();
        /* for callback execution */
        err_t e = http_cache_lookup(uri, conn);
        if (e != ERR_OK) {
            conn->error = 1;
            send_response(conn);
        }
        break;

    default:
        DEBUGPRINT("http_server_recv(): data received in wrong state (%d)!\n",
                     conn->state);
        pbuf_free(p);
        conn->error = 1;
        send_response(conn);
        break;
    }
    return ERR_OK;

invalid:
    DEBUGPRINT("invalid request: %s\n", conn->request);
    DEBUGPRINT("%d: invalid request: %s\n",conn->request_no, conn->request);
    conn->state = HTTP_STATE_CLOSING;
    http_server_close(tpcb, conn);
    return ERR_OK;
}

static err_t http_server_accept(void *arg, struct tcp_pcb *tpcb, err_t err)
{
#if TCP_LISTEN_BACKLOG
    /* Decrease the listen backlog counter */
    struct tcp_pcb_listen *lpcb = (struct tcp_pcb_listen*)arg;
    tcp_accepted(lpcb);
#endif

    tcp_setprio(tpcb, TCP_PRIO_MIN);

    struct http_conn *conn = http_conn_new();
    if (conn == NULL) {
        DEBUGPRINT("http_accept: Out of memory\n");
        return ERR_MEM;
    }
    DEBUGPRINT("accpet called: %s\n", conn->request);
    increment_http_conn_reference (conn);
    /* NOTE: This initial increment marks the basic assess and it will be
        decremented by http_server_invalidate */

    tcp_arg(tpcb, conn);

    tcp_recv(tpcb, http_server_recv);
    tcp_err(tpcb, http_server_err);
    tcp_poll(tpcb, http_poll, 4);

    return ERR_OK;
}


static void realinit(void)
{

    uint64_t ts = rdtsc();
    struct tcp_pcb *pcb = tcp_new();
//    err_t e = tcp_bind(pcb, IP_ADDR_ANY, (HTTP_PORT + disp_get_core_id()));
    err_t e = tcp_bind(pcb, IP_ADDR_ANY, HTTP_PORT);
    assert(e == ERR_OK);
    pcb = tcp_listen(pcb);
    assert(pcb != NULL);
    tcp_arg(pcb, pcb);
    tcp_accept(pcb, http_server_accept);
    printf("HTTP setup time %"PU"\n", in_seconds(get_time_delta(&ts)));
    printf("#######################################################\n");
    printf("Starting webserver\n");
    printf("#################### Starting webserver ##############\n");
    printf("#######################################################\n");

}

void http_server_init(struct ip_addr server, const char *path)
{
    http_cache_init(server, path, realinit);
}


uint64_t get_time_delta(uint64_t *l_ts)
{
    uint64_t ct = rdtsc();
    uint64_t delta = ct - *l_ts;
    *l_ts = ct;
    return delta;
    //  return delta / (2800 * 1000);
} // end function: get_time_delta

