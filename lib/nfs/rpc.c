/**
 * \file
 * \brief RPC implementation
 */

/*
 * Copyright (c) 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <lwip/pbuf.h>
#include <lwip/udp.h>
#include <assert.h>
#include <lwip/init.h>

#include <barrelfish/barrelfish.h>
#include <bench/bench.h>
#include <contmng/netbench.h>

// XXX: kludge making it possible to use bench_tsc without -lbench
#if defined(__i386__) || defined(__x86_64__)
bool rdtscp_flag;
#endif

#define FALSE   false
#define TRUE    true

#include <nfs/xdr.h>
#include "rpc.h"
#include "rpc_debug.h"
#include "xdr_pbuf.h"

/// RPC authentication flavour
enum rpc_auth_flavor {
    RPC_AUTH_NULL       = 0,
    RPC_AUTH_UNIX       = 1,
    RPC_AUTH_SHORT      = 2,
    RPC_AUTH_DES        = 3
};

/// RPC message type
enum rpc_msg_type {
    RPC_CALL  = 0,
    RPC_REPLY = 1
};

//#define RPC_TIMER_PERIOD (5000 * 1000) ///< Time between RPC timer firings (us)
#define RPC_TIMER_PERIOD (1000 * 1000) ///< Time between RPC timer firings (us)
#define RPC_RETRANSMIT_AFTER 8   ///< Number of timer firings before a retransmit
#define RPC_MAX_RETRANSMIT  60  ///< Max number of retransmissions before giving up

/* XXX: hardcoded data for authentication info */
#define AUTH_MACHINE_NAME       "barrelfish"
#define AUTH_MACHINE_NAME_LEN   10 /* XXX: must match string above */
#define AUTH_UID    0
#define AUTH_GID    0
#define AUTH_SIZE   (RNDUP(AUTH_MACHINE_NAME_LEN) + BYTES_PER_XDR_UNIT * 5)

/// bytes needed for full RPC call header
#define RPC_CALL_HEADER_LEN (10 * BYTES_PER_XDR_UNIT + AUTH_SIZE)

// XXX: lwip synchronisation kludges
extern struct thread_mutex *lwip_mutex;
extern struct waitset *lwip_waitset;

static uint8_t net_debug_state = 0;

static int hash_function(uint32_t xid)
{
    return (xid % RPC_HTABLE_SIZE);
}

/// Data for an outstanding (unreplied) RPC call
struct rpc_call {
    uint32_t xid;               ///< Transaction ID (XID)
    uint16_t timers, retries;   ///< Number of timer expiries and retries
    struct pbuf *pbuf;          ///< LWIP pbuf pointer for packet data
    rpc_callback_t callback;    ///< Callback function pointer
    void *cbarg1, *cbarg2;      ///< Callback function opaque arguments
    struct rpc_call *next;      ///< Next call in queue
};

/// Utility function to prepare an outgoing packet buffer with the RPC call header
static err_t rpc_call_init(XDR *xdr, uint32_t xid, uint32_t prog, uint32_t vers,
                           uint32_t proc)
{
    int32_t *buf;
    bool rb;

    /* reserve space for the first part of the header */
    if ((buf = XDR_INLINE(xdr, 9 * BYTES_PER_XDR_UNIT)) == NULL) {
        return ERR_BUF;
    }

    // XID comes first
    IXDR_PUT_UINT32(buf, xid);

    // message type: call
    IXDR_PUT_UINT32(buf, RPC_CALL);

    // RPC version
    IXDR_PUT_UINT32(buf, 2);

    // program number, version number, procedure number
    IXDR_PUT_UINT32(buf, prog);
    IXDR_PUT_UINT32(buf, vers);
    IXDR_PUT_UINT32(buf, proc);

    // CRED: auth_unix (hardcoded)
    IXDR_PUT_UINT32(buf, RPC_AUTH_UNIX);
    IXDR_PUT_UINT32(buf, AUTH_SIZE);
    IXDR_PUT_UINT32(buf, 0); // stamp

    /* here endeth the reserved space. now the machine name string */
    char *machname = AUTH_MACHINE_NAME;
    rb = xdr_string(xdr, &machname, AUTH_MACHINE_NAME_LEN);
    if (!rb) {
        return ERR_BUF;
    }

    /* reserve some more space for the rest, which is done inline again */
    if ((buf = XDR_INLINE(xdr, 5 * BYTES_PER_XDR_UNIT)) == NULL) {
        return ERR_BUF;
    }

    // Rest of the CRED
    IXDR_PUT_UINT32(buf, AUTH_UID);
    IXDR_PUT_UINT32(buf, AUTH_GID);
    IXDR_PUT_UINT32(buf, 0); // GIDs

    // VERF: auth_null
    IXDR_PUT_UINT32(buf, RPC_AUTH_NULL);
    IXDR_PUT_UINT32(buf, 0);

    return ERR_OK;
}

/// Utility function to skip over variable-sized authentication data in a reply
static err_t xdr_skip_auth(XDR *xdr)
{
    int32_t *buf;

    if ((buf = XDR_INLINE(xdr, 2 * BYTES_PER_XDR_UNIT)) == NULL) {
        return ERR_BUF;
    }

    (void) IXDR_GET_UINT32(buf); // skip auth flavour

    size_t auth_size = IXDR_GET_UINT32(buf);

    /* skip over auth data bytes */
    if (auth_size > 0) {
        buf = XDR_INLINE(xdr, auth_size);
        if (buf == NULL) {
            return ERR_BUF;
        }
    }

    return ERR_OK;
}

/// Generic handler for all incoming RPC messages. Finds the appropriate call
/// instance, checks arguments, and notifies the callback.
static void rpc_recv_handler(void *arg, struct udp_pcb *pcb, struct pbuf *pbuf,
                             struct ip_addr *addr, u16_t port)
{

//    uint64_t ts = rdtsc();
    uint32_t replystat, acceptstat;
    XDR xdr;
    err_t r;
    bool rb;

    struct rpc_client *client = arg;
    struct rpc_call *call = NULL;

    xdr_pbuf_create_recv(&xdr, pbuf);

    int32_t *buf;
    if ((buf = XDR_INLINE(&xdr, 3 * BYTES_PER_XDR_UNIT)) == NULL) {
        fprintf(stderr, "RPC: packet too small, dropped\n");
        goto out;
    }

    // XID comes first
    uint32_t xid = IXDR_GET_UINT32(buf);
    // message type
    uint32_t msgtype = IXDR_GET_UINT32(buf);
    if (msgtype != RPC_REPLY) {
        fprintf(stderr, "RPC: Received non-reply message, dropped\n");
        goto out;
    }
    RPC_DEBUGP("rpc_recv_call: RPC callback for xid %u x0%x\n", xid, xid);


    int hid = hash_function(xid);
    struct rpc_call *hash_list = client->call_hash[hid];
    // find matching call and dequeue it
    struct rpc_call *prev = NULL;
    for (call = hash_list; call && call->xid != xid; call = call->next){
        prev = call;
    }
    if (call == NULL) {
        fprintf(stderr, "RPC: Unknown XID 0x%" PRIx32 " in reply, dropped\n", xid);
/*        fprintf(stderr, "RPC:[%d:%s] Unknown XID 0x%x in reply, dropped\n",
                disp_get_domain_id(), disp_name(), xid);
*/
        goto out;
    } else if (prev == NULL) {
    	client->call_hash[hid] = call->next;
    } else {
        prev->next = call->next;
    }

    replystat = IXDR_GET_UINT32(buf);
    if (replystat == RPC_MSG_ACCEPTED) {
        r = xdr_skip_auth(&xdr);
        if (r != ERR_OK) {
            fprintf(stderr, "RPC: Error in incoming auth data, dropped\n");
            goto out;
        }

        rb = xdr_uint32_t(&xdr, &acceptstat);
        if (!rb) {
            fprintf(stderr, "RPC, Error decoding accept status, dropped\n");
            goto out;
        }
    } else {
        acceptstat = -1;
    }

//    lwip_record_event_simple(RPC_RECV_T, ts);
//    ts = rdtsc();
    call->callback(client, call->cbarg1, call->cbarg2, replystat, acceptstat,
                   &xdr);
//    lwip_record_event_simple(RPC_CALLBACK_T, ts);

out:
//    ts = rdtsc();
    pbuf_free(pbuf); // freeing the pbuf from RX packet
    if (call != NULL) {
        // We got reply, so there is not need for keeping TX packet saved
        // here for retransmission.  Lets free it up.
        pbuf_free(call->pbuf);
        free(call);
    }
//    lwip_record_event_simple(RPC_RECV_OUT_T, ts);
}

static void traverse_hash_bucket(int hid, struct rpc_client *client)
{
    struct rpc_call *call, *next, *prev = NULL;

    for (call = client->call_hash[hid]; call != NULL; call = next) {
        next = call->next;
        bool freed_call = false;
        if (++call->timers >= RPC_RETRANSMIT_AFTER) {
            if (call->retries++ == RPC_MAX_RETRANSMIT) {
                /* admit failure */
                printf("##### [%d][%"PRIuDOMAINID"] "
                       "RPC: timeout for XID 0x%"PRIu32"\n",
                       disp_get_core_id(), disp_get_domain_id(), call->xid);
                pbuf_free(call->pbuf);
                if (prev == NULL) {
                    client->call_hash[hid] = call->next;
                } else {
                    prev->next = call->next;
                }
                call->callback(client, call->cbarg1, call->cbarg2, -1, -1, NULL);
                free(call);
                freed_call = true;
            } else {
                /*
                if(net_debug_state == 0) {
                    net_debug_state = 1;
                    printf("starting the debug in network driver\n");
                    lwip_benchmark_control(0, BMS_START_REQUEST,
                            0, rdtsc());
                    lwip_benchmark_control(1, BMS_START_REQUEST,
                            0, rdtsc());
                } else {
                    printf("already started the debug in network driver\n");
                }
                */

                /* retransmit */
                printf("###### [%d][%"PRIuDOMAINID"] "
                       "RPC: retransmit XID 0x%"PRIu32"\n",
                       disp_get_core_id(), disp_get_domain_id(), call->xid);

                // throw away (hide) UDP/IP/ARP headers from previous transmission
                err_t e = pbuf_header(call->pbuf,
                                      -UDP_HLEN - IP_HLEN - PBUF_LINK_HLEN);
                assert(e == ERR_OK);

                e = udp_send(client->pcb, call->pbuf);
                if (e != ERR_OK) {
                    /* XXX: assume that this is a transient condition, retry */
                    fprintf(stderr, "RPC: retransmit failed! will retry...\n");
                    call->timers--;
                } else {
                    call->timers = 0;
                }
            }
        }
        if (!freed_call) {
            prev = call;
        }
    } /* end for: */
}

/// Timer callback: walk the queue of pending calls, and retransmit/expire them
static void rpc_timer(void *arg)
{
    struct rpc_client *client = arg;
    RPC_DEBUGP("rpc_timer fired\n");
    if (lwip_mutex != NULL) {
        thread_mutex_lock(lwip_mutex);
    }
    for (int i = 0; i < RPC_HTABLE_SIZE; ++i) {
    	if (client->call_hash[i] != NULL) {
            traverse_hash_bucket(i, client);
    	}
    }
    if (lwip_mutex != NULL) {
        thread_mutex_unlock(lwip_mutex);
    }
}


/**
 * \brief Initialise a new RPC client instance
 *
 * \param client Pointer to memory for RPC client data, to be initialised
 * \param server IP address of server to be called
 *
 * \returns Error code (ERR_OK on success)
 */
err_t rpc_init(struct rpc_client *client, struct ip_addr server)
{
    errval_t err;
    client->pcb = udp_new();
    if (client->pcb == NULL) {
        return ERR_MEM;
    }

    net_debug_state = 0;

    client->server = server;

    for (int i = 0; i < RPC_HTABLE_SIZE; ++i) {
    	client->call_hash[i] = NULL;
    }

    /* XXX: (very) pseudo-random number for initial XID */
    client->nextxid = (uint32_t)bench_tsc();

    RPC_DEBUGP("###### Initial sequence no. is %"PRIu32" 0x%"PRIx32"\n",
    		client->nextxid, client->nextxid);
    udp_recv(client->pcb, rpc_recv_handler, client);

    err = periodic_event_create(&client->timer, lwip_waitset,
                                RPC_TIMER_PERIOD, MKCLOSURE(rpc_timer, client));
    assert(err_is_ok(err));
    if (err_is_fail(err)) {
    	printf("rpc timer creation failed\n");
        udp_remove(client->pcb);
        return ERR_MEM;
    }
    RPC_DEBUGP("rpc timer created\n");

    return ERR_OK;
}



/**
 * \brief Initiate an RPC Call
 *
 * \param client RPC client, previously initialised by a call to rpc_init()
 * \param port UDP port on server to call
 * \param prog RPC program number
 * \param vers RPC program version
 * \param proc RPC procedure number
 * \param args_xdrproc XDR serialisation function for arguments to call
 * \param args Argument data to be passed to #args_xdrproc
 * \param args_size Upper bound on size of serialised argument data
 * \param callback Callback function to be invoked when call either completes or fails
 * \param cbarg1,cbarg2 Opaque arguments to be passed to callback function
 *
 * \returns Error code (ERR_OK on success)
 */
err_t rpc_call(struct rpc_client *client, uint16_t port, uint32_t prog,
               uint32_t vers, uint32_t proc, xdrproc_t args_xdrproc, void *args,
               size_t args_size, rpc_callback_t callback, void *cbarg1,
               void *cbarg2)
{

    uint64_t ts = rdtsc();
    XDR xdr;
    err_t r;
    bool rb;
    uint32_t xid;
    RPC_DEBUGP("rpc_call: started, trying to get a lock\n");
    if (lwip_mutex != NULL) {
        if(thread_mutex_trylock(lwip_mutex)) {
           printf("rpc_call: thread_mutex_trylock failed\n");
           abort();
        }
    }

    RPC_DEBUGP("rpc_call:  calling xdr_pbuf_create_send\n");
    rb = xdr_pbuf_create_send(&xdr, args_size + RPC_CALL_HEADER_LEN);
    if (!rb) {
        return ERR_MEM;
    }

    xid = client->nextxid++;

    RPC_DEBUGP("rpc_call: calling rpc_call_init\n");
    r = rpc_call_init(&xdr, xid, prog, vers, proc);
    if (r != ERR_OK) {
        XDR_DESTROY(&xdr);
        return r;
    }
    RPC_DEBUGP("rpc_call: rpc_call_init done\n");

    rb = args_xdrproc(&xdr, args);
    if (!rb) {
        XDR_DESTROY(&xdr);
        return ERR_BUF;
    }

    struct rpc_call *call = malloc(sizeof(struct rpc_call));
    if (call == NULL) {
        XDR_DESTROY(&xdr);
        return ERR_MEM;
    }
    call->xid = xid;
    call->retries = call->timers = 0;
    call->pbuf = (struct pbuf *)xdr.x_private;
    call->callback = callback;
    call->cbarg1 = cbarg1;
    call->cbarg2 = cbarg2;
    call->next = NULL;

    RPC_DEBUGP("rpc_call: RPC call for xid %u x0%x\n", xid, xid);
    /* XXX: fix size on pbuf in case the buffer was too big */
    if (((struct pbuf *)xdr.x_base)->len > xdr.x_handy) {
        /* FIXME: intermediate pbufs will have the wrong tot_len */
        call->pbuf->tot_len -= ((struct pbuf *)xdr.x_base)->len - xdr.x_handy;
        ((struct pbuf *)xdr.x_base)->len = xdr.x_handy;
    }
    RPC_DEBUGP("rpc_call: calling UPD_connect\n");
    r = udp_connect(client->pcb, &client->server, port);
    if (r != ERR_OK) {
        XDR_DESTROY(&xdr);
        free(call);
        return r;
    }

    /* enqueue */
    int hid = hash_function(xid);
    call->next = client->call_hash[hid];
    client->call_hash[hid] = call;

    RPC_DEBUGP("rpc_call: calling UPD_send\n");
    r = udp_send(client->pcb, call->pbuf);
    if (r != ERR_OK) {
        /* dequeue */
        assert(client->call_hash[hid] == call);
        client->call_hash[hid] = call->next;
        /* destroy */
        XDR_DESTROY(&xdr);
        free(call);
    }

    RPC_DEBUGP("rpc_call: rpc_call done\n");
    lwip_record_event_simple(RPC_CALL_T, ts);
    return r;
}

/// Destroy the given client, freeing any associated memory
void rpc_destroy(struct rpc_client *client)
{
    periodic_event_cancel(&client->timer);

    /* go through list of pending requests and free them */
    struct rpc_call *call, *next;
    for(int i = 0; i < RPC_HTABLE_SIZE; ++i) {
        for (call = client->call_hash[i]; call != NULL; call = next) {
            pbuf_free(call->pbuf);
            next = call->next;
            free(call);
        }
        client->call_hash[i] = NULL;
    }

    udp_remove(client->pcb);
}
