/**
 * \file
 * \brief Domain management
 *
 * \bug This is misnamed. It is "local kernel notification
 * management", part of which is domain management.
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/core_state.h>
#include <barrelfish/dispatch.h>
#include "monitor.h"
#include <if/mem_rpcclient_defs.h>

#ifndef __arm__
static errval_t reclaim_memory(genpaddr_t base, uint8_t bits)
{
    /* XXX: mem client is only defined for the bsp core.
     * For app cores, just return */
    if (get_mem_client() == NULL) {
        return SYS_ERR_OK;
    }

    // Fabricate new RAM cap and hand back to mem_serv
    struct capability c = {
        .type = ObjType_RAM,
        .u.ram = {
            .base = base,
            .bits = bits,
        }
    };
    struct capref ramcap;
    errval_t err = slot_alloc(&ramcap);
    if(err_is_fail(err)) {
        return err;
    }

    err = monitor_cap_create(ramcap, &c, disp_get_core_id());
    if(err_is_fail(err)) {
        return err;
    }

    struct ram_alloc_state *ram_alloc_state = get_ram_alloc_state();
    errval_t result;
    thread_mutex_lock(&ram_alloc_state->ram_alloc_lock);
    struct mem_rpc_client *b = get_mem_client();
    // XXX: This should not be an RPC! It could stall the monitor, but
    // we trust mem_serv for the moment.
    err = b->vtbl.free_monitor(b, ramcap, base, bits, &result);
    thread_mutex_unlock(&ram_alloc_state->ram_alloc_lock);
    if(err_is_fail(err)) {
        return err;
    }
    if(err_is_fail(result)) {
        return result;
    }

    return cap_destroy(ramcap);
}
#endif

static void handle_notification(void *arg)
{
    struct lmp_endpoint *ep = arg;
    errval_t err;

    do { // consume messages
        struct lmp_recv_msg msg = LMP_RECV_MSG_INIT;
        err = lmp_endpoint_recv(ep, &msg.buf, NULL);

        if (err_is_ok(err)) {
            if(msg.buf.msglen == 1) {
                domainid_t domid = msg.words[0];

                // XXX: This is done by spawnd now
                if (domid != 0) {
                    debug_printf("Dispatcher with domain ID %"PRIuDOMAINID" exited\n",
                                 domid);
                }
            } else if(msg.buf.msglen == sizeof(struct RAM) / sizeof(uintptr_t) + 1) {
#ifndef __arm__
                //defined(__x86_64__) || defined(__i386__)
                union rammsg {
                    uintptr_t msgwords[LMP_MSG_LENGTH];
                    struct RAM ram;
                } *u;
                u = (union rammsg *)&msg.words;

                /* printf("%s.%d: RAM cap deleted, base = %" PRIxGENPADDR ", bits = %u\n", */
                /*        disp_name(), disp_get_core_id(), ram->base, ram->bits); */

                err = reclaim_memory(u->ram.base, u->ram.bits);
                if(err_is_fail(err)) {
                    DEBUG_ERR(err, "reclaim_memory");
                }
#else
                /* XXX: Disabling memory reclamation on ARM. I
                 * couldn't get the compiler to accept the above code
                 * due to strict aliasing restrictions. I do believe
                 * though that the above is according to the C99
                 * spec. Please help fix it, so that it can be
                 * enabled.
                 */
#endif
            } else {
                printf("%s: Unknown kernel notification of length %zu received\n",
                       disp_name(), msg.buf.msglen);
            }
        } else if (err_no(err) != LIB_ERR_NO_LMP_MSG) {
            DEBUG_ERR(err, "unexpected error from lmp_endpoint_recv");
        }
    } while(err_is_ok(err));

    // re-register
    struct event_closure cl = {
        .handler = handle_notification,
        .arg = arg,
    };
    err = lmp_endpoint_register(ep, get_default_waitset(), cl);
    assert(err_is_ok(err));
}

void domain_mgmt_init(void)
{
    errval_t err;

    /* Register notification endpoint with kernel */
    struct capref epcap;
    struct lmp_endpoint *notifyep;
    // XXX: This has to be huge so we can receive a batch of
    // notifications when deleting CNodes recursively.
    err = endpoint_create(100 * 12, &epcap, &notifyep);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed creating endpoint");
    }

    // register to receive on this endpoint
    struct event_closure cl = {
        .handler = handle_notification,
        .arg = notifyep,
    };
    err = lmp_endpoint_register(notifyep, get_default_waitset(), cl);
    assert(err_is_ok(err));

    err = invoke_monitor_register(epcap);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "Could not register with kernel");
    }
}
