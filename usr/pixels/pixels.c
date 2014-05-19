/**
 * \file
 * \brief Barrelfish trace server
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <trace/trace.h>

#include <if/pixels_defs.h>

/// Called when servers setup their services
static void _listening(struct pixels_service *_st, iref_t iref)
{
    char service[16];

    /* Register service */
    struct chips_context *context = chips_get_context();
    printf("pixels.%d listening!\n", disp_get_core_id());

    sprintf(service, "pixels.%d", disp_get_core_id());

    context->register_service(service, iref, NULL, NULL);
    printf("Registered %s\n", service);
}

static void pixels_display(struct pixels_service_response *cl, uint64_t arg)
{
    uint64_t now = rdtsc();
    //    printf("display %lx\n", arg);
    while (rdtsc() - now < arg) ;
    cl->f->ack(cl);
}

int main(int argc, char**argv)
{
    errval_t err;

    printf("%.*s running on core %d\n", DISP_NAME_LEN, disp_name(),
           disp_get_core_id());

    /* Setup a server */
    static struct pixels_server_call_vtbl call_vtbl = {
        ._listening  = _listening,
        ._disconnect = NULL,
        ._connected  = NULL,
        
        .display = pixels_display,
    };
    static struct pixels_service service = {
        .f = &call_vtbl,
    };
    err = pixels_listen(&service);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to setup a server");
        exit(EXIT_FAILURE);
    }

    messages_handler_loop();

    return 0;
}

