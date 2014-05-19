/**
 * \file
 * \brief Framebuffer glue
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/fb_rpcclient_defs.h>

#include "demo.h"

static struct fb_rpc_client fb_client;
static bool init = false;

static void fb_bind_cb(void *st, errval_t err, struct fb_binding *b)
{
    assert(err_is_ok(err));

    err = fb_rpc_client_init(&fb_client, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in mem_rpc_client_init");
    }

    init = true;
}

static int fb_client_connect(void)
{
    iref_t iref;
    errval_t err;

    err = nameservice_blocking_lookup("framebuffer", &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameserver lookup");
    }

    err = fb_bind(iref, fb_bind_cb, NULL, get_default_waitset(),
                  IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "fb bind");
    }

    while(!init) {
        messages_wait_and_handle_next();
    }

    return 0;
}

void wait_for_vsync(void)
{
    errval_t err = fb_client.vtbl.vsync(&fb_client);
    assert(err_is_ok(err));
}

int main(int argc, char *argv[])
{
    errval_t err, ret;

    // Parse commandline
    if(argc < 4) {
        fprintf(stderr, "Usage: %s <xres> <yres> <bpp>\n",
                argv[0]);
        return EXIT_FAILURE;
    }

    int xres = atoi(argv[1]);
    int yres = atoi(argv[2]);
    int bpp = atoi(argv[3]);

    fb_client_connect();

    err = fb_client.vtbl.set_videomode(&fb_client, xres, yres, bpp, &ret);
    assert(err_is_ok(err));
    if(err_is_fail(ret)) {
        fprintf(stderr, "Error: failed to set video mode %dx%d %dbpp\n",
                xres, yres, bpp);
        return EXIT_FAILURE;
    }

    // Get and map framebuffer
    struct capref fbcap;
    uint32_t fboffset;
    err = fb_client.vtbl.get_framebuffer(&fb_client, &ret, &fbcap, &fboffset);
    assert(err_is_ok(err));
    assert(err_is_ok(ret));

    struct frame_identity fbid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(fbcap, &fbid);
    assert(err == 0);
    char *vidmem;
    err = vspace_map_one_frame((void**)&vidmem, 1ul << fbid.bits, fbcap,
                               NULL, NULL);
    assert(err_is_ok(err));

    vidmem += fboffset;

    vesa_demo(vidmem, xres, yres, bpp);

    return 0;
}
