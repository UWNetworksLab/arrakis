/**
 * \file
 * \brief Framebuffer glue
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <if/fb_rpcclient_defs.h>
#include <if/keyboard_defs.h>
#include <barrelfish/nameservice_client.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>

#include "slideshow.h"

static struct fb_rpc_client fb_client;
static struct keyboard_binding *kb_client;

static char *vidmem;    /// Pointer to video memory

// original video mode
static uint16_t origmode;
static bool origlinear;
static char fontbackup[65536];

void wait_for_vsync(void)
{
    errval_t err = fb_client.vtbl.vsync(&fb_client);
    assert(err_is_ok(err));
}

void quit(void)
{
    errval_t err, ret;

    // Restore font backup
    memcpy(vidmem, fontbackup, 65536);

    err = fb_client.vtbl.set_vesamode(&fb_client, origmode, origlinear,
                                      true, &ret);
    assert(err_is_ok(err));
    assert(err_is_ok(ret));
    exit(0);
}

static struct keyboard_rx_vtbl keyboard_rx_vtbl = {
    .key_event = keyboard_key_event,
};

static void keyboard_connected_callback(void *st, errval_t err,
                                        struct keyboard_binding *cl)
{
    assert(err_is_ok(err));
    cl->rx_vtbl = keyboard_rx_vtbl;
    kb_client = cl;
}

static void fb_connected_callback(void *st, errval_t err, struct fb_binding *b)
{
    assert(err_is_ok(err));

    err = fb_rpc_client_init(&fb_client, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error in mem_rpc_client_init");
    }
}

static void start_keyboard_client(void)
{
    iref_t iref;
    errval_t err;

    err = nameservice_blocking_lookup("keyboard", &iref);
    if (err_is_fail(err)) {
        fprintf(stderr, "slideshow: could not connect to the keyboard driver.\n");
        abort();
    }
    assert(iref != 0);

    err = keyboard_bind(iref, keyboard_connected_callback, NULL,
                        get_default_waitset(), IDC_BIND_FLAGS_DEFAULT);
    assert(err_is_ok(err));
}

static int fb_client_connect(void)
{
    iref_t iref;
    errval_t err;

    err = nameservice_blocking_lookup("framebuffer", &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not lookup IREF for framebuffer driver");
        abort();
    }

    err = fb_bind(iref, fb_connected_callback, NULL, get_default_waitset(),
                  IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "could not connect to framebuffer driver");
        abort();
    }

    return 0;
}

static int keyboard_client_connect(void)
{
    start_keyboard_client();
    while (kb_client == NULL) {
        messages_wait_and_handle_next();
    }
    return 0;
}

static int cmpstringp(const void *p1, const void *p2)
{
    return strcmp(*(char * const *)p1, *(char * const *)p2);
}

static void load_slides(const char *indir)
{
    errval_t err;

    // Make dir relative to '/'
    char *dir = vfs_path_mkabsolute("/", indir);
    assert(dir != NULL);

    vfs_handle_t dh;
    err = vfs_opendir(dir, &dh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error opening directory\n");
    }

    char *paths[MAX_SLIDES] = { NULL };
    for(;;) {
        struct vfs_fileinfo info;
        char *name;

        err = vfs_dir_read_next(dh, &name, &info);
        if (err_is_fail(err)) {
            if (err_no(err) == FS_ERR_INDEX_BOUNDS) {
                break;
            } else {
                USER_PANIC_ERR(err, "error reading directory\n");
            }
        }

        if (info.type == VFS_DIRECTORY) {
            printf("Skipped %s", name);
            free(name);
            continue;
        }

        paths[nslides] = vfs_path_mkabsolute(dir, name);
        assert(paths[nslides] != NULL);
        free(name);

        if (++nslides == MAX_SLIDES) {
            printf("slideshow: reached MAX_SLIDES (%d)\n", MAX_SLIDES);
            break;
        }
    }

    // Sort filenames
    qsort(paths, nslides, sizeof(char *), cmpstringp);

    for(int i = 0; i < nslides; i++) {
        printf("Opening %s\n", paths[i]);

        vfs_handle_t fh;
        err = vfs_open(paths[i], &fh);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to open '%s'\n", paths[i]);
        }

        struct vfs_fileinfo info;
        err = vfs_stat(fh, &info);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to stat '%s'\n", paths[i]);
        }

        void *buf = malloc(info.size);
        assert(buf != NULL);

        size_t len;
        err = vfs_read(fh, buf, info.size, &len);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "vfs_read '%s' failed\n", paths[i]);
        } else if (len != info.size) {
            USER_PANIC("short read %lu != %lu\n", len, info.size);
        }

        vfs_close(fh);
        free(paths[i]);

        slide[i] = buf;
        slide_length[i] = info.size;
    }

    vfs_closedir(dh);
    free(dir);
}

int main(int argc, char *argv[])
{
    int xres, yres, bpp;
    errval_t err, ret;

    // Parse commandline
    if(argc < 5) {
        fprintf(stderr, "Usage: %s <xres> <yres> <bpp> <slides path>\n",
                argv[0]);
        return EXIT_FAILURE;
    }

    xres = atoi(argv[1]);
    yres = atoi(argv[2]);
    bpp = atoi(argv[3]);

    vfs_init();
    
    // Connect to framebuffer driver
    fb_client_connect();

    // Connect to keyboard driver
    keyboard_client_connect();

    // Load the slides off multiboot
    load_slides(argv[4]);

    // Get current video mode
    err = fb_client.vtbl.get_vesamode(&fb_client, &origmode, &origlinear, &ret);
    assert(err_is_ok(err));
    assert(err_is_ok(ret));

    // Set videomode
    err = fb_client.vtbl.set_videomode(&fb_client, xres, yres, bpp, &ret);
    assert(err_is_ok(err));
    assert(err_is_ok(ret));

    // Get and map framebuffer
    struct capref fbcap;
    uint32_t fboffset;
    err = fb_client.vtbl.get_framebuffer(&fb_client, &ret, &fbcap, &fboffset);
    assert(err_is_ok(err));
    assert(err_is_ok(ret));

    struct frame_identity fbid = { .base = 0, .bits = 0 };
    err = invoke_frame_identify(fbcap, &fbid);
    assert(err_is_ok(err));
    err = vspace_map_one_frame((void**)&vidmem, 1 << fbid.bits, fbcap,
                               NULL, NULL);
    assert(err_is_ok(err));

    vidmem += fboffset;

    // Save textmode bit of framebuffer and hopefully capture the font
    memcpy(fontbackup, vidmem, 65536);

    // Start the show
    slideshow(vidmem, xres, yres, bpp);
    messages_handler_loop();
}
