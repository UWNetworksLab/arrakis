/**
 * \file
 */

/*
 * Copyright (c) 2009, 2010, 2011, 2012 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#include "vmkitmon.h"

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/terminal.h>
#include <vfs/vfs.h>
#include <timer/timer.h>
#include "realmode.h"
#include <bench/bench.h>
#include "pci/devids.h"
#include <pci/pci.h>

#define VFS_MOUNTPOINT  "/vm"
#define IMAGEFILE       (VFS_MOUNTPOINT "/system-bench.img")
#define GRUB_IMG_PATH   (VFS_MOUNTPOINT "/vmkitmon_grub")

void *          grub_image = NULL;
size_t          grub_image_size = 0;
void *          hdd0_image = NULL;
size_t          hdd0_image_size = 0;
struct guest *  guest = NULL;

static void
vfs_load_file_to_memory (const char *file, void **data, size_t *size)
{
    assert(data != NULL);
    assert(size != NULL);

    errval_t err;
    vfs_handle_t vh;

    err = vfs_open(file, &vh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Error opening %s", file);
    }

    struct vfs_fileinfo info;
    err = vfs_stat(vh, &info);
    assert_err(err, "vfs_stat");

    *data = malloc(info.size);
    assert(*data != NULL);

    err = vfs_read(vh, *data, info.size, size);
    assert_err(err, "vfs_read");
    assert(*size == info.size);

    vfs_close(vh);
}

int main (int argc, char *argv[])
{
    errval_t err;
    char *cardName = NULL;

    const char *imagefile = IMAGEFILE;

    vfs_init();
    bench_init();

    err = timer_init();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "error initialising timer client library\n");
    }

    if (argc < 3) {
        printf("Usage: %s <Network card Name> <vfs mount URI> [disk image path]\n",
               argv[0]);
        printf("<Network card Name> value is ignored in this version\n");
        return 1;
    }

    if (argc > 3) {
        imagefile = argv[3];
    }

    cardName = argv[1];
    printf("vmkitmon: start\n");

    //Poll bebe
    printf("Ignoring the cardname [%s], and using the default one from vfs_mount\n",
            cardName);
    vfs_mkdir(VFS_MOUNTPOINT);
    err = vfs_mount(VFS_MOUNTPOINT, argv[2]);
    if (err_is_fail(err)) {
        printf("vmkitmon: error mounting %s: %s\n", argv[2], err_getstring(err));
        return 1;
    }

    /* Initialization */
    err = realmode_init();
    assert_err(err, "realmode_init");
    // fetch all relevant multiboot data
    //load_multiboot_files();

    // load files
    // FIXME: use a dynamic way to specify those arguments
    printf("Loading file [%s]\n", GRUB_IMG_PATH);
    vfs_load_file_to_memory(GRUB_IMG_PATH, &grub_image, &grub_image_size);
    printf("Loading file [%s]\n", imagefile);
    vfs_load_file_to_memory(imagefile, &hdd0_image, &hdd0_image_size);
    printf("Done with file loading\n");

    /* Guest execution */
    // perform some sanity checks
    if (grub_image == NULL) {
        printf("vmkitmon: no grub image available, abort\n");
        return 1;
    }

    guest = guest_create();
    assert(guest != NULL);
    err = guest_make_runnable(guest, true);
    assert_err(err, "guest_make_runnable");

    printf("vmkitmon: end\n");

    //messages_handler_loop();
    while (1) {
		messages_wait_and_handle_next();
	}

}
