/**
 * \file
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "vmkitmon.h"
#include "hdd.h"
#include <stdlib.h>
#include <string.h>

#define BLOCK_SIZE 512

static void
calc_chs (struct hdd *hdd)
{
    // assure there is disk with at least 516096 bytes space (16H, 63S, 1C)
    assert(hdd->disk_image != NULL && hdd->disk_image_size >= 516096);

    /* Here we use a predefined sceme to calculate the CHS of the disk. If the
     * disk is bigger then 8MiB we use 255 heads, if it is smaller we use 16
     * heads. This conforms (at least for 8MiB and bigger disks) with the Linux
     * fdisk tools and their undestanding of the geometry of non physical disks.
     * Secotors are always 63 per track wich leaves the cylinder value to be the
     * only one which really needs to be calculated.
     * Personally I have no idea how wide spread this understanding is!. */

    // set the heads
    if (hdd->disk_image_size >= 0x800000) {
        hdd->heads = 255;
    } else {
        hdd->heads = 16;
    }

    // set the sectors per track
    hdd->sectors = 63;

    // calculate the size
    hdd->track_size = hdd->sectors * BLOCK_SIZE;
    hdd->cylinder_size = hdd->heads * hdd->track_size;
    // the disk has to be a multiple of the cylinder size, so we round the real
    // disk size down the nearest multiple of cylinder size
    size_t real_disk_size = hdd->disk_image_size -
                            (hdd->disk_image_size % hdd->cylinder_size);
    assert(real_disk_size >= 516096);
    // now we may safely calculate the cylinders count
    hdd->cylinders = real_disk_size / hdd->cylinder_size;
}

struct hdd *
hdd_new_from_memory (void *disk_image, size_t disk_image_size)
{
    struct hdd *ret = calloc(1, sizeof(struct hdd));

    ret->disk_image = disk_image;
    ret->disk_image_size = disk_image_size;

    calc_chs(ret);

    return ret;
}

void
hdd_reset (struct hdd *hdd)
{
}

int
hdd_get_geometry_chs (struct hdd *hdd, uint16_t *cylinders, uint8_t *heads,
                      uint8_t *sectors)
{
    *cylinders = hdd->cylinders;
    *heads = hdd->heads;
    *sectors = hdd->sectors;

    return 0;
}

size_t
hdd_get_blocks_count (struct hdd *hdd)
{
    return hdd->disk_image_size / BLOCK_SIZE;
}

int
hdd_read_blocks (struct hdd *hdd, size_t start_block, size_t *count,
                 uintptr_t buffer)
{
    if (((start_block + *count) * BLOCK_SIZE) > hdd->disk_image_size) {
        *count = hdd->disk_image_size/BLOCK_SIZE - start_block;
    }

    memcpy((void *)buffer, (void *)(hdd->disk_image + start_block*BLOCK_SIZE),
           *count * BLOCK_SIZE);

    return HANDLER_ERR_OK;
}
