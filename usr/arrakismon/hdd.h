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

#ifndef DISK_H
#define DISK_H

#include <stdint.h>

struct hdd {
    void *disk_image;
    size_t disk_image_size;
    uint16_t cylinders;
    uint8_t heads;
    uint8_t sectors;
    size_t cylinder_size;
    size_t track_size;
};

struct hdd * hdd_new_from_memory (void *disk_image, size_t disk_image_size);
void hdd_reset (struct hdd *hdd);
int hdd_get_geometry_chs (struct hdd *hdd, uint16_t *cylinders, uint8_t *heads,
                          uint8_t *sectors);
size_t hdd_get_blocks_count (struct hdd *hdd);
int hdd_read_blocks (struct hdd *hdd, size_t start_block, size_t *count,
                     uintptr_t buffer);

#endif // DISK_H
