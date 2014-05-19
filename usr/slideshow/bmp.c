/**
 * \file
 * \brief BMP Loader
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

#include "bmp.h"

#define MUST(x)         if(!(x)) return 1;

struct bmp_header {
    char        header[2];
    uint32_t    size;
    uint32_t    reserved;
    uint32_t    bitmap_start;
} __attribute__ ((packed));

enum comp_alg {
    COMP_BI_RGB         = 0,
    COMP_BI_RLE8        = 1,
    COMP_BI_RLE4        = 2,
    COMP_BI_BITFIELDS   = 3,
    COMP_BI_JPEG        = 4,
    COMP_BI_PNG         = 5
};

struct dib_header {
    uint32_t            size;
    uint32_t            width, height;
    uint16_t            planes;
    uint16_t            bpp;
    enum comp_alg       compression;
    uint32_t            image_size;
    uint32_t            hres, vres;
    uint32_t            colors, important;
} __attribute__ ((packed));

int bmp_load(void *data, size_t size)
{
    struct bmp_header *bh = data;
    struct dib_header *dh = data + sizeof(struct bmp_header);

    MUST(size > sizeof(struct bmp_header) + sizeof(struct dib_header));
    MUST(!strncmp(bh->header, "BM", 2));
    MUST(dh->size == 40);
    MUST(dh->planes == 1);
    MUST(dh->compression == COMP_BI_RGB);

    char *bitmap = data + bh->bitmap_start;

    assert(24 == dh->bpp);
    assert(dh->height <= yres);
    assert(dh->width <= xres);

    size_t linesize = dh->image_size / dh->height;
    int mbpp = dh->bpp / 8;

    // BMPs are stored upside-down
    for(size_t y = 0; y < dh->height; y++) {
        for(size_t x = 0; x < dh->width; x++) {
            SCREEN(x, y)[0] = bitmap[(linesize * (dh->height - y - 1)) + x * mbpp + 0];
            SCREEN(x, y)[1] = bitmap[(linesize * (dh->height - y - 1)) + x * mbpp + 1];
            SCREEN(x, y)[2] = bitmap[(linesize * (dh->height - y - 1)) + x * mbpp + 2];
        }
    }

    return 0;
}
