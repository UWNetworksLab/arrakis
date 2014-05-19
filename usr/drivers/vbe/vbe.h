/**
 * \file
 * \brief VESA BIOS Extensions (VBE) driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VBE_H
#define VBE_H

#include <stdbool.h>
#include <pci/mem.h>

#define VBE_OK          0x004f

struct vbeinfoblock {
    // All VBE revisions
    char        signature[4];
    uint16_t    version;
    uint32_t    oemstringptr;
    uint32_t    capabilities;
    uint32_t    videomodeptr;
    uint16_t    totalmemory;

    // VBE 2.0 and up
    uint16_t    oemsoftwarerev;
    uint32_t    oemvendornameptr;
    uint32_t    oemproductnameptr;
    uint32_t    oemproductrevptr;
    uint8_t     reserved[222];
    uint8_t     oemdata[256];
} __attribute__ ((packed));

struct vbemodeinfoblock {
    // All VBE revisions
    uint16_t    modeattributes;
    uint8_t     winaattributes, winbattributes;
    uint16_t    wingranularity;
    uint16_t    winsize;
    uint16_t    winasegment, winbsegment;
    uint32_t    winfuncptr;
    uint16_t    bytesperscanline;

    // VBE 1.2 and up
    uint16_t    xresolution, yresolution;
    uint8_t     xcharsize, ycharsize;
    uint8_t     numberofplanes;
    uint8_t     bitsperpixel;
    uint8_t     numberofbanks;
    uint8_t     memorymodel;
    uint8_t     banksize;
    uint8_t     numberofimagepanes;
    uint8_t     reserved;

    // Direct color fields
    uint8_t     redmasksize, redfieldposition;
    uint8_t     greenmasksize, greenfieldposition;
    uint8_t     bluemasksize, bluefieldposition;
    uint8_t     rsvdmasksize, rsvdfieldposition;
    uint8_t     directcolormodeinfo;

    // VBE 2.0 and up
    uint32_t    physbaseptr;
    uint32_t    offscreenmemoffset;
    uint16_t    offscreenmemsize;
    uint8_t     reserved2[206];
} __attribute__ ((packed));

enum modeattributes {
    MODE_SUPPORTED              = 1 << 0,
    TTY_OUTPUT_SUPPORTED        = 1 << 2,
    COLOR_MODE                  = 1 << 3,
    GRAPHICS_MODE               = 1 << 4,
    MODE_NOT_VGA_COMPATIBLE     = 1 << 5,
    WINDOWED_MODE_NOT_AVAILABLE = 1 << 6,
    LINEAR_MODE_AVAILABLE       = 1 << 7
};

enum windowattributes {
    RELOCATABLE_WINDOWS_SUPPORTED       = 1 << 0,
    WINDOW_READABLE                     = 1 << 1,
    WINDOW_WRITEABLE                    = 1 << 2
};

enum memorymodel {
    MODEL_TEXT = 0,
    MODEL_CGA = 1,
    MODEL_HERCULES = 2,
    MODEL_PLANAR = 3,
    MODEL_PACKED_PIXEL = 4,
    MODEL_NON_CHAIN_4 = 5,
    MODEL_DIRECT_COLOR = 6,
    MODEL_YUV = 7
};

enum directcolormode {
    DCM_RAMP_PROGRAMMABLE       = 1 << 0,
    DCM_RSVD_BITS_USABLE        = 1 << 1
};

uint32_t vbe_controller_info(struct vbeinfoblock *ib);
uint32_t vbe_mode_info(uint16_t mode, struct vbemodeinfoblock *mib);
uint32_t vbe_setmode(uint16_t mode, bool linear, bool clear);
uint32_t vbe_getmode(uint16_t *retmode, bool *retlinear);
uint16_t *vbe_getmodes(void);
uint32_t vbe_savestate(void);
uint32_t vbe_restorestate(void);
errval_t vbe_get_framebuffer_cap(struct capref *cap, size_t *offset);
void vbe_vsync(void);

void vbe_init(struct device_mem *bar_info, int nr_mapped_regions);

void vbe_driver_init_done(void);

#endif
