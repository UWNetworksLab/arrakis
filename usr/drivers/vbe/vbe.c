/**
 * \file
 * \brief VESA BIOS Extensions (VBE) 2.0 compatible video card driver.
 *
 * This driver only supports one primary video card that has already
 * been booted by the system BIOS on native x86 hardware. Secondary
 * cards are not supported.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <acpi_client/acpi_client.h>
#include <mackerel/mackerel.h>
#include <inttypes.h>

#include "vbe.h"
#include "int10.h"

/// Segment to put data into (needs to be free RAM)
#define DATASEG         0x40000

/// Maximum number of video modes supported
#define MAX_MODES       256

/// Pointer to emulated x86's RAM
static void *myram = NULL;

/// Array of all supported video modes for this adapter
static uint16_t modes[MAX_MODES];

/// Array of BARs for the graphics card
static struct device_mem *bars = NULL;

/// Number of BARs in the array
static int nbars = 0;

/// The current video mode
static uint16_t current_mode = 0;

static inline uint32_t far_to_linear(uint32_t farptr)
{
    uint32_t seg = farptr >> 16;
    uint32_t off = farptr & 0xffff;

    return (seg << 4) + off;
}

uint32_t vbe_controller_info(struct vbeinfoblock *ib)
{
    struct int10_regs regs = {
        .eax = 0x4f00,
        .es = DATASEG >> 4,
        .edi = 0
    };

    struct vbeinfoblock *b = (struct vbeinfoblock *)(myram + DATASEG);

    // Request extended VBE2.0 data
    strncpy(b->signature, "VBE2", 4);

    int10(&regs);
    *ib = *b;

    return regs.eax & 0xffff;
}

uint32_t vbe_mode_info(uint16_t mode, struct vbemodeinfoblock *mib)
{
    struct int10_regs regs = {
        .eax = 0x4f01,
        .ecx = mode,
        .es = DATASEG >> 4,
        .edi = 0
    };

    int10(&regs);
    *mib = *(struct vbemodeinfoblock *)(myram + DATASEG);

    return regs.eax & 0xffff;
}

uint32_t vbe_setmode(uint16_t mode, bool linear, bool clear)
{
    struct int10_regs regs = {
        .eax = 0x4f02,
        .ebx = (mode & 0x1ff) | (linear ? 1 << 14 : 0) | (clear ? 0 : 1 << 15)
    };

    int10(&regs);

    if((regs.eax & 0xffff) == VBE_OK) {
        current_mode = mode;
    }

    return regs.eax & 0xffff;
}

uint32_t vbe_getmode(uint16_t *retmode, bool *retlinear)
{
    struct int10_regs regs = {
        .eax = 0x4f03,
    };

    int10(&regs);

    if((regs.eax & 0xffff) == VBE_OK) {
        *retmode = regs.ebx & 0x3fff;
        *retlinear = (regs.ebx & (1u << 14)) != 0;
    }

    return regs.eax & 0xffff;
}

uint16_t *vbe_getmodes(void)
{
    return modes;
}

// FIXME: this API is broken, because it assumes that the entire framebuffer
// can be mapped by a single cap. in the long run we probably don't want to be
// handing out the raw hardware cap anyway...
errval_t vbe_get_framebuffer_cap(struct capref *cap, size_t *retoffset)
{
    struct vbemodeinfoblock mib;
    int first_mem_bar = -1;

    uint32_t r = vbe_mode_info(current_mode, &mib);
    assert((r & 0xffff) == VBE_OK);

    lpaddr_t fbphys = mib.physbaseptr;
    size_t fbsize = (size_t)mib.xresolution * mib.yresolution
                    * (mib.bitsperpixel / 8);

    printf("vbe: looking for BAR cap with PA %"PRIuLPADDR"-%"PRIuLPADDR"\n",
           fbphys, fbphys + fbsize);

    for(int i = 0; i < nbars; i++) {
        if (bars[i].type == 0) { // memory bar
            printf("%d: %"PRIxGENPADDR"-%"PRIxGENPADDR"\n", i,
                   bars[i].paddr, bars[i].paddr + bars[i].bytes);
            if (first_mem_bar == -1) {
                first_mem_bar = i;
            }
            if (bars[i].paddr <= fbphys
                && bars[i].paddr + bars[i].bytes >= fbphys + fbsize) {
                // it's in this BAR, but do we have a single cap that covers it?
                // XXX: assume uniformly-sized caps
                size_t bytes_per_cap = bars[i].bytes / bars[i].nr_caps;
                size_t fboffset = fbphys - bars[i].paddr;

                if (fboffset / bytes_per_cap
                    != (fboffset + fbsize - 1) / bytes_per_cap) {
                    USER_PANIC("can't return multiple caps to framebuffer from"
                               " broken API");
                }

                // does framebuf start at cap boundary?
                *cap = bars[i].frame_cap[fboffset / bytes_per_cap];
                *retoffset = fboffset % bytes_per_cap;
                return SYS_ERR_OK;
            }
        } else {
            printf("%d: IO cap\n", i);
        }
    }

    // XXX: Hack to get right framebuffer (usually it's the first memory BAR)
    printf("vbe: not found, falling back to %d: %" PRIxGENPADDR "\n",
           first_mem_bar, bars[first_mem_bar].paddr);
    assert(bars[first_mem_bar].nr_caps == 1);
    *cap = bars[first_mem_bar].frame_cap[0];

    return SYS_ERR_OK;
}

/* XXX: hack to wait for a vertical retrace */
void vbe_vsync(void)
{
    /* wait until any previous retrace has ended */
    while(mackerel_read_io_8(0, 0x3da) & 8);

    /* wait until a new retrace has just begun */
    while(!(mackerel_read_io_8(0, 0x3da) & 8));
}

static char *buf = NULL;
static size_t bufsize = 0;

uint32_t vbe_savestate(void)
{
    // Request size of state buffer
    struct int10_regs regs = {
        .eax = 0x4f04,
        .edx = 0,
        .ecx = 0xf,
    };
    int10(&regs);
    bufsize = regs.ebx * 64;
    buf = realloc(buf, bufsize);
    assert(buf != NULL);

    // Save buffer
    struct int10_regs nregs = {
        .eax = 0x4f04,
        .edx = 1,
        .ecx = 0xf,
        .es = DATASEG >> 4,
        .ebx = 0
    };
    int10(&nregs);

    char *b = (myram + DATASEG);
    memcpy(buf, b, bufsize);

    return nregs.eax & 0xffff;
}

uint32_t vbe_restorestate(void)
{
    assert(buf != NULL);
    char *b = (myram + DATASEG);
    memcpy(b, buf, bufsize);

    // Restore buffer
    struct int10_regs regs = {
        .eax = 0x4f04,
        .edx = 2,
        .ecx = 0xf,
        .es = DATASEG >> 4,
        .ebx = 0
    };
    int10(&regs);

    return regs.eax & 0xffff;
}

void vbe_init(struct device_mem *bar_info, int nr_mapped_regions)
{
    errval_t err;
    printf("vbe: initialising graphics hardware...\n");

    // Map BIOS memory region
    struct capref bioscap;
    size_t size;
    err = acpi_get_vbe_bios_cap(&bioscap, &size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_get_vbe_bios_cap failed");
        return;
    }

    void *lowmem;
    err = vspace_map_one_frame(&lowmem, size, bioscap, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed");
        return;
    }

    myram = malloc(REALMODE_MEM_SIZE);
    memcpy(myram, lowmem, REALMODE_MEM_SIZE);

    int10_init(myram);

    struct vbeinfoblock ib;

    uint32_t r = vbe_controller_info(&ib);
    assert(r == VBE_OK);

    if(strncmp(ib.signature, "VESA", 4) || ib.version < 0x200) {
        printf("Graphics card does not support at least VBE 2.0\n");
        return;
    } else {
        printf("VBE %u.%u compatible graphics card: %s\n",
               ib.version >> 8, ib.version & 0xff,
               (char *)myram + far_to_linear(ib.oemstringptr));
        printf("Available video memory: %u MB\n", (ib.totalmemory << 16) / 1024 / 1024);

        // make a local copy of video modes (256 modes should be enough)
        memcpy(modes, myram + far_to_linear(ib.videomodeptr), 256 * 2);

#if 0
        // Print supported VESA modes
        for(int i = 0; modes[i] != 0xffff; i++) {
            struct vbemodeinfoblock mib;

            if(vbe_mode_info(modes[i], &mib) != VBE_OK) {
                return;
            }

            if (mib.xresolution == 0 && mib.yresolution == 0) {
                continue;
            }

            printf("Mode 0x%x (%dx%dx%d) [0x%x:0x%x:0x%x]: %s%s%s%s%s%s%s\n",
                   modes[i], mib.xresolution, mib.yresolution, mib.bitsperpixel,
                   mib.winasegment, mib.winbsegment, mib.physbaseptr,
                   mib.modeattributes & MODE_SUPPORTED ? "supported" : "",
                   mib.modeattributes & TTY_OUTPUT_SUPPORTED ? " TTY" : "",
                   mib.modeattributes & COLOR_MODE ? " color" : " monochrome",
                   mib.modeattributes & GRAPHICS_MODE ? " graphics" : " text",
                   mib.modeattributes & MODE_NOT_VGA_COMPATIBLE ? "" : " VGA",
                   mib.modeattributes & WINDOWED_MODE_NOT_AVAILABLE ? "" : " windowed",
                   mib.modeattributes & LINEAR_MODE_AVAILABLE ? " linear" : "");
        }
#endif
    }

    uint16_t curmode = 0;
    bool islinear = false;
    r = vbe_getmode(&curmode, &islinear);
    assert(r == VBE_OK);
    printf("Current mode: 0x%x %s\n", curmode, islinear ? "linear": "windowed");

    bars = bar_info;
    nbars = nr_mapped_regions;

    vbe_driver_init_done();
}
