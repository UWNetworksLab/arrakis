/**
 * \file
 * \brief Slide show
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
#include <if/keyboard_defs.h>

#include "slideshow.h"
#include "bmp.h"
#include "zlib_load.h"

#define SLIDE_BMP_SIZE          3145728

char    *fb = NULL;
int     xres, yres, bpp;

static char                     *screen = NULL;
void                            *slide[MAX_SLIDES];
size_t                          slide_length[MAX_SLIDES];
size_t                          nslides = 0;
size_t                          curslide = 0;

#if 0
static void cls(void)
{
    memset(fb, 0, SCREEN_SIZE);
}
#endif

static void vsync(void)
{
    wait_for_vsync();
    memcpy(screen, fb, SCREEN_SIZE);
}

static void draw_slide(void)
{
    void *myslide = malloc(SLIDE_BMP_SIZE);
    assert(myslide != NULL);
    size_t myslide_length;

    int r = zlib_load(myslide, SLIDE_BMP_SIZE, &myslide_length,
                      slide[curslide], slide_length[curslide]);
    assert(r == 0);

    r = bmp_load(myslide, myslide_length);
    assert(r == 0);

    free(myslide);

    vsync();
}

void keyboard_key_event(struct keyboard_binding *b, uint8_t scancode, bool ext)
{
#if 0
    bool pressed = scancode & 0x80 ? false : true;

    if(pressed) {
        printf("Keypress: %d\n", scancode);
    }
#endif

    switch(scancode) {
    case 77:        // Right arrow
    case 81:        // Page down
    case 80:        // Down arrow
    case 57:        // Space
        if(curslide < nslides - 1) {
            curslide++;
        }
        printf("Next slide\n");
        break;

    case 75:        // Left arrow
    case 73:        // Page up
    case 72:        // Up arrow
        if(curslide > 0) {
            curslide--;
        }
        printf("Prev slide\n");
        break;

    case 1:         // escape
        printf("Exit\n");
        quit();

    default:
        return;
    }

    draw_slide();
}

void slideshow(char *scrn, int xr, int yr, int bp)
{
    xres = xr;
    yres = yr;
    assert(bp == 32 || bp == 24);
    bpp = bp / 8;

    // Get an appropriately sized off-screen surface
    fb = malloc(SCREEN_SIZE);
    assert(fb != NULL);

    screen = scrn;

    draw_slide();
}
