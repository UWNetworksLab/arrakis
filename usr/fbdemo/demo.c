/**
 * \file
 * \brief "Barrelfish posse in full effect", by dynamite
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <barrelfish/barrelfish.h>
#include <mackerel/mackerel.h>

#include "demo.h"
#include "bf_logo.h"
#include "lord_of_the_sith_bi0.h"

#define STARTX  (xres / 2 - 100)
#define STARTY  (yres / 2 - 100)

#define SCREEN_SIZE     (xres * yres * bpp)

#define SCREEN(x, y)    (&fb[(y) * xres * bpp + (x) * bpp])

#define FONTSIZEX       26
#define FONTSIZEY       16

#define FONTSTARTX      0
#define FONTSTARTY      37

static char     *screen = NULL;
static char     *fb = NULL;
static int      xres, yres, bpp, cidx;

static void cls(void)
{
    memset(fb, 0, SCREEN_SIZE);
}

static void copperpixel(int x, int y)
{
    // Clip
    if(x >= xres || y >= yres || x < 0 || y < 0) {
        return;
    }

    SCREEN(x, y)[0] = fabs(sin(M_PI * (double)(x + y + cidx) / yres)) * 255.0;
    SCREEN(x, y)[1] = fabs(sin(M_PI * (double)(x + y + 64) / yres)) * 255.0;
    SCREEN(x, y)[2] = fabs(sin(M_PI * (double)(x + y + 32) / yres)) * 255.0;
}

static void putcharxy(int px, int py, char c)
{
    int idx = c - 'A';

    for(int y = 0; y < FONTSIZEY; y++) {
        for(int x = 0; x < FONTSIZEX; x++) {
            if(!font1_data[(FONTSTARTY + y) * font1_width + FONTSTARTX + (idx * FONTSIZEX) + x]) {
                copperpixel(px * FONTSIZEX + x, py * FONTSIZEY + y);
            }
        }
    }
}

static void printxy(int px, int py, char *str)
{
    for(int i = 0; i < strlen(str); i++) {
        putcharxy(px + i, py, str[i]);
    }
}

static void vsync(void)
{
    wait_for_vsync();
    memcpy(screen, fb, SCREEN_SIZE);
}

static char scrolltext[] = "             BARRELFISH POSSE IN FULL EFFECT";

static void scroller(void)
{
    static int xpos = 0, nidx = 0;
    static double yidx = 0.0;

    for(int y = 0; y < FONTSIZEY; y++) {
        for(int x = 0; x < xres; x++) {
            int idx = scrolltext[((x + xpos) / FONTSIZEX) % strlen(scrolltext)];
            int ypos = (yres / 2) + sin(2 * M_PI * ((double)(x + nidx + yidx) / xres)) * 100;

            if(idx != ' ') {
                idx -= 'A';
                if(!font1_data[(FONTSTARTY + y) * font1_width + FONTSTARTX + (idx * FONTSIZEX) + ((x + xpos) % FONTSIZEX)]) {
                    copperpixel(x, y + ypos);
                }
            }
        }

/*     yidx++; */
    }

    nidx++;
    xpos += 4;
}

void vesa_demo(char *scrn, int xr, int yr, int bp)
{
    xres = xr;
    yres = yr;
    assert(bp == 32 || bp == 24);
    bpp = bp / 8;

    // Get an appropriately sized off-screen surface
    fb = malloc(SCREEN_SIZE);
    assert(fb != NULL);

    screen = scrn;

#if 0
    double idx3 = 0;
#endif
    for(;;) {
        cls();

#if 1
        // draw logo
        char *p = header_data;
        static int idx2 = 0;
        for(int y = 0; y < height; y++) {
            for(int x = 0; x < width; x++) {
                int rx = x + sin(2 * M_PI * (double)(y + idx2) / height * 2) * 10.0;
                int ry = y + sin(2 * M_PI * (double)(x + idx2) / width) * 10.0;

                HEADER_PIXEL(p, SCREEN(STARTX + rx, STARTY + ry));
            }
        }

        idx2 += 2;
#endif

#if 0
        // draw plasma
        for(int x = 0; x < xres; x++) {
            for(int y = 0; y < yres; y++) {
                SCREEN(x, y)[0] = fabs(sin(M_PI * (double)(x + y + idx3) / yres)) * 255.0;
                SCREEN(x, y)[1] = fabs(sin(M_PI * (double)(x + y + 64) / yres)) * 255.0;
                SCREEN(x, y)[2] = fabs(sin(M_PI * (double)(x + y + 32) / yres)) * 255.0;
                idx3++;
            }
        }
#endif

        cidx++;

        scroller();

        printxy(0, 0, "DYNAMITE");

        vsync();
    }
}
