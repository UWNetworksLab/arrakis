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

#ifndef DEMO_H
#define DEMO_H

void vesa_demo(char *screen, int xr, int yr, int bp);
int set_videomode(int xres, int yres, int bpp);
void wait_for_vsync(void);

#endif
