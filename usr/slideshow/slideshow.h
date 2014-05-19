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

#ifndef SLIDESHOW_H
#define SLIDESHOW_H

#define MAX_SLIDES      200

extern void *slide[MAX_SLIDES];
extern size_t slide_length[MAX_SLIDES];
extern size_t nslides;

void slideshow(char *scrn, int xr, int yr, int bp);
void keyboard_key_event(struct keyboard_binding *b, uint8_t scancode, bool ext);
void wait_for_vsync(void);
void quit(void);

#endif
