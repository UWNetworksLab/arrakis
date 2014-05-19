/*
 * Copyright (c) 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr 6, CH-8092 Zurich.
 */

#ifndef FDIF_H_
#define FDIF_H_

#include <stdio.h>
#include <barrelfish/types.h>

void play_with_fdif(void);

struct gimage {
  uint32_t       width;
  uint32_t       height;
  uint32_t       bytes_per_pixel; /* 2:RGB16, 3:RGB, 4:RGBA */ 
  uint8_t    pixel_data[320 * 240];
};

#define FDIF_DEBUG(x...) printf("fdif: " x)

#endif // FDIF_H_