/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef STORAGE_H
#define STORAGE_H

#include <storage/vsa.h>
#include <storage/vsic.h>

#define storage_alloca(vsic, size)		\
  alloca(STORAGE_VSIC_ROUND(vsic, size))

#define storage_malloc(vsic, size) 		\
  malloc(STORAGE_VSIC_ROUND(vsic, size))

#define storage_realloc(vsic, ptr, size)       	\
  realloc(ptr, STORAGE_VSIC_ROUND(vsic, size))

#define storage_free(vsic, ptr)			\
  free(ptr)

errval_t storage_init(int argc, const char **argv);

#endif
