/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LOCAL_H
#define LOCAL_H

#include <stdio.h>
#include <stdarg.h>

int __srefill(FILE *f);
size_t
__fread(void * __restrict buf, size_t size, size_t count, FILE * __restrict fp);
size_t strlcpy(char *, const char *, size_t);
int __svfscanf(FILE *fp, const char *fmt0, va_list ap);

#endif
