/*
 * io.h
 *
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IO_H_
#define IO_H_

static inline void writeb(unsigned char b, volatile void *addr)
{
	*(volatile unsigned char *) addr = b;
}
static inline void writew(unsigned short b, volatile void *addr)
{
	*(volatile unsigned short *) addr = b;
}
static inline void writel(uint32_t b, char *addr)
{
	*(volatile uint32_t *) addr = b;
}
static inline void writeq(unsigned int b, volatile void *addr)
{
	*(volatile unsigned long long *) addr = b;
}

#endif /* IO_H_ */
