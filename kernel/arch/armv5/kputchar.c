/**
 * \file
 * \brief The world's simplest serial driver.
 *
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <serial.h>
#include <kputchar.h>

#define KPBUFSZ 256
static char kputbuf[KPBUFSZ];
static int kcount = 0;

static void kflush(void)
{
    for(int i=0; i<kcount; i++)
        serial_console_putchar(kputbuf[i]);
    kcount = 0;
}

void kprintf_begin(void)
{
	kcount = 0;
}

int kputchar(int c)
{
    kputbuf[kcount++] = c;
    if (kcount == KPBUFSZ || c == '\n')
        kflush();
    return c;
}

void kprintf_end(void)
{
    kflush();
}

// End
