/**
 * \file
 * \brief Library routines cause we don't have a libc
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>

void __assert(const char *exp, const char *file, const char *func, int line)
{
/*     panic("elver assertion \"%s\" failed at %s:%d", exp, file, line); */
    for (;;) ;
}

int printf(const char *fmt, ...)
{
    return -1;
}

void *
memset (void *s, int c, size_t n)
{
    uint8_t *p = (uint8_t *)s;
    for (size_t m = 0; m < n; m++) {
        *p++ = c;
    }
    return s;
}

void *
memcpy(void *dst, const void *src, size_t len)
{
    char *d = dst;
    const char *s = src;

    /* check that we don't overlap (should use memmove()) */
    assert((src < dst && src + len <= dst) || (dst < src && dst + len <= src));

    while (len--)
        *d++ = *s++;

    return dst;
}

char *
strrchr(const char *s, int c)
{
    unsigned int i;

    if(strlen(s) == 0)
        return NULL;

    for(i = strlen(s) - 1; i != 0; i--) {
        if(s[i] == c)
            return (char *)&s[i];
    }

    return NULL;
}

int
strncmp(const char *s1, const char *s2, size_t n)
{
    int result;

    for(unsigned int i = 0; i < n; i++) {
        if((result = s2[i] - s1[i]) != 0) {
            return result;
        }

        if(s1[i] == '\0' || s2[i] == '\0') {
            break;
        }
    }

    return 0;
}

size_t
strlen(const char *s)
{
    size_t i = 0;

    while (*s != '\0') {
        i++;
        s++;
    }

    return i;
}

int
strcmp(const char* a, const char* b)
{
    while (*a == *b && *a != '\0')
    {
        a++;
        b++;
    }

    return *a - *b;
}
