/**
 * \file
 * \brief Implementations of standard libc string functions.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>
#include <assert.h>
#include <stddef.h>
#include <string.h>

#if 0
void *
memset (void *s, int c, size_t n)
{
    uint8_t *p = (uint8_t *)s;
    for (size_t m = 0; m < n; m++) {
        *p++ = c;
    }
    return s;
}
#endif

void *
memchr(const void *s, int c, size_t n)
{
    size_t i;
    const uint8_t *p;

    for (i = 0, p = s; i < n; i++, p++)
        if (*p == c)
            return (void *)p;

    return NULL;
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

char *
strncpy(char *dest, const char *src, size_t count)
{
    char *tmp = dest;
    int c = count;
    while (c >= 0) {
        if ((*tmp = *src) != 0) src++;
        tmp++;
        c--;
    }
    return dest;
}

char *
strcpy(char *dest, const char *src)
{
    char *pos = dest;
    while ((*pos++ = *src++) != 0);
    return dest;
}

#if 0
void *
memmove(void *dst, const void *src, size_t sz)
{
    size_t      i;
    const char  *source = src;
    char        *dest = dst;

    if(sz == 0) {
        return dst;
    }

    /* XXX: This is slooooooooooowwwwwwwwwww ...... */
    if(src >= dst) {   // copy front to back
        for(i = 0; i < sz; i++)
            dest[i] = source[i];
    } else {            // copy back to front
        for(i = sz - 1; i > 0; i--)
            dest[i] = source[i];

        dest[0] = source[0];
    }

    return dst;
}
#endif

void *
memcpy(void *dst, const void *src, size_t len)
{
    char *d = dst;
    const char *s = src;

    /* check that we don't overlap (should use memmove()) */
    assert((src < dst && (char *)src + len <= (char *)dst)
           || (dst < src && (char *)dst + len <= (char *)src));

    while (len--)
        *d++ = *s++;

    return dst;
}

char *
strchr(const char *s, int c)
{
    unsigned int i;
    unsigned int n = strlen(s);

    for(i = 0; i < n ; i++) {
        if(s[i] == c)
            return (char *)&s[i];
    }

    return NULL;
}

char *
strrchr(const char *s, int c)
{
    unsigned int i;
    unsigned int n = strlen(s);

    if(n == 0)
        return NULL;

    for(i = n - 1; ; i--) {
        if (s[i] == c) {
            return (char *)&s[i];
        } else if (i == 0) {
            return NULL;
        }
    }
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

int
strcmp(const char *s1, const char *s2)
{
    size_t len1 = strlen(s1), len2 = strlen(s2),
        maxlen = len1 > len2 ? len1 : len2;

    return strncmp(s1, s2, maxlen);
}

size_t
strspn(const char* s, const char* set)
{
    const char* l = s;

    while (('\0' != *l) && (NULL != strchr(set, *l)))
    {
        l++;
    }
    return (l - s);
}

size_t
strcspn(const char* s, const char* set)
{
    const char* l = s;

    while (('\0' != *l)  && (NULL == strchr(set, *l)))
    {
        l++;
    }
    return (l - s);
}

char *
strtok(char *s, const char *delim)
{
    static char *save;

    if (s == NULL)
    {
        s = save;
    }

    s = s + strspn(s, delim);
    if ('\0' == *s)
    {
        return NULL;
    }

    save = s + strcspn(s, delim);
    if ('\0' != *save)
    {
        *save++ = '\0';
    }
    return s;
}
