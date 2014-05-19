/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <stdint.h>

#define LOWBITS (sizeof(uintptr_t)-1)

void *memmove(void *s1, const void *s2, size_t n)
{
    uintptr_t from = (uintptr_t)s2;
    uintptr_t to = (uintptr_t)s1;

    if (to <= from) {
	// Work forwards

	if (((from ^ to) & LOWBITS) == 0) {
	    // They have the same alignment
	    
	    // Copy bytes until aligned to a word boundary
	    while (n != 0 && ((from & LOWBITS) != 0)) {
		*(char *)to = *(const char *)from;
		from++;
		to++;
		n--;
	    }

	    // Copy words
	    while(n >= sizeof(uintptr_t)) {
		*(uintptr_t *)to = *(const uintptr_t *)from;
		from += sizeof(uintptr_t);
		to += sizeof(uintptr_t);
		n -= sizeof(uintptr_t);
	    }
	}

	// Copy (remaining) bytes
	while (n != 0) {
	    *(char *)to = *(const char *)from;
	    from++;
	    to++;
	    n--;
	}
    }
    else {
	// Work backwards
	from += n;
	to += n;

	if (((from ^ to) & LOWBITS) == 0) {
	    // They have the same alignment

	    // Copy bytes until aligned to a word boundary
	    while (n != 0 && ((from & LOWBITS) != 0)) {
		from--;
		to--;
		*(char *)to = *(const char *)from;
		n--;
	    }

	    // Copy words
	    while(n >= sizeof(uintptr_t)) {
		from -= sizeof(uintptr_t);
		to -= sizeof(uintptr_t);
		*(uintptr_t *)to = *(const uintptr_t *)from;
		n -= sizeof(uintptr_t);
	    }
	}

	// Copy (remaining) bytes
	while (n != 0) {
	    from--;
	    to--;
	    *(char *)to = *(const char *)from;
	    n--;
	}
    }
    return s1;
}
