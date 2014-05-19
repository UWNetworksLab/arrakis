/*
 * Copyright (c) 2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>

void *
bsearch(const void *key, const void *base, size_t nmemb, size_t size, 
        int (*compar)(const void *, const void*))
{
    int i, low, high;
    if (nmemb <= 1)
        return NULL;

    low  = 0;
    high = nmemb;

    while (low < high) {
        i = low + ((high - low) / 2);
        
        int comp = compar(key, (char*) base + (i * size));
        if (comp == 0) {
            return (char*) base + (i * size);
        } else if (comp < 0) {
            high = i - 1;
        } else { // comp > 0
            low = i + 1;
        }
    }

    if (compar(key, (char*) base + (low * size)) == 0) {
        return (char*) base + (low * size);
    } else { 
        return NULL;
    }
}
