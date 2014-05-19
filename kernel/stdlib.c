/**
 * \file
 * \brief Standard libc library functions.
 */

/*
 * Copyright (c) 2007-2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>

/*
 * Copied here, as there are conflicting types between oldc and newlib
 */
static inline int
isdigit(int c)
{
        return '0' <= c && c <= '9';
}

static inline int
isxdigit(int c)
{
        return isdigit(c) || ('A' <= c && c <= 'F') || ('a' <= c && c <= 'f');
}

static inline int
islower(int c)
{
        return 'a' <= c && c <= 'z';
}

/**
 * \brief Convert ASCII digit to integer.
 *
 * Converts an ASCII digit to the corresponding integer digit. Handles
 * hexadecimal digits. Returns -1 on error.
 *
 * \param c     ASCII digit to convert
 *
 * \return Integer digit corresponding to 'c' or -1 on error.
 */
static inline int
ascii_to_int(int c)
{
    if(!isxdigit(c)) {
        return -1;
    }

    if(isdigit(c)) {            // c in ['0'..'9']
        return c - '0';
    }

    if(islower(c)) {            // c in ['a'..'f']
        return 10 + c - 'a';
    } else {                    // c in ['A'..'F']
        return 10 + c - 'A';
    }
}

/*
 * XXX doesn't actually deal with a sign character!!!
 */
long int strtol(const char *nptr, char **endptr, int base)
{
    assert(base == 10 || base == 16);
    
    long int retval = 0;

    for(int i = 0;
        (base == 10 && isdigit((int)nptr[i])) || (base == 16 && isxdigit((int)nptr[i]));
        i++) {
        // Shift and add a digit
        retval = retval * base + ascii_to_int(nptr[i]);
    }

    return retval;
}
unsigned long int strtoul(const char *nptr, char **endptr, int base)
{
    assert(base == 10 || base == 16);
    
    unsigned long int retval = 0;

    for(int i = 0;
        (base == 10 && isdigit((int)nptr[i])) || (base == 16 && isxdigit((int)nptr[i]));
        i++) {
        // Shift and add a digit
        retval = retval * base + ascii_to_int(nptr[i]);
    }

    return retval;
}
