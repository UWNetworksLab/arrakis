/**
 * \file
 * \brief C++ startup code. Contains .ctors section header and _main().
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdint.h>

void *__dso_handle = &__dso_handle;

/// A global constructor takes no arguments and returns nothing
typedef void (*CDtor)(void);

/// First entry in .ctors array is the unspecified length marker (-1)
static CDtor ctors[1]
__attribute__ ((unused, section(".ctors"), aligned(sizeof(CDtor))))
    = { (CDtor)(-1) };

/**
 * Symbols provided by linker that point to the first and after the
 * last entry of the .init_array section, respectively.
 */
extern CDtor __init_array_start __attribute__ ((visibility ("hidden")));
extern CDtor __init_array_end __attribute__ ((visibility ("hidden")));

int _main(int argc, char *argv[]);
int main(int argc, char *argv[]);

/**
 * \brief Call global (static) constructors, listed in .ctors
 *
 * This function walks an array of function pointers, contained in the
 * .ctors section and calls each function therein, in reverse
 * order. The array starts with a length word, which may be -1, in
 * which case the array has to be NULL terminated.
 */
static void call_global_ctors(void)
{
    // First word gives length of ctors array, or -1
    intptr_t n = (intptr_t)ctors[0];

    // If first word -1, we count array entries until we encounter 0
    if(n == -1) {
        for(n = 0; ctors[n + 1] != 0; n++);
    }

    // Now call all functions, in reverse order
    for(intptr_t i = n; i >= 1; i--) {
        ctors[i]();
    }
}

/**
 * \brief Call global (static) constructors, listed in .init_array
 *
 * This function walks an array of function pointers, contained in the
 * .init_array section and calls each function therein, in order. The
 * array bounds are delineated by the __init_array_start and
 * __init_array_end symbols, respectively.
 */
static void call_init_array(void)
{
    for(CDtor *ctor = &__init_array_start; ctor < &__init_array_end; ctor++) {
        (*ctor)();
    }
}

int _main(int argc, char *argv[])
{
    call_global_ctors();
    call_init_array();
    return main(argc, argv);
}
