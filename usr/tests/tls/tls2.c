/**
 * \file
 * \brief Thread-local storage (TLS) test program.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>

__thread int test_extern = 42;

void test_extern_func(void);
void test_extern_func(void)
{
    assert(test_extern == 42);
    test_extern *= 2;
}
