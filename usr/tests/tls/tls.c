/**
 * \file
 * \brief Thread-local storage (TLS) test program.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>

#define INIT_VAL 37

static __thread int test_bss[10];
static __thread int test_init = INIT_VAL;

extern __thread int test_extern; // tls2.c

void test_extern_func(void);

static int thread_mod(void *arg)
{
    uintptr_t tid = (uintptr_t)arg;
    int mytest = INIT_VAL;

    for(;;) {
        if (mytest % 100000 == 0) {
            debug_printf("%lu: %d\n", tid, mytest);
        }
        test_init++;
        mytest++;
        assert(test_init == mytest);
    }

    return 0;
}

int main(int argc, char *argv[])
{
    assert(test_bss[3] == 0);
    assert(test_init == INIT_VAL);

    void *p = &test_init;
    debug_printf("&test_init = %p\n", p);
    assert(*(int *)p == INIT_VAL);

    assert(test_extern == 42);
    test_extern_func();
    assert(test_extern == 42 * 2);

    thread_create(thread_mod, (void *)1);
    thread_create(thread_mod, (void *)2);

    thread_mod((void *)0);

    return 0;
}
