/**
 * \file
 * \brief Test spanning of domains across cores
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>
#include <trace/trace.h>
#include <trace_definitions/trace_defs.h>


#define LOCKDEC(x)    spinlock_t x;
#define LOCK(x)     acquire_spinlock(&x)
#define UNLOCK(x)   release_spinlock(&x)

#define BARDEC(x)                                        \
    struct {                                             \
        volatile uint32_t  counter;                      \
        volatile int   	   cycle;                        \
    } x;

#define BARINIT(x,y)                                            \
    x.counter = 0;                                              \
    x.cycle = 0;

#define NEWBARRIER(x,y) {                                              \
        int cycle = x.cycle;                                        \
        if (__sync_fetch_and_add(&x.counter, 1) == (y-1)) {         \
            x.counter = 0;                                          \
            x.cycle = !x.cycle;                                     \
        } else {                                                    \
            while (cycle == x.cycle);                               \
        }                                                           \
    }

#define BARRIER(x,y) {                                  \
        int cycle = x.cycle;                            \
        if (__sync_fetch_and_add(&x.counter, 1) == (y-1)) {       \
            x.counter = 0;                              \
            x.cycle = !cycle;                           \
        } else {                                        \
            while (cycle == x.cycle);                   \
        }                                               \
    }

static uint64_t times[32];

int    null(int);
int    malloctest(int);
int    printftest(int);
int    locktest1(int);
int    locktest4(int);
int    inctest1(int);
int    inctest4(int);
int    bartest1(int);
int    bartest4(int);

int null(int arg)
{

    return 0;
}

extern size_t terminal_write(const char *data, size_t length);

static int sysprinttest(int arg)
{
    char out = 'A'+arg;
    for(int i=0; i<1<<10; i++) {
        sys_print(&out, 1);
    }
    return 0;
}
static int termwritetest(int arg)
{
    char out = 'A'+arg;
    for(int i=0; i<1<<10; i++) {
        terminal_write(&out, 1);
    }
    return 0;
}

int printftest(int arg)
{
    char msg[2];
    msg[0] = 'A'+arg;
    msg[1] = 0;
    for(int i=0; i<1<<10; i++) {
        printf("%s", msg);
    }
    return 0;
}

int malloctest(int arg)
{
    for(int i=0; i<1<<10; i++) {
        if (arg==1) sys_print(".", 1);
        void *tmp = malloc(i);
        free(tmp);
    }
    return 0;
}

LOCKDEC(spinlock);
static uint64_t gcount = 0;

int locktest1(int arg)
{
    for(int i=0; i<1<<20; i++) {
        LOCK(spinlock);
        gcount++;
        UNLOCK(spinlock);
    }
    return 0;
}
int locktest4(int arg)
{
    for(int i=0; i<1<<20; i++) {
        LOCK(spinlock);
        gcount++;
        UNLOCK(spinlock);
        LOCK(spinlock);
        gcount++;
        UNLOCK(spinlock);
        LOCK(spinlock);
        gcount++;
        UNLOCK(spinlock);
        LOCK(spinlock);
        gcount++;
        UNLOCK(spinlock);
    }
    return 0;
}

static uint32_t atomiccounter = 0;

int inctest1(int arg)
{
    for(int i=0; i<1<<20; i++) {
        __sync_fetch_and_add(&atomiccounter, 1);
    }
    return 0;
}

int inctest4(int arg)
{
    uint64_t lcount = 0;
    char msg[128];
    for(int i=0; i<1<<20; i++) {
        lcount++;
        __sync_fetch_and_add(&atomiccounter, 1);
        lcount++;
        __sync_fetch_and_add(&atomiccounter, 1);
        lcount++;
        __sync_fetch_and_add(&atomiccounter, 1);
        lcount++;
        __sync_fetch_and_add(&atomiccounter, 1);
    }
    sprintf(msg, "%x: Count %lx %p\n", disp_get_core_id(), lcount, &lcount);
    sys_print(msg, strlen(msg));
    return 0;
}

BARDEC(barrier);
uint32_t NPROC = 0;

int bartest1(int arg)
{
    uint64_t count = 0;
    char msg[128];

    for(int i=0; i<1<<20; i++) {
        BARRIER(barrier, NPROC);
        count++;
    }
    // Take address of count to prevent optimisation
    sprintf(msg, "%x: Count %lx %p\n", disp_get_core_id(), count, &count);
    sys_print(msg, strlen(msg));
    return 0;
}

int bartest4(int arg)
{
    uint64_t count = 0;
    char msg[128];
    for(int i=0; i<1<<20; i++) {
        BARRIER(barrier, NPROC);
        count++;
        BARRIER(barrier, NPROC);
        count++;
        BARRIER(barrier, NPROC);
        count++;
        BARRIER(barrier, NPROC);
        count++;
    }
    // Take address of count to prevent optimisation
    sprintf(msg, "%x: Count %lx %p\n", disp_get_core_id(), count, &count);
    sys_print(msg, strlen(msg));
    return 0;
}

static struct thread_mutex print_mutex = THREAD_MUTEX_INITIALIZER;

static int mutextest(int arg)
{
    for(int i = 0; i < 100000; i++) {
        thread_mutex_lock(&print_mutex);
        printf("%d: test_thread %d\n", disp_get_core_id(), i);
        thread_mutex_unlock(&print_mutex);
    }

    return 0;
}

static int remote(void *dummy)
{
    uint64_t time = rdtsc();
    int core = disp_get_core_id();
    times[core] = time;
    printf("remote running on %d after %lu\n", core, time-times[0]);

    if (core == 1) sys_print("Null\n", 5);
    BARRIER(barrier, NPROC);
    //sys_print("*", 1);
    null(core);

    if (core == 1) sys_print("sys_print\n", 10);
    BARRIER(barrier, NPROC);
    sysprinttest(core);
    BARRIER(barrier, NPROC);

    if (core == 1) sys_print("terminal_write\n", 15);
    BARRIER(barrier, NPROC);
    termwritetest(core);
    BARRIER(barrier, NPROC);

    if (core == 1) sys_print("printf\n", 7);
    BARRIER(barrier, NPROC);
    printftest(core);
    BARRIER(barrier, NPROC);

    if (core == 1) sys_print("malloc\n", 7);
    BARRIER(barrier, NPROC);
    malloctest(core);
    BARRIER(barrier, NPROC);

    if (core == 1) printf("\nlocktest1\n");
    gcount = 0;

    BARRIER(barrier, NPROC);
    locktest1(core);
    BARRIER(barrier, NPROC);

    if (core == 1) {
        printf("gcount %lx\n", gcount);
        gcount = 0;
        printf("locktest4\n");
    }

    BARRIER(barrier, NPROC);
    locktest4(core);
    BARRIER(barrier, NPROC);

    if (core == 1) printf("gcount %lx\n", gcount);

    if (core == 1) printf("inctest1\n");
    atomiccounter = 0;

    BARRIER(barrier, NPROC);
    inctest1(core);
    BARRIER(barrier, NPROC);

    if (core == 1) {
        printf("count %x\n", atomiccounter);
        atomiccounter = 0;
        printf("inctest4\n");
    }

    BARRIER(barrier, NPROC);
    inctest4(core);
    BARRIER(barrier, NPROC);

    if (core == 1) {
        printf("count %x\n", atomiccounter);
        atomiccounter = 0;
        printf("bartest1\n");
    }
    BARRIER(barrier, NPROC);
    bartest1(core);
    BARRIER(barrier, NPROC);

    if (core == 1) printf("bartest4\n");

    BARRIER(barrier, NPROC);
    bartest4(core);
    BARRIER(barrier, NPROC);

    if (core == 1) printf("mutextest\n");

    BARRIER(barrier, NPROC);
    mutextest(core);
    BARRIER(barrier, NPROC);

    if (core == 1) printf("Done\n");
    
    return 0;
}

int ndispatchers = 1;

static void domain_spanned_callback(void *arg, errval_t err)
{
    //sys_print("D", 1);
    ndispatchers++;
}

int main(int argc, char *argv[])
{
    errval_t err;
    if (argc != 2) {
        printf("Usage %s: <Num additional threads>\n", argv[0]);
        exit(-1);
    }

     
    //printf("main running on %d\n", disp_get_core_id());

    int cores = strtol(argv[1], NULL, 10) + 1;

    NPROC = cores -1;
    BARINIT(barrier, NPROC);

    uint64_t before = rdtsc();
    times[0] = before;

    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 1);
    for (int i = 1; i < cores; i++) {
        err = domain_new_dispatcher(i + disp_get_core_id(), 
                                    domain_spanned_callback, 
                                    (void*)(uintptr_t)i);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "domain_new_dispatcher failed");
        }
    }

    while (ndispatchers < cores) {
        thread_yield();
    }
    uint64_t finish = rdtsc();

    trace_event(TRACE_SUBSYS_BENCH, TRACE_EVENT_BENCH_PCBENCH, 0);

    //sys_print("\nDone\n", 6);
    printf("spantest: Done in %ld cycles\n", finish-before);

    //trace_dump();

    for(int i = 1; i < cores; i++) {
        err = domain_thread_create_on(i, remote, NULL);
        assert(err_is_ok(err));
    }

    messages_handler_loop();
    return 0;
}
