/**
 * \file
 * \brief User-space Microbenchmarks.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/dispatch.h>

/* #define MINIMAL_BARRELFISH */

#ifndef BENCH_POSIX
#       include <barrelfish/barrelfish.h>
#       include <barrelfish_kpi/syscalls.h>
#       include <barrelfish_kpi/types.h>
#       include <barrelfish/syscalls.h>
#       include <barrelfish/sys_debug.h>
// #       include <barrelfish/backwards/idc.h>
#       include <stdarg.h>
#       include <string.h>
#else
#       include <sys/types.h>
#       include <unistd.h>
#endif

#define MICROBENCH_ITERATIONS 1000000

typedef void (*Benchmark)(void);

#ifdef BENCH_POSIX

static inline uint64_t rdtsc(void)
{
    uint64_t eax, edx;
    __asm volatile ("rdtsc" : "=a" (eax), "=d" (edx));
    return (edx << 32) | eax;
}

#else

// Dummy mcount() that does nothing. For profiling.
static __attribute__ ((unused,no_instrument_function)) void mcount(void)
{
}

#ifdef MINIMAL_BARRELFISH

static int myprintf(const char *fmt, ...)
{
    va_list     argptr;
    char        str[256];
    int         ret;

    va_start(argptr, fmt);
    ret = vsnprintf(str, sizeof(str), fmt, argptr);
    sys_print(str, strlen(str));
    va_end(argptr);

    return ret;
}

#       define printf  myprintf

#endif

#endif

static uint64_t
divide_round(uint64_t quotient, uint64_t divisor)
{
    if ((quotient % divisor) * 2 >= divisor) {
        // round up
        return (quotient / divisor) + 1;
    } else {
        return (quotient / divisor);
    }
}

static inline void print_result(uint64_t result)
{
    printf("%" PRIu64 " ticks. Done %u iterations in %" PRIu64 " ticks.\n",
           divide_round(result, MICROBENCH_ITERATIONS), MICROBENCH_ITERATIONS,
           result);
}

static void syscall_benchmark(void)
{
    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
#ifndef BENCH_POSIX
        sys_nop();
#else
        getpid();
#endif
    }
}

static void rdtsc_benchmark(void)
{
    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        rdtsc();
    }
}

#ifdef __scc__
static void cl1flushmb_benchmark(void)
{
    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        cl1flushmb();
    }
}
#endif

static void xchg_benchmark(void)
{
    uint32_t mem = 1, reg = 7;

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__(
            "xchg %0, %1"
            : "=a" (reg), "=m" (mem)
            : "a" (reg));
    }
}

static void cmpxchg_benchmark(void)
{
    uint32_t src = 5, dest = 5, reg = 5;

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__(
            "lock cmpxchg %1, %2"
            : "=a" (reg)
            : "q"(src), "m"(dest), "0"(reg)
            : "memory");
    }
}

static void dec_benchmark(void)
{
    uint32_t val = 1000000;

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__(
            "lock dec{l} %0"
            : "+m" (val)
            :
            : "memory");
    }
}

#if 0

/*
 * XXX: These two benchmarks are not implemented safely (xor'ing rdx,
 * going back to C and then assuming rdx is still zero doesn't work)
 * and do not currently compile this way. I'm disabling them until
 * someone fixes them.
 */

static void bts_benchmark(void)
{
    uint64_t mem = 3;

    __asm__ __volatile__ ("xor %%rdx, %%rdx" ::);

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__ (
            "lock bts %%rdx,%0"
            : /* no output */
            : "m" (mem)
            : "%rdx");
    }
}

static void bts_clr_benchmark(void)
{
    uint64_t mem = 3;

    __asm__ __volatile__("xor %%rdx,%%rdx");

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__ (
            "lock bts %%rdx,%0\n\t"
            "movq $0,%0\n\t"
            : /* no output */
            : "m" (mem)
            : "%rdx");
    }
}

#endif

static void clr_benchmark(void)
{
    uint64_t mem;

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__ (
            "movl $0,%0\n\t"
            : /* no output */
            : "m" (mem)
            : "%rax");
    }
}

#ifdef __scc__
#       include <barrelfish_kpi/shared_mem_arch.h>

#       define LUT_SIZE                 0x1000000

static void clr_shared_benchmark(void)
{
    uintptr_t *mem;
    uint64_t start, end;

    for(int mc = 0; mc < 4; mc++) {
        struct capref frame;
        size_t framesize = BASE_PAGE_SIZE;
    ram_set_affinity(SHARED_MEM_MIN + mc * LUT_SIZE,
                     SHARED_MEM_MIN + (mc + 1) * LUT_SIZE);
    errval_t err = frame_alloc(&frame, framesize, &framesize);
    assert(err_is_ok(err));
    ram_set_affinity(0, 0);

    // map it in
    void *buf;
    /* err = vspace_map_one_frame_attr(&buf, framesize, frame, */
    /*                                 VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL); */
    err = vspace_map_one_frame_attr(&buf, framesize, frame,
                                    VREGION_FLAGS_READ_WRITE_MPB, NULL, NULL);
    assert(err_is_ok(err));

    mem = buf;

    printf("clear shared memory MC %d:\n", mc);

    start = rdtsc();
    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        __asm__ __volatile__ (
                              "movl $0,%0\n\t"
                              "movl $0,4%0\n\t"
                              "movl $0,8%0\n\t"
                              "movl $0,12%0\n\t"
                              "movl $0,16%0\n\t"
                              "movl $0,20%0\n\t"
                              "movl $0,24%0\n\t"
                              "movl $0,28%0\n\t"
                              : /* no output */
                              : "m" (*mem)
                              : "%rax");
    }

    end = rdtsc();
    print_result(end - start);
    }
}
#endif

#ifndef BENCH_POSIX

static void rdtscp_benchmark(void)
{
    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        rdtscp();
    }
}

#if 0   // XXX: update to new IDC system to make it work
static struct capref capaddr;

static void capinvoke_benchmark(void)
{
    struct idc_send_msg msg;

    idc_msg_init(&msg);

    for(int i = 0; i < MICROBENCH_ITERATIONS; i++) {
        cap_invoke(capaddr, &msg);
    }
}

static void idc_benchmark(void)
{
    struct idc_send_msg msg;
    struct idc_recv_msg rmsg;
    struct idc_endpoint *ep;
    struct capref epcap;

    int r = endpoint_create(NULL, NULL, DEFAULT_IDC_BUF_WORDS, &epcap, &ep);
    assert(r == 0);

    idc_msg_init(&msg);

    uint64_t minres = 0, maxres = 0, sum = 0;
    for(int i = 0; i < 1000; i++) {
        uint64_t start = rdtsc();
        cap_invoke(epcap, &msg);
        messages_idc_wait(ep);
        idc_endpoint_poll(ep, &rmsg, NULL);
        uint64_t duration = rdtsc() - start;

        sum += duration;
        if(minres == 0 && maxres == 0) {
            minres = maxres = duration;
        } else {
            if(duration < minres) {
                minres = duration;
            } else {
                if(duration > maxres) {
                    maxres = duration;
                }
            }
        }
    }

    uint64_t variance = maxres - minres;
    printf("min: %" PRIu64 " ticks, max: %" PRIu64 " ticks, "
           "variance: %" PRIu64 " ticks, avg: %" PRIu64 " ticks\n",
           minres, maxres, variance, divide_round(sum, 1000));
}
#endif

#endif

static uint64_t benchmark(Benchmark bench)
{
    uint64_t start, end;

    start = rdtsc();
    bench();
    end = rdtsc();

    return end - start;
}

int main(int argc, char *argv[])
{
    printf("bench running on core %d.\n", disp_get_core_id());

    printf("NOP system call: ");
    print_result(benchmark(syscall_benchmark));

#ifdef __scc__
    printf("CL1FLUSHMB instruction: ");
    print_result(benchmark(cl1flushmb_benchmark));

    clr_shared_benchmark();
#endif

    printf("RDTSC instruction: ");
    print_result(benchmark(rdtsc_benchmark));

#ifndef BENCH_POSIX
    printf("RDTSCP instruction: ");
    print_result(benchmark(rdtscp_benchmark));

    printf("XCHG instruction: ");
    print_result(benchmark(xchg_benchmark));

    printf("LOCK CMPXCHG instruction: ");
    print_result(benchmark(cmpxchg_benchmark));

    printf("LOCK DEC instruction: ");
    print_result(benchmark(dec_benchmark));

#if 0
    printf("atomic test & set: ");
    print_result(benchmark(bts_benchmark));

    printf("atomic test & set when cleared: ");
    print_result(benchmark(bts_clr_benchmark));
#endif

    printf("clear memory: ");
    print_result(benchmark(clr_benchmark));

#if 0 /* FIXME: change this code to avoid hardcoded caddr manipulation! */
    printf("NULL cap invocation (1 level deep): ");
    // 4th entry in rootcn
    capaddr = 4U << 26;
    print_result(benchmark(capinvoke_benchmark));

    printf("NULL cap invocation (2 levels deep): ");
    // last (64th) entry in taskdircn
    capaddr = 63U << 6;
    print_result(benchmark(capinvoke_benchmark));
#endif

#if 0   // XXX: update to new IDC system to make it work
    printf("NULL cap invocation (3 levels deep): ");
    // 1st entry in taskcn
    //capaddr = CPTR_NULL;
    print_result(benchmark(capinvoke_benchmark));

    printf("Local IDC: ");
    idc_benchmark();
#endif

//     printf("Async IDC: %lu ticks\n",
//            divide_round(benchmark(asyncidc_benchmark), 2));

/*     printf("Local IDC w/ cap transfer: "); */
/*     localcaptransfer_benchmark(); */
#endif

    printf("End of benchmarks.\n");
    return 0;
}
