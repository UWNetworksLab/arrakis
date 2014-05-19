/**
 * \file
 * \brief Phase-change scalability benchmark
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/resource_ctrl.h>
#include <barrelfish/sys_debug.h>
#include <bench/bench.h>

struct workcnt {
  uint64_t	cnt;
} __attribute__ ((aligned (64)));

static const char *my_manifest =
    "B 1\n"                     // Normal phase
    "B 1\n";                    // Other normal phase

static int init_done = 1;
static rsrcid_t my_rsrc_id;
static struct thread_sem init_sem = THREAD_SEM_INITIALIZER;
static struct thread_mutex init_lock = THREAD_MUTEX_INITIALIZER;
static struct thread_cond init_cond = THREAD_COND_INITIALIZER;
static struct workcnt workcnt[MAX_CPUS];

static int remote_init(void *arg)
{
    errval_t err = rsrc_join(my_rsrc_id);
    assert(err_is_ok(err));

    thread_mutex_lock(&init_lock);
    thread_sem_post(&init_sem);
    thread_cond_wait(&init_cond, &init_lock);
    thread_mutex_unlock(&init_lock);

    for(;;) {
        workcnt[disp_get_core_id()].cnt++;
    }

    return 0;
}

static void domain_spanned(void *arg, errval_t reterr)
{
    assert(err_is_ok(reterr));
    init_done++;
}

int main(int argc, char *argv[])
{
    int my_core_id = disp_get_core_id();
    errval_t err;
    uint64_t tscperms;

    if(argc < 4) {
        printf("Usage: %s threads phase-delay timeout\n", argv[0]);
    }
    int nthreads = atoi(argv[1]);
    int delay = atoi(argv[2]);
    int timeout = atoi(argv[3]);

    err = sys_debug_get_tsc_per_ms(&tscperms);
    assert(err_is_ok(err));

    bench_init();

    // Submit manifest (derived from program)
    err = rsrc_manifest(my_manifest, &my_rsrc_id);

    /* Span domain to all cores */
    for (int i = my_core_id + 1; i < nthreads + my_core_id; i++) {
        err = domain_new_dispatcher(i, domain_spanned, NULL);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "failed to span domain");
        }
        assert(err_is_ok(err));
    }

    while(init_done < nthreads) {
        thread_yield();
    }

    for (int i = my_core_id + 1; i < nthreads + my_core_id; i++) {
        err = domain_thread_create_on(i, remote_init, NULL);
        assert(err_is_ok(err));
        thread_sem_wait(&init_sem);
    }

    thread_mutex_lock(&init_lock);
    thread_cond_broadcast(&init_cond);
    thread_mutex_unlock(&init_lock);

    // Run benchmark
    bool flip = false;
    cycles_t begin = bench_tsc(), end = 0;
    for(;;) {
        if(delay != 0) {
            flip = !flip;
            err = rsrc_phase(my_rsrc_id, flip ? 1 : 0);
            assert(err_is_ok(err));
            end = bench_tsc();
        }

        // Let it cool down
        do {
            workcnt[disp_get_core_id()].cnt++;

            // Stop when timeout is reached
            if(bench_tsc() >= begin + (timeout * tscperms)) {
                goto out;
            }
        } while(bench_tsc() < end + (delay * tscperms));
    }

 out:
    for(int i = 0; i < nthreads; i++) {
        printf("workcnt %d: %lu\n", i, workcnt[i].cnt);
    }
    printf("number of threads: %d, delay: %d\n", nthreads, delay);
    printf("client done.\n");
    return 0;
}
