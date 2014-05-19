/**
 * \file
 * \brief API to use the bomp library
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "libbomp.h"
#include "omp.h"
#include "backend.h"

unsigned bomp_num_threads = 1;
bool bomp_dynamic_behaviour = false;
bool bomp_nested_behaviour = false;

void bomp_custom_init(void)
{
    parallel_init();
    backend_init();
}

void omp_set_num_threads(int num_threads)
{
    if ( num_threads > 0 ) {
        bomp_num_threads  = num_threads;
    }
}

int omp_get_num_threads(void)
{
    return g_thread_numbers;
}

int omp_get_max_threads(void)
{
    return 1;
}

int omp_get_thread_num(void)
{
    if (g_thread_numbers == 1) {
        return 0;
    }

    struct bomp_thread_local_data *tls = backend_get_tls();
    return tls->work->thread_id;
}

int omp_get_num_procs(void)
{
    return 1;
}

void omp_set_dynamic(int dynamic_threads)
{
    if ( dynamic_threads == 0 ) {
        bomp_dynamic_behaviour  = false;
    }
    else {
        bomp_dynamic_behaviour  = true;
    }
}

int omp_get_dynamic(void)
{
    return bomp_dynamic_behaviour;
}

int omp_in_parallel(void)
{
    if(g_thread_numbers == 1) {
        return 0;
    }
    return g_thread_numbers;
}

void omp_set_nested(int nested)
{
    if (nested == 0) {
        bomp_nested_behaviour = false;
    } else {
        bomp_dynamic_behaviour = true;
    }
}

int omp_get_nested(void)
{
    return bomp_dynamic_behaviour;
}
