/**
 * \file
 * \brief Declerations of functions that differ in
 *  implementation between linux and barrelfish
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BOMP_BACKEND_H
#define BOMP_BACKEND_H

typedef int (*bomp_thread_func_t)(void *);

void backend_set_numa(unsigned id);
void backend_run_func_on(int core_id, void* cfunc, void *arg);
void* backend_get_tls(void);
void backend_set_tls(void *data);
void* backend_get_thread(void);
void backend_init(void);
void backend_thread_exit(void);
struct thread *backend_thread_create_varstack(bomp_thread_func_t start_func,
                                              void *arg, size_t stacksize);

#endif /* BOMP_BACKEND_H */
