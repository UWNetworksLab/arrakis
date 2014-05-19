/**
 * \file
 * \brief ABI support glue for thread-local storage
 */

/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include "threads_priv.h"

struct tls_index {
    uintptr_t ti_module;
    uintptr_t ti_offset;
};

#  if defined(__i386__)
void * __attribute__ ((__regparm__ (1))) ___tls_get_addr(struct tls_index *ti);
void * __attribute__ ((__regparm__ (1))) ___tls_get_addr(struct tls_index *ti)
#  else // x86_64
void *__tls_get_addr(struct tls_index *ti);
void *__tls_get_addr(struct tls_index *ti)
#  endif
{
    struct thread *me = thread_self();
    assert(me->tls_dtv != NULL); // TLS-using program
    assert(ti->ti_module == 1); // TLS block in initial program (dynamic libs NYI)
    char *tls_block = me->tls_dtv->dtv[ti->ti_module - 1];
    return tls_block + ti->ti_offset;
}
