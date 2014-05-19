/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef STORAGE_VSA_H
#define STORAGE_VSA_H

#ifdef BARRELFISH
#include <barrelfish/barrelfish.h>

struct storage_vsa {
    struct capref       vsacap;
    size_t		size;
};
#elif defined(__linux__)
struct storage_vsa {
    int                 fd;
    size_t		size;
};
#else
#       error "Unknown operating system!"
#endif

/**
 * \brief Allocates a virtual storage area (VSA).
 *
 * Allocates the next available VSA of the specified size. More
 * specific functions may be provided in the future that allow
 * allocating VSAs of specific characteristics (e.g., storage medium,
 * access latency, controller). An example invocation might allocate 2
 * VSAs on flash that have 2 different controllers for parallel
 * access.
 *
 * \param size  Size (in bytes) of the VSA.
 *
 * \return Error code.
 */
errval_t storage_vsa_alloc(struct storage_vsa *vsa, size_t size);

errval_t storage_vsa_acquire(struct storage_vsa *vsa, const char *name,
			     size_t size);

errval_t storage_vsa_resize(struct storage_vsa *vsa, size_t size);

#endif
