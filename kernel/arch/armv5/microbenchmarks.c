/**
 * \file
 * \brief Architecture-specific microbenchmarks.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <microbenchmarks.h>

struct microbench arch_benchmarks[] = {
};

size_t arch_benchmarks_size = sizeof(arch_benchmarks) / sizeof(struct microbench);
