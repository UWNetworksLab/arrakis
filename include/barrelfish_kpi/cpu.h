/**
 * \file
 * \brief Architecture specific CPU bits.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef BARRELFISH_KPI_CPU_H
#define BARRELFISH_KPI_CPU_H

#ifndef __ASSEMBLER__

enum cpu_type {
    CPU_X86_64,
    CPU_X86_32,
    CPU_SCC,
    CPU_ARM7,
    CPU_ARM5,
    CPU_TYPE_NUM // must be last
};

#include <barrelfish/static_assert.h>

static inline const char *cpu_type_to_archstr(enum cpu_type cpu_type)
{
    STATIC_ASSERT(CPU_TYPE_NUM == 5, "knowledge of all CPU types here");
    switch(cpu_type) {
    case CPU_X86_64:    return "x86_64";
    case CPU_X86_32:    return "x86_32";
    case CPU_SCC:       return "scc";
    case CPU_ARM7:      return "armv7";
    case CPU_ARM5:      return "armv5";
    default:            return "(unknown)";
    }
}

#endif

// XXX: Code that needs these includes should includes should include it directly
#include <barrelfish_kpi/generic_arch.h>

#endif // BARRELFISH_KPI_CPU_H
