/**
 * \file
 * \brief Performace monitoring support for x86\
 * This should be working on Intel and AMD platforms.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef BARRELFISH_KPI_PERFMON_H
#define BARRELFISH_KPI_PERFMON_H

#include <barrelfish_kpi/lmp.h>

#define min_pm(a,b) ((a) < (b) ? (a) : (b))

// LMP_MAX_LENGTH is given in words, need to convert to bytes.
#define LMP_MAX_BYTES (LMP_MSG_LENGTH*sizeof(uint64_t)/sizeof(char))

// Make sure the size of the struct does not exceed the maximum
// length used for message transfer
#define PERFMON_DATA_LEN (min_pm(DISP_NAME_LEN, LMP_MAX_BYTES))
#define PERFMON_DISP_NAME_LEN (PERFMON_DATA_LEN-sizeof(uint64_t))

struct perfmon_overflow_data {
    uint64_t ip; // Instruction pointer when overflow was fired
    char name[PERFMON_DISP_NAME_LEN]; // Name of the running task
};

#endif //BARRELFISH_KPI_PERFMON_H

