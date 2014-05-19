/**
 * \file
 * \brief Intel 64 performance monitoring infrastructure.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdint.h>

#include <dev/cpuid_dev.h>
#include <dev/ia32_dev.h>

#include <arch/x86/perfmon_intel.h>

static struct cpuid_t mycpuid;
static struct ia32_t ia32;

errval_t perfmon_intel_init(void)
{
    cpuid_initialize(&mycpuid);
    ia32_initialize(&ia32);

    if(cpuid_max_biv_rd(&mycpuid) < 0xa) {
        printf("Intel Architectural Performance Monitoring not supported!"
               "cpuid_max_biv_rd is %"PRIu32"\n", cpuid_max_biv_rd(&mycpuid));
        return SYS_ERR_PERFMON_NOT_AVAILABLE;
    }

    cpuid_apm_gen_t apmgen = cpuid_apm_gen_rd(&mycpuid);

    printf("Architectural Performance Monitoring version %d, # counters %d "
           "(%d bits wide), # events %d\n",
           apmgen.version, apmgen.num_counters, apmgen.width,
           apmgen.vec_length);

    if(apmgen.version >= 2) {
        cpuid_apm_fixed_t apmfixed = cpuid_apm_fixed_rd(&mycpuid);
        printf("# fixed function counters %d (%d bits wide)\n",
               apmfixed.num, apmfixed.width);
    }

    char str[256];
    cpuid_apm_feat_pr(str, 256, &mycpuid);
    printf("Supported events:\n%s\n", str);

    uint32_t apmfeat = cpuid_apm_feat_rd_raw(&mycpuid);
    printf("Supported events: %x\n", apmfeat);

    uint32_t status = ia32_perf_global_ctrl_rd(&ia32);
    printf("Enabling counter %d, old val %x\n", 0, status);
    ia32_perf_global_ctrl_pmc0_wrf(&ia32, 1);

    return SYS_ERR_OK;
}

void perfmon_intel_reset(void)
{
    uint32_t status = ia32_perf_global_status_rd(&ia32);
    ia32_perf_global_over_wr(&ia32, status);
}

void perfmon_intel_measure_start(uint8_t event, uint8_t umask, bool os, 
                                 uint8_t idx, bool intr)
{
    ia32_perfevtsel_t sel0 = ia32_perfevtsel_default;
    sel0 = ia32_perfevtsel_evsel_insert(sel0, event);
    sel0 = ia32_perfevtsel_umask_insert(sel0, umask);
    sel0 = ia32_perfevtsel_usr_insert(sel0, 1);
    sel0 = ia32_perfevtsel_os_insert(sel0, os ? 1 : 0);
    sel0 = ia32_perfevtsel_intr_insert(sel0, intr ? 1 : 0);
    sel0 = ia32_perfevtsel_en_insert   (sel0, 1);
    ia32_perfevtsel0_wr(&ia32, sel0);
}

uint64_t perfmon_intel_measure_read(void)
{
    return ia32_pmc0_rd(&ia32);
}

void perfmon_intel_measure_write(uint64_t val)
{
    ia32_pmc0_wr(&ia32, val);
}
