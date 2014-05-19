/* Copyright (c) 2007-2009, Stanford University
* All rights reserved.
*
* Redistribution and use in source and binary forms, with or without
* modification, are permitted provided that the following conditions are met:
*     * Redistributions of source code must retain the above copyright
*       notice, this list of conditions and the following disclaimer.
*     * Redistributions in binary form must reproduce the above copyright
*       notice, this list of conditions and the following disclaimer in the
*       documentation and/or other materials provided with the distribution.
*     * Neither the name of Stanford University nor the names of its 
*       contributors may be used to endorse or promote products derived from 
*       this software without specific prior written permission.
*
* THIS SOFTWARE IS PROVIDED BY STANFORD UNIVERSITY ``AS IS'' AND ANY
* EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
* WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
* DISCLAIMED. IN NO EVENT SHALL STANFORD UNIVERSITY BE LIABLE FOR ANY
* DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
* (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
* LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
* ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
* SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/ 

#include <assert.h>

#include "scheduler.h"
#include "locality.h"
#include "processor.h"

/* TODO: Detect this automatically. */
#ifdef _SOLARIS_
#define NUM_CORES_PER_CHIP      8
#define NUM_STRANDS_PER_CORE    8
#endif

static int map_fill_strand(sched_policy* sp, int thr);
static int map_fill_core(sched_policy* sp, int thr);
static int map_fill_chip(sched_policy* sp, int thr);

struct sched_policy
{
    int     num_cpus;
    int     num_chips_per_sys;
    int     (*map)(sched_policy* sp, int thr_idx);
};

struct sched_policy policies[SCHED_POLICY_LAST + 1] = {
    { .map = map_fill_strand },
    { .map = map_fill_core },
    { .map = map_fill_chip },
};

sched_policy* sched_policy_get(unsigned int policy)
{
    assert (policy <= SCHED_POLICY_LAST);
    /* XXX make this configurable */
    policies[policy].num_cpus = proc_get_num_cpus();
    policies[policy].num_chips_per_sys = loc_get_num_lgrps ();
    return &policies[policy];
}

void sched_policy_put(sched_policy* sp)
{
    /* STUB */
}

/**
 * Given a thread index, gives the cpu it should be assigned to.
 */
int sched_thr_to_cpu(sched_policy* sp, int thr)
{
    return sp->map(sp, thr);
}

static int map_fill_strand(sched_policy* sp, int thr)
{
    int num_cpus = sp->num_cpus;
    return (thr % num_cpus);
}

static int map_fill_core(sched_policy* sp, int thr)
{
    int num_cpus = sp->num_cpus;

#ifdef NUM_CORES_PER_CHIP
    int core, strand;

    thr %= num_cpus;
    core = thr % (NUM_CORES_PER_CHIP * sp->num_chips_per_sys);
    strand = (thr / (NUM_CORES_PER_CHIP * sp->num_chips_per_sys));
    strand %= NUM_STRANDS_PER_CORE;
    return (core * NUM_STRANDS_PER_CORE + strand);
#else
    return thr % num_cpus;
#endif
}

static int map_fill_chip(sched_policy* sp, int thr)
{
    int num_cpus = sp->num_cpus;

#ifdef NUM_CORES_PER_CHIP
    int chip,  core, strand;

    thr %= num_cpus;

    chip = thr % sp->num_chips_per_sys;
    core = (thr / sp->num_chips_per_sys) % NUM_CORES_PER_CHIP;
    strand = thr / (NUM_CORES_PER_CHIP * NUM_STRANDS_PER_CORE);
    strand %= NUM_STRANDS_PER_CORE;

    return (chip * (NUM_CORES_PER_CHIP * NUM_STRANDS_PER_CORE) +
            core * (NUM_STRANDS_PER_CORE) +
            strand);
#else
    return thr % num_cpus;
#endif
}
