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

/* OS specific headers and defines. */
#ifdef _LINUX_
#define _GNU_SOURCE
#include <sched.h>

#elif defined (_SOLARIS_)
#include <sys/procset.h>
#include <sys/processor.h>
#include <sys/lgrp_user.h>

#elif defined (BARRELFISH)
#else
#error OS not supported
#endif

#include <stdlib.h>
#include <sys/types.h>
#include <assert.h>

#include "processor.h"
#include "memory.h"

int num_cpus = 2;

/* Query the number of CPUs online. */
int proc_get_num_cpus (void)
{
    return num_cpus;

    /* int num_cpus; */
    /* char *num_proc_str; */

    /* num_cpus = sysconf(_SC_NPROCESSORS_ONLN); */

    /* /\* Check if the user specified a different number of processors. *\/ */
    /* if ((num_proc_str = getenv("MAPRED_NPROCESSORS"))) */
    /* { */
    /*     int temp = atoi(num_proc_str); */
    /*     if (temp < 1 || temp > num_cpus) */
    /*         num_cpus = 0; */
    /*     else */
    /*         num_cpus = temp; */
    /* } */

    /* return num_cpus; */
}

#ifdef _LINUX_
static cpu_set_t    full_cs;
static cpu_set_t* proc_get_full_set(void)
{
    static int          inited = 0;

    if (inited == 0) {
        int i;
        int n_cpus;

        CPU_ZERO (&full_cs);
        n_cpus = sysconf(_SC_NPROCESSORS_ONLN);
        for (i = 0; i < n_cpus; i++) {
            CPU_SET(i, &full_cs);
        }

        inited = 1;
    }

    return &full_cs;
}
#endif

/* Bind the calling thread to run on CPU_ID. 
   Returns 0 if successful, -1 if failed. */
int proc_bind_thread (int cpu_id)
{
#ifdef _LINUX_
    cpu_set_t   cpu_set;

    CPU_ZERO (&cpu_set);
    CPU_SET (cpu_id, &cpu_set);

    return sched_setaffinity (0, sizeof (cpu_set), &cpu_set);
#elif defined (_SOLARIS_)
    return processor_bind (P_LWPID, P_MYID, cpu_id, NULL);
#elif defined (BARRELFISH)
    return 0;
#endif
}

int proc_unbind_thread ()
{
#ifdef _LINUX_
    return sched_setaffinity (0, sizeof (cpu_set_t), proc_get_full_set());
#elif defined (_SOLARIS_)
    return processor_bind (P_LWPID, P_MYID, PBIND_NONE, NULL);
#elif defined (BARRELFISH)
    return 0;
#endif
}

/* Test whether processor CPU_ID is available. */
bool proc_is_available (int cpu_id)
{
#ifdef _LINUX_
    int ret;
    cpu_set_t cpu_set;
    
    ret = sched_getaffinity (0, sizeof (cpu_set), &cpu_set);
    if (ret < 0) return false;

    return CPU_ISSET (cpu_id, &cpu_set) ? true : false;
#elif defined (_SOLARIS_)
    return (p_online (cpu_id, P_STATUS) == P_ONLINE);
#elif defined (BARRELFISH)
    return true;
#endif
}

int proc_get_cpuid (void)
{
#ifdef _LINUX_
    int i, ret;
    cpu_set_t cpu_set;
    
    ret = sched_getaffinity (0, sizeof (cpu_set), &cpu_set);
    if (ret < 0) return -1;

    for (i = 0; i < CPU_SETSIZE; ++i)
    {
        if (CPU_ISSET (i, &cpu_set)) break;
    }
    return i;
#elif defined (_SOLARIS_)
    return getcpuid ();
#elif defined (BARRELFISH)
    return disp_get_core_id();
#endif
}
