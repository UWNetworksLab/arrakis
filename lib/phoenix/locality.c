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

#include "locality.h"
#include "stddefines.h"
#include "processor.h"

#ifdef _LINUX_

#elif defined (_SOLARIS_)
#include <sys/lgrp_user.h>
#include <sys/mman.h>

#elif defined (BARRELFISH)
#else
#error OS not supported
#endif

/* Retrieve the number of processors that belong to the locality
   group of the calling LWP. */
int loc_get_lgrp_size ()
{
#ifdef _LINUX_
    /* XXX smarter implementation? have all cpus local to this thread */
    return proc_get_num_cpus ();
#elif defined (_SOLARIS_)
    int ret, num_cpus;
    lgrp_id_t lgrp;
    lgrp_cookie_t cookie;

    cookie = lgrp_init (LGRP_VIEW_CALLER);

    lgrp = lgrp_home (P_LWPID, P_MYID);
    num_cpus = lgrp_cpus (cookie, lgrp, NULL, 0, LGRP_CONTENT_DIRECT);
    assert (num_cpus > 0);

    ret = lgrp_fini (cookie);
    assert (! ret);

    return num_cpus;
#elif defined(BARRELFISH)
    return proc_get_num_cpus();
#endif
}

/* Retrieve the number of total locality groups on system. */
int loc_get_num_lgrps ()
{
#ifdef _LINUX_
    /* XXX only one locality group, all processors */
    return 1;
#elif defined (_SOLARIS_)
    int ret;
    lgrp_cookie_t cookie;
    int nlgrps;

    cookie = lgrp_init (LGRP_VIEW_CALLER);
    nlgrps = lgrp_nlgrps (cookie);
    ret = lgrp_fini (cookie);
    assert (!ret);

    if (nlgrps > 1)
    {
        /* Do not count the locality group that encompasses all the 
           locality groups. */
        nlgrps -= 1;
    }

    return nlgrps;
#elif defined (BARRELFISH)
    return 1;
#endif
}

/* Retrieve the locality group of the calling LWP. */
int loc_get_lgrp ()
{
#ifdef _LINUX_
    return 0;
#elif defined (_SOLARIS_)
    int lgrp = lgrp_home (P_LWPID, P_MYID);

    if (lgrp > 0) {
        /* On a system with multiple locality groups, there exists a
           mother locality group (lgroup 0) that encompasses all the 
           locality groups. Collapse down the hierarchy. */
        lgrp -= 1;
    }
    
    return lgrp;
#elif defined(BARRELFISH)
    return 0;
#endif
}

/* Retrieve the locality group of the physical memory that backs
   the virtual address ADDR. */
int loc_mem_to_lgrp (void *addr)
{
#ifdef _LINUX_
    /* XXX just one for now */
    return 0;
#elif defined (_SOLARIS_)
    uint_t info = MEMINFO_VLGRP;
    uint64_t inaddr;
    uint64_t lgrp;
    uint_t validity;

    if (sizeof (void *) == 4) {
        /* 32 bit. */
        inaddr = 0xffffffff & (intptr_t)addr;
    } else {
        /* 64 bit. */
        assert (sizeof (void *) == 8);
        inaddr = addr;
    }

    CHECK_ERROR(meminfo (&inaddr, 1, &info, 1, &lgrp, &validity));
    if (validity != 3)
    {
        /* VALIDITY better be 3 here. 
           If it is 1, it means the memory has been assigned, but
           not allocated yet. */
        lgrp = 1;
    }

    if (lgrp > 0) {
        /* On a system with multiple locality groups, there exists a
           mother locality group (lgroup 0) that encompasses all the 
           locality groups. Collapse down the hierarchy. */
        lgrp -= 1;
    }
    
    return lgrp;
#elif defined(BARRELFISH)
    return 0;
#endif
}
