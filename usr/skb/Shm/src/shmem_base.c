/* BEGIN LICENSE BLOCK
 * Version: CMPL 1.1
 *
 * The contents of this file are subject to the Cisco-style Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file except
 * in compliance with the License.  You may obtain a copy of the License
 * at www.eclipse-clp.org/license.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
 * the License for the specific language governing rights and limitations
 * under the License. 
 * 
 * The Original Code is  The ECLiPSe Constraint Logic Programming System. 
 * The Initial Developer of the Original Code is  Cisco Systems, Inc. 
 * Portions created by the Initial Developer are
 * Copyright (C) 1992-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * 
 * END LICENSE BLOCK */

/*---------------------------------------------------------------------
 * IDENTIFICATION	shared_mem_base.c
 *			(ar truncates the name, therefore shmem_base.c)
 *
 * VERSION		$Id: shmem_base.c,v 1.1.1.1 2006/09/23 01:56:26 snovello Exp $
 *
 * CONTENTS		Computation of the shm base address
 *---------------------------------------------------------------------*/

#include "config.h"

#ifndef _WIN32

#ifdef HAVE_UNISTD_H
#include <unistd.h>	/* need the sbrk() declaration */
#endif

#ifdef HAVE_STRING_H
#include <string.h>
#endif

#ifdef STDC_HEADERS
#include <stdlib.h>
#else
extern long strtol();
extern char *getenv();
#endif

#define MB			1048576
#define KB			1024


/*
 * Return a start address that can be used to map a lot of shared memory.
 * This is very machine-dependent.
 * The value is chosen taking into account the needs of eclipse:
 * There must be at least space for a private heap and two mapped
 * stacks between the brk and the start of shared memory.
 * A return value of NULL indicates that the choice should be left
 * to the operating system.
 * This function is in a separate file so that it can be replaced
 * if the underlying architecture has a different memory layout.
 */

char *
shared_mem_base(void)
{
    char *base, *env_shmbase;

    env_shmbase = getenv("ECLIPSE_SHMBASE");
    if (env_shmbase)
    {
	base = (char *) strtol(env_shmbase, 0, 0);
    }
    else
    {
#ifdef _AIX
    /* There is an unmappable area between the program and the
       large mappable pool, so we use fixed addresses. */
	base = (char *) 0x40000000;
#else
#ifdef mips
    /* sgi_irix52 (mips) starts the program at 0x10000000 */
	base = (char *) 0x30000000;
#else
#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
	/* HP 9000/700 can only MAP_SHARED between 80000000 and efffffff.
	 * A range that is MAP_SHARED in one process is reserved by the OS
	 * for all processes (in case they want to map it there as well).
	 * Therefore MAP_FIXED does almost never work, and it is best
	 * to let the OS choose the address.
	 */
	base = (char *) 0;
#else
#if (defined(sun) && defined(mc68000))
    /* max virtual address on sun 3 is somewhat above 200 MB */
	base = (char *) (161*MB);
#else
#if (defined(linux) && defined(__ELF__))
#ifdef __alpha
        base = (char *) 0x200000000;
#else
        base = (char *) 0x20000000;
#endif
#else
#ifdef __alpha
	base = (char *) 0x55000000;
#else
    /* max virtual address on sparc is around 512 MB */
	base = (char *) (320*MB);
#endif
#endif
#endif
#endif
#endif
#endif
    }

    /* make sure the address is somewhat above the brk */
    if (base && (char *) sbrk(0) + 16*MB >= base)
    {
	(void) write(2, "ECLiPSe: bad memory layout\n", 27);
    }
    return base;
}

#endif /* _WIN32 */
