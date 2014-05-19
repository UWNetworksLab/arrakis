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
 * Copyright (C) 1994-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): Joachim Schimpf, ECRC.
 * Contributor(s): Kees Schuerman, ECRC.
 * 
 * END LICENSE BLOCK */

/*---------------------------------------------------------------------
 * IDENTIFICATION	mutex.c
 *
 * CONTENTS		Code for spin locks
 *---------------------------------------------------------------------*/

#include "config.h"
#include "memman.h"

/*---------------------------------------------------------------------
 * mutex_lock_failed() is normally implemented in assembler
 * using an atomic test-and-set instruction (lock.S)
 *---------------------------------------------------------------------*/

#if (defined(lint) || defined(__APPLE__) || !(defined(m88k) || defined(sparc) || defined(mc68000) || defined(i386) || defined(mips) || defined(__alpha__) || defined(_PA_RISC1_0) || defined(_PA_RISC1_1)))

int
mutex_lock_failed(plock)	/* dummy (not atomic) */
a_mutex_t *plock;
{
    if (*plock)
	return 1;
    *plock = 1;
    return 0;
}

#endif

/*--------------------------------------------------*/
#if (defined(_PA_RISC1_0) || defined(_PA_RISC1_1))
/*--------------------------------------------------*/

/*
** 1. The HP wants a lock aligned at a 16-bytes boundary!
**    We have an array of 4 ints and use the one that is aligned.
** 2. Locked = 0 ; Unlocked = 1
*/

#define RealLock(arr)	(&(arr)[3] - (unsigned)(&(arr)[3] - (int*)0) % 4)

a_mutex_lock(mutex)
volatile a_mutex_t *mutex;
{
    volatile int *plock = RealLock(*mutex);
    Disable_Int();
    while (mutex_lock_failed(plock))
    {
	Enable_Int();
	while (!(*plock))
	    continue;
	Disable_Int();
    }
}

a_mutex_unlock(mutex)
a_mutex_t *mutex;
{
    int *plock = RealLock(*mutex);
#ifdef PRINTAM
    if (*plock) {
	(void) write(2, "INTERNAL ERROR: unlocking unlocked mutex\n", 41);
    }
#endif
    *plock = ~0;
    Enable_Int();
}

a_mutex_init(mutex)
a_mutex_t *mutex;
{
    *RealLock(*mutex) = ~0;
}

a_mutex_destroy(mutex)
a_mutex_t *mutex;
{
    *RealLock(*mutex) = ~0;
}

/*--------------------------------------------------*/
#else
/*--------------------------------------------------*/

a_mutex_lock(plock)
volatile a_mutex_t *plock;
{
    Disable_Int();
    while (mutex_lock_failed(plock))
    {
	Enable_Int();
	while (*plock)
	    continue;
	Disable_Int();
    }
}

a_mutex_unlock(plock)
a_mutex_t *plock;
{
#ifdef PRINTAM
    if (*plock == 0) {
	(void) write(2, "INTERNAL ERROR: unlocking unlocked mutex\n", 41);
    }
#endif
    *plock = 0;
    Enable_Int();
}

a_mutex_init(plock)
a_mutex_t *plock;
{
    *plock = 0;
}

a_mutex_destroy(plock)
a_mutex_t *plock;
{
    *plock = 0;
}

#endif
