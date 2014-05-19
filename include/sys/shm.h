/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SYS_SHM_H
#define SYS_SHM_H

#include <sys/cdefs.h>
#include <sys/ipc.h>
#include <sys/_types.h>
#include <time.h>
#include <barrelfish_kpi/paging_arch.h>

#define SHM_RDONLY 010000
#define SHM_RND 020000
#define SHMLBA BASE_PAGE_SIZE

#define SHM_R (IPC_R)
#define SHM_W (IPC_W)

#define	SHM_LOCK 11
#define	SHM_UNLOCK 12

#define	SHM_STAT 13
#define	SHM_INFO 14

#ifndef _PID_T_DECLARED
typedef	__pid_t		pid_t;
#define	_PID_T_DECLARED
#endif

#ifndef _TIME_T_DECLARED
typedef	__time_t	time_t;
#define	_TIME_T_DECLARED
#endif

#ifndef _SIZE_T_DECLARED
typedef	__size_t	size_t;
#define	_SIZE_T_DECLARED
#endif

struct shmid_ds {
    struct ipc_perm shm_perm;
    size_t shm_segsz;
    pid_t shm_lpid;
    pid_t shm_cpid;
    int shm_nattch;
    time_t shm_atime;
    time_t shm_dtime;
    time_t shm_ctime;
};

#ifndef _SIZE_T_DECLARED
typedef __size_t        size_t;
#define _SIZE_T_DECLARED
#endif

__BEGIN_DECLS

void *shmat(int, const void *, int);
int shmget(key_t, size_t, int);
int shmctl(int, int, struct shmid_ds *);
int shmdt(const void *);

__END_DECLS

#endif
