/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*-
 * Copyright (c) 1982, 1986, 1989, 1991, 1993
 *      The Regents of the University of California.  All rights reserved.
 * (c) UNIX System Laboratories, Inc.
 * All or some portions of this file are derived from material licensed
 * to the University of California by American Telephone and Telegraph
 * Co. or Unix System Laboratories, Inc. and are reproduced herein with
 * the permission of UNIX System Laboratories, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)signal.h    8.4 (Berkeley) 5/4/95
 * $FreeBSD$
 */

#ifndef BARRELFISH_SIGNAL_H_
#define BARRELFISH_SIGNAL_H_

#include <sys/cdefs.h>
#include <sys/_types.h>
#include <sys/_sigset.h>

__BEGIN_DECLS

#define SIGHUP          1       /* hangup */
#define SIGINT          2       /* interrupt */
#define SIGQUIT         3       /* quit */
#define SIGILL          4       /* illegal instr. (not reset when caught) */
#define SIGTRAP         5       /* trace trap (not reset when caught) */
#define SIGABRT         6       /* abort() */
#define SIGIOT          SIGABRT /* compatibility */
#define SIGEMT          7       /* EMT instruction */
#define SIGFPE          8       /* floating point exception */
#define SIGKILL         9       /* kill (cannot be caught or ignored) */
#define SIGBUS          10      /* bus error */
#define SIGSEGV         11      /* segmentation violation */
#define SIGSYS          12      /* non-existent system call invoked */
#define SIGPIPE         13      /* write on a pipe with no one to read it */
#define SIGALRM         14      /* alarm clock */
#define SIGTERM         15      /* software termination signal from kill */
#define SIGURG          16      /* urgent condition on IO channel */
#define SIGSTOP         17      /* sendable stop signal not from tty */
#define SIGTSTP         18      /* stop signal from tty */
#define SIGCONT         19      /* continue a stopped process */
#define SIGCHLD         20      /* to parent on child stop or exit */
#define SIGTTIN         21      /* to readers pgrp upon background tty read */
#define SIGTTOU         22      /* like TTIN if (tp->t_local&LTOSTOP) */
#define SIGIO           23      /* input/output possible signal */
#define SIGXCPU         24      /* exceeded CPU time limit */
#define SIGXFSZ         25      /* exceeded file size limit */
#define SIGVTALRM       26      /* virtual time alarm */
#define SIGPROF         27      /* profiling time alarm */
#define SIGWINCH        28      /* window size changes */
#define SIGINFO         29      /* information request */
#define SIGUSR1         30      /* user defined signal 1 */
#define SIGUSR2         31      /* user defined signal 2 */
#define SIGTHR          32      /* reserved by thread library. */
#define SIGLWP          SIGTHR

#define SIGRTMIN        65
#define SIGRTMAX        126

#define NSIG            32      /* number of old signals (counting 0) */

#if __POSIX_VISIBLE || __XSI_VISIBLE
#define SA_NOCLDSTOP    0x0008  /* do not generate SIGCHLD on child stop */
#endif /* __POSIX_VISIBLE || __XSI_VISIBLE */

#if __XSI_VISIBLE
#define SA_ONSTACK      0x0001  /* take signal on signal stack */
#define SA_RESTART      0x0002  /* restart system call on signal return */
#define SA_RESETHAND    0x0004  /* reset to SIG_DFL when taking signal */
#define SA_NODEFER      0x0010  /* don't mask the signal we're delivering */
#define SA_NOCLDWAIT    0x0020  /* don't keep zombies around */
#define SA_SIGINFO      0x0040  /* signal handler with SA_SIGINFO args */
#endif

/* Adjusted to linux, has unused sa_restorer field and unsigned long
   sa_flags; relatively unimportant though.  */
/* Type of a signal handler.  */
typedef void (*__sighandler_t)(int);

/* The type used in newlib sources.  */
typedef __sighandler_t _sig_func_ptr;

#define SIG_ERR ((_sig_func_ptr) -1)
#define SIG_DFL ((_sig_func_ptr) 0)
#define SIG_IGN ((_sig_func_ptr) 1)
#define SIG_HOLD ((_sig_func_ptr)3)

#ifndef _PID_T_DECLARED
typedef	__pid_t		pid_t;
#define	_PID_T_DECLARED
#endif

typedef void (*signalhandler_t)(int);

#ifndef _SIGSET_T_DECLARED
#define _SIGSET_T_DECLARED
typedef __sigset_t      sigset_t;
#endif

union sigval {
        /* Members as suggested by Annex C of POSIX 1003.1b. */
        int     sival_int;
        void    *sival_ptr;
        /* 6.0 compatibility */
        int     sigval_int;
        void    *sigval_ptr;
};

typedef struct __siginfo {
        int     si_signo;               /* signal number */
        int     si_errno;               /* errno association */
        /*
         * Cause of signal, one of the SI_ macros or signal-specific
         * values, i.e. one of the FPE_... values for SIGFPE.  This
         * value is equivalent to the second argument to an old-style
         * FreeBSD signal handler.
         */
        int     si_code;                /* signal code */
        __pid_t si_pid;                 /* sending process */
        __uid_t si_uid;                 /* sender's ruid */
        int     si_status;              /* exit value */
        void    *si_addr;               /* faulting instruction */
        union sigval si_value;          /* signal value */
        union   {
                struct {
                        int     _trapno;/* machine specific trap code */
                } _fault;                                               
                struct { 
                        int     _timerid;
                        int     _overrun;
                } _timer;                
                struct { 
                        int     _mqd;
                } _mesgq;            
                struct { 
                        long    _band;          /* band event for SIGPOLL */
                } _poll;                        /* was this ever used ? */  
                struct {                                                  
                        long    __spare1__;
                        int     __spare2__[7];
                } __spare__;                  
        } _reason;
} siginfo_t;

#define si_trapno       _reason._fault._trapno
#define si_timerid      _reason._timer._timerid
#define si_overrun      _reason._timer._overrun
#define si_mqd          _reason._mesgq._mqd
#define si_band         _reason._poll._band

/* struct sigaction notes from POSIX:
 *
 *  (1) Routines stored in sa_handler should take a single int as
 *      their argument although the POSIX standard does not require this.
 *  (2) The fields sa_handler and sa_sigaction may overlap, and a conforming
 *      application should not use both simultaneously.
 */

struct sigaction {
  int         sa_flags;       /* Special flags to affect behavior of signal */
  sigset_t    sa_mask;        /* Additional set of signals to be blocked */
                              /*   during execution of signal-catching */
                              /*   function. */
  union {
    _sig_func_ptr _handler;  /* SIG_DFL, SIG_IGN, or pointer to a function */
    void      (*_sigaction)( int, siginfo_t *, void * );
  } _signal_handlers;
};

#define sa_handler    _signal_handlers._handler
#define sa_sigaction  _signal_handlers._sigaction

/*
 * Flags for sigprocmask:
 */
#define SIG_BLOCK       1       /* block specified signal set */
#define SIG_UNBLOCK     2       /* unblock specified signal set */
#define SIG_SETMASK     3       /* set specified signal set */

signalhandler_t signal(int sugnum, signalhandler_t handler);
int kill(pid_t pid, int sig);
int sigprocmask(int how, const sigset_t *set, sigset_t *oset);

int sigaddset(sigset_t *set, int signo);
int sigdelset(sigset_t *set, int signo);
int sigemptyset(sigset_t *set);
int sigfillset(sigset_t *set);
int sigismember(const sigset_t *set, int signo);
int sigaction(int signum, const struct sigaction *act,
              struct sigaction *oldact);
int raise(int sig);
int pthread_sigmask(int how, const sigset_t *set, sigset_t *oldset);
__END_DECLS

#endif // BARRELFISH_SIGNAL_H_
