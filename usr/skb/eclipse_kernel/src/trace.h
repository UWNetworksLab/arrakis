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
 * Copyright (C) 1995-2006 Cisco Systems, Inc.  All Rights Reserved.
 * 
 * Contributor(s): 
 * 
 * END LICENSE BLOCK */

/**********************************************************************
**      System: Parallel Eclipse
**        File: trace.h
**      Author: Shyam Mudambi
** Description: Header file for Event logging definitions and macros
**
**********************************************************************/

#if defined(HAVE_GETHRTIME) || defined(HAVE_SGIHRCLOCK)
#define WORKER_TRACING
#ifdef HAVE_SGIHRCLOCK
#ifdef HAVE_SGI64BITCLOCK
typedef unsigned long long hrtime_t;
#else
typedef unsigned int hrtime_t;
#endif
extern __psunsigned_t cycleval;
extern volatile hrtime_t * timer_addr;
#define gethrtime()  (*timer_addr) 
#endif  /* HAVE_SGIHRCLOCK */
#endif /* HAVE_GETHRTIME || HAVE_SGIHRCLOCK */

/* Events to be logged */
#define IDLE 0
#define WORKING 1
#define SCHEDULING 2
#define COPY_TO 3
#define COPY_FROM 4
#define SCH_CUT 5
#define SCH_BACKTRACK 6
#define SCH_ASYNC 7
#define ENG_RECOMPUTING 8
#define TRACE_BUF_OVERFLOW 9

#define MAX_NUM_EVENTS 10
#define MAX_NESTING 20

#ifdef WORKER_TRACING

typedef struct
{
  int event;
  hrtime_t time;
} trace_event_t;

#define MAX_EVENTS 10000

typedef struct
{  
  int entries;
  hrtime_t start_time;
} trace_header_t;

extern trace_header_t theader;
extern trace_event_t events[MAX_EVENTS];
extern volatile int tracing;


#define Log_Current_Activity(EVENT)				\
{								\
    hrtime_t now = gethrtime();					\
    wstat_.cumulated_event_time[wstat_.current_activity] += \
	    now - wstat_.current_activity_start;		\
    wstat_.current_activity = EVENT;			\
    wstat_.current_activity_start = now;			\
    if (tracing)						\
    {								\
	events[theader.entries % MAX_EVENTS].event = EVENT;	\
	events[theader.entries % MAX_EVENTS].time = now;	\
	theader.entries++;					\
	if ((theader.entries % MAX_EVENTS) == (MAX_EVENTS - 1))	\
	    write_trace_buffer(1);				\
    }								\
}

#define LOG_INIT(EVENT)		log_init(EVENT);

#define LOG_EVENT(EVENT)					\
{								\
    Disable_Int();						\
    Log_Current_Activity(EVENT);				\
    Enable_Int();						\
}

#define LOG_EVENT_PUSH(EVENT)					\
{								\
    Disable_Int();						\
    wstat_.trace_stack[wstat_.trace_stack_ptr++] =	\
		wstat_.current_activity;			\
    Log_Current_Activity(EVENT);				\
    Enable_Int();						\
}


#define LOG_EVENT_POP						\
{								\
    Disable_Int();						\
    --wstat_.trace_stack_ptr;				\
    Log_Current_Activity(wstat_.trace_stack[wstat_.trace_stack_ptr]); \
    Enable_Int();						\
}

#else /* if !WORKER_TRACING */

#define LOG_INIT(EVENT)
#define LOG_EVENT(EVENT)
#define LOG_EVENT_PUSH(EVENT)
#define LOG_EVENT_POP

#endif


struct worker_stat {
    int		job_count,
		copy_from_bytes,
		copy_to_bytes,
		copy_to_extra,
    		cut_count,
    		prune_count,
    		parallel_chpts,
    		publish_count,
    		parallel_alts,
    		published_chpts,
    		published_alts,
    		polls_expired,
    		polls_succeeded,
		rpc_sleep,
    		copy_more_count,
    		copy_from_count,
    		copy_to_count;

    double	times[3];

#ifdef WORKER_TRACING
    hrtime_t	current_activity_start;
    hrtime_t	cumulated_event_time[MAX_NUM_EVENTS];
    int		current_activity;
    int		trace_stack_ptr;
    int		trace_stack[MAX_NESTING];
#endif
};

/* Machine independent worker_stat definition -
   replace hr_time_t by doubles */
struct worker_stat_ext {
    int		job_count,
		copy_from_bytes,
		copy_to_bytes,
		copy_to_extra,
    		cut_count,
    		prune_count,
    		parallel_chpts,
    		publish_count,
    		parallel_alts,
    		published_chpts,
    		published_alts,
    		polls_expired,
    		polls_succeeded,
		rpc_sleep,
    		copy_more_count,
    		copy_from_count,
    		copy_to_count;

    double	times[3];

    double	current_activity_start;
    double	cumulated_event_time[MAX_NUM_EVENTS];
    int		current_activity;
    int		trace_stack_ptr;
    int		trace_stack[MAX_NESTING];
};

extern struct worker_stat_ext wstat_ext;
extern struct worker_stat wstat_;
