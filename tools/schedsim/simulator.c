/**
 * \file
 * \brief Scheduler system simulator
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>

/***** Prerequisite definitions copied from Barrelfish headers *****/

#define trace_event(x,y,z)

#define DISP_NAME_LEN   16

enum task_type {
    TASK_TYPE_BEST_EFFORT,
    TASK_TYPE_SOFT_REALTIME,
    TASK_TYPE_HARD_REALTIME
};

enum objtype {
    ObjType_CNode,
    ObjType_EndPoint
};

struct capability {
    enum objtype        type;
};

struct cte {
    struct capability cap;
};

typedef uintptr_t dispatcher_handle_t;

struct dispatcher_shared_generic {
    char        name[DISP_NAME_LEN];///< Name of domain, for debugging purposes
};

struct dcb {
    dispatcher_handle_t disp;
    struct cte          cspace;
    struct cte          ep;
    size_t              vspace;
    struct dcb          *next;          ///< Next DCB in schedule
    unsigned long       release_time, etime, last_dispatch;
    unsigned long       wcet, period, deadline;
    unsigned short      weight;
    enum task_type      type;

    // Simulator state
    int                 id;
    bool                dispatched;
    unsigned long       blocktime;
    struct dispatcher_shared_generic dsg;
};

static void panic(const char *msg, ...)
{
    va_list ap;

    fprintf(stderr, "Scheduler panic: ");
    va_start(ap, msg);
    vfprintf(stderr, msg, ap);
    va_end(ap);
    fprintf(stderr, "\n");

    exit(EXIT_FAILURE);
}

static __attribute__ ((unused)) struct dispatcher_shared_generic *
get_dispatcher_shared_generic(dispatcher_handle_t handle)
{
    return (struct dispatcher_shared_generic *)handle;
}

static size_t kernel_now = 0;
static int kernel_timeslice = 80;
static struct dcb *dcb_current = NULL;

/***** Including scheduler C file *****/

#include "../../kernel/schedule_rbed.c"

/***** Simulator internal definitions *****/

#define MAXTASKS        10

static struct dcb *sched, **allptrs;

static void init_dcb(struct dcb *dcb, int id)
{
    dcb->disp = (uintptr_t)&dcb->dsg;
    dcb->cspace.cap.type = ObjType_CNode;
    dcb->ep.cap.type = ObjType_EndPoint;
    dcb->vspace = 1;
    dcb->next = NULL;
    dcb->release_time = 0;
    dcb->wcet = 0;
    dcb->period = 0;
    dcb->weight = 0;
    dcb->etime = 0;

    dcb->id = id;
    snprintf(dcb->dsg.name, DISP_NAME_LEN, "%d", id);
}

static inline char typechar(enum task_type type)
{
    switch(type) {
    case TASK_TYPE_HARD_REALTIME:
        return 'h';

    case TASK_TYPE_SOFT_REALTIME:
        return 's';

    case TASK_TYPE_BEST_EFFORT:
        return 'b';

    default:
        printf("unknown task type!\n");
        abort();
        break;
    }
}

int main(int argc, char **argv)
{
    int tasks = 0, alltasks = MAXTASKS, runtime, quantum = 1;

    if(argc < 3) {
        printf("Usage: %s <config.cfg> <runtime> [quantum]\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    runtime = atoi(argv[2]);
    if(argc >= 4) {
        quantum = atoi(argv[3]);
    }

    sched = malloc(sizeof(struct dcb) * runtime * alltasks);
    allptrs = calloc(alltasks, sizeof(struct dcb *));

    FILE *f = fopen(argv[1], "r");
    assert(f != NULL);
    bool readline = true;

    for(kernel_now = 0; kernel_now < runtime; kernel_now++) {
        unsigned long time, wcet, period, weight, id, blocktime, deadline, rd;
        char b[512], *r;

        for(;;) {
            if(readline) {
                do {
                    r = fgets(b, 512, f);
                } while(r != NULL && (b[0] == '#' || b[0] == '\n'));

                if(r == NULL) {
                    break;
                }
            } else {
                readline = true;
            }

            if((rd = sscanf(b, "%lu H %lu %lu %lu %lu", &time, &wcet, &period, &blocktime, &deadline)) >= 4) {
                if(time != kernel_now) { readline = false; break; }
                // Create new hard real-time task
                struct dcb *dcb = malloc(sizeof(struct dcb));
                init_dcb(dcb, tasks);
                dcb->type = TASK_TYPE_HARD_REALTIME;
                dcb->wcet = wcet;
                dcb->period = period;
                dcb->blocktime = blocktime;
                dcb->release_time = kernel_now;
                snprintf(dcb->dsg.name, DISP_NAME_LEN, "h %d", tasks);
                if(rd == 5) {
                    dcb->deadline = deadline;
                } else {
                    dcb->deadline = period;
                }
                make_runnable(dcb);
                assert(tasks < MAXTASKS);
                allptrs[tasks++] = dcb;
            } else if(sscanf(b, "%lu S %lu %lu", &time, &wcet, &period) == 3) {
                if(time != kernel_now) { readline = false; break; }
                // Create new soft real-time task
                struct dcb *dcb = malloc(sizeof(struct dcb));
                init_dcb(dcb, tasks);
                dcb->type = TASK_TYPE_SOFT_REALTIME;
                dcb->wcet = wcet;
                dcb->period = period;
                snprintf(dcb->dsg.name, DISP_NAME_LEN, "s %d", tasks);
                make_runnable(dcb);
                assert(tasks < MAXTASKS);
                allptrs[tasks++] = dcb;
            } else if(sscanf(b, "%lu B %lu", &time, &weight) == 2) {
                if(time != kernel_now) { readline = false; break; }
                // Create new best-effort task
                struct dcb *dcb = malloc(sizeof(struct dcb));
                init_dcb(dcb, tasks);
                dcb->type = TASK_TYPE_BEST_EFFORT;
                dcb->weight = weight;
                snprintf(dcb->dsg.name, DISP_NAME_LEN, "b %d", tasks);
                make_runnable(dcb);
                assert(tasks < MAXTASKS);
                allptrs[tasks++] = dcb;
            } else if(sscanf(b, "%lu d %lu", &time, &id) == 2) {
                if(time != kernel_now) { readline = false; break; }
                // Delete task with given ID
                assert(id < MAXTASKS);
                scheduler_remove(allptrs[id]);
            } else if(sscanf(b, "%lu r %lu", &time, &id) == 2) {
                if(time != kernel_now) { readline = false; break; }
                // Re-release task with given ID
                assert(id < MAXTASKS);
                if(allptrs[id]->type != TASK_TYPE_BEST_EFFORT) {
                    allptrs[id]->release_time = kernel_now;
                }
                make_runnable(allptrs[id]);
            } else if(sscanf(b, "%lu y %lu", &time, &id) == 2) {
                if(time != kernel_now) { readline = false; break; }
                // Yield task with given ID
                assert(id < MAXTASKS);
                scheduler_yield(allptrs[id]);
            } else if(sscanf(b, "%lu c %lu", &time, &id) == 2) {
                if(time != kernel_now) { readline = false; break; }
                // Context switch to task with given ID
                assert(id < MAXTASKS);
                dcb_current = allptrs[id];
                continue;
            } else {
                fprintf(stderr, "Invalid line: %s\n", b);
                abort();
            }

            dcb_current = schedule();
        }

        for(int i = 0; i < alltasks; i++) {
            struct dcb *cd = allptrs[i];
            if(cd != NULL) {
                cd->dispatched = false;

#if 0
                if(cd->type == TASK_TYPE_HARD_REALTIME) {
                    if(cd->etime >= cd->blocktime) {
                        scheduler_remove(cd);
                    }
                }
#endif
            }
        }

        if(kernel_now % quantum == 0) {
            dcb_current = schedule();
        }

        if(dcb_current != NULL) {
            dcb_current->dispatched = true;

            /* printf("%4d: dispatching %2d, release time: %4lu, deadline: %4lu, period: %3lu, WCET: %3lu/%3lu\n", kernel_now, dcb_current->id, dcb_current->release_time, dcb_current->deadline, dcb_current->period, dcb_current->etime, dcb_current->wcet); */
        }
        for(int i = 0; i < alltasks; i++) {
            if(allptrs[i] != NULL) {
                sched[kernel_now * alltasks + i] = *allptrs[i];
            }
        }
    }

    fclose(f);

    // Print schedule
    printf("     ");
    for(int t = 0; t < runtime; t++) {
        if(t % 1000 == 0) {
            printf("%d", (t / 1000) % 10);
        } else {
            printf(" ");
        }
    }
    printf("\n");
    printf("     ");
    for(int t = 0; t < runtime; t++) {
        if(t % 100 == 0) {
            printf("%d", (t / 100) % 10);
        } else {
            printf(" ");
        }
    }
    printf("\n");
    printf("     ");
    for(int t = 0; t < runtime; t++) {
        if(t % 10 == 0) {
            printf("%d", (t / 10) % 10);
        } else {
            printf(" ");
        }
    }
    printf("\n");

    printf("     ");
    for(int t = 0; t < runtime; t++) {
        printf("%d", t % 10);
    }
    printf("\n");

    for(int i = 0; i < tasks; i++) {
        struct dcb *ct = allptrs[i];
        printf("%c%2d: ", typechar(ct->type), i);
        for(int t = 0; t < runtime; t++) {
            struct dcb *s = &sched[t * alltasks + i];

            if(s->dispatched) {
                printf("#");
            } else {
                printf(" ");
            }
        }
        printf("\n");
        printf("     ");
        for(int t = 0; t < runtime; t++) {
            struct dcb *s = &sched[t * alltasks + i];

            if(s->release_time == t) {
                printf("r");
            } else {
                printf(" ");
            }
        }
        printf("\n");
    }

    free(sched);
    free(allptrs);
    return 0;
}
