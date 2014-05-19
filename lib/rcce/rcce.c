/**
 * \file
 * \brief RCCE library
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <rcce/RCCE.h>
#include <rcce/RCCE_lib.h>
#include "internal.h"

/// Define this to measure time spent in RCCE comm vs. program
//#define MEASURE_TIME
//#define MEASURE_DATA

//......................................................................................
// GLOBAL VARIABLES USED BY THE LIBRARY
//......................................................................................
int       RCCE_NP;               // number of participating cores
double    RC_REFCLOCKGHZ;        // baseline CPU frequency (GHz)
int       RC_MY_COREID;          // physical ID of calling core
int       RC_COREID[RCCE_MAXNP]; // array of physical core IDs for all participating 
                                 // cores, sorted by rank
int       RCCE_IAM=-1;           // rank of calling core (invalid by default)
RCCE_COMM RCCE_COMM_WORLD;       // predefined global communicator
int       RCCE_BUFF_SIZE;        // available MPB size
t_vcharp  RCCE_comm_buffer[RCCE_MAXNP]; // starts of MPB, sorted by rank
// ......................... non-GORY communication mode .............................
// synchronization flags are predefined and maintained by the library
RCCE_FLAG RCCE_sent_flag[RCCE_MAXNP], RCCE_ready_flag[RCCE_MAXNP];

#ifdef MEASURE_TIME
static double measure_start, measure_rcce_time;
#endif

#ifdef MEASURE_DATA
#       define MAX_PHASES       10
static size_t measure_rcce_data[MAX_PHASES][RCCE_MAXNP];
#endif
int rcce_curphase = 0;

int id_compare(
  const void *e1, // first element to be compared
  const void *e2  // second element to be compared
  ) {
  int v1 = *(int *)e1;
  int v2 = *(int *)e2;
  return(v1<v2) ? -1 : (v1>v2) ? 1 : 0;
}

static int MYCOREID(void)
{
    return disp_get_core_id();
}

int RCCE_init(int *argc, char ***argv)
{
    int ue;
    void *nothing = NULL;

    assert(*argc >= 3);

    setup_routes(*argc, *argv);

    // save pointer to executable name for later insertion into the argument list
    char *executable_name = (*argv)[0];

    RCCE_NP        = atoi(*(++(*argv)));
    RC_REFCLOCKGHZ = atof(*(++(*argv)));

    if(RC_REFCLOCKGHZ == 0) {
        printf("Barrelfish RCCE extension: Computing reference clock GHz automatically...\n");
        uint64_t tscperms;
        errval_t err = sys_debug_get_tsc_per_ms(&tscperms);
        assert(err_is_ok(err));
        RC_REFCLOCKGHZ = ((double)tscperms) / 1000000.0;
        printf("Reference clock computed to be %.2g\n", RC_REFCLOCKGHZ);
    }

    // put the participating core ids (unsorted) into an array
    for (ue=0; ue<RCCE_NP; ue++) {
        RC_COREID[ue] = atoi(*(++(*argv)));
    }

    // make sure executable name is as expected
    (*argv)[0] = executable_name;

    RC_MY_COREID = MYCOREID();

    // adjust apparent number of command line arguments, so it will appear to main
    // program that number of UEs, clock frequency, and core ID list were not on
    // command line
    *argc -= RCCE_NP+2;

    // sort array of participating phyical core IDs to determine their ranks
    qsort((char *)RC_COREID, RCCE_NP, sizeof(int), id_compare);

    // determine rank of calling core
    for (ue=0; ue<RCCE_NP; ue++) {
        if (RC_COREID[ue] == RC_MY_COREID) RCCE_IAM = ue;
    }

    // leave in one reassuring debug print
    printf("My rank is %d, physical core ID is %d\n", RCCE_IAM, RC_MY_COREID);
    if (RCCE_IAM<0) {
        return(RCCE_ERROR_CORE_NOT_IN_HOSTFILE);
    }

    // create global communicator (equivalent of MPI_COMM_WORLD); this will also allocate
    // the two synchronization flags associated with the global barrier
    RCCE_comm_split(RCCE_global_color, nothing, &RCCE_COMM_WORLD);

#ifdef MEASURE_TIME
    measure_start = RCCE_wtime();
    measure_rcce_time = 0.0;
#endif

#ifdef MEASURE_DATA
    memset(measure_rcce_data, 0, sizeof(measure_rcce_data));
#endif

    return (RCCE_SUCCESS);
}

int RCCE_ue(void)
{
    return (RCCE_IAM);
}

int RCCE_num_ues(void)
{
    return RCCE_NP;
}

double RCCE_wtime(void)
{
  return ( ((double)rdtsc())/((double)RC_REFCLOCKGHZ*1.e9));
}

int RCCE_barrier(RCCE_COMM *comm)
{
    assert(comm == &RCCE_COMM_WORLD);
    if(RCCE_debug_synch) {
        printf("UE %d has checked into barrier\n", RCCE_IAM);
    }
    if(RCCE_NP != 1) {
        barrier_wait();
    }
    if(RCCE_debug_synch) {
        printf("UE %d has cleared barrier\n", RCCE_IAM);
    }
    return (RCCE_SUCCESS);
}

int RCCE_send(char *privbuf, size_t size, int dest)
{
#ifdef MEASURE_TIME
    double send_start = RCCE_wtime();
#endif

    if (dest<0 || dest >= RCCE_NP) {
        return(RCCE_error_return(RCCE_debug_comm,RCCE_ERROR_ID));
    }

    errval_t err = send_message(privbuf, size, RC_COREID[dest]);
    assert(err_is_ok(err));

#ifdef MEASURE_TIME
    measure_rcce_time += RCCE_wtime() - send_start;
#endif

#ifdef MEASURE_DATA
    measure_rcce_data[rcce_curphase][dest] += size;
#endif

    return (RCCE_SUCCESS);
}

#ifdef RCCE_PERF_MEASURE
#       include <barrelfish/dispatcher_arch.h>
#       include <barrelfish/curdispatcher_arch.h>
#       define PERF(x)  d->timestamp[x] = rdtsc()
#       define PERFM(x) x
#else
#       define PERF(x)
#       define PERFM(x)
#endif

int RCCE_recv(char *privbuf, size_t size, int source)
{
    errval_t err;
#ifdef MEASURE_TIME
    double recv_start = RCCE_wtime();
#endif

#ifdef RCCE_PERF_MEASURE
    dispatcher_handle_t handle = curdispatcher();
    struct dispatcher_shared_generic* d =
        get_dispatcher_shared_generic(handle);
#endif

    if (source<0 || source >= RCCE_NP) {
        return(RCCE_error_return(RCCE_debug_comm,RCCE_ERROR_ID));
    }

    int core_id = RC_COREID[source];
    struct msg_buf *mb = &msgbuf[core_id];
#ifdef BULK_TRANSFER_ENABLED
    mb->bulk_ready = true;
    mb->length = size;
    mb->current = 0;
    mb->msg = privbuf;
#endif

    dprintf("%d: R(%lu,%d,%p,%d,%p)\n", my_core_id, size, source, mb, mb->pending, privbuf);

#ifdef BULK_TRANSFER_ENABLED
    err = barray[core_id]->tx_vtbl.bulk_recv_ready(barray[core_id], NOP_CONT,
                                                   my_core_id, size);
    assert(err_is_ok(err));
#endif

    PERF(30);

    while(!mb->pending) {
        messages_wait_and_handle_next();
    }

    PERF(31);

    dprintf("%d: msg arrived\n", my_core_id);

    /* if(size <= DEFAULT_UMP_BUFLEN) { */
#ifndef BULK_TRANSFER_ENABLED
        assert(size == mb->length);
        memcpy(privbuf, mb->msg, size);
    /* } else { */
#else
        assert(mb->bulk);
#endif
    /* } */
    mb->pending = false;

#ifndef BULK_TRANSFER_ENABLED
    assert(!mb->bulk);
    free(mb->msg);
    PERF(32);
    err = barray[core_id]->tx_vtbl.message_reply(barray[core_id],
                                                 NOP_CONT, my_core_id);
    PERF(33);
    assert(err_is_ok(err));
#else
    assert(mb->bulk);
#endif

#ifdef MEASURE_TIME
    measure_rcce_time += RCCE_wtime() - recv_start;
#endif

    return (RCCE_SUCCESS);
}

int RCCE_finalize(void)
{
#ifdef MEASURE_TIME
    double measure_end = RCCE_wtime();
    printf("%d: Time spent in RCCE communication %.5g seconds. "
           "%.5g seconds total program run-time.\n", RCCE_ue(),
           measure_rcce_time, measure_end - measure_start);
#endif

#ifdef MEASURE_DATA
    for(int phase = 0; phase < MAX_PHASES; phase++) {
        printf("%d: Phase %d: ", RCCE_ue(), phase);
        for(int i = 0; i < RCCE_NP; i++) {
            printf("%lu ", measure_rcce_data[phase][i]);
        }
        printf("\n");
    }
#endif

    return (RCCE_SUCCESS);
}

int RCCE_flag_alloc(RCCE_FLAG *flag)
{
    /* printf("Warning: RCCE_flag_alloc ignored\n"); */
    return (RCCE_SUCCESS);
}

int RCCE_flag_free(RCCE_FLAG *flag)
{
    printf("Warning: RCCE_flag_free ignored\n");
    return (RCCE_SUCCESS);
}
