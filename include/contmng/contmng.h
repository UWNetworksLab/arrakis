/**
 * \file
 * \brief continuation management
 *
 * This file provides a generic way of managing continuation for messages
 * of different types
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */


#ifndef CONTMNG_H_
#define CONTMNG_H_

#include <stdio.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

/*********************************************************************/
/* Implementation of generic queue */

/* !!!!!!!!!!!!! ASSUMPTIONS: READ THIS BEFORE USING IT  !!!!!!!!!!!!!!!!!!
 * 1. There are less than MAX_PARAMS no. of parameters.
 * 2. All the data which is sent is of type uint64_t.
 * 3. Only exception is struct cap, which is dealt separately.
 * 4. The errval_t datatype is casted into uint64_t which is
 *    assumed to be lossless.
 * 5. void * pointers are casted to uint64_t which is assumed to be lossless.
 * */

#define MAX_QUEUE_SIZE 1024
#define MAX_PARAMS 10

struct q_entry {
    void *binding_ptr;
    uint64_t plist[MAX_PARAMS]; /* Assuming max parameters are MAX_PARAMS */
    struct capref cap;
    struct capref cap2;
    errval_t (*handler)(struct q_entry entry);
    char *fname;
    int state;
};

struct cont_queue {
    char name[64]; /* for debugging purposes */
    int running;
    int head;
    int tail;
    struct q_entry qelist[MAX_QUEUE_SIZE];
    uint8_t debug;
};

/***** helper functions *****/
/* create new queue */
struct cont_queue *create_cont_q(char *name);
bool can_enqueue_cont_q(struct cont_queue *q);
void enqueue_cont_q(struct cont_queue *q, struct q_entry *entry);
void cont_queue_callback(void *arg);
void cont_queue_show_queue(struct cont_queue *q);
int queue_free_slots(struct cont_queue *q);
int queue_used_slots(struct cont_queue *q);
int is_enough_space_in_queue(struct cont_queue *q);

void show_binary_blob (void *data, int len);

__END_DECLS

#endif // CONTMNG_H_
