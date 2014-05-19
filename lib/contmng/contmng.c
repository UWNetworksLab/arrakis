/**
 * \file
 * \brief e1000 continuation management
 *
 * This file provides a generic way of managing continuation for messages
 * of different types
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef CONTMNG_C_
#define CONTMNG_C_

#include <string.h>
#include <barrelfish/barrelfish.h>

#include <contmng/contmng.h>

/* QUEUE_DEBUG enables the debug statements
 * NOTE: This option does not compile for all architecture/machine due to
 * dependance on disp_name and dispatch.h
 */
//#define QUEUE_DEBUG 1

static void cont_queue_send_next_message(struct cont_queue *q);


#ifdef QUEUE_DEBUG
#include <dispatch.h>
static void qprintf_mac(struct cont_queue *q, char *msg)
{
    if (q->debug) {
        printf("CQ: %s [%d:%.*s] [%d] [%d] [%d] qqqqqq\n",
                msg, disp_get_core_id(),
                64, q->name, q->head, q->tail, q->running);
    }
}
#define qprintf(x...) qprintf_mac(x)

#else
#define qprintf(x...) ((void)0)
#endif /* QUEUE_DEBUG */


//****************************************************************************
/************** Generic continuation queue implementation *******************/
/* WARN: use only when your responses contains only integers */


/* allocates the memory for continuation queue
    It includes the memory for MAX_QUEUE_SIZE of elements also */
struct cont_queue *create_cont_q(char *name)
{
    struct cont_queue *ptr = NULL;
    ptr = (struct cont_queue *) malloc(sizeof(struct cont_queue));
    if (ptr == NULL) {
        printf("ERROR: malloc failed in create_cont_q\n");
        abort();
        /* FIXME: introduce new error and return the error */
    }
    memset(ptr, 0, sizeof(struct cont_queue));
    if (name != NULL) {
        strncpy(ptr->name, name, 63);
    }
    return ptr;
}/* end function: create_cont_q */



// Tells how many slots are actually used
int queue_used_slots(struct cont_queue *q)
{
    uint64_t nhead = q->head % MAX_QUEUE_SIZE;
    uint64_t ntail = q->tail % MAX_QUEUE_SIZE;
    if ( nhead >= ntail) {
        return (nhead - ntail);
    } else {
        return (ntail + (MAX_QUEUE_SIZE - nhead));
    }
} // end function: queue_used_slots



/* Tells if queue has enough space to add more events,
 * or if the producer should pause for a while */
int queue_free_slots(struct cont_queue *q)
{

    if (((q->head + 1) % MAX_QUEUE_SIZE) > q->tail) {
        return (MAX_QUEUE_SIZE -
                    (((q->head + 1) % MAX_QUEUE_SIZE) - q->tail)
               );
    } else {
        return q->tail - ((q->head + 1) % MAX_QUEUE_SIZE);
    }
} // end function


int is_enough_space_in_queue(struct cont_queue *q)
{
    if (queue_free_slots(q) > (MAX_QUEUE_SIZE/10)) {
        return true;
    } else {
        return false;
    }
} // end function: is_enough_space_in_queue


bool can_enqueue_cont_q(struct cont_queue *q)
{
    if (((q->head + 1) % MAX_QUEUE_SIZE) == q->tail) {
        return false;
    } else {
        return true;
    }
} // end function: can_enqueue_cont_q

/* Adds element to the queue */
void enqueue_cont_q(struct cont_queue *q, struct q_entry *entry)
{

    if (((q->head + 1) % MAX_QUEUE_SIZE) == q->tail)
    {
        qprintf(q, "ERROR: Queue full");
        printf("ERROR:  Queue [%s] is full\n", q->name);
        printf("ERROR: %.*s\n", 64, q->name);
        printf("ERROR: CQ: head [%d], tail [%d] qqqqqq\n", q->head, q->tail);
        /* __builtin_return_address is not supported in ARM toolchain */

#ifdef QUEUE_DEBUG
        printf("callstack: %p %p %p %p\n",
	     __builtin_return_address(0),
	     __builtin_return_address(1),
	     __builtin_return_address(2),
	     __builtin_return_address(3));
#endif // QUEUE_DEBUG

        // Following two lines are there to force the seg-fault of the domain
        // as abort was showing some strange behaviour
//        int *p = NULL;
//        *p = 43;
        abort();
//        return CONT_ERR_NO_MORE_SLOTS;
    }

    /* Copying the structure */
    q->qelist[q->head].binding_ptr = entry->binding_ptr;
    q->qelist[q->head].cap = entry->cap;
    q->qelist[q->head].handler = entry->handler;
    q->qelist[q->head].state = 0;
    q->qelist[q->head].fname = entry->fname;

    for(int i = 0; i < MAX_PARAMS; ++i) {
        q->qelist[q->head].plist[i] = entry->plist[i];
    }

    q->head = (q->head + 1) % MAX_QUEUE_SIZE;

    // If no continuations are running, then execute this one directly
    if (((q->tail + 1) % MAX_QUEUE_SIZE) == q->head) {
//    if (q->running == 0) {
        q->running = 1;
        qprintf(q, "directly-sending");
        cont_queue_send_next_message(q);
    }

    //otherwise continuation function will trigger sending next queue element
} // end function: enqueue_cont_q


// called from continuation registered with flounder
//    WARN: should not be called directly
void cont_queue_callback(void *arg)
{
    struct cont_queue *q = (struct cont_queue *)arg;

    q->tail = (q->tail + 1) % MAX_QUEUE_SIZE;
    qprintf(q, "from-continuation");
    cont_queue_send_next_message(q);
} /* end function: cont_queue_callback */


/* Sends the top of queue msg
    NOTE: this function does not increment the tail.  It calls handler,
        which registers "cont_queue_callback" as callback with flounder,
        and that callback function increments the tail!!!
            complicated? huh.. :-P */
void cont_queue_send_next_message(struct cont_queue *q)
{
//    qprintf(q, "sending-msg");

    if(q->head == q->tail){
        q->running = 0;
        qprintf(q, "Queue-empty-Recursion-End!!");
        return;
    }
    errval_t err = q->qelist[q->tail].handler(q->qelist[q->tail]);
    if (err_is_fail(err)) {
        if (err == FLOUNDER_ERR_TX_BUSY ) {
            qprintf(q, "sending:FLO-BUSY");
        } else {
            qprintf(q, "sending:FLO FAIL");
            USER_PANIC_ERR(err, "cont_queue_send_next_message ");
        }
    }
    qprintf(q, "sending-msg done: ");
} /* end function: cont_queue_send_next_message */

void cont_queue_show_queue(struct cont_queue *q)
{

    int len = 0;
    len = q->head - q->tail;
    printf("queue [%s] len [%d]==> head [%d] - tail [%d]\n",
            q->name, len, q->head, q->tail);

    /*
    int i = 0;
    int idx = 0;
    idx = q->tail;
    while (idx != q->head){
        printf("elem %d: [%s], state %u\n", idx, q->qelist[idx].fname,
                q->qelist[idx].history);
        idx = (idx + 1) % MAX_QUEUE_SIZE;
    }

    printf("Showing elements which are already sent!!\n");
    idx = q->tail;
    for (i = 0; i < 10; ++i){
        idx = (idx - 1);
        if (idx < 0) {
            idx = MAX_QUEUE_SIZE - 1;
        }
        printf("elem %d: [%s], state %d\n", idx, q->qelist[idx].fname,
                q->qelist[idx].history);
    }
    */

} // end function: cont_queue_show_queue


/* Following function has nothing to do with cont_queue_management,
 * and is to be used for debugging.  Ideally this function should not exist. */
void show_binary_blob (void *data, int len)
{
	printf("\nBlob_%d[", len);
	uint8_t *ptr = data;
	for (int i = 0 ; i < len; ++i){
		printf("0x%x,", ptr[i]);
	}
	printf("]\n");
}

#endif // CONTMNG_C_
