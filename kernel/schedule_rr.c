/**
 * \file
 * \brief Kernel round-robin scheduling policy
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <kernel.h>
#include <dispatch.h>

#include <timer.h> // update_sched_timer

static struct dcb *ring_current = NULL;

/**
 * \brief Scheduler policy.
 *
 * \return Next DCB to schedule or NULL if wait for interrupts.
 */
struct dcb *schedule(void)
{
    // empty ring
    if(ring_current == NULL) {
        return NULL;
    }

    assert(ring_current->next != NULL);
    assert(ring_current->prev != NULL);

    ring_current = ring_current->next;
    #ifdef CONFIG_ONESHOT_TIMER
    update_sched_timer(kernel_now + kernel_timeslice);
    #endif
    return ring_current;
}

void make_runnable(struct dcb *dcb)
{
    // Insert into schedule ring if not in there already
    if(dcb->prev == NULL || dcb->next == NULL) {
        assert(dcb->prev == NULL && dcb->next == NULL);

        // Ring empty
        if(ring_current == NULL) {
            ring_current = dcb;
            dcb->next = dcb;
        }

        // Insert after current ring position
        dcb->prev = ring_current;
        dcb->next = ring_current->next;
        ring_current->next->prev = dcb;
        ring_current->next = dcb;
    }
}

/**
 * \brief Remove 'dcb' from scheduler ring.
 *
 * Removes dispatcher 'dcb' from the scheduler ring. If it was not in
 * the ring, this function is a no-op. The postcondition for this
 * function is that dcb is not in the ring.
 *
 * \param dcb   Pointer to DCB to remove.
 */
void scheduler_remove(struct dcb *dcb)
{
    // No-op if not in scheduler ring
    if(dcb->prev == NULL || dcb->next == NULL) {
        assert(dcb->prev == NULL && dcb->next == NULL);
        return;
    }

    struct dcb *next = ring_current->next;

    // Remove dcb from scheduler ring
    dcb->prev->next = dcb->next;
    dcb->next->prev = dcb->prev;
    dcb->prev = dcb->next = NULL;

    // Removing ring_current
    if(dcb == ring_current) {
        if(dcb == next) {
            // Only guy in the ring
            ring_current = NULL;
        } else {
            // Advance ring_current
            ring_current = next;
        }
    }
}

/**
 * \brief Yield 'dcb' for the rest of the current timeslice.
 *
 * Re-sorts 'dcb' into the scheduler queue with its release time increased by
 * the timeslice period. It is an error to yield a dispatcher not in the
 * scheduler queue.
 *
 * \param dcb   Pointer to DCB to remove.
 */
void scheduler_yield(struct dcb *dcb)
{
    if(dcb->prev == NULL || dcb->next == NULL) {
        struct dispatcher_shared_generic *dsg =
            get_dispatcher_shared_generic(dcb->disp);
        panic("Yield of %.*s not in scheduler queue", DISP_NAME_LEN,
              dsg->name);
    }

    // No-op for the round-robin scheduler
}

void scheduler_reset_time(void)
{
    // No-Op in RR scheduler
}
