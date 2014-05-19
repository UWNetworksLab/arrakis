/**
 * \file
 * \brief Barrelfish timer interface.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef BF_TIMER_H
#define BF_TIMER_H

#include <stdbool.h>
#include <stdint.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

struct timer;

/**
 * Timer callback function prototype.
 */
typedef void (*timer_callback_fn)(struct timer *, void *);

struct timer {
    struct timer        *prev, *next;
    uint64_t            expires;
    uint64_t            duration;
    bool                periodic;
    timer_callback_fn   callback;
    void                *data;
};

errval_t timer_init(void);
struct timer *timer_new(void);
void timer_start(struct timer *timer);
void timer_stop(struct timer *timer);
uint64_t timer_remaining(struct timer *timer);
void timer_destroy(struct timer *timer);
bool timer_is_running(struct timer *timer);

/**
 * \brief Set timer duration.
 *
 * \param timer         Pointer to timer to set duration for.
 * \param duration      Duration in us to set.
 * \param periodic      true if timer is periodic.
 */
static inline void timer_set_duration(struct timer *timer, uint64_t duration,
                                      bool periodic)
{
    timer->duration = duration;
    timer->periodic = periodic;
}

/**
 * \brief Set timeout callback function.
 *
 * \param timer         Pointer to timer to set callback function for.
 * \param callback      Callback function.
 * \param data          Private data of callback function.
 */
static inline void timer_set_callback(struct timer *timer,
                                      timer_callback_fn callback, void *data)
{
    timer->callback = callback;
    timer->data = data;
}

static inline struct timer *timer_create(uint64_t duration, bool periodic,
                                         timer_callback_fn callback, void *data)
{
    struct timer *newtimer = timer_new();

    assert(newtimer != NULL);
    timer_set_duration(newtimer, duration, periodic);
    timer_set_callback(newtimer, callback, data);

    return newtimer;
}

__END_DECLS

#endif
