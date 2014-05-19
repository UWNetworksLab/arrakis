/**
 * \file
 * \brief Prototypes for use by flounder-generated stubs
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FLOUNDER_SUPPORT_H
#define __FLOUNDER_SUPPORT_H

#include <flounder/flounder.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

/// State associated with an ongoing generic bind attempt
struct flounder_generic_bind_attempt {
    int driver_num;
    iref_t iref;
    struct waitset *waitset;
    void *callback; // XXX: actually interface-specific binding callback type
    void *st;
    void *binding;
    idc_bind_flags_t flags;
};

struct waitset_chanstate;
struct monitor_binding;

extern struct event_closure dummy_event_closure;

void flounder_support_trigger_chan(struct waitset_chanstate *wc);
errval_t flounder_support_register(struct waitset *ws,
                                   struct waitset_chanstate *wc,
                                   struct event_closure ec,
                                   bool trigger_now);
void flounder_support_deregister_chan(struct waitset_chanstate *wc);
void flounder_support_waitset_chanstate_init(struct waitset_chanstate *wc);
void flounder_support_waitset_chanstate_destroy(struct waitset_chanstate *wc);
errval_t flounder_support_change_monitor_waitset(struct monitor_binding *mb,
                                                 struct waitset *ws);
void flounder_support_monitor_mutex_enqueue(struct monitor_binding *mb,
                                            struct event_queue_node *qn,
                                            struct event_closure cl);
void flounder_support_monitor_mutex_unlock(struct monitor_binding *mb);
void flounder_support_migrate_notify(struct waitset_chanstate *chan,
                                     struct waitset *new_ws);

#if defined(FLOUNDER_DEBUG)
# include <barrelfish_kpi/dispatcher_shared.h>
# include <barrelfish/dispatch.h>
# include <barrelfish/debug.h>
# define FL_DEBUG(msg...) debug_printf(msg)
#else
# define FL_DEBUG(msg...) ((void)0)
#endif

__END_DECLS

#endif // __FLOUNDER_SUPPORT_H
