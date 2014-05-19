/**
 * \file
 * \brief internal functions
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INTERNAL_H_
#define INTERNAL_H_

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <skb/skb.h>
#include <if/boot_perfmon_defs.h>
#include <barrelfish/coreset.h>
#include <bench/bench.h>

#define MAX_COUNT 10

struct timestamp {
    cycles_t time[MAX_COUNT];
};

struct relations;

extern int global_argc;
extern char **global_argv;
extern coreid_t my_core_id;
extern struct relations *rel;
extern struct coreset *set;
extern struct timestamp *timestamp;

typedef void (*callback)(void);
typedef errval_t (*relations_iterator_fn)(void *st, coreid_t id,
                                          struct boot_perfmon_binding *b);

void exit_msg(struct boot_perfmon_binding *b);

bool check_leader(void);
coreid_t get_leader_id(void);
errval_t set_leader(callback cb);

errval_t relations_new(struct relations **retrel);
errval_t relations_add(struct relations *rel, coreid_t id,
                       struct boot_perfmon_binding *b);
errval_t relations_remove(struct relations *rel, coreid_t id);
errval_t relations_get(struct relations *rel, coreid_t id,
                       struct boot_perfmon_binding **b);
int relations_count(struct relations *rel);
errval_t relations_iterate(struct relations *rel, void *st,
                           relations_iterator_fn func);

errval_t spawn(callback cb);

errval_t connect(callback cb);

errval_t tests(callback cb);
void ping(struct boot_perfmon_binding *b, cycles_t time);
void pong(struct boot_perfmon_binding *b, cycles_t time);

#endif //INTERNAL_H_
