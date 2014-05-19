/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBBARRELFISH_CORESET_H
#define LIBBARRELFISH_CORESET_H

#include <sys/cdefs.h>

__BEGIN_DECLS

typedef coreid_t coreset_token_t;

#define CORESET_INIT_TOKEN 0

struct coreset;

/**
 * \brief Function pointer used to call on every core in the coreset
 *
 * \param st    User supplied state
 * \param id    The core id this is called for
 *
 * Refer to #coreset_iterate.
 */
typedef errval_t (*coreset_iterator_fn)(void *st, coreid_t id);

errval_t coreset_new(struct coreset **set);
void coreset_destroy(struct coreset *set);
errval_t coreset_add(struct coreset *set, coreid_t id);
errval_t coreset_remove(struct coreset *set, coreid_t id);
bool coreset_test(struct coreset *set, coreid_t id);
errval_t coreset_get_next(struct coreset *set, coreset_token_t *token,
                          coreid_t *id);
errval_t coreset_iterate(struct coreset *set, void *st,
                         coreset_iterator_fn func); 

errval_t coreset_to_coremask(struct coreset *set, coremask_t *mask);
errval_t coreset_from_coremask(coremask_t mask, struct coreset **set);

coreid_t coreset_count(struct coreset *set);

__END_DECLS

#endif
