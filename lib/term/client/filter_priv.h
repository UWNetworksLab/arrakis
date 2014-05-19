/**
 * \file
 * \brief Private filter header.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef LIBTERM_FILTER_PRIV_H
#define LIBTERM_FILTER_PRIV_H

#include <collections/list.h>
#include <term/client/filter.h>

struct term_filter {
    term_filter_fn *filter;
    term_filter_id_t id;
};

void term_filter_free(void *data);

void term_filter_apply(collections_listnode *filter_list, char **data,
                       size_t *length);

#endif // LIBTERM_FILTER_PRIV_H
