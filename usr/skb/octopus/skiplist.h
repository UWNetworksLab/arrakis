/**
 * \file
 * \brief Header file for skip list.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef SKIPLIST_H_
#define SKIPLIST_H_

#include <barrelfish/types.h>

struct skip_list {
    struct skip_node* header;
    size_t level;
    size_t entries; // used to sort lists based on #entries for intersect
};

struct skip_node {
    char* element;
    struct skip_node** forward;
};

errval_t skip_create_list(struct skip_list**);
void skip_insert(struct skip_list*, char*);
bool skip_contains(struct skip_list*, char*);
char* skip_delete(struct skip_list*, char*);
char* skip_intersect(struct skip_list**, size_t, char*);

void skip_print_list(struct skip_list*);

#endif /* SKIPLIST_H_ */
