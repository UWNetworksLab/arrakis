/**
 * \file
 * \brief Barrelfish collections library hash table
 */
/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _HASH_TABLE_H_
#define _HASH_TABLE_H_

#include "collections/list.h"

/*
 * a simple hash table. 
 */

typedef void (* collections_hash_data_free)(void *);

typedef struct	_collections_hash_table {
	// number of buckets in the table.
	int			num_buckets;

	// pointer to the buckets.
	collections_listnode	**buckets;

	// total number of elements in the table.
	uint32_t	num_elems;

    // function that knows how to free inserted data resources
    collections_hash_data_free data_free;

	// a pointer to keep track of 
	// traversing the hash table
	int32_t		cur_bucket_num;
} collections_hash_table;

/*
 * Structure of a hash table element.
 */
typedef struct	_collections_hash_elem {

	uint64_t	key;

	void	*data;
} collections_hash_elem;

#define NUM_BUCKETS	1013

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus


/*
 * functions ...
 */
void		collections_hash_create(collections_hash_table **t, collections_hash_data_free f);
void		collections_hash_create_with_buckets(collections_hash_table **t, int num_buckets, collections_hash_data_free f);
void		collections_hash_release(collections_hash_table *t);
void		collections_hash_insert(collections_hash_table *t, uint64_t key, void *data);
void*		collections_hash_find(collections_hash_table *t, uint64_t key);
void		collections_hash_delete(collections_hash_table *t, uint64_t key);
uint32_t	collections_hash_size(collections_hash_table *t);
int32_t		collections_hash_traverse_start(collections_hash_table* t);
void*		collections_hash_traverse_next(collections_hash_table* t, uint64_t *key);
int32_t		collections_hash_traverse_end(collections_hash_table* t);

/*
 * Visitor function: returns 0 when visit should be considered finish.
 */
typedef int (*collections_hash_visitor_func)(uint64_t key, void *data, void *arg);

/*
 * Apply function to all elements in hash table or until function indicates
 * function application should stop.
 *
 * Returns non-zero if all elements in table visited, 0 otherwise.
 */
int    collections_hash_visit(collections_hash_table *t, collections_hash_visitor_func collections_hash_visitor, void *arg);

#ifdef __cplusplus
}
#endif // __cplusplus

#endif
