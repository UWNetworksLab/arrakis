/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIBMDB_MDB_H
#define LIBMDB_MDB_H

#include <sys/cdefs.h>

#include <barrelfish/types.h>
#include <errors/errno.h> // For errval_t
#include <barrelfish_kpi/capabilities.h>
#include <mdb/types.h>

__BEGIN_DECLS

struct capability;
struct cte;

void set_init_mapping(struct cte *dest_start, size_t num);
bool has_descendants(struct cte *cte);
bool has_ancestors(struct cte *cte);
bool has_copies(struct cte *cte);
void remove_mapping(struct cte *cte);
errval_t mdb_get_copy(struct capability *cap, struct capability **ret);
bool mdb_is_sane(void);
void set_cap_remote(struct cte *cte, bool is_remote);
bool is_cap_remote(struct cte *cte);

__END_DECLS

#endif // LIBMDB_MDB_H
