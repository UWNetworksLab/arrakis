/**
 * \file
 * \brief Header file for C predicates.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef PREDICATES_H_
#define PREDICATES_H_

int p_notify_client(void);
int p_trigger_watch(void);

int p_save_index(void);
int p_remove_index(void);
int p_index_intersect(void);
int p_index_union(void);

int p_bitfield_add(void);
int p_bitfield_remove(void);
int p_bitfield_union(void);

#endif /* PREDICATES_H_ */
