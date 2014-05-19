/**
 * \file
 * \brief multimap implementation
 */

/*
 * Copyright (c) 2009, 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef MULTIMAP_H_
#define MULTIMAP_H_

#include <hashtable/hashtable.h>
#include <sys/cdefs.h>

__BEGIN_DECLS

struct multimap {
    struct hashtable h;
    int (*put)(struct multimap*, char*, void*);
    int (*remove)(struct multimap*, char*, void*);
    int (*get_first)(struct multimap*, char*, void**);
    int (*get_all)(struct multimap*, char*, void**, int);
};

struct multimap* create_multimap(void);

__END_DECLS

#endif /* MULTIMAP_H_ */
