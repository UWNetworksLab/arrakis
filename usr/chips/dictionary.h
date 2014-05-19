/**
 * \file
 * \brief Dictionary type
 */

/*
 * Copyright (c) 2008, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#ifndef DICTIONARY_H_
#define DICTIONARY_H_

#include <barrelfish/barrelfish.h>

typedef enum uint8_t {
    TYPE_STRING = 1,
    TYPE_WORD,
    TYPE_OPAQUE,
    TYPE_CAPABILITY,
} ENTRY_TYPE;

/**
 * \brief dictionary_t is the abstract type of
 *   datastructures that are able to store key/value pairs
 */
struct dictionary {
    int (*put_string)(struct dictionary*, char*, char*);
    int (*put_word)(struct dictionary*, char*, uintptr_t);
    int (*put_opaque)(struct dictionary*, char*, void*);
    int (*put_capability)(struct dictionary*, char*, struct capref);
    ENTRY_TYPE (*get)(struct dictionary*, char*, void**);
    ENTRY_TYPE (*get_capability)(struct dictionary*, char*, struct capref*);
    int (*size)(struct dictionary*);
    int (*remove)(struct dictionary*, char*);
    int (*get_key_set)(struct dictionary*, char**);
};

#endif /*DICTIONARY_H_*/
