/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef FILTER_H_
#define FILTER_H_

#include <stdbool.h>
#include <stdio.h>
#include "dictionary.h"

/**
 * \brief identifier for the filter part type
 */
#define EMPTY   0
#define SIMPLE  1
#define COMPLEX 2

// comperators
enum {
    EQUALS = 1, PRESENT, APROX, LESS, GREATER,
};

// operators
enum {
    AND_OPERATOR = 1, OR_OPERATOR, NOT_OPERATOR,
};

/**
 * \brief a simple filter is a literal of the form
 * "id" "comperator" "value"
 */
typedef struct {
    int comp;
    char* id;
    char* value;
} _simple_filter;

/**
 * \brief a complex filter is a logical function over one or more filter parts
 */
typedef struct {
    int op;
    void* f1;
    void* f2;
} _complex_filter;

/**
 * \brief a filter node is either a simple or a complex filter
 */
typedef union {
    _simple_filter simple;
    _complex_filter complex;
} _filter_node;

/**
 * \brief a filter part is a filter node together with a type flag
 */
typedef struct {
    int type;
    _filter_node node;
} _filter_part;

/**
 * \brief a filter is described by its root filter part node
 */
typedef const _filter_part *filter_t;

/**
 * \brief creates a new filter
 * \param str the LDAP string to create a filter from
 * \return the filter
 */
extern filter_t create_filter(char *str);

/**
 * \brief destroy a filter
 * \param f the filter to destroy
 */
extern void destroy_filter(filter_t f);

/**
 * \brief test if a filter matches a dictionary of properties
 * \param dict the property dictionary
 * \param filter the filter
 * \return true if the filter matches, false otherwise
 */
extern bool match_filter(struct dictionary *dict, filter_t filter);

/**
 * \brief print a filter to stdout. Useful for debugging.
 * \param filter the filter to print
 */
extern void print_filter(FILE *stream, filter_t filter);

#endif /*FILTER_H_*/
