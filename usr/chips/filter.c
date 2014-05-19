/**
 * \file filter.c
 * \brief Chips LDAP filter implementation
 */

/*
 * Copyright (c) 2008,2009,2010 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdbool.h>
#include <inttypes.h>
#include "filter.h"

#define MAX_STACK 100

static void cleanup(_filter_part*, _filter_part**);

/**
 * \brief allocate a simple filter and set the id part
 * \param str the LDAP string from which to parse the id
 * \param offset the offset from the beginning the string
 * \param len the length of the substring to parse
 * \return the filter part
 */
static inline _filter_part* allocate_simple_filter(char *str, size_t offset,
        size_t len)
{
    // allocate a new filter part
    _filter_part* filter = malloc(sizeof(_filter_part));

    // simple filter
    filter->type = SIMPLE;

    // allocate and assign the id string
    char* id = malloc(len + 1 * sizeof(char));
    memcpy(id, str + offset, len);

    // terminate by zero
    id[len] = '\0';

    // set the id
    filter->node.simple.id = id;
    return filter;
}

/**
 * \brief create and parse a new filter
 * \param str the LDAP string
 * \return the filter
 */
filter_t create_filter(char *str)
{
    register size_t i = 0;
    size_t sp = -1;
    int operator = 0;
    size_t oper_pos = 0;
    size_t start = -1;
    char nextChar;
    _filter_part* filter = NULL;

    // check if the string is NULL
    // in this case, return an empty filter
    if (str == NULL) {
        _filter_part* empty = malloc(sizeof(_filter_part));
        empty->type = EMPTY;
        return empty;
    }

    // allocate a stack
    _filter_part **stack = malloc(sizeof(_filter_part*) * MAX_STACK);
    memset(stack, 0, sizeof(_filter_part*) * MAX_STACK);

    // loop until the end of the string
    for (i = 0; str[i] != '\0'; i++) {
        //take the next character
        nextChar = str[i + 1];

        // strip whitespaces
        while (nextChar != '\0' && isspace((int)str[i]) != 0) {
            i++;
            nextChar = str[i + 1];
        }

        switch (str[i]) {
        case '(':
            //lookahead
            switch (nextChar) {
            case '&':
                operator = AND_OPERATOR;
                break;
            case '|':
                operator = OR_OPERATOR;
                break;
            case '!':
                operator = NOT_OPERATOR;
                break;
            default:
                //simple filter ahead
                if (start == -1) {
                    start = i;
                    // filter will be allocated when
                    // the operator is detected
                    continue;
                } else {
                    fprintf(stderr, "PARSE ERROR: Surplus left parenthesis at "
                        "position %zu, %s\n", i, str);
                    cleanup(filter, stack);
                    return NULL;
                }
            }
            filter = malloc(sizeof(_filter_part));
            filter->type = COMPLEX;
            filter->node.complex.op = operator;
            filter->node.complex.f1 = NULL;
            filter->node.complex.f2 = NULL;
            sp++;
            stack[sp] = filter;
            continue;
        case ')':
            if (start == -1) {
                // complex filter ends
                filter = stack[sp];
                sp--;
                if (sp == -1) {
                    // empty stack

                    // save the top element
                    stack[0] = NULL;
                    return filter;
                }
                _filter_part* parent = stack[sp];
                if (parent->node.complex.op == NOT_OPERATOR
                        && parent->node.complex.f1 != NULL) {
                    fprintf(stderr,
                            "PARSE ERROR: Unexpected literal after NOT "
                                "operator at position %zu in '%s'\n", i, str);
                    cleanup(filter, stack);
                    return NULL;
                }

                if (parent->node.complex.f1 == NULL) {
                    parent->node.complex.f1 = filter;
                } else {
                    parent->node.complex.f2 = filter;
                }

                if (nextChar == '\0') {
                    fprintf(stderr,
                            "PARSE ERROR: Missing right parenthesis at "
                                "position %zu in '%s'\n", i, str);
                    cleanup(filter, stack);
                    return NULL;
                }
                continue;
            } else {
                // simple filter ends
                if (oper_pos == 0) {
                    fprintf(stderr, "PARSE ERROR: Missing operator in '%s'\n",
                            str);
                    cleanup(filter, stack);
                    return NULL;
                }

                if (sp == -1) {
                    // empty stack
                    if (nextChar == '\0') {
                        // just a single simple filter
                        size_t value_len = i - oper_pos - 1;
                        char* value = malloc(value_len + 1 * sizeof(char));
                        memcpy(value, str + oper_pos + 1, value_len);
                        value[value_len] = '\0';

                        if (filter->node.simple.comp == EQUALS && strcmp(value,
                                "*") == 0) {
                            filter->node.simple.comp = PRESENT;
                        } else {
                            filter->node.simple.value = value;
                        }

                        cleanup(NULL, stack);
                        return filter;
                    } else {
                        fprintf(stderr, "PARSE ERROR: Unexpected token at "
                            "position %zu in '%s'\n", i, str);
                        cleanup(filter, stack);
                        return NULL;
                    }
                }

                // get the parent from stack
                _filter_part *parent = stack[sp];

                size_t value_len = i - oper_pos - 1;
                char* value = malloc(value_len * sizeof(char));
                memcpy(value, &str[oper_pos + 1], value_len);

                if (filter->node.simple.comp == EQUALS && strcmp(value, "*")
                        == 0) {
                    filter->node.simple.comp = PRESENT;
                } else {
                    filter->node.simple.value = value;
                }

                // link current element to parent
                if (parent->node.complex.f1 == NULL) {
                    parent->node.complex.f1 = filter;
                } else {
                    parent->node.complex.f2 = filter;
                }

                filter = parent;

                oper_pos = 0;
                start = -1;
                continue;
                case '~':
                if (oper_pos == 0 && str[i + 1] == '=') {
                    filter = allocate_simple_filter(str, start + 1, i - start
                            - 1);
                    filter->node.simple.comp = APROX;
                    oper_pos = ++i;
                    continue;
                } else {
                    fprintf(stderr,
                            "PARSE ERROR: Unexpected character '%c' at "
                                "position %zu in '%s'\n", str[i + 1], i + 1,
                            str);
                    cleanup(filter, stack);
                    return NULL;
                }
                case '>':
                if (oper_pos == 0 && str[i + 1] == '=') {
                    filter = allocate_simple_filter(str, start + 1, i - start
                            - 1);
                    filter->node.simple.comp = GREATER;
                    oper_pos = ++i;
                    continue;
                } else {
                    fprintf(stderr,
                            "PARSE ERROR: Unexpected character '%c' at "
                                "position %zu in '%s'\n", str[i + 1], i + 1,
                            str);
                    cleanup(filter, stack);
                    return NULL;
                }
                case '<':
                if (oper_pos == 0 && str[i + 1] == '=') {
                    filter = allocate_simple_filter(str, start + 1, i - start
                            - 1);
                    filter->node.simple.comp = LESS;
                    oper_pos = ++i;
                    continue;
                } else {
                    fprintf(stderr,
                            "PARSE ERROR: Unexpected character '%c' at "
                                "position %zu in '%s'\n", str[i + 1], i + 1,
                            str);
                    cleanup(filter, stack);
                    return NULL;
                }
                case '=':
                // could also be a "=*" present production.
                // if this is the case, it is fixed later, because
                // value=* and value=*key would require a lookahead of at
                // least two. (the symbol "=*" alone is ambiguous).
                filter = allocate_simple_filter(str, start + 1, i - start - 1);
                filter->node.simple.comp = EQUALS;
                oper_pos = i;
                continue;
            }
        }
    }

    // parsing failed, clear the stack
    cleanup(filter, stack);
    return NULL;
}

/**
 * \brief destroy a filter and free the resources
 * \param f the filter
 */
void destroy_filter(filter_t f)
{
    // we have to check because the function can be
    // invoked in an exceptional state in which a node
    // might not be fully initialized with values
    if (f == NULL) {
        return;
    }

    if (f->type == COMPLEX) {
        // recurse
        if (f->node.complex.f1 != NULL) {
            destroy_filter(f->node.complex.f1);
        }
        if (f->node.complex.f2 != NULL) {
            destroy_filter(f->node.complex.f2);
        }
        // free the node
        free((_filter_part*) f);
    } else if (f->type == SIMPLE) {
        // free the strings
        if (f->node.simple.id != NULL) {
            free(f->node.simple.id);
        }
        if (f->node.simple.value != NULL) {
            free(f->node.simple.value);
        }
        // free the node
        free((_filter_part*) f);
    }
}

/**
 * \brief do a cleanup of the filter and the parser stack
 * \param f the (partially populated) filter
 * \param stack the parser stack
 */
static void cleanup(_filter_part* f, _filter_part **stack)
{
    register int i;
    // destroy the filter
    if (f != NULL) {
        destroy_filter(f);
    }
    // destroy the stack
    if (stack != NULL) {
        for (i = 0; i < MAX_STACK; i++) {
            if (stack[i] != NULL) {
                destroy_filter(stack[i]);
            }
        }
    }
}

/**
 * \brief get a lower case copy of a string
 * \param str the original string
 * \return the version in lower case
 */
static inline char* to_lower(const char *str)
{
    size_t len = strlen(str);
    register size_t i;
    char* lower = malloc(len * sizeof(char*));
    for (i = 0; i < len; i++) {
        lower[i] = tolower((int)str[i]);
    }
    return lower;
}

/**
 * \brief match a complex filter
 * \param dict the property dictionary
 * \param complex the complex filter
 * \return true if the filter matches the properties
 */
static int __match_complex_filter(struct dictionary *dict, filter_t complex)
{
    if (complex->node.complex.op == AND_OPERATOR) {
        return match_filter(dict, complex->node.complex.f1) && match_filter(
                dict, complex->node.complex.f2);
        /*
         * TODO: allow variable size of operands
         for (int i = 0; i < operandArray.length; i++) {
         if (!operandArray[i].match(values)) {
         return false;
         }
         }
         return true;
         */

    } else if (complex->node.complex.op == OR_OPERATOR) {
        return match_filter(dict, complex->node.complex.f1) || match_filter(
                dict, complex->node.complex.f2);
        /*
         * TODO: allow variable size of operands

         for (int i = 0; i < operandArray.length; i++) {
         if (operandArray[i].match(values)) {
         return true;
         }
         }
         return false;
         */
    } else if (complex->node.complex.op == NOT_OPERATOR) {
        return !match_filter(dict, complex->node.complex.f1);
    }
    return 0;
}

/**
 * \brief compare two strings case insensitive and regarding wildcards
 */
static int __compare_string(const char* c1, size_t p1, const size_t l1,
        const char* c2, size_t p2, const size_t l2)
{

    if (p1 == l1) {
        return 0;
    }

    while (p1 < l1 && p2 < l2) {
        char cc1 = c1[p1];
        char cc2 = c2[p2];

        // optimistic: case sensitive
        if (cc1 == cc2) {
            p1++;
            p2++;
            continue;
        }

        // then try case insensitive
        if (cc1 > 'A' && cc1 < 'Z') {
            cc1 = (char) (cc1 + 32);
        }
        if (cc2 > 'A' && cc2 < 'Z') {
            cc2 = (char) (cc2 + 32);
        }
        if (cc1 == cc2) {
            p1++;
            p2++;
            continue;
        }

        // wildcard?
        if (cc1 == '*') {
            p1++;
            do {
                if (__compare_string(c1, p1, l1, c2, p2, l2) == 0) {
                    return 0;
                }
                p2++;
            } while (l2 - p2 > -1);
            return 1;
        } else {
            if (cc1 < cc2) {
                return -1;
            } else if (cc1 > cc2) {
                return 1;
            }
        }
    }
    if (p1 == l1 && p2 == l2 && c1[p1 - 1] == c2[p2 - 1]) {
        return 0;
    }
    if (c1[p1 - 1] == '*' && p1 == l1 && p2 == l2) {
        return 0;
    }
    int min = l1 < l2 ? l1 : l2;
    return l1 == min ? -1 : 1;
}

/**
 * \brief match a simple filter
 * \param dict the property dictionary
 * \param simple the simple filter
 * \return true if the filter matches the properties
 */
static int __match_simple_filter(struct dictionary *dict, filter_t simple)
{
    void* value;
    // case insensitive matching
    // check as it is
    uint8_t type = dict->get(dict, simple->node.simple.id, &value);
    if (value == NULL) {
        // bad luck, try lower case
        char* lower = to_lower(simple->node.simple.id);
        type = dict->get(dict, lower, &value);
        free(lower);
        if (value == NULL) {
            // TODO: check all keys
            // requires an iterator structure on the dict
            return 0;
        }
    }

    // are we just checking for presence?
    // then we are done.
    if (simple->node.simple.comp == PRESENT) {
        return -1;
    }

    char* str_val;
    uintptr_t wrd_val;
    uintptr_t f_val;

    switch (type) {
    case TYPE_STRING:
        str_val = (char*) value;
        switch (simple->node.simple.comp) {
        case APROX:
            // TODO: strip whitespaces
        case EQUALS:
            return __compare_string(simple->node.simple.value, 0, strlen(
                    simple->node.simple.value), str_val, 0, strlen(str_val)) == 0;
        case GREATER:
            return __compare_string(simple->node.simple.value, 0, strlen(
                    simple->node.simple.value), str_val, 0, strlen(str_val)) <= 0;
        case LESS:
            return __compare_string(simple->node.simple.value, 0, strlen(
                    simple->node.simple.value), str_val, 0, strlen(str_val)) >= 0;
        }
    case TYPE_WORD:
        wrd_val = (uintptr_t) value;
        f_val = atoi(simple->node.simple.value);
        switch (simple->node.simple.comp) {
        case APROX:
        case EQUALS:
            return wrd_val == f_val;
        case GREATER:
            return wrd_val >= f_val;
        case LESS:
            return wrd_val <= f_val;
        }

    }
    return 0;
}

/**
 * \brief match a filter
 * \param dict the property dictionary
 * \param filter the filter
 * \return true if the filter matches the properties
 */
bool match_filter(struct dictionary *dict, filter_t filter)
{
    return filter->type == SIMPLE ? __match_simple_filter(dict, filter)
            : __match_complex_filter(dict, filter);
}

/**
 * \brief print a simple filter
 * \param node a simple filter node
 */
static void __print_simple_filter(FILE *stream, _filter_node node)
{
    fprintf(stream, "%s", node.simple.id);
    switch (node.simple.comp) {
    case EQUALS:
        fprintf(stream, "=");
        break;
    case PRESENT:
        fprintf(stream, "=*");
        break;
    case APROX:
        fprintf(stream, "~=");
        break;
    case LESS:
        fprintf(stream, "=<");
        break;
    case GREATER:
        fprintf(stream, "=>");
        break;
    }
    fprintf(stream, "%s", node.simple.value);
}

/**
 * \brief print a complex filter
 * \param node a complex filter node
 */
static void __print_complex_filter(FILE *stream, _filter_node node)
{
    switch (node.complex.op) {
    case AND_OPERATOR:
        fprintf(stream, "&");
        print_filter(stream, (filter_t) node.complex.f1);
        print_filter(stream, (filter_t) node.complex.f2);
        break;
    case NOT_OPERATOR:
        fprintf(stream, "!");
        print_filter(stream, (filter_t) node.complex.f1);
        break;
    case OR_OPERATOR:
        fprintf(stream, "|");
        print_filter(stream, (filter_t) node.complex.f1);
        print_filter(stream, (filter_t) node.complex.f2);
    }
}

/**
 * \brief print a filter
 * \param filter a filter
 */
void print_filter(FILE *stream, filter_t filter)
{
    if (filter->type == EMPTY) {
        fprintf(stream, "empty_filter");
    } else if (filter->type == SIMPLE) {
        fprintf(stream, "(");
        __print_simple_filter(stream, filter->node);
        fprintf(stream, ")");
    } else if (filter->type == COMPLEX) {
        fprintf(stream, "(");
        __print_complex_filter(stream, filter->node);
        fprintf(stream, ")\n");
    } else {
        fprintf(stream, "not a filter at all\n");
    }
}
