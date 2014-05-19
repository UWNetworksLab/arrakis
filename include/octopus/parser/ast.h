/**
 * \file
 * \brief Header file containing definitions for the record ast.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef OCTOPUS_AST_H_
#define OCTOPUS_AST_H_

#include <stdlib.h>
#include <string.h>

#ifndef TEST_PARSER
#include <barrelfish/barrelfish.h>
#include <octopus/definitions.h>
#else
#define MAX_QUERY_LENGTH (5*1024)
#define SYS_ERR_OK 0
#define OCT_ERR_PARSER_FAIL 2
typedef long errval_t;
typedef long int64_t;
static int err_is_ok(errval_t err)
{
    return err == 0;
}
#endif

struct oct_parser_state {
    void* scanner;
    errval_t err;
    struct ast_object* ast;
};

struct string_buffer {
    // Buffer overflow should not be a problem as long as
    // we always check client input against MAX_QUERY_LENGTH
    char buffer[MAX_QUERY_LENGTH];
    char* ptr;
};

enum constraint_type {
    constraint_GT,
    constraint_GE,
    constraint_LT,
    constraint_LE,
    constraint_EQ,
    constraint_NE,
    constraint_REGEX
};

enum node_type {
    nodeType_Unset, // To detect errors
    nodeType_Float,
    nodeType_Constant,
    nodeType_Boolean,
    nodeType_String,
    nodeType_Ident,
    nodeType_Attribute,
    nodeType_Pair,
    nodeType_Constraint,
    nodeType_Object,
    nodeType_Scan,
    nodeType_Variable,
};

struct node_record {
    struct ast_object* name;
    struct ast_object* attrs;
    struct ast_object* constraints;
};

struct node_constant {
    int64_t value;
};

struct node_boolean {
    int value;
};

struct node_float {
    double value;
};

struct node_ident {
    char* str;
};

struct node_string {
    char* str;
};

struct node_scan {
    char c;
};

struct node_variable {
};

struct node_constraint {
    enum constraint_type op;
    struct ast_object* value;
};

struct node_attribute {
    struct ast_object* attr;
    struct ast_object* next;
};

struct node_pair {
    struct ast_object* left;
    struct ast_object* right;
};

struct ast_object {
    enum node_type type;

    union {
        struct node_constant cn;
        struct node_constraint cnsn;
        struct node_scan scn;
        struct node_boolean bn;
        struct node_float fn;
        struct node_ident in;
        struct node_string sn;
        struct node_attribute an;
        struct node_pair pn;
        struct node_record on;
        struct node_variable nv;
    } u;
};

errval_t generate_ast(const char* input, struct ast_object** record);
void free_ast(struct ast_object* p);

void ast_append_attribute(struct ast_object*, struct ast_object*);
struct ast_object* ast_find_attribute(struct ast_object*, char*);
struct ast_object* ast_remove_attribute(struct ast_object*, char*);

static inline struct ast_object* ast_alloc_node(void)
{
    struct ast_object* p = malloc(sizeof(struct ast_object));
    if (p == NULL) {
        assert(!"no more mem, bad!\n");
        // TODO how to abort parsing here to free up space?
    }
    memset(p, 0, sizeof(struct ast_object));

    return p;
}

static inline struct ast_object* ast_boolean(int value)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Boolean;
    p->u.bn.value = value;

    return p;
}

static inline struct ast_object* ast_variable(void)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Variable;

    return p;
}

static inline struct ast_object* ast_constraints(enum constraint_type op,
        struct ast_object* value)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Constraint;
    p->u.cnsn.op = op;
    p->u.cnsn.value = value;

    return p;
}

static inline struct ast_object* ast_floatingpoint(double value)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Float;
    p->u.fn.value = value;

    return p;
}

static inline struct ast_object* ast_scan(char c)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Scan;
    p->u.scn.c = c;

    return p;
}

static inline struct ast_object* ast_object(struct ast_object* name,
        struct ast_object* attrs)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Object;
    p->u.on.name = name;
    p->u.on.attrs = attrs;

    return p;
}

static inline struct ast_object* ast_attribute(struct ast_object* attr,
        struct ast_object* next)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Attribute;
    p->u.an.attr = attr;
    p->u.an.next = next;

    return p;
}

static inline struct ast_object* ast_pair(struct ast_object* left,
        struct ast_object* right)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Pair;
    p->u.pn.left = left;
    p->u.pn.right = right;

    return p;
}

static inline struct ast_object* ast_ident(char* str)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Ident;
    p->u.in.str = str;

    return p;
}

static inline struct ast_object* ast_string(char* str)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_String;
    p->u.sn.str = str;

    return p;
}

static inline struct ast_object* ast_num(int64_t value)
{
    struct ast_object* p = ast_alloc_node();

    p->type = nodeType_Constant;
    p->u.cn.value = value;

    return p;
}

#endif // OCTOPUS_AST_H_
