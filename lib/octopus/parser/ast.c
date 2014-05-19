/**
 * \file
 * \brief Function to generate/manipulate abstract syntax tree for records.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>

#ifdef TEST_PARSER
#include "../../../include/octopus/parser/ast.h"
#else
#include <octopus/parser/ast.h>
#endif

#include "y.tab.h"
#include "flex.h"

void free_ast(struct ast_object* p)
{
    if (p == NULL) {
        return;
    }

    switch (p->type) {
    case nodeType_Object:
        free_ast(p->u.on.name);
        free_ast(p->u.on.attrs);
        break;

    case nodeType_Attribute:
        free_ast(p->u.an.attr);
        free_ast(p->u.an.next);
        break;

    case nodeType_String:
        free(p->u.sn.str);
        p->u.sn.str = NULL;
        break;

    case nodeType_Ident:
        free(p->u.in.str);
        p->u.in.str = NULL;
        break;

    case nodeType_Constraint:
        free_ast(p->u.cnsn.value);
        break;

    case nodeType_Pair:
        free_ast(p->u.pn.left);
        free_ast(p->u.pn.right);
        break;

    case nodeType_Unset:
        assert(!"nodeType_Unset encountered in free_ast!");
        abort();
        break;

    default:
        // Nothing special to do for value nodes
        break;
    }

    free(p);
}

void ast_append_attribute(struct ast_object* ast, struct ast_object* to_insert)
{
    struct ast_object** attr = &ast->u.on.attrs;
    for (; *attr != NULL; attr = &(*attr)->u.an.next) {
        // continue
    }

    struct ast_object* new_attr = ast_alloc_node();
    new_attr->type = nodeType_Attribute;
    new_attr->u.an.attr = to_insert;
    new_attr->u.an.next = NULL;
    *attr = new_attr;
}

struct ast_object* ast_find_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->u.on.attrs;

    for (; *attr != NULL; attr = &(*attr)->u.an.next) {

        assert((*attr)->type == nodeType_Attribute);
        if (strcmp((*attr)->u.an.attr->u.pn.left->u.in.str, name) == 0) {
            return (*attr)->u.an.attr;
        }

    }

    return NULL;
}

struct ast_object* ast_remove_attribute(struct ast_object* ast, char* name)
{
    struct ast_object** attr = &ast->u.on.attrs;

    for (; *attr != NULL; attr = &(*attr)->u.an.next) {

        assert((*attr)->type == nodeType_Attribute);
        struct ast_object* pair = (*attr)->u.an.attr;
        struct ast_object* left = pair->u.pn.left;

        if (strcmp(left->u.in.str, name) == 0) {
            struct ast_object* current_attr = *attr;

            *attr = current_attr->u.an.next;

            current_attr->u.an.next = NULL;
            current_attr->u.an.attr = NULL;
            free_ast(current_attr);

            return pair;
        }

    }

    return NULL;
}

errval_t generate_ast(const char* input, struct ast_object** record)
{
    // Save re-entrant state for Flex/Bison
    struct oct_parser_state p;
    p.ast = NULL;
    p.err = SYS_ERR_OK;
    p.scanner = NULL;

    struct string_buffer buf;
    buf.ptr = NULL;

    int res = octyy_lex_init_extra(&buf, &p.scanner);
    if (res != 0) {
        goto out;
    }

    // Run Lexer and Parser
    octyy__scan_string(input, p.scanner);
    res = octyy_parse((void*) &p);
    octyy_lex_destroy(p.scanner);
    if (res == 0) {
        *record = p.ast;
        p.ast = NULL;
        return SYS_ERR_OK;
    }

    out:
    // Memory got cleaned up by bison destructors...
    *record = NULL;
    return OCT_ERR_PARSER_FAIL;
}
