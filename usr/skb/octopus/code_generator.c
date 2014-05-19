/**
 * \file
 * \brief This file contains functions to generate Eclipse CLP terms based on
 * the AST of a given record.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include "code_generator.h"

// forward decl
static inline struct pword_pair create_constraint(struct ast_object*);

struct pword_pair {
    bool is_attribute;
    pword value;
    pword op;
};

static inline struct pword_pair visit_attribute_right(struct ast_object* p)
{
    struct pword_pair terms;
    terms.is_attribute = true;

    switch (p->type) {
    case nodeType_Ident:
        terms.value = ec_atom(ec_did(p->u.in.str, 0));
        break;

    case nodeType_String:
        assert(p->u.sn.str != NULL);
        terms.value = ec_string(p->u.sn.str);
        break;

    case nodeType_Float:
        terms.value = ec_double(p->u.fn.value);
        break;

    case nodeType_Constant:
        terms.value = ec_long(p->u.cn.value);
        break;

    case nodeType_Variable:
        terms.value = ec_newvar();
        break;

    case nodeType_Constraint:
        terms = create_constraint(p);
        break;

    default:
        assert(!"Should not happen, check your parser!");
        break;

    }

    return terms;
}

static inline struct pword_pair create_constraint(struct ast_object* p)
{
    assert(p != NULL);
    assert(p->type == nodeType_Constraint);

    struct pword_pair terms = visit_attribute_right(p->u.cnsn.value);
    terms.is_attribute = false;

    switch (p->u.cnsn.op) {
    case constraint_GT:
        terms.op = ec_atom(ec_did(">", 2));
        break;

    case constraint_GE:
        terms.op = ec_atom(ec_did(">=", 2));
        break;

    case constraint_LT:
        terms.op = ec_atom(ec_did("<", 2));
        break;

    case constraint_LE:
        terms.op = ec_atom(ec_did("<=", 2));
        break;

    case constraint_EQ:
        terms.op = ec_atom(ec_did("==", 2));
        break;

    case constraint_NE:
        terms.op = ec_atom(ec_did("!=", 2));
        break;

    case constraint_REGEX:
        terms.op = ec_atom(ec_did("match", 0));
        break;

    default:
        assert(!"OP code not supported");
        break;
    }

    return terms;
}

static void translate(struct ast_object* p, struct skb_ec_terms* ss)
{
    assert(p != NULL);
    assert(p->type == nodeType_Object);

    ss->attribute_list = ec_nil();
    ss->constraint_list = ec_nil();

    struct ast_object* name = p->u.on.name;

    if (name->type == nodeType_Ident) {
        dident name_id = ec_did(name->u.in.str, 0);
        ss->name = ec_atom(name_id);
    } else if (name->type == nodeType_Variable) {
        ss->name = ec_newvar();
    } else if (name->type == nodeType_Constraint) {
        assert(name->u.cnsn.op == constraint_REGEX);
        // Construct match term for name regex
        dident constraint = ec_did("name_constraint", 1);
        pword regex = ec_string(name->u.cnsn.value->u.sn.str);
        pword constraint_term = ec_term(constraint, regex);
        ss->name = constraint_term;
    }
    else {
        assert(!"Scan types not allowed here");
    }


    struct ast_object* iter = p->u.on.attrs;
    for (; iter != NULL; iter = iter->u.an.next) {
        assert(iter->type == nodeType_Attribute);
        struct ast_object* left = iter->u.an.attr->u.pn.left;
        struct ast_object* right = iter->u.an.attr->u.pn.right;

        dident attr_id = ec_did(left->u.in.str, 0);
        pword left_term = ec_atom(attr_id);

        struct pword_pair right_terms = visit_attribute_right(right);

        if (right_terms.is_attribute) {
            pword entry = ec_term(ec_did("val", 2), left_term,
                    right_terms.value);
            ss->attribute_list = ec_list(entry, ss->attribute_list);
        } else { // is constraint
            dident constraint = ec_did("constraint", 3);
            pword entry = ec_term(constraint, left_term, right_terms.op,
                    right_terms.value);
            ss->constraint_list = ec_list(entry, ss->constraint_list);
        }
    }

}

errval_t transform_record(struct ast_object* ast, struct skb_ec_terms* record)
{
    assert(ast != NULL);
    translate(ast, record);

    return SYS_ERR_OK;
}

