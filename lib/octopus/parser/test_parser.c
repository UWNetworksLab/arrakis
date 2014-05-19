/**
 * \file
 * \brief Use this file for quick parser tests on your machine.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#ifdef TEST_PARSER

#include <stdio.h>
#include <assert.h>
#include <stdarg.h>
#include <string.h>


#ifdef TEST_PARSER
#include "../../../include/octopus/parser/ast.h"
#else
#include <octopus/parser/ast.h>
#endif

#include "y.tab.h"


#define INITIAL_LENGTH 255

struct writer {
    char* output;
    size_t pos;
    size_t length;
};

struct skb_record {
	struct writer name;
	struct writer attributes;
	struct writer constraints;
};

void emit(struct writer*, const char*, ...);


// State used by tranlate()
static struct skb_record* sr;
static struct writer* w;
struct ast_object* attribute_name;


#ifdef PARSER_DEBUG
static void print_writer(struct writer* w) {
    printf("\tpos: %lu\n", w->pos);
    printf("\tlength: %lu\n", w->length);
    printf("\toutput: %s\n", w->output);
}
#endif


void emit(struct writer* w, const char* format, ...)
{
	assert(w != NULL);
	assert(format != NULL);
	va_list  args;

    if(w->output == NULL) {
        w->output = malloc(INITIAL_LENGTH);
        assert(w->output != NULL);
        w->pos = 0;
        w->length = INITIAL_LENGTH;
    }

	va_start(args, format);
	int append_len = vsnprintf(NULL, 0, format, args);
    va_end(args);

    size_t occupied = w->pos + append_len;
    if(w->length < occupied) {
        w->output = realloc(w->output, occupied+1);
        w->length = occupied;
    }

    va_start(args, format);
	int bytes_written = vsnprintf(w->output+w->pos, append_len+1, format, args);
    va_end(args);
    assert(bytes_written == append_len);

    w->pos = occupied;
}

static void translate(struct ast_object* p) {
	assert(sr != NULL);
    assert(p != NULL);

    switch(p->type) {
        case nodeType_Object:
            assert(p->u.on.name != NULL);
            w = &sr->name;

            translate(p->u.on.name);

            w = &sr->attributes;
            emit(w, "[ ");

			emit(&sr->constraints, "[ ");
            if(p->u.on.attrs) {

                translate(p->u.on.attrs);
            }
			emit(w, " ]");

            size_t len = strlen(sr->constraints.output);
            if(sr->constraints.output[len-2] == ',') {
				sr->constraints.output[len-2] = ' ';
				sr->constraints.output[len-1] = ']';
            }
            else {
            	emit(&sr->constraints, " ]");
            }

        break;

        case nodeType_Attribute:
            assert(p->u.an.attr != NULL);

            translate(p->u.an.attr);
            if(p->u.an.next != NULL) {
                emit(w, ", ");
                translate(p->u.an.next);
            }
        break;

        case nodeType_Pair:
            assert(p->u.pn.left != NULL);
            assert(p->u.pn.right != NULL);

            attribute_name = p->u.pn.left;

            translate(p->u.pn.left);
            emit(w, "::");
            translate(p->u.pn.right);
        break;

        case nodeType_Constraint:
            assert(p->u.cnsn.value != NULL);
            // prolog variable, dont care about result, just make sure it's set
            emit(w, "_");

            w = &sr->constraints;
            char* operator;
            switch(p->u.cnsn.op) {
                case constraint_GT:
                    operator = ">";
                break;
                case constraint_GE:
                    operator = ">=";
                break;
                case constraint_LT:
                    operator = "<";
                break;
                case constraint_LE:
                    operator = "=<";
                break;
                case constraint_EQ:
                    operator = "==";
                break;
                case constraint_NE:
                    operator = "=/=";
                break;
                case constraint_REGEX:
                	operator = "match";
				break;
                default:
                    assert(!"OP code not supported");
                break;
            }
            emit(w, "constraint(");
            translate(attribute_name);
            emit(w, ", ");
            emit(w, "'%s'", operator);
            emit(w, ", ");
            translate(p->u.cnsn.value);
            emit(w, "), ");
            w = &sr->attributes;
        break;

        case nodeType_Float:
            emit(w, "%f", p->u.fn.value);
        break;

        case nodeType_Boolean:
            if(p->u.bn.value) {
            	emit(w, "true");
            }
            else {
            	emit(w, "false");
            }
        break;

        case nodeType_Constant:
            emit(w, "%d", p->u.cn.value);
        break;

        case nodeType_String:
            emit(w, "\'");
            emit(w, p->u.sn.str);
            emit(w, "\'");
        break;

        case nodeType_Ident:
            emit(w, p->u.in.str);
        break;

        case nodeType_Unset:
        	assert(!"nodeType_Unset");
		break;
   }

}


static void walk_attributes(struct ast_object* ast) {
	struct ast_object* attr = ast->u.on.attrs;

	while(attr != NULL) {
		assert(attr->type == nodeType_Attribute);

		struct ast_object* pair = attr->u.an.attr;
		assert(pair != NULL);
		assert(pair->type == nodeType_Pair);

		struct ast_object* left = pair->u.pn.left;
		struct ast_object* right = pair->u.pn.right;;
		assert(left != NULL);
		assert(right != NULL);


		attr = attr->u.an.next;
	}
}

void transform_query(char* obj)
{

	struct ast_object* ast = NULL;
	errval_t err = generate_ast(obj, &ast);
	if(err == 0) {
	    printf("AST seems ok, translate...\n");
        sr = malloc(sizeof(struct skb_record));
        memset(sr, 0, sizeof(struct skb_record));
        translate(ast);
	}
	else {
	    assert(!"ast generation failed");
	}
	//free_ast(ast);
}


int main(int argc, char** argv)
{
	transform_query("spawn.1.ready");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	/*transform_query("obj2 {}");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	transform_query("obj3 { int: -11, fl: 12.0}");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	transform_query("obj4 { int: 12, fl: .0012321, fl2: .22123100 }");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	transform_query("obj5 { reference: bla, integer: 12, str: '[]String!@#%^&*$&^*(_)(-=\\'', float: 12.0, bool: true }");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	transform_query("obj5 { str1: 'String1', str2: 'String2' }");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	//transform_query("obj7 { c1: < 10, c1: > 11.0, c3: == 0, c4: >= 0, c5: <= .123 }");
	//printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);

	transform_query("obj7 { c1: r'^12.*ab$', c2: r'^ \\\\ ab*ab$  asd \\naasdf' }");
	printf("result: %s:\n\t%s\n\t%s\n", sr->name.output, sr->attributes.output, sr->constraints.output);
*/
    return 0;
}
#endif
