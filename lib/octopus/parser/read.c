/**
 * \file
 * \brief Helper functions to read record contents.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Universitaetstr. 6, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>

#include <barrelfish/barrelfish.h>

#include <octopus/getset.h>
#include <octopus/parser/ast.h>

/**
 * \brief Reads the content of a record string based on the provided format.
 * Currently supported %d (int64_t*), %f (double*?), %s (char**).
 * TODO: docs && support proper scan types from (inttypes.h)
 *
 * \param record Record to read.
 * \param format What you want to read.
 * \param ... Values read are stored in the provided arguments.
 *
 * \retval SYS_ERR_OK
 * \retval OCT_ERR_ATTRIBUTE_NOT_FOUND
 * \retval OCT_ERR_TYPE_MISMATCH
 * \retval OCT_ERR_RECORD_NAME_MISMATCH
 * \retval OCT_ERR_ATTRIBUTE_MISMATCH
 */
errval_t oct_read(const char* record, const char* format, ...)
{
	errval_t err = SYS_ERR_OK;
	va_list args;
	va_start(args, format);

	char** s = NULL;
	int64_t* i = NULL;
	double* d = NULL;

	// Parse record and format strings
	struct ast_object* ast = NULL;
	struct ast_object* format_ast = NULL;

	err = generate_ast(record, &ast);
	if(err_is_fail(err)) {
		goto out;
	}
	err = generate_ast(format, &format_ast);
	if(err_is_fail(err)) {
		goto out;
	}

	// Scan Name
	struct ast_object* format_name = format_ast->u.on.name;
	switch(format_name->type) {
	case nodeType_Scan:
		if(format_name->u.scn.c != 's') {
			err = OCT_ERR_INVALID_FORMAT;
			goto out;
		}
		s = va_arg(args, char**);
		*s = ast->u.on.name->u.in.str;
		// Remove from AST so client has to free it
		ast->u.on.name->u.in.str = NULL;
		break;

	case nodeType_Variable:
		// Just ignore record name
		break;

	default:
		err = OCT_ERR_INVALID_FORMAT;
		goto out;
		break;
	}

	// Scan Attributes
	struct ast_object* attr = format_ast->u.on.attrs;
	for(; attr != NULL; attr = attr->u.an.next) {

		struct ast_object* format_attr = attr->u.an.attr;

		// Enforced by Parser
		assert(format_attr->type == nodeType_Pair);
		assert(format_attr->u.pn.left->type == nodeType_Ident);
		if(format_attr->u.pn.right->type != nodeType_Scan) {
			err = OCT_ERR_INVALID_FORMAT;
			goto out;
		}

		// Try to find attribute in record AST
		struct ast_object* record_attr = ast_find_attribute(ast,
		        format_attr->u.pn.left->u.in.str);
		if(record_attr == NULL) {
			err = OCT_ERR_UNKNOWN_ATTRIBUTE;
			goto out;
		}
		struct ast_object* value = record_attr->u.pn.right;

		switch(format_attr->u.pn.right->u.scn.c) {
		case 's':
			s = va_arg(args, char**);
			if(value->type == nodeType_Ident) {
				*s = value->u.in.str;
				value->u.in.str = NULL;
			}
			else if(value->type == nodeType_String) {
				*s = value->u.sn.str;
				value->u.sn.str = NULL;
			}
			else {
				err = OCT_ERR_INVALID_FORMAT;
				goto out;
			}
			break;

		case 'd':
			i = va_arg(args, int64_t*);
			if(value->type == nodeType_Constant) {
				*i = value->u.cn.value;
			}
			else {
				*i = 0;
				err = OCT_ERR_INVALID_FORMAT;
				goto out;
			}
			break;

		case 'f':
			d = va_arg(args, double*);
			if(value->type == nodeType_Float) {
				*d = value->u.fn.value;
			}
			else {
				*d = 0.0;
				err = OCT_ERR_INVALID_FORMAT;
				goto out;
			}
			break;

		default:
			err = OCT_ERR_INVALID_FORMAT;
			goto out;
			break;
		}
	}
	va_end(args);

	out:
	free_ast(ast);
	free_ast(format_ast);
	return err;
}
