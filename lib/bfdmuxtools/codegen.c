/**
 * \file
 * \brief Code synthesizer for bfdmux filters
 *
 * This file provides functions to create byte code in Bfdmux Intermediate Language
 * from filter strings. This byte code can be executed using the functions in vm.c
 * to filter network packets.
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DOXYGEN
// exclude system headers from documentation

#include <stdio.h>
#include <stdlib.h>
//#include <strings.h>
#include <string.h>
#include <stdbool.h>
//#include <errno.h>

//int errno;

#endif							// DOXYGEN

#include <bfdmuxtools/codegen.h>
#include <bfdmuxtools/filter.h>
#include <bfdmuxtools/tools.h>
#include "opdefs.h"

/**
 * \brief Searches for 'substr' in 'str', between 'start_pos' and 'end_pos' inclusive, and not inside brackets!
 * @param str the string to be searched
 * @param start_pos the index of the first character of the search interval
 * @param end_pos the index of the last character of the search interval
 * @param substr the substring to look for
 * @return returns the position at which 'substr' has been found, -1 if not found, -2 on bracket error
 */
static inline int
substrfind(char *str, int start_pos, int end_pos, char *substr)
{
	int             i,
	                j;
	int             open_braces = 0;
	int             sub_len = strlen(substr);
	int             i_end = end_pos - sub_len + 1;
	for (i = start_pos; i <= i_end; i++) {
		if ((str[i] == '(') || (str[i] == '[')) {
			open_braces++;
		} else if ((str[i] == ')') || (str[i] == ']')) {
			open_braces--;
			if (open_braces < 0)
				return -2;
		} else if (open_braces == 0) {	// try matching substr at this
			// position
			j = 0;
			while ((str[i + j] == substr[j]) && (j < sub_len)) {
				j++;
			}
			if (j == sub_len)
				return i;
		}
	}
	return -1;
}

/**
 * \brief Checks if 'out' still has 'space_needed' empty bytes
 * @param out the byte array
 * @param out_len the number of bytes already in the array 'out'
 * @param out_sz the length of the currently reserved space of 'out'
 * @param space_needed the number of bytes that need to be appended to 'out'
 * @return returns true if enough space or reservation of larger space succeeded, otherwise false
 */
static inline          bool
ensure_enough_space(uint8_t ** out, int *out_len, int *out_sz,
					int space_needed)
{
	int             need = *out_len + space_needed - *out_sz;
	if (need > 0) {				// need realloc!
		*out_sz =
			*out_sz +
			INCREMENTAL_ALLOC_SIZE * (need / INCREMENTAL_ALLOC_SIZE + 1);
		if ((*out_sz) > MAX_FILTER_CODE_SIZE)
			return false;
		void           *new_out = realloc(*out, *out_sz);
		if (new_out == NULL)
			return false;
		*out = new_out;
	}
	return true;
}

/**
 * \brief Tries to find the lowest precedence operator in an interval of 'expr'
 * @param expr the expression to search in
 * @param from_pos the first character of the serach interval
 * @param to_pos the last character of the search interval
 * @param pos the position where the operator has been found, if any
 * @return the index of the operator in the op_list or -1 if no operator found
 */
static int
find_operator(char *expr, int from_pos, int to_pos, int *pos)
{
	int             i,
	                oppos;
	i = 0;
	while (op_list[i].opcode != 0) {
		oppos = substrfind(expr, from_pos, to_pos, op_list[i].opstr);
		if (oppos >= 0) {
			*pos = oppos;
			return i;
		}
		i++;
	}
	return -1;
}

/**
 * \brief Removes leading and trailing spaces, and brackets that sourround the expression
 * @param expr the expression to work with
 * @param from_pos the start of the inverval to consider; may be shifted to the right
 * @param to_pos the end of the interval to consider; may be shifted left
 */
static void
remove_spaces_and_braces(char *expr, int *from_pos, int *to_pos)
{
	int             i,
	                cnt;

	// Cut off spaces and braces around expr
	while ((*from_pos) <= (*to_pos)) {
		if (expr[*from_pos] == ' ') {
			(*from_pos)++;
		} else if (expr[*to_pos] == ' ') {
			(*to_pos)--;
		} else if ((expr[*from_pos] == '(') && (expr[*to_pos] == ')')) {
			// check if this is a matching bracket pair, eg. (...), and
			// not (...).(...)
			i = (*from_pos) + 1;
			cnt = 0;
			while (i < (*to_pos)) {
				if (expr[i] == '(') {
					cnt++;
				} else if (expr[i] == ')') {
					cnt--;
				}
				if (cnt < 0) {
					break;		// The bracket at 'from_pos' was closed.
					// pair does not match!
				}
				i++;
			}
			// Remove brackets, if matching pair
			if (cnt == 0) {
				(*from_pos)++;
				(*to_pos)--;
			} else {
				break;
			}
		} else if ((expr[*from_pos] == '[') && (expr[*to_pos] == ']')) {	// same
																			//
			//
			// for
			// square
			// brackets
			// check if this is a matching bracket pair, eg. (...), and
			// not (...).(...)
			i = (*from_pos) + 1;
			cnt = 0;
			while (i < (*to_pos)) {
				if (expr[i] == '[') {
					cnt++;
				} else if (expr[i] == ']') {
					cnt--;
				}
				if (cnt < 0) {
					break;		// The bracket at 'from_pos' was closed.
					// pair does not match!
				}
				i++;
			}
			// Remove brackets, if matching pair
			if (cnt == 0) {
				(*from_pos)++;
				(*to_pos)--;
			} else {
				break;
			}
		} else {
			break;
		}
	}
}


/**
 * \brief Compiles a whole subexpression and appends the byte code to 'out'
 * @param expr the expression to work with
 * @param from_pos the first character of the subexpression to compile
 * @param to_pos the last character of the subexpression to compile
 * @param out a pointer to the array that holds the compiled byte code
 * @param out_len the number of bytes already in the array
 * @param out_sz the current size of the array (reallocation, if full)
 * @return returns 0 on success, -1 on memory error, or an index of a character in the subexpression that failed to compile
 */
static int
compile_subtree(char *expr, int from_pos, int to_pos, uint8_t ** out,
				int *out_len, int *out_sz)
{
	int             pos,
	                old_len,
	                msb;
	int             op,
	                err_pos;

	remove_spaces_and_braces(expr, &from_pos, &to_pos);

	if (from_pos > to_pos)
		return (from_pos + to_pos) / 2 + 1;

	// Try to find an operator (op will be it's index in the array, or -1
	op = find_operator(expr, from_pos, to_pos, &pos);
	if (op >= 0) {
		int             opstrlen = strlen(op_list[op].opstr);
		// Enforce syntax: Don't ignore left part of the expression, if
		// operator binds to the right, or vice versa!
		if ((op_list[op].arity == 0x10) && (pos + opstrlen <= to_pos)) {
			return pos + 1;
		} else if ((op_list[op].arity == 0x01) && (pos > from_pos)) {
			return pos + 1;
		}
		if (!ensure_enough_space
			(out, out_len, out_sz, op_list[op].reserved_length))
			return -1;
		(*out)[*out_len] = op_list[op].opcode;
		*out_len += op_list[op].reserved_length;
		old_len = *out_len;		// save output length
		if (op_list[op].arity & 0x10) {	// left side relevant for operator
			if ((err_pos =
				 compile_subtree(expr, from_pos, pos - 1, out, out_len,
								 out_sz)) != 0)
				return err_pos;
		}
		if (op_list[op].arity & 0x01) {	// right side relevant for
			// operator
			if ((err_pos =
				 compile_subtree(expr, pos + opstrlen, to_pos, out,
								 out_len, out_sz)) != 0)
				return err_pos;
		}

		if ((op_list[op].opcode == OP_OR) || (op_list[op].opcode == OP_AND)) {	// special
																				//
			//
			// treatment
			// for
			// logical
			// or/and
			// operations
			// compute and store subtree size in empty 32bits after opcode
			*((uint32_t *) & ((*out)[old_len - 4])) = *out_len - old_len;
		}
		return 0;
	}
	// If no operator found, try to interpret string as integer constant
	//errno = 0;
	char           *endptr = expr + from_pos;
	//uint64_t val = (long long int)strtol(endptr + from_pos, &endptr, 10);
    	uintmax_t        val = strtoumax(expr + from_pos, &endptr, 0);
	//if ((errno == 0) && (endptr == expr + to_pos + 1)) {	// ensures
	if ((val != -1) && (endptr == expr + to_pos + 1)) {	// ensures
		// that all
		// characters
		// were valid!
		msb = find_msb(val);	// Determine size of operand
		if (msb <= 8) {
			if (!ensure_enough_space(out, out_len, out_sz, 1 + 1))
				return -1;
			(*out)[*out_len] = OP_INT8;
			(*out_len)++;
			(*out)[*out_len] = (uint8_t) val;
			(*out_len)++;
		} else if (msb <= 16) {
			if (!ensure_enough_space(out, out_len, out_sz, 1 + 2))
				return -1;
			(*out)[*out_len] = OP_INT16;
			(*out_len)++;
			*((uint16_t *) & ((*out)[*out_len])) = (uint16_t) val;
			*out_len += 2;
		} else if (msb <= 32) {
			if (!ensure_enough_space(out, out_len, out_sz, 1 + 4))
				return -1;
			(*out)[*out_len] = OP_INT32;
			(*out_len)++;
			*((uint32_t *) & ((*out)[*out_len])) = (uint32_t) val;
			*out_len += 4;
		} else {
			if (!ensure_enough_space(out, out_len, out_sz, 1 + 8))
				return -1;
			(*out)[*out_len] = OP_INT64;
			(*out_len)++;
			*((uint64_t *) & ((*out)[*out_len])) = val;
			*out_len += 8;
		}
		return 0;
	}
	return (from_pos + to_pos) / 2 + 1 + 1;
}

/**
 * \brief Compiles a filter expression in Bfdmux Filter Language into Bfdmux Intermediate Code
 * @param expression the expression to compile
 * @param[out] filter_code Points to the memory buffer that contains the compiled filter, or NULL, if an error occurred
 * @param[out] filter_len Indicates the length of the compiled code, or contains the error position in the filter string on failure
 */
void
compile_filter(char *expression, uint8_t ** filter_code, int32_t *filter_len)
{
	int             out_sz = INITIAL_ALLOC_SIZE;	// initially allocate
	// 64 bytes for code
	// array
	int             out_len = 0;
	int             err_pos = 0;
	uint8_t        *output = malloc(sizeof(uint8_t) * out_sz);
	if (output != NULL) {
		// int len = strlen(expression);
//		printf("Filter %s\n", expression);
		if ((err_pos =
			 compile_subtree(expression, 0, strlen(expression) - 1,
							 &output, &out_len, &out_sz)) != 0) {
			free(output);
			output = NULL;
			out_len = err_pos;	// return error position
		} else {
			void           *new_output =
				(uint8_t *) realloc(output, out_len);
			if (new_output) {
				output = new_output;
			}
		}
	} else {
		printf("compile_filter: malloc failed\n");
	}

	*filter_code = output;
	*filter_len = out_len;
}
