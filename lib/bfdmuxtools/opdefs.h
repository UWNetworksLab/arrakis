/**
 * \file
 * \brief Header file for opcode definitions
 *
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __OPDEFS_H__
#define __OPDEFS_H__

#ifndef DOXYGEN
// exclude system headers from documentation

#include <stdint.h>

#endif							// DOXYGEN

#include <bfdmuxtools/filter.h>

#define MAX_OPERATOR_STRING_LENGTH 9 /**< \brief Maximum length of an operator string in characters */

/**
 * \brief Defines a type for operator definition entries
 * \warning Operator strings cannot contain brackets!
 */
typedef struct {
	char            opstr[MAX_OPERATOR_STRING_LENGTH];
											/**< \brief The string representing the operator */
	uint8_t         opcode;
					/**< \brief The binary opcode the operator maps to */
	uint8_t         reserved_length;
							 /**< \brief The number of bytes that should be reserved for this operator. Usually this is exactly one byte. See c file for exceptions. */
	uint8_t         arity;
				   /**< \brief Specifies if the operator expects left, right, or both sides to be operands. 0x10 for left-unary, 0x01 for right-unary, 0x11 for binary operators. */
} op_def_t;

extern op_def_t op_list[];

#endif
