/**
 * \file
 * \brief Bfdmux core functionality
 *
 * Operator precedence definition and opcode/opstring binding
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "opdefs.h"

/**
 * \brief List of operators and opcodes
 *
 * Operators with lower indices have lower precedence.
 * \warning If one operator string is contained in another one, the longer opstring needs to reside in a lower index in the array!
 * Example: "<" and "<=". If this is not the case the string "4 <= 5" will be split into "4" < "= 5", which will lead to a compile
 * error in the right subexpression!
 * \note For OP_OR and OP_AND \n
 * 5 specifies that 4 extra bytes should be left after the opcode: they will hold the left subtree length for skipping the right subtree evaluation if possible.
 * \note The empty operator signals the end of the operator list, do not remove it!
 */
op_def_t        op_list[] = {
	{"||", OP_OR, 5, 0x11}
	,
	{"&&", OP_AND, 5, 0x11}
	,
	{"|", OP_BOR, 1, 0x11}
	,
	{"^", OP_BXOR, 1, 0x11}
	,
	{"&", OP_BAND, 1, 0x11}
	,
	{"==", OP_EQUAL, 1, 0x11}
	,
	{"!=", OP_UNEQUAL, 1, 0x11}
	,
	{">=", OP_SGREATEREQUAL, 1, 0x11}
	,
	{"<=", OP_SLESSEQUAL, 1, 0x11}
	,
	{"}=", OP_UGREATEREQUAL, 1, 0x11}
	,
	{"{=", OP_ULESSEQUAL, 1, 0x11}
	,
	{">", OP_SGREATER, 1, 0x11}
	,
	{"<", OP_SLESS, 1, 0x11}
	,
	{"}", OP_UGREATER, 1, 0x11}
	,
	{"{", OP_ULESS, 1, 0x11}
	,
	{"+", OP_ADD, 1, 0x11}
	,
	{"-", OP_SUB, 1, 0x11}
	,
	{"*", OP_MULT, 1, 0x11}
	,
	{"/", OP_IDIV, 1, 0x11}
	,
	{"%", OP_MOD, 1, 0x11}
	,
	{"!", OP_NOT, 1, 0x01}
	,
	{"~", OP_BNOT, 1, 0x01}
	,
	{"int8", OP_LOAD8, 1, 0x01}
	,
	{"int16", OP_LOAD16, 1, 0x01}
	,
	{"int32", OP_LOAD32, 1, 0x01}
	,
	{"int64", OP_LOAD64, 1, 0x01}
	,
	{"", 0, 0, 0}
};
