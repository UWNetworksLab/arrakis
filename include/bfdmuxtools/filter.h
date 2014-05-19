/**
 * \file
 * \brief Application registration API
 *
 * This file contains definitions for the bfdmux application register.
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __FILTER_H__
#define __FILTER_H__


#include <stdint.h>


/*
 * Op-Codes
 */

// Comparision
#define OP_EQUAL	0x11 /**< \brief Operator == */
#define OP_SGREATER	0x12 /**< \brief Operator > (signed) */
#define OP_SLESS	0x13 /**< \brief Operator < (signed) */
#define OP_UGREATER	0x14 /**< \brief Operator > (unsigned) */
#define OP_ULESS	0x15 /**< \brief Operator < (unsigned) */
#define OP_UNEQUAL	0x21 /**< \brief Operator != */
#define OP_SGREATEREQUAL 0x22 /**< \brief Operator >= (signed) */
#define OP_SLESSEQUAL	0x23 /**< \brief Operator <= (signed) */
#define OP_UGREATEREQUAL 0x24 /**< \brief Operator >= (unsigned) */
#define OP_ULESSEQUAL	0x25 /**< \brief Operator <= (unsigned) */

// Arithmetic
#define OP_ADD	0x31 /**< \brief Operator + */
#define OP_SUB	0x32 /**< \brief Operator - */
#define OP_MULT	0x33 /**< \brief Operator * */
#define OP_IDIV	0x34 /**< \brief Operator / (integer division) */
#define OP_MOD	0x35 /**< \brief Operator % */

// Logical
#define OP_NOT	0x41 /**< \brief Operator ! */
/**
 * \brief Operator &&
 *
 * Expects an additional 32bit word before the two operands
 * holding the code size of the first operand subtree in bytes.
 * This is used to speed up filter execution when the first operand
 * already determines the result of the operation, e.g. false && something.
*/
#define OP_AND	0x42
/**
 * \brief Operator ||
 *
 * Expects an additional 32bit word before the two operands
 * holding the code size of the first operand subtree in bytes.
 * This is used to speed up filter execution when the first operand
 * already determines the result of the operation, e.g. true || something.
 */
#define OP_OR	0x43

// Bitwise
#define OP_BNOT	0x51 /**< \brief Operator ~ */
#define OP_BAND	0x52 /**< \brief Operator & */
#define OP_BOR	0x53 /**< \brief Operator | */
#define OP_BXOR	0x54 /**< \brief Operator ^ */

// Immediate values
#define OP_INT8		0x61 /**< \brief 8 bit immediate value, data follows */
#define OP_INT16	0x62 /**< \brief 16 bit immediate value, data follows */
#define OP_INT32	0x63 /**< \brief 32 bit immediate value, data follows */
#define OP_INT64	0x64 /**< \brief 64 bit immediate value, data follows */

// Load data
#define OP_LOAD8	0x71 /**< \brief 8 bit indirect value, location follows */
#define OP_LOAD16	0x72 /**< \brief 16 bit indirect value, location follows */
#define OP_LOAD32	0x73 /**< \brief 32 bit indirect value, location follows */
#define OP_LOAD64	0x74 /**< \brief 64 bit indirect value, location follows */


#endif
