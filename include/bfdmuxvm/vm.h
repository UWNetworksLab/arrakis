/**
 * \file
 * \brief Interface for filter execution virtual machine
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

#ifndef __VM_H__
#define __VM_H__

#ifndef DOXYGEN
// exclude system headers from documentation

#include <stdbool.h>
//#include <arpa/inet.h>

#endif                          // DOXYGEN

#include <bfdmuxtools/filter.h>


/*
 * Errors
 */
#define ERR_BAD_OP	-1 /**< \brief Execution failed because of an unknown opcode */
#define ERR_BAD_ACCESS	-2 /**< \brief Filter did not match because it tried to access a non existing location in the packet */
#define ERR_UNKNOWN	-3 /**< \brief An unknown internal error occurred during the execution */

/**
 * \brief Define opcode type as single byte
 */
typedef uint8_t op_t;

bool execute_filter(uint8_t * filter_code, int filter_len,
                    uint8_t * packet_data, int packet_len, int *error_out);
#endif
