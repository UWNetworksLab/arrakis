/**
 * \file
 * \brief Code synthesizer for bfdmux filters
 *
 * This file provides the interface for the filter code generator.
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef __CODEGEN_H__
#define __CODEGEN_H__

#include <bfdmuxtools/filter.h>

#include <inttypes.h>

/**
 * \brief Maximum number of bytes for a compiled filter
 *
 * This is used to limit bfdmux's workload in some way.
 * \todo Implement a better restriction for the filter processing time per application
 */
#define MAX_FILTER_CODE_SIZE 256

#define INITIAL_ALLOC_SIZE 64 /**< Size of initially allocated filter code block */
#define INCREMENTAL_ALLOC_SIZE 64 /**< Size of realloc'ed blocks if filter code doesn't fit */

void compile_filter(char *expression, uint8_t ** filter_code, int32_t *filter_len);

#endif
