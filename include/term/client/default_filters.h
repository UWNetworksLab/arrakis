/**
 * \file
 * \brief Set of default filters for terminal client library.
 */

/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef LIBTERM_CLIENT_DEFAULT_FILTERS_H
#define LIBTERM_CLIENT_DEFAULT_FILTERS_H

#include <stddef.h>

void term_filter_cr2lf(char **data, size_t *length);
void term_filter_ctrlhat(char **data, size_t *length);
void term_filter_lf2crlf(char **data, size_t *length);

#endif // LIBTERM_CLIENT_DEFAULT_FILTERS_H
