/**
 * \file
 * \brief Terminal emulator.
 */

/*
 * Copyright (c) 2007, 2008, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, CAB F.78, Universitaetstr. 6, CH-8092 Zurich,
 * Attn: Systems Group.
 */

#ifndef BARRELFISH_TERMINAL_H
#define BARRELFISH_TERMINAL_H

#include <sys/cdefs.h>

__BEGIN_DECLS

size_t terminal_write(const char *data, size_t length);
size_t terminal_read(char *data, size_t count);

errval_t terminal_init(void);
void terminal_exit(void);

__END_DECLS

#endif // BARRELFISH_TERMINAL_H
