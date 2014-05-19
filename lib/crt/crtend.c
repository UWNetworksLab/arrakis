/**
 * \file
 * \brief C++ startup code. Contains .ctors section footer and ABI support.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <sys/_types.h>

/// A global constructor takes no arguments and returns nothing
typedef void (*CDtor)(void);

/// NULL terminate the .ctors array
static CDtor ctors[1]
__attribute__ ((used, section(".ctors"), aligned(sizeof(CDtor))))
    = { (CDtor)(0) };
