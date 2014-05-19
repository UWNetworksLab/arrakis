/**
 * \file
 * \brief scc shared memory
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_SCC_BARRELFISH_KPI_SHARED_MEM_H
#define ARCH_SCC_BARRELFISH_KPI_SHARED_MEM_H

#ifdef __scc__
#define SHARED_MEM_MIN          0x80000000
#define SHARED_MEM_MAX          0xc0000000
#define SHARED_MEM_SIZE         0x40000000
#define PERCORE_MEM_SIZE        0x1000000

#define PRIVATE_MEM_MAX          0x27000000

#define EXTRA_SHARED_MEM_MIN    0x70000000
#define EXTRA_SHARED_MEM_MAX    0x80000000

#endif

#endif // ARCH_SCC_BARRELFISH_KPI_SHARED_MEM_H
