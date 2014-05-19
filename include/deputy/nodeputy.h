/** \file
 * \brief Macros for disabling Deputy annotations.
 *
 * This file defines away all the deputy annotations, for
 * use with the -imacros argument to GCC.
 */

/*
 * Copyright (c) 2007, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define BOUNT(b,e)
#define BND(b,e)
#define COUNT(n)
#define CT(n)
#define SAFE
#define SNT
#define SEQ
#define FSEQ
#define NONNULL
#define OPT
#define NT
#define NTS
#define NTDROP(e) (e)
#define NTEXPAND(e) (e)
#define WHEN(e)
#define TRUSTED
#define TRUSTEDBLOCK
#define TC(e) (e)

#define DALLOC(e)
#define DFREE(p)
#define DREALLOC(p, e)
#define DMEMCPY(x,y,z)
#define DMEMSET(x,y,z)
#define DMEMCMP(x,y,z)
