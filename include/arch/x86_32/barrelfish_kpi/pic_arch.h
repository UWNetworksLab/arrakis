/**
 * \file
 * \brief x86-32 Position Independent code assembly support definitions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef ARCH_X86_32_BARRELFISH_KPI_PIC_H
#define ARCH_X86_32_BARRELFISH_KPI_PIC_H

#ifdef __PIC__

# undef __i686 /* gcc builtin define gets in our way */
# define MUNG_LOCAL(sym)   sym ## @GOTOFF(%ecx)
# define MUNG_EXTERN(sym)  sym ## @GOT(%ecx)
# define DEREF_EXTERN(reg) movl (reg), reg
# define INIT_PIC() \
	call __i686.get_pc_thunk.cx ; \
	addl $_GLOBAL_OFFSET_TABLE_, %ecx

#else

# define MUNG_LOCAL(sym)   sym
# define MUNG_EXTERN(sym)  sym
# define DEREF_EXTERN(reg)
# define INIT_PIC()

#endif

#endif // ARCH_X86_32_BARRELFISH_KPI_PIC_H
