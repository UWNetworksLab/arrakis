/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Derived from the The Open Group Base Specifications Issue 6, IEEE
 * Std 1003.1, 2004 Edition. 
 * http://www.opengroup.org/onlinepubs/009695399/basedefs/nl_types.h.html
 */

#ifndef _NL_TYPES_H_
#define _NL_TYPES_H_

#include <sys/cdefs.h>

__BEGIN_DECLS

#define	NL_SETD		0
#define	NL_CAT_LOCALE	1

typedef void    *nl_catd;

// TODO: Need nl_item

nl_catd  catopen(const char *, int);
char    *catgets(nl_catd, int, int, const char *);
int	 catclose(nl_catd);

__END_DECLS

#endif
