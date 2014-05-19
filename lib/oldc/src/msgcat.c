/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <nl_types.h>
#include <stdio.h>

nl_catd catopen(const char *name, int type)
{
    printf("Warning: catopen ignored\n");
    return (nl_catd)-1;
}

char *catgets(nl_catd catd, int set_id, int msg_id, const char *s)
{
    printf("catgets NYI\n");
    return NULL;
}

int catclose(nl_catd catd)
{
    printf("catclose NYI\n");
    return 0;
}
