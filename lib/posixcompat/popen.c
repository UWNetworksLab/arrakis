/*
 * Copyright (c) 2007-2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>

FILE *popen(const char *command, const char *type)
{
    fprintf(stderr, "Warning: popen() unimplemented\n");
    return NULL;
}

int pclose(FILE *stream)
{
    fprintf(stderr, "Warning: pclose() unimplemented\n");
    return -1;
}

/*
 * Although fdopen is POSIX, we use the implementation provided by newlib.
 * Providing an own implementation would require to include many
 * newlib-internal headers for the FILE * type and its manipulation.
 */

#if 0
FILE *fdopen(int fd, const char *mode)
{
    fprintf(stderr, "Warning: fdopen() unimplemented\n");
    return NULL;
}
#endif
