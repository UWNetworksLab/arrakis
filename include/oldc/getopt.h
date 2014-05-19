/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _GETOPT_H_
#define _GETOPT_H_

int getopt(int, char * const [], const char *);

extern char *optarg;                    /* getopt(3) external variables */
extern int optind, opterr, optopt;
#ifndef _OPTRESET_DECLARED
#define _OPTRESET_DECLARED
extern int optreset;                    /* getopt(3) external variable */
#endif

#endif
