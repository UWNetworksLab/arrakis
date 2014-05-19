/**
 * \file
 * \brief Commandline parameter parsing.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef GETOPT_H
#define GETOPT_H

#include <sys/cdefs.h>

__BEGIN_DECLS

enum argtype {
    ArgType_Int,
    ArgType_UInt,
    ArgType_Bool,
    ArgType_Custom
};

typedef int (*cmdarg_handler)(const char *arg, const char *val);

struct cmdarg {
    const char          *arg;
    enum argtype        type;

    union {
        int             *integer;
        unsigned        *uinteger;
        bool            *boolean;
        cmdarg_handler  handler;
    } var;
};

extern void parse_commandline(const char *cmdline, struct cmdarg *cmdargs);

__END_DECLS

#endif // GETOPT_H
