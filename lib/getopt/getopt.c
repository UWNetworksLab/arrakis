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

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <limits.h>
#include <getopt/getopt.h>

static int handle_uint(unsigned int *var, const char *val)
{
    assert(var != NULL);
    int base;

    // determine base (0x -> hex, anything else -> decimal)
    if (val[0] == '0' && val[1] == 'x') {
        base = 16;
        val += 2;
    } else {
        base = 10;
    }

    unsigned long x = strtoul(val, NULL, base);
    if (x > UINT_MAX) { 
	x = UINT_MAX;
    }
    *var = (unsigned)x;
    return 0;
}

static int handle_int(int *var, const char *val)
{
    assert(var != NULL);
    int base;

    // determine base (0x -> hex, anything else -> decimal)
    if (val[0] == '0' && val[1] == 'x') {
        base = 16;
        val += 2;
    } else {
        base = 10;
    }

    long x = strtol(val, NULL, base);
    assert(x >= INT_MIN && x <= INT_MAX); // XXX
    *var = (int)x;

    return 0;
}

static int handle_bool(bool *var, const char *val)
{
    assert(var != NULL);
    if(!strncmp(val, "true", 4) || !strncmp(val, "yes", 3)) {
        *var = true;
        return 0;
    } else if(!strncmp(val, "false", 5) || !strncmp(val, "no", 2)) {
        *var = false;
        return 0;
    }

    return -1;
}

static int handle_argument(const char *var, const char *val,
                           struct cmdarg *cmdargs)
{
    // compare var against array of recognized arguments
    for(int i = 0; cmdargs[i].arg != NULL; i++) {
        struct cmdarg *a = &cmdargs[i];
        if(!strncmp(var, a->arg, strlen(a->arg))) {
            switch(a->type) {
            case ArgType_Int:
                return handle_int(a->var.integer, val);

            case ArgType_UInt:
                return handle_uint(a->var.uinteger, val);

            case ArgType_Bool:
                return handle_bool(a->var.boolean, val);

            case ArgType_Custom:
                return a->var.handler(var, val);

            default:
                assert(!"Unknown type %d in kernel argument array!");
                return -1;
            }
        }
    }

    return 0;
}

/**
 * \brief Search backwards for character 'c' in string 'p'.
 *
 * Starts at the character, pointed to by 'p', within a string and searches
 * backwards for 'c', returning a pointer to its position.
 *
 * WARNING: 'c' MUST exist prior to calling this function!!
 *
 * \param p     Character to start searching backwards from.
 * \param c     Character to look for.
 *
 * \return Pointer to 'c' in string.
 */
static const char *look_back(const char *p, int c)
{
    while(*p != c) {
        p--;
    }

    return p;
}

/**
 * \brief Parse commandline parameters in 'cmdline'.
 *
 * Parses 'cmdline' for kernel commandline parameters and sets configuration
 * variables accordingly.
 *
 * \param cmdline       Kernel commandline string.
 */
void parse_commandline(const char *cmdline, struct cmdarg *cmdargs)
{
    // Parse argument string into whitespace-separated 'var=val' tokens
    for(const char *p = strchr(cmdline, '='); p != NULL;
        p = strchr(p + 1, '=')) {
        const char *var = look_back(p, ' ') + 1, *val = p + 1;

        if(handle_argument(var, val, cmdargs) != 0) {
            assert(!"parse_commandline: Parse error in commandline");
        }
    }
}
