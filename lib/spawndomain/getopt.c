/**
 * \file
 * \brief Somewhat simple commandline argument parsing.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <spawndomain/spawndomain.h>
#include "spawn.h"

const char *getopt(const char **optstring, char *buf, size_t buflen,
                   size_t *optlen)
{
    bool inquote = false, backslash = false;
    const char *pos = *optstring;
    int bufpos = 0;

    if (pos == NULL) {
        return NULL;
    }

    /* skip over leading whitespace */
    while (*pos == ' ' || *pos == '\t') {
        pos++;
    }

    /* collect next option, respecting " and \ */
    for (char c = *pos; c != '\0' && bufpos < buflen; c = *++pos) {
        if (backslash) {
            buf[bufpos++] = c;
            backslash = false;
        } else if (c == '\\') {
            backslash = true;
        } else if (c == '"') {
            inquote = !inquote;
        } else if (!inquote && (c == ' ' || c == '\t')) {
            break;
        } else {
            buf[bufpos++] = c;
        }
    }

    if (bufpos == 0) {
        return NULL;
    }

    // Make ASCIIZ string
    buf[bufpos < buflen ? bufpos++ : buflen - 1] = '\0';

    if (optlen) {
        *optlen = bufpos < buflen ? bufpos : buflen;
    }

    *optstring = pos;
    return buf;
}
