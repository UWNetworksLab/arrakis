/*
 * Copyright (c) 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <assert.h>
#include <unistd.h>

#include "posixcompat.h"

/**
 * \brief Arrange to deliver SIGALARM to the calling process in 'seconds'
 *        seconds.

 * \returns Number of seconds previously scheduled alarm would have generated
 *          a SIGALRM or zero if there was no previous alarm.
 */
unsigned int alarm(unsigned int seconds)
{
    if (seconds == 0) { // Cancel any previous alarm.
        POSIXCOMPAT_DEBUG("alarm(0): cancel any previous alarm.");
        // Nothing to be done.
        return 0;
    } else {
        /* assert(!"NYI"); */
        return 0;
    }
}
