/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/time.h>
#include <assert.h>
#include "posixcompat.h"

int setitimer(int which, const struct itimerval *new_value,
              struct itimerval *old_value)
{
    POSIXCOMPAT_DEBUG("Warning: setitimer(%d, %p, %p) ignored\n",
                      which, new_value, old_value);
    return 0;
}
