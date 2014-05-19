/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef UTIME_BARRELFISH_H_
#define UTIME_BARRELFISH_H_

#include <sys/cdefs.h>
#include <time.h>

struct utimbuf {
    time_t actime;
    time_t modtime;
};

__BEGIN_DECLS
int utime(const char *filename, const struct utimbuf *times);
__END_DECLS

#endif // UTIME_BARRELFISH_H_
