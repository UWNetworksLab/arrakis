/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <sys/types.h>
#include <vfs/vfs_fd.h>

off_t lseek(int fd, off_t offset, int whence)
{
    return vfsfd_lseek(fd, offset, whence);
}
