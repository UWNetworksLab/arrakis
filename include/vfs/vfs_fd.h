/*
 * Copyright (c) 2012, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_FD_H
#define VFS_FD_H

#include <sys/cdefs.h> /* for __BEGIN_DECLS, __END_DECLS */
#include <sys/types.h> /* for off_t */

__BEGIN_DECLS

int   vfsfd_open(const char *pathname, int flags);
int   vfsfd_read(int fd, void *buf, size_t len);
int   vfsfd_write(int fd, const void *buf, size_t len);
int   vfsfd_close(int fd);
off_t vfsfd_lseek(int fd, off_t off, int whence);

__END_DECLS

#endif /* VFS_FD_H */
