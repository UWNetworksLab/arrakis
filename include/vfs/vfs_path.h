/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef VFS_PATH_H
#define VFS_PATH_H

#include <sys/cdefs.h>

__BEGIN_DECLS

/// path separator used by the VFS
#define VFS_PATH_SEP        '/'
/// path separator used by the VFS, as a string constant
#define VFS_PATH_SEP_STR    "/"

void vfs_path_normalise(char *path);
char *vfs_path_mkabsolute(const char *cwd, const char *path);
char *vfs_path_mkabs(const char *path);

__END_DECLS

#endif
