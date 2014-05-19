/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef DIRENT_H_
#define DIRENT_H_

#include <sys/cdefs.h>

#define NAME_MAX 512

struct dirent {
//    long d_ino;
//    off_t d_off;
//    unsigned short d_reclen;
    char d_name[NAME_MAX + 1];
};

typedef struct {
    struct dirent dirent;
    void *vh; // really a vfs_handle_t
} DIR;

__BEGIN_DECLS
DIR *opendir(const char *pathname);
struct dirent *readdir(DIR* dir);
int closedir(DIR *dir);
__END_DECLS

#endif
