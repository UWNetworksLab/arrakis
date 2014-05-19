/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2012, 2013, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef STAT_BARRELFISH_H_
#define STAT_BARRELFISH_H_

#include <sys/cdefs.h>
#include <sys/types.h> // for mode_t and dev_t

struct stat {
    long st_uid;
    long st_ctime;
    long st_mtime;
    long st_atime;
    dev_t st_dev;
    long st_gid;
    ino_t st_ino;
    long st_nlink;
    long st_size;
    mode_t st_mode;
    long st_blksize;
};

#define S_IFMT   0170000
	
#define	S_IFSOCK 0140000	
#define	S_IFLNK	 0120000
#define S_IFREG  0100000	
#define	S_IFBLK	 0060000
#define S_IFDIR  0040000
#define S_IFCHR	 0020000	
#define	S_IFIFO	 0010000	

#define	S_ISUID	04000
#define	S_ISGID	02000
#if __XSI_VISIBLE
#define S_ISVTX  01000        /* save swapped text even after use */
#endif

#define S_IRUSR 00400
#define S_IWUSR 00200
#define S_IXUSR 00100
#define S_IRGRP 00040
#define S_IWGRP 00020
#define S_IXGRP 00010
#define S_IROTH 00004
#define S_IWOTH 00002
#define S_IXOTH 00001

#define	S_IRWXU	(S_IRUSR|S_IWUSR|S_IXUSR)
#define	S_IRWXG	(S_IRGRP|S_IWGRP|S_IXGRP)
#define	S_IRWXO	(S_IROTH|S_IWGRP|S_IXGRP)

#define S_ISSOCK(mode)   (((mode) & S_IFMT) == S_IFSOCK)
#define S_ISLNK(mode)    (((mode) & S_IFMT) == S_IFLNK)
#define S_ISREG(mode)    (((mode) & S_IFMT) == S_IFREG)
#define S_ISBLK(mode)    (((mode) & S_IFMT) == S_IFBLK)
#define S_ISDIR(mode)    (((mode) & S_IFMT) == S_IFDIR)
#define S_ISCHR(mode)    (((mode) & S_IFMT) == S_IFCHR)
#define S_ISFIFO(mode)   (((mode) & S_IFMT) == S_IFIFO)

__BEGIN_DECLS
mode_t umask(mode_t mask);
int chmod(const char *path, mode_t mode);
int mkdir(const char *pathname, int mode);
int mkfifo(const char *pathname, mode_t mode);
int stat(const char *pathname, struct stat *buf);
int fstat(int fd, struct stat*buf);
int lstat(const char *path, struct stat *buf);
__END_DECLS

#endif //  STAT_BARRELFISH_H_
