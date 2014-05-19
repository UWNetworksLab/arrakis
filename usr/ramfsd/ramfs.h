/**
 * \file
 * \brief RAMFS header
 */

/*
 * Copyright (c) 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/types.h>

/* ramfs.c */
struct dirent;

struct dirent *ramfs_init(void);
void ramfs_incref(struct dirent *e);
void ramfs_decref(struct dirent *e);

const char *ramfs_get_name(struct dirent *e);
bool ramfs_isdir(struct dirent *e);
size_t ramfs_get_size(struct dirent *e);
bool ramfs_islive(struct dirent *e);

errval_t ramfs_readdir(struct dirent *dir, uint32_t index, struct dirent **ret);
errval_t ramfs_lookup(struct dirent *dir, const char *name, struct dirent **ret);
errval_t ramfs_read(struct dirent *f, off_t offset, uint8_t **retbuf,
                    size_t *maxlen);
errval_t ramfs_grow(struct dirent *f, off_t offset, size_t len, uint8_t **retbuf);
errval_t ramfs_resize(struct dirent *f, size_t newlen);
errval_t ramfs_create(struct dirent *dir, const char *name, struct dirent **ret);
errval_t ramfs_mkdir(struct dirent *dir, const char *name, struct dirent **ret);
errval_t ramfs_delete(struct dirent *e);

/* service.c */
errval_t start_service(struct dirent *root);
