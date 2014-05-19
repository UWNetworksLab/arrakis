/**
 * \file
 * \brief Trivial RAMFS implementation
 */

/*
 * Copyright (c) 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <if/trivfs_defs.h>
#include "ramfs.h"

struct dirent {
    struct dirent *next;   ///< next entry in same directory
    struct dirent **prevp; ///< locn where the preceding child / parent links us
    struct dirent *parent; ///< parent directory
    const char *name;   ///< malloc'ed name buffer
    bool isdir;         ///< is a directory or a file?
    bool islive;        ///< false if this has been deleted but not yet freed
    unsigned refcount;  ///< outstanding references (handles and/or ongoing IDCs)
    union {
        struct {
            uint8_t *data;  ///< on heap
            size_t size;    ///< size of data
        } file;
        struct {
            struct dirent *entries; ///< children of this dir
            size_t nentries;        ///< number of children
        } dir;
    } u;
};

struct dirent *ramfs_init(void)
{
    struct dirent *root = malloc(sizeof(struct dirent));
    assert(root != NULL);

    root->next = NULL;
    root->prevp = NULL;
    root->parent = NULL;
    root->name = "";
    root->isdir = true;
    root->islive = true;
    root->u.dir.entries = NULL;
    root->u.dir.nentries = 0;
    root->refcount = 1;

    return root;
}

inline void ramfs_incref(struct dirent *e)
{
    assert(e->refcount > 0);
    assert(e->islive);
    e->refcount++;
}

void ramfs_decref(struct dirent *e)
{
    assert(e->refcount > 0);
    if (--e->refcount == 0) {
        assert(!e->islive);
        assert(e->next == NULL && e->prevp == NULL);
        free((void *)e->name);
        if (e->isdir) {
            assert(e->u.dir.nentries == 0);
            assert(e->u.dir.entries == NULL);
        } else {
            free(e->u.file.data);
        }
        free(e);
    }
}

const char *ramfs_get_name(struct dirent *e)
{
    assert(e->islive && e->refcount > 0);
    return e->name;
}

bool ramfs_isdir(struct dirent *e)
{
    assert(e->islive && e->refcount > 0);
    return e->isdir;
}

bool ramfs_islive(struct dirent *e)
{
    assert(e->refcount > 0);
    return e->islive;
}

size_t ramfs_get_size(struct dirent *e)
{
    assert(e->islive && e->refcount > 0);
    return e->isdir ? e->u.dir.nentries : e->u.file.size;
}

errval_t ramfs_readdir(struct dirent *dir, uint32_t idx, struct dirent **ret)
{
    assert(dir->islive && dir->refcount > 0);

    if (!dir->isdir) {
        return FS_ERR_NOTDIR;
    }

    struct dirent *e = dir->u.dir.entries;
    while (e != NULL && idx > 0) {
        e = e->next;
        idx--;
    }

    if (idx > 0 || e == NULL) {
        return FS_ERR_INDEX_BOUNDS;
    }

    assert(ret != NULL);
    *ret = e;
    return SYS_ERR_OK;
}

errval_t ramfs_lookup(struct dirent *dir, const char *name, struct dirent **ret)
{
    assert(dir != NULL);
    assert(dir->islive && dir->refcount > 0);

    if (!dir->isdir) {
        return FS_ERR_NOTDIR;
    }

    struct dirent *e;
    for (e = dir->u.dir.entries; e != NULL; e = e->next) {
        if (strcmp(e->name, name) == 0) {
            *ret = e;
            return SYS_ERR_OK;
        }
    }

    return FS_ERR_NOTFOUND;
}

errval_t ramfs_read(struct dirent *f, off_t offset, uint8_t **retbuf,
                    size_t *maxlen)
{
    assert(f->islive && f->refcount > 0);

    if (f->isdir) {
        return FS_ERR_NOTFILE;
    }

    if (offset < 0 || offset >= f->u.file.size) {
        *retbuf = NULL;
        *maxlen = 0;
    } else {
        assert(f->u.file.data != NULL);
        *retbuf = &f->u.file.data[offset];
        *maxlen = f->u.file.size - offset;
    }

    return SYS_ERR_OK;
}

/// Ensure that the file has at least space for a write of the given size at
/// the given offset, return buffer pointer
errval_t ramfs_grow(struct dirent *f, off_t offset, size_t len,
                    uint8_t **retbuf)
{
    assert(f->islive && f->refcount > 0);

    if (f->isdir) {
        return FS_ERR_NOTFILE;
    }

    assert(offset >= 0);

    // do we need to grow the file? if not, we're done
    if (offset + len <= f->u.file.size) {
        *retbuf = &f->u.file.data[offset];
        return SYS_ERR_OK;
    }

    uint8_t *newdata = realloc(f->u.file.data, (size_t)offset + len);
    if (newdata == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    } else {
        f->u.file.data = newdata;
        f->u.file.size = (size_t)offset + len;
        *retbuf = &newdata[offset];
        // XXX: new data is not initialised, we rely on the caller to do this!
        return SYS_ERR_OK;
    }
}

errval_t ramfs_resize(struct dirent *f, size_t newlen)
{
    assert(f->islive && f->refcount > 0);

    if (f->isdir) {
        return FS_ERR_NOTFILE;
    }

    uint8_t *newdata = realloc(f->u.file.data, newlen);
    if (newdata == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    if (newlen > f->u.file.size) {
        // zero-fill new data
        memset(&newdata[f->u.file.size], 0, newlen - f->u.file.size);
    }

    f->u.file.data = newdata;
    f->u.file.size = newlen;

    return SYS_ERR_OK;
}

static errval_t addchild(struct dirent *dir, struct dirent *child)
{
    assert(child->refcount == 1);
    if (dir->u.dir.entries == NULL) {
        assert(dir->u.dir.nentries == 0);
        dir->u.dir.entries = child;
        child->prevp = &dir->u.dir.entries;
    } else {
        // search to end of list
        struct dirent *e;
        for (e = dir->u.dir.entries; ; e = e->next) {
            // check for duplicates
            if (strcmp(e->name, child->name) == 0) {
                return FS_ERR_EXISTS;
            }
            if (e->next == NULL) {
                break;
            }
        }
        e->next = child;
        child->prevp = &e->next;
    }
    child->parent = dir;
    dir->u.dir.nentries++;
    return SYS_ERR_OK;
}

errval_t ramfs_create(struct dirent *dir, const char *name, struct dirent **ret)
{
    assert(dir != NULL);
    assert(dir->islive && dir->refcount > 0);

    if (!dir->isdir) {
        return FS_ERR_NOTDIR;
    }

    struct dirent *f = malloc(sizeof(struct dirent));
    if (f == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    f->next = NULL;
    f->name = name; /* XXX: takes ownership of name buffer */
    f->isdir = false;
    f->refcount = 1;
    f->islive = true;
    f->u.file.data = NULL;
    f->u.file.size = 0;

    errval_t err = addchild(dir, f);

    if (err_is_ok(err)) {
        if (ret != NULL) {
            *ret = f;
        }
    } else {
        free(f);
    }

    return err;
}

errval_t ramfs_mkdir(struct dirent *dir, const char *name, struct dirent **ret)
{
    assert(dir != NULL);
    assert(dir->islive && dir->refcount > 0);

    if (!dir->isdir) {
        return FS_ERR_NOTDIR;
    }

    struct dirent *d = malloc(sizeof(struct dirent));
    if (d == NULL) {
        return LIB_ERR_MALLOC_FAIL;
    }

    d->next = NULL;
    d->name = name; /* XXX: takes ownership of name buffer */
    d->isdir = true;
    d->refcount = 1;
    d->islive = true;
    d->u.dir.entries = NULL;
    d->u.dir.nentries = 0;

    errval_t err = addchild(dir, d);

    if (err_is_fail(err)) {
        free(d);
    }

    if (ret != NULL && err_is_ok(err)) {
        *ret = d;
    }

    return err;
}

errval_t ramfs_delete(struct dirent *e)
{
    assert(e != NULL);
    assert(e->islive && e->refcount > 0);

    // check if we can delete this
    if (e->isdir && e->u.dir.entries != NULL) {
        return FS_ERR_NOTEMPTY;
    }

    // XXX: prevent deletion of root dir, even if empty
    if (e->prevp == NULL) {
        assert(e->isdir);
        assert(e->next == NULL);
        return FS_ERR_NOTEMPTY; // XXX
    }

    // unlink from parent directory
    assert(e->prevp != NULL);
    *e->prevp = e->next;
    if (e->next != NULL) {
        e->next->prevp = e->prevp;
    }

    // update parent's child count
    assert(e->parent != NULL && e->parent->isdir);
    assert(e->parent->u.dir.nentries > 0);
    e->parent->u.dir.nentries--;

    // mark dead, deref and we're done
    e->islive = false;
    ramfs_decref(e);

    return SYS_ERR_OK;
}
