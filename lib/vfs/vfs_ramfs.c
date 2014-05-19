/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/bulk_transfer.h>
#include <vfs/vfs_path.h>
#include <if/trivfs_defs.h>
#include <if/trivfs_rpcclient_defs.h>
#include <if/monitor_defs.h>
#ifdef __scc__
#       include <barrelfish_kpi/shared_mem_arch.h>
#endif

#include "vfs_backends.h"

/// configuration setting to use bulk data (TODO: make this a mount option?)
static const bool use_bulk_data = true;

#define BULK_MEM_SIZE       (1U << 16)      // 64kB
#define BULK_BLOCK_SIZE     BULK_MEM_SIZE   // (it's RPC)

struct ramfs_client {
    struct trivfs_rpc_client rpc;
    struct bulk_transfer bulk;
    trivfs_fh_t rootfh;
    bool bound;
};

struct ramfs_handle {
    struct vfs_handle common;
    char *path;
    bool isdir;
    trivfs_fh_t fh;
    size_t pos;
};

static errval_t resolve_path(struct ramfs_client *cl, const char *path,
                             trivfs_fh_t *retfh, size_t *retpos, bool *retisdir)
{
restart: ;
    errval_t err, msgerr = SYS_ERR_OK;
    bool isdir = true;

    /* resolve path, starting from the root */
    trivfs_fh_t fh = cl->rootfh;

    // skip leading /
    size_t pos = 0;
    if (path[0] == VFS_PATH_SEP) {
        pos++;
    }

    while (path[pos] != '\0') {
        // copy next chunk of path to private buffer
        char *nextsep = strchr(&path[pos], VFS_PATH_SEP);
        size_t nextlen;
        if (nextsep == NULL) {
            nextlen = strlen(&path[pos]);
        } else {
            nextlen = nextsep - &path[pos];
        }

        char pathbuf[nextlen + 1];
        memcpy(pathbuf, &path[pos], nextlen);
        pathbuf[nextlen] = '\0';

        // lookup
        trivfs_fh_t nextfh;
        err = cl->rpc.vtbl.lookup(&cl->rpc, fh, pathbuf, &msgerr, &nextfh, &isdir);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "transport error in lookup");
            return err;
        } else if (err_is_fail(msgerr)) {
            if (err_no(msgerr) == FS_ERR_INVALID_FH) {
                if (fh == cl->rootfh) { // revalidate root
                    err = cl->rpc.vtbl.getroot(&cl->rpc, &cl->rootfh);
                    if (err_is_fail(err)) {
                        USER_PANIC_ERR(err, "failed to get root fh");
                    }
                    fh = cl->rootfh;
                    continue;
                } else {
                    USER_PANIC("vfs_ramfs: handle we just received is invalid?\n");
                    goto restart;
                }
            } else if (err_no(msgerr) != FS_ERR_NOTFOUND) {
                DEBUG_ERR(msgerr, "server error in lookup of '%s' while at '%s'",
                          path, pathbuf);
            }
            goto out;
        } else if (!isdir && nextsep != NULL) {
            // not a directory, don't bother going further
            fh = nextfh;
            pos += nextlen + 1;
            msgerr = FS_ERR_NOTDIR;
            goto out;
        }

        fh = nextfh;
        if (nextsep == NULL) {
            break;
        }

        pos += nextlen + 1;
    }

out:
    if (retpos != NULL) {
        *retpos = pos;
    }
    if (retfh != NULL) {
        *retfh = fh;
    }
    if (retisdir != NULL) {
        *retisdir = isdir;
    }
    return msgerr;
}

static errval_t open(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct ramfs_client *cl = st;
    trivfs_fh_t fh;
    bool isdir;
    errval_t err;

    err = resolve_path(cl, path, &fh, NULL, &isdir);
    if (err_is_ok(err)) {
        if (isdir) {
            err = FS_ERR_NOTFILE;
        } else {
            struct ramfs_handle *handle = malloc(sizeof(struct ramfs_handle));
            assert(handle != NULL);

            handle->path = strdup(path);
            assert(handle->path != NULL);
            handle->fh = fh;
            handle->pos = 0;
            handle->isdir = false;

            *rethandle = handle;
        }
    }

    return err;
}

static errval_t create(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct ramfs_client *cl = st;
    struct ramfs_handle *handle;
    trivfs_fh_t fh;
    errval_t err, msgerr;
    bool isdir;
    size_t pos = 0;

    // try to open it normally
    err = resolve_path(cl, path, &fh, &pos, &isdir);
    if (err_is_ok(err)) {
        if (isdir) {
            return FS_ERR_NOTFILE;
        } else {
            goto out; // ok
        }
    } else if (err_no(err) != FS_ERR_NOTFOUND
               || strchr(&path[pos], VFS_PATH_SEP) != NULL) {
        // failed before getting to the last part of the path: error
        return err;
    }

    // create the last part of the path
    err = cl->rpc.vtbl.create(&cl->rpc, fh, &path[pos], &msgerr, &fh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in create");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "server error in create");
        return msgerr;
    }
    err = msgerr;

out:
    handle = malloc(sizeof(struct ramfs_handle));
    assert(handle != NULL);

    handle->path = strdup(path);
    assert(handle->path != NULL);
    handle->fh = fh;
    handle->pos = 0;
    handle->isdir = false;

    *rethandle = handle;
    return err;
}

static errval_t ramfs_remove(void *st, const char *path)
{
    struct ramfs_client *cl = st;
    trivfs_fh_t fh;
    errval_t err, msgerr;
    bool isdir;

    err = resolve_path(cl, path, &fh, NULL, &isdir);
    if (err_is_fail(err)) {
        return err;
    } else if (isdir) {
        return FS_ERR_NOTFILE;
    }

    err = cl->rpc.vtbl.delete(&cl->rpc, fh, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in delete");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "server error in delete");
        return msgerr;
    }

    return msgerr;
}

static errval_t read(void *st, vfs_handle_t handle, void *buffer, size_t bytes,
                     size_t *bytes_read)
{
    struct ramfs_handle *h = handle;
    struct ramfs_client *cl = st;
    int restarts = 0;
    errval_t err, msgerr;

    assert(!h->isdir);

    uint8_t *mybuf = NULL;

restart:
    err = cl->rpc.vtbl.read(&cl->rpc, h->fh, h->pos, bytes,
                            &msgerr, &mybuf, bytes_read);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in read");
        return err;
    } else if (err_is_fail(msgerr)) {
        assert(mybuf == NULL);
        if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
            // revalidate handle and try again
            msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
            if (err_is_ok(msgerr)) {
                goto restart;
            }
        }
        DEBUG_ERR(msgerr, "server error in read");
        return msgerr;
    }

    h->pos += *bytes_read;
    memcpy(buffer, mybuf, *bytes_read);
    free(mybuf);

    if (*bytes_read < bytes) { // XXX: this can only mean EOF for ramfs
        return VFS_ERR_EOF;
    } else {
        return SYS_ERR_OK;
    }
}

static errval_t write(void *st, vfs_handle_t handle, const void *buffer,
                      size_t bytes, size_t *bytes_written)
{
    struct ramfs_handle *h = handle;
    struct ramfs_client *cl = st;
    int restarts = 0;
    errval_t err, msgerr;

    assert(!h->isdir);

restart:
    err = cl->rpc.vtbl.write(&cl->rpc, h->fh, h->pos, buffer, bytes, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in write");
        return err;
    } else if (err_is_fail(msgerr)) {
        if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
            // revalidate handle and try again
            msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
            if (err_is_ok(msgerr)) {
                goto restart;
            }
        }
        DEBUG_ERR(msgerr, "server error in write");
        return msgerr;
    }

    h->pos += bytes;
    if (bytes_written != NULL) {
        *bytes_written = bytes;
    }

    return msgerr;
}

static errval_t read_bulk(void *st, vfs_handle_t handle, void *buffer,
                          size_t bytes, size_t *ret_bytes_read)
{
    struct ramfs_handle *h = handle;
    struct ramfs_client *cl = st;
    trivfs_fsize_t reqlen, retlen;
    size_t bytes_read = 0;
    errval_t err, msgerr, reterr = SYS_ERR_OK;

    assert(!h->isdir);

    struct bulk_buf *buf = bulk_alloc(&cl->bulk);
    assert(buf != NULL); // shouldn't fail; we only ever use one at a time!

    void *mybuf = bulk_buf_get_mem(buf);

    uintptr_t bufid = bulk_buf_get_id(buf);
    trivfs_bulkid_t txbufid = bufid;
    assert(bufid == txbufid);

    while (bytes_read < bytes) {
        if (bytes - bytes_read > BULK_BLOCK_SIZE) {
            reqlen = BULK_BLOCK_SIZE;
        } else {
            reqlen = bytes - bytes_read;
        }

        int restarts = 0;

restart:
        err = cl->rpc.vtbl.read_bulk(&cl->rpc, h->fh, h->pos, reqlen,
                                     txbufid, &msgerr, &retlen);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "transport error in read");
            reterr = err;
            goto out;
        } else if (err_is_fail(msgerr)) {
            if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
                // revalidate handle and try again
                msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
                if (err_is_ok(msgerr)) {
                    goto restart;
                }
            }
            DEBUG_ERR(msgerr, "server error in read");
            reterr = msgerr;
            goto out;
        }

        bulk_prepare_recv(buf);

        memcpy((char *)buffer + bytes_read, mybuf, retlen);
        h->pos += retlen;
        bytes_read += retlen;

        if (retlen < reqlen) { // XXX: this can only mean EOF for ramfs
            reterr = VFS_ERR_EOF;
            goto out;
        }
    }

out:
    err = bulk_free(&cl->bulk, bufid);
    assert(err_is_ok(err));

    if (ret_bytes_read != NULL) {
        *ret_bytes_read = bytes_read;
    }

    return reterr;
}

static errval_t write_bulk(void *st, vfs_handle_t handle, const void *buffer,
                           size_t bytes, size_t *ret_bytes_written)
{
    struct ramfs_handle *h = handle;
    struct ramfs_client *cl = st;
    size_t bytes_written = 0;
    trivfs_fsize_t reqlen;
    errval_t err, msgerr, reterr = SYS_ERR_OK;

    assert(!h->isdir);

    struct bulk_buf *buf = bulk_alloc(&cl->bulk);
    assert(buf != NULL); // shouldn't fail; we only ever use one at a time!

    void *mybuf = bulk_buf_get_mem(buf);

    while (bytes_written < bytes) {
        if (bytes - bytes_written > BULK_BLOCK_SIZE) {
            reqlen = BULK_BLOCK_SIZE;
        } else {
            reqlen = bytes - bytes_written;
        }

        memcpy(mybuf, (char *)buffer + bytes_written, reqlen);
        uintptr_t bufid = bulk_prepare_send(buf);

        int restarts = 0;

restart:
        err = cl->rpc.vtbl.write_bulk(&cl->rpc, h->fh, h->pos, reqlen, bufid,
                                      &msgerr);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "transport error in write");
            reterr = err;
            goto out;
        } else if (err_is_fail(msgerr)) {
            if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
                // revalidate handle and try again
                msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
                if (err_is_ok(msgerr)) {
                    goto restart;
                }
            }
            DEBUG_ERR(msgerr, "server error in write");
            reterr = msgerr;
            goto out;
        }

        h->pos += reqlen;
        bytes_written += reqlen;
    }

out:
    err = bulk_free(&cl->bulk, bulk_buf_get_id(buf));
    assert(err_is_ok(err));

    if (ret_bytes_written != NULL) {
        *ret_bytes_written = bytes_written;
    }

    return reterr;
}

static errval_t truncate(void *st, vfs_handle_t handle, size_t bytes)
{
    struct ramfs_handle *h = handle;
    struct ramfs_client *cl = st;
    int restarts = 0;
    errval_t err, msgerr;

    assert(!h->isdir);

restart:
    err = cl->rpc.vtbl.truncate(&cl->rpc, h->fh, bytes, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in truncate");
        return err;
    } else if (err_is_fail(msgerr)) {
        if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
            // revalidate handle and try again
            msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
            if (err_is_ok(msgerr)) {
                goto restart;
            }
        }
        DEBUG_ERR(msgerr, "server error in truncate");
        return msgerr;
    }

    return msgerr;
}

static errval_t tell(void *st, vfs_handle_t handle, size_t *pos)
{
    struct ramfs_handle *h = handle;
    *pos = h->pos;
    return SYS_ERR_OK;
}

static errval_t stat(void *st, vfs_handle_t inhandle, struct vfs_fileinfo *info)
{
    struct ramfs_handle *h = inhandle;
    struct ramfs_client *cl = st;
    trivfs_fsize_t size;
    bool isdir;
    errval_t err, msgerr;
    int restarts = 0;

restart:
    err = cl->rpc.vtbl.getattr(&cl->rpc, h->fh, &msgerr, &isdir, &size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in getattr");
        return err;
    } else if (err_is_fail(msgerr)) {
        if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
            // revalidate handle and try again
            msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
            if (err_is_ok(msgerr)) {
                goto restart;
            }
        }
        DEBUG_ERR(msgerr, "server error in getattr");
        return msgerr;
    }

    assert(isdir == h->isdir);

    assert(info != NULL);
    info->type = isdir ? VFS_DIRECTORY : VFS_FILE;
    info->size = size;

    return SYS_ERR_OK;
}

static errval_t seek(void *st, vfs_handle_t handle, enum vfs_seekpos whence,
                     off_t offset)
{
    struct ramfs_handle *h = handle;
    struct vfs_fileinfo info;
    errval_t err;

    switch (whence) {
    case VFS_SEEK_SET:
        assert(offset >= 0);
        h->pos = offset;
        break;

    case VFS_SEEK_CUR:
        assert(offset >= 0 || -offset <= h->pos);
        h->pos += offset;
        break;

    case VFS_SEEK_END:
        err = stat(st, handle, &info);
        if (err_is_fail(err)) {
            return err;
        }
        assert(offset >= 0 || -offset <= info.size);
        h->pos = info.size + offset;
        break;

    default:
        USER_PANIC("invalid whence argument to ramfs seek");
    }

    return SYS_ERR_OK;
}

static errval_t close(void *st, vfs_handle_t inhandle)
{
    struct ramfs_handle *handle = inhandle;
    assert(!handle->isdir);
    free(handle->path);
    free(handle);
    return SYS_ERR_OK;
}

static errval_t opendir(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct ramfs_client *cl = st;
    struct ramfs_handle *handle;
    trivfs_fh_t fh;
    errval_t err;
    bool isdir;

    err = resolve_path(cl, path, &fh, NULL, &isdir);
    if (err_is_ok(err)) {
        if (isdir) {
            handle = malloc(sizeof(struct ramfs_handle));
            assert(handle != NULL);

            handle->path = strdup(path);
            assert(handle->path != NULL);
            handle->fh = fh;
            handle->pos = 0;
            handle->isdir = true;

            *rethandle = handle;
        } else {
            err = FS_ERR_NOTDIR;
        }
    }

    return err;
}

static errval_t dir_read_next(void *st, vfs_handle_t inhandle, char **retname,
                              struct vfs_fileinfo *info)
{
    struct ramfs_handle *h = inhandle;
    struct ramfs_client *cl = st;
    char *name;
    trivfs_fsize_t size;
    bool isdir;
    errval_t err, msgerr;
    int restarts = 0;

    assert(h->isdir);

restart:
    err = cl->rpc.vtbl.readdir(&cl->rpc, h->fh, h->pos,
                               &msgerr, &name, &isdir, &size);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in readdir");
        return err;
    } else if (err_is_fail(msgerr)) {
        assert(name == NULL);
        if (err_no(msgerr) == FS_ERR_INVALID_FH && !restarts++) {
            // revalidate handle and try again
            if (h->fh == cl->rootfh) { // XXX: revalidate root
                err = cl->rpc.vtbl.getroot(&cl->rpc, &cl->rootfh);
                if (err_is_fail(err)) {
                    USER_PANIC_ERR(err, "failed to get root fh");
                }
                h->fh = cl->rootfh;
                goto restart;
            } else {
                msgerr = resolve_path(cl, h->path, &h->fh, NULL, NULL);
                if (err_is_ok(msgerr)) {
                    goto restart;
                }
            }
        }
        if (err_no(msgerr) != FS_ERR_INDEX_BOUNDS) {
            DEBUG_ERR(msgerr, "server error in readdir");
        }
        return msgerr;
    }

    h->pos++;

    if (retname != NULL) {
        *retname = name;
    }

    if (info != NULL) {
        info->type = isdir ? VFS_DIRECTORY : VFS_FILE;
        info->size = size;
    }

    return SYS_ERR_OK;
}

static errval_t closedir(void *st, vfs_handle_t dhandle)
{
    struct ramfs_handle *handle = dhandle;
    assert(handle->isdir);
    free(handle->path);
    free(handle);
    return SYS_ERR_OK;
}

// fails if already present
static errval_t mkdir(void *st, const char *path)
{
    struct ramfs_client *cl = st;
    trivfs_fh_t parent;
    const char *childname;
    errval_t err, msgerr;
    bool isdir;

    // find parent directory
    char *lastsep = strrchr(path, VFS_PATH_SEP);
    if (lastsep != NULL) {
        childname = lastsep + 1;

        size_t pathlen = lastsep - path;
        char pathbuf[pathlen + 1];
        memcpy(pathbuf, path, pathlen);
        pathbuf[pathlen] = '\0';

        // resolve parent directory
        err = resolve_path(cl, pathbuf, &parent, NULL, &isdir);
        if (err_is_fail(err)) {
            return err;
        } else if (!isdir) {
            return FS_ERR_NOTDIR; // parent is not a directory
        }
    } else {
        parent = cl->rootfh;
        childname = path;
    }

    // create child
    trivfs_fh_t newfh;
    err = cl->rpc.vtbl.mkdir(&cl->rpc, parent, childname, &msgerr, &newfh);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in mkdir");
        return err;
    }

    return msgerr;
}

static errval_t rmdir(void *st, const char *path)
{
    struct ramfs_client *cl = st;
    trivfs_fh_t fh;
    errval_t err, msgerr;
    bool isdir;

    err = resolve_path(cl, path, &fh, NULL, &isdir);
    if (err_is_fail(err)) {
        return err;
    } else if (!isdir) {
        return FS_ERR_NOTDIR;
    }

    err = cl->rpc.vtbl.delete(&cl->rpc, fh, &msgerr);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "transport error in delete");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "server error in delete");
        return msgerr;
    }

    return msgerr;
}

static struct vfs_ops ramfsops_non_bulk = {
    .open = open,
    .create = create,
    .remove = ramfs_remove,
    .read = read,
    .write = write,
    .truncate = truncate,
    .seek = seek,
    .tell = tell,
    .stat = stat,
    .close = close,
    .opendir = opendir,
    .dir_read_next = dir_read_next,
    .closedir = closedir,
    .mkdir = mkdir,
    .rmdir = rmdir,
};

static struct vfs_ops ramfsops_bulk = {
    .open = open,
    .create = create,
    .remove = ramfs_remove,
    .read = read_bulk,
    .write = write_bulk,
    .truncate = truncate,
    .seek = seek,
    .tell = tell,
    .stat = stat,
    .close = close,
    .opendir = opendir,
    .dir_read_next = dir_read_next,
    .closedir = closedir,
    .mkdir = mkdir,
    .rmdir = rmdir,
};

static void bind_cb(void *st, errval_t err, struct trivfs_binding *b)
{
    struct ramfs_client *cl = st;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    err = trivfs_rpc_client_init(&cl->rpc, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "RPC init failed");
    }

    cl->bound = true;
}

struct iref_request_state {
    bool is_done;
    iref_t iref;
    errval_t err;
};

static void get_ramfs_iref_reply(struct monitor_binding* mb, iref_t iref,
        uintptr_t state){
    struct iref_request_state* irs = (struct iref_request_state*) state;

    irs->iref = iref;
    irs->err = (iref != 0) ? SYS_ERR_OK : LIB_ERR_GET_RAMFS_IREF;
    irs->is_done = true;
}

static errval_t get_ramfs_iref(iref_t* iref)
{
    // Request iref for ramfsd directly from monitor (needed for SKB)
    // XXX: broken :-( uintptr_t + message_wait_and_handle_next()
    struct iref_request_state irs = { 0, 0, 0 };
    struct monitor_binding *mb = get_monitor_binding();
    mb->rx_vtbl.get_ramfs_iref_reply = get_ramfs_iref_reply;

    errval_t err = mb->tx_vtbl.get_ramfs_iref_request(mb, NOP_CONT, (uintptr_t)&irs);
    if (err_is_fail(err)) {
        return err;
    }

    while (!irs.is_done) {
        messages_wait_and_handle_next();
    }

    *iref = irs.iref;
    return irs.err;
}

errval_t vfs_ramfs_mount(const char *uri, void **retst, struct vfs_ops **retops)
{
    errval_t err, msgerr;
    iref_t iref = 0;

    // skip over protocol part of URI to get service name
    char *service = strstr(uri, "://");
    if (service == NULL) {
        return VFS_ERR_BAD_URI;
    }
    service += 3;

    // default service name
    if (*service == '\0') {
        service = "ramfs";
    }

    err = get_ramfs_iref(&iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "get ramfs iref");
        return err;
    }

    struct ramfs_client *client = malloc(sizeof(struct ramfs_client));
    assert(client != NULL);

    client->bound = false;

    err = trivfs_bind(iref, bind_cb, client, get_default_waitset(),
                      use_bulk_data
                        ? IDC_BIND_FLAG_RPC_CAP_TRANSFER
                        : IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        free(client);
        return err; // FIXME
    }

    // XXX: block for bind completion (broken API!)
    while (!client->bound) {
        messages_wait_and_handle_next();
    }

    // get root fh
    err = client->rpc.vtbl.getroot(&client->rpc, &client->rootfh);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "failed to get root fh");
    }

    if (use_bulk_data) {
        // Init bulk data lib
        struct capref shared_frame;
        err = bulk_create(BULK_MEM_SIZE, BULK_BLOCK_SIZE, &shared_frame,
                          &client->bulk, false);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "bulk_create");
        }

        // Send bulk frame cap to server
        err = client->rpc.vtbl.bulk_init(&client->rpc, shared_frame, &msgerr);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "failed to call bulk_init");
        } else if (err_is_fail(msgerr)) {
            USER_PANIC_ERR(msgerr, "bulk_init failed");
        }
    }

    if (use_bulk_data) {
        *retops = &ramfsops_bulk;
    } else {
        *retops = &ramfsops_non_bulk;
    }
    *retst = client;

    return SYS_ERR_OK;
}
