/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/waitset.h>
#include <barrelfish/debug.h>
#include <nfs/nfs.h>
#include <vfs/vfs.h>
#include <vfs/vfs_path.h>

// networking stuff
#include <lwip/netif.h>
#include <lwip/dhcp.h>
#include <netif/etharp.h>
#include <lwip/init.h>
#include <lwip/tcp.h>
#include <netif/bfeth.h>
#include <lwip/ip_addr.h>

#include "vfs_backends.h"

/// Define to enable asynchronous writes
//#define ASYNC_WRITES

#define MAX_NFS_READ_CHUNKS  40  // FIXME: Not used anymore, should be removed

//#define NONBLOCKING_NFS_READ   1
#define MAX_NFS_READ_BYTES   1330 /*14000*//*workaround for breakage in lwip*/

#define MAX_NFS_WRITE_BYTES  1330 /* workaround for breakage in lwip */
#define MAX_NFS_WRITE_CHUNKS 1    /* workaround for breakage in lwip */
#define NFS_WRITE_STABILITY  UNSTABLE

#define MIN(a,b) ((a)<(b)?(a):(b))
#define assert_err(e,m)     \
do {                        \
    if (err_is_fail(e)) {   \
        DEBUG_ERR(e,m);     \
        abort();            \
    }                       \
} while (0)

#include "vfs_nfs.h"

// condition used to singal controlling code to wait for a condition
static bool wait_flag;
static struct thread_cond wait_cond = THREAD_COND_INITIALIZER;

// XXX: lwip idc_barrelfish.c
extern struct waitset *lwip_waitset;

static void check_and_handle_other_events(void)
{
    if (lwip_mutex == NULL) { // single-threaded
        while (true) {
            errval_t err = event_dispatch_non_block(lwip_waitset);
            if (err == LIB_ERR_NO_EVENT) {
                return;
            }
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "in event_dispatch_non_block");
                break;
            }
        }
    } else {
        assert(!"NYI: ");
    }
}

static void wait_for_condition(void)
{
    if (lwip_mutex == NULL) { // single-threaded
        while (!wait_flag) {
            errval_t err = event_dispatch(lwip_waitset);
            assert(err_is_ok(err));
        }
        wait_flag = false;
    } else {
        thread_cond_wait(&wait_cond, lwip_mutex);
    }
}

// NOTE: just like above function, but it checks all events instead of
// blocking on any perticular event
// Above function was blocking on waiting for timer event even when there
// are incoming packets to be processed. (only in the case of UMP)
// FIXME: this is used only in read function and other functions are still
// using above function. But this function should replace above function.
static void wait_for_condition_fair(void)
{
    if (lwip_mutex == NULL) { // single-threaded
        while (!wait_flag) {
            check_and_handle_other_events();
            wrapper_perform_lwip_work();
            if (wait_flag) {
                break;
            }
            errval_t err = event_dispatch(lwip_waitset);
            assert(err_is_ok(err));
        }
        wait_flag = false;
    } else {
        thread_cond_wait(&wait_cond, lwip_mutex);
    }
}


static void signal_condition(void)
{
    if (lwip_mutex == NULL) { // single-threaded
        wait_flag = true;
    } else {
        assert(!thread_mutex_trylock(lwip_mutex));
        thread_cond_signal(&wait_cond);
    }
}

typedef void resolve_cont_fn(void *st, errval_t err, struct nfs_fh3 fh,
                             struct fattr3 *fattr /* optional */);

// state for an ongoing multi-component name resolution op
struct nfs_resolve_state {
    struct nfs_state *nfs;
    const char *path;
    int path_pos;
    bool islast;
    struct nfs_fh3 curfh;
    resolve_cont_fn *cont;
    void *cont_st;
};

static void resolve_lookup_cb(void *arg, struct nfs_client *client,
                              LOOKUP3res *result)
{
    LOOKUP3resok *resok = &result->LOOKUP3res_u.resok;
    struct nfs_resolve_state *st = arg;

    if (result == NULL || result->status != NFS3_OK
        || !resok->obj_attributes.attributes_follow) { // failed

        st->cont(st->cont_st, FS_ERR_NOTFOUND, NULL_NFS_FH, NULL);
out:
        free(st);
        xdr_LOOKUP3res(&xdr_free, result);
        return;
    }

    // was this the last lookup?
    if (st->islast) {
        st->cont(st->cont_st, SYS_ERR_OK, resok->object,
                 &resok->obj_attributes.post_op_attr_u.attributes);
        goto out;
    } else if (resok->obj_attributes.post_op_attr_u.attributes.type != NF3DIR) {
        // must be a directory to recurse
        st->cont(st->cont_st, FS_ERR_NOTFOUND, resok->object, NULL);
        goto out;
    }

    // recurse...
    st->curfh = resok->object;

    // copy next chunk of path to private buffer
    while (st->path[st->path_pos] == VFS_PATH_SEP) {
        st->path_pos++;
    }

    char *nextsep = strchr(&st->path[st->path_pos], VFS_PATH_SEP);
    size_t nextlen;
    if (nextsep == NULL) {
        st->islast = true;
        nextlen = strlen(&st->path[st->path_pos]);
    } else {
        nextlen = nextsep - &st->path[st->path_pos];
    }
    char pathbuf[nextlen + 1];
    memcpy(pathbuf, &st->path[st->path_pos], nextlen);
    pathbuf[nextlen] = '\0';
    st->path_pos += nextlen + 1;

    err_t e = nfs_lookup(st->nfs->client, st->curfh, pathbuf, resolve_lookup_cb,
                         st);
    assert(e == ERR_OK);

    // free arguments
    xdr_LOOKUP3res(&xdr_free, result);
}

static void initiate_resolve(struct nfs_state *nfs, const char *path,
                             resolve_cont_fn *cont, void *cont_st)
{
    if (strlen(path) == (size_t)0) { // Resolving the root of the mount point
        cont(cont_st, SYS_ERR_OK, nfs->rootfh, NULL);
        return;
    }

    struct nfs_resolve_state *st = malloc(sizeof(struct nfs_resolve_state));
    assert(st != NULL);

    assert(path != NULL);

    st->nfs = nfs;
    st->path = path;
    st->path_pos = 0;
    st->curfh = nfs->rootfh;
    st->cont = cont;
    st->cont_st = cont_st;

    // skip leading '/'s
    while (st->path[st->path_pos] == VFS_PATH_SEP) {
        st->path_pos++;
    }

    // locate first component of path
    char *nextsep = strchr(&st->path[st->path_pos], VFS_PATH_SEP);
    size_t nextlen;
    if (nextsep == NULL) {
        st->islast = true;
        nextlen = strlen(&st->path[st->path_pos]);
    } else {
        st->islast = false;
        nextlen = nextsep - &st->path[st->path_pos];
    }

    char pathbuf[nextlen + 1];
    memcpy(pathbuf, &st->path[st->path_pos], nextlen);
    pathbuf[nextlen] = '\0';
    st->path_pos += nextlen + 1;

    // initiate the first lookup
    err_t e = nfs_lookup(nfs->client, st->curfh, pathbuf, resolve_lookup_cb, st);
    assert(e == ERR_OK);
}

struct nfs_file_io_handle {
    void        *data;
    size_t      offset;
    size_t      size;
    size_t      size_complete;
    nfs_fh3     handle;
    int         chunk_count;
    size_t      chunk_pos;
    int         chunks_in_progress;
    nfsstat3    status;
    struct nfs_handle *back_fh;
};

struct nfs_file_parallel_io_handle {
    struct nfs_file_io_handle    *fh;
    size_t                  chunk_start;
    size_t                  chunk_size;
};

static void read_callback(void *arg, struct nfs_client *client, READ3res *result)
{
    struct nfs_file_parallel_io_handle *pfh = arg;

    assert(result != NULL);
    uint64_t ts = rdtsc();
    // error
    if (result->status != NFS3_OK) {
        pfh->fh->status = result->status;
        free(pfh);
        signal_condition();
        goto out;
    }

    READ3resok *res = &result->READ3res_u.resok;
    assert(res->count == res->data.data_len);
    assert(res->data.data_len <= pfh->chunk_size);

    // copy the data
    memcpy((char *)pfh->fh->data + pfh->chunk_start, res->data.data_val,
           res->data.data_len);
    pfh->fh->size_complete += res->data.data_len;

    // is this the end of the file?
    if (res->eof) {
        // reduce the file size to match whatever we got and avoid useless calls
        size_t newsize = pfh->chunk_start + res->data.data_len;
        if (pfh->fh->size > newsize) {
            pfh->fh->size = newsize;
        }
    }
    // check whether the whole chunk was transmitted
    else if (res->data.data_len < pfh->chunk_size) {
        debug_printf("unexpected short read\n");
        pfh->chunk_start += res->data.data_len;
        pfh->chunk_size -= res->data.data_len;

        err_t e = nfs_read(client, pfh->fh->handle,
                           pfh->fh->offset + pfh->chunk_start,
                           pfh->chunk_size, read_callback, pfh);
        assert(e == ERR_OK);

        goto out;
    }

    assert(pfh->fh->size >= pfh->fh->size_complete);

    // check whether all chunks have been transmitted
    if (pfh->fh->size == pfh->fh->size_complete) {
        free(pfh);
    }
    // else create a new request
    else if (pfh->fh->chunk_pos < pfh->fh->size) {
        pfh->chunk_start =  pfh->fh->chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_READ_BYTES, pfh->fh->size - pfh->chunk_start);
        pfh->fh->chunk_pos += pfh->chunk_size;
        pfh->fh->chunks_in_progress++;
        err_t r = nfs_read(client, pfh->fh->handle,
                           pfh->fh->offset + pfh->chunk_start,
                           pfh->chunk_size, read_callback, pfh);
        assert(r == ERR_OK);
    } else {
        free(pfh);
    }

out:
    pfh->fh->chunks_in_progress--;

    // allow the request thread to resume if we're the last chunk
    if (pfh->fh->chunks_in_progress == 0) {
        signal_condition();
    }
    // free arguments
    xdr_READ3res(&xdr_free, result);
    lwip_record_event_simple(NFS_READCB_T, ts);
}

static void write_callback(void *arg, struct nfs_client *client, WRITE3res *result)
{
    struct nfs_file_parallel_io_handle *pfh = arg;

    assert(result != NULL);

    // error
    if (result->status != NFS3_OK) {
        pfh->fh->status = result->status;

#ifdef ASYNC_WRITES
        printf("write_callback: NFS error status %d\n", result->status);

        pfh->fh->back_fh->inflight--;
        assert(pfh->fh->back_fh->inflight >= 0);
        free(pfh->fh);
#endif

        free(pfh);
        signal_condition();
        goto out;
    }

    WRITE3resok *res = &result->WRITE3res_u.resok;
    assert(res->count == pfh->chunk_size);
    pfh->fh->size_complete += res->count;

    assert(pfh->fh->size >= pfh->fh->size_complete);

    // check whether all chunks have been transmitted
    if (pfh->fh->size == pfh->fh->size_complete) {
#ifdef ASYNC_WRITES
        pfh->fh->back_fh->inflight--;
        assert(pfh->fh->back_fh->inflight >= 0);
        free(pfh->fh);
#endif

        signal_condition();
        free(pfh);
    }
    // else create a new request
    else if (pfh->fh->chunk_pos < pfh->fh->size) {
        pfh->chunk_start = pfh->fh->chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_WRITE_BYTES, pfh->fh->size - pfh->chunk_start);
        pfh->fh->chunk_pos += pfh->chunk_size;
        err_t r = nfs_write(client, pfh->fh->handle,
                            pfh->fh->offset + pfh->chunk_start,
                            (char *)pfh->fh->data + pfh->chunk_start, pfh->chunk_size,
                            NFS_WRITE_STABILITY, write_callback, pfh);
        assert(r == ERR_OK);
    } else {
        free(pfh);
    }

out:
    // free arguments
    xdr_WRITE3res(&xdr_free, result);
}

static void open_resolve_cont(void *st, errval_t err, struct nfs_fh3 fh,
                              struct fattr3 *fattr)
{
    struct nfs_handle *h = st;

    if (err_is_ok(err)) {
        // make a copy of the file handle, because we are returning it to the user
        nfs_copyfh(&h->fh, fh);
        if (fattr != NULL) {
            h->type = fattr->type;
        }
    }

    signal_condition();
}

static errval_t open(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = malloc(sizeof(struct nfs_handle));
    assert(h != NULL);

    h->isdir = false;
    h->u.file.pos = 0;
    h->nfs = nfs;
    h->fh = NULL_NFS_FH;
#ifdef ASYNC_WRITES
    h->inflight = 0;
#endif
#ifdef WITH_META_DATA_CACHE
    h->filesize_cached = false;
    h->cached_filesize = 0;
#endif

    lwip_mutex_lock();
    initiate_resolve(nfs, path, open_resolve_cont, h);
    wait_for_condition();
    lwip_mutex_unlock();

    if (h->fh.data_len > 0 && h->type != NF3DIR) {
        *rethandle = h;
        return SYS_ERR_OK;
    } else if (h->fh.data_len > 0 && h->type == NF3DIR) {
        free(h);
        return FS_ERR_NOTFILE;
    } else {
        free(h);
        return FS_ERR_NOTFOUND;
    }
}

static void create_callback(void *arg, struct nfs_client *client,
                            CREATE3res *result)
{
    struct nfs_handle *h = arg;

    assert(result != NULL);

    // error
    if (result->status == NFS3_OK) {
        struct CREATE3resok *res = &result->CREATE3res_u.resok;
        assert(res->obj.handle_follows);
        // make a copy of the file handle, because we are returning it to the user
        nfs_copyfh(&h->fh, res->obj.post_op_fh3_u.handle);
        if (res->obj_attributes.attributes_follow) {
            h->type = res->obj_attributes.post_op_attr_u.attributes.type;
        }
    } else { // XXX: Proper error handling
        debug_printf("Error in create_callback %d\n", result->status);
    }

    signal_condition();
}

static void create_resolve_cont(void *st, errval_t err, struct nfs_fh3 fh,
                               struct fattr3 *fattr)
{
    struct nfs_handle *h = st;

    if (err_is_fail(err) || (fattr != NULL && fattr->type != NF3DIR)) {
        DEBUG_ERR(err, "failure in create_resolve_cont");
        // FIXME: failed to lookup directory. return meaningful error
        signal_condition();
        return;
    }

    static struct sattr3 nulattr;
    err_t r = nfs_create(h->nfs->client, fh, h->st, false, nulattr,
                         create_callback, h);
    assert(r == ERR_OK);
}

static errval_t create(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct nfs_state *nfs = st;

    // find last path component
    char *filename = strrchr(path, VFS_PATH_SEP);
    if (filename == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // get directory name in separate nul-terminated string buffer
    char *dir = malloc(filename - path + 1);
    assert(dir != NULL);
    memcpy(dir, path, filename - path);
    dir[filename - path] = '\0';

    // advance past separator
    filename = filename + 1;

    struct nfs_handle *h = malloc(sizeof(struct nfs_handle));
    assert(h != NULL);

    h->isdir = false;
    h->u.file.pos = 0;
    h->nfs = nfs;
    h->fh = NULL_NFS_FH;
    h->st = filename;
    h->fh.data_len = 0;
#ifdef ASYNC_WRITES
    h->inflight = 0;
#endif
#ifdef WITH_META_DATA_CACHE
    h->filesize_cached = false;
    h->cached_filesize = 0;
#endif

    lwip_mutex_lock();
    initiate_resolve(nfs, dir, create_resolve_cont, h);
    wait_for_condition();
    lwip_mutex_unlock();

    free(dir);

    if (h->fh.data_len > 0) {
        *rethandle = h;
        return SYS_ERR_OK;
    } else {
        free(h);
        return FS_ERR_NOTFOUND; // XXX
    }
}

static errval_t opendir(void *st, const char *path, vfs_handle_t *rethandle)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = malloc(sizeof(struct nfs_handle));
    assert(h != NULL);

    h->isdir = true;
    h->u.dir.readdir_result = NULL;
    h->u.dir.readdir_next = NULL;
    h->u.dir.readdir_prev = NULL;
    h->nfs = nfs;
    h->fh = NULL_NFS_FH;
#ifdef ASYNC_WRITES
    h->inflight = 0;
#endif

    // skip leading '/'s
    while (*path == VFS_PATH_SEP) {
        path++;
    }

    // short-circuit lookup of root dir
    if (*path == '\0') {
        nfs_copyfh(&h->fh, nfs->rootfh);
        h->type = NF3DIR;
        *rethandle = h;
        return SYS_ERR_OK;
    }

    lwip_mutex_lock();
    initiate_resolve(nfs, path, open_resolve_cont, h);
    wait_for_condition();
    lwip_mutex_unlock();

    if (h->fh.data_len > 0 && h->type == NF3DIR) {
        *rethandle = h;
        return SYS_ERR_OK;
    } else {
        free(h);
        return FS_ERR_NOTFOUND;
    }
}

static errval_t read(void *st, vfs_handle_t inhandle, void *buffer,
        size_t bytes, size_t *bytes_read)
{
    uint64_t ts = rdtsc();
    struct nfs_state *nfs = st;
    struct nfs_handle *h = inhandle;
    assert(h != NULL);
    err_t e;

    assert(!h->isdir);

    // set up the handle
    struct nfs_file_io_handle fh;
    memset(&fh, 0, sizeof(struct nfs_file_io_handle));

    fh.data = buffer;
    fh.size = bytes;
    fh.offset = h->u.file.pos;
    fh.status = NFS3_OK;
    fh.handle = h->fh;
    fh.chunks_in_progress = 0;

    lwip_mutex_lock();

    // start a parallel load of the file, wait for it to complete
    int chunks = 0;
    while (fh.chunk_pos < fh.size && chunks < MAX_NFS_READ_CHUNKS) {
//    while (fh.chunk_pos < fh.size) {
        struct nfs_file_parallel_io_handle *pfh =
            malloc(sizeof(struct nfs_file_parallel_io_handle));

        pfh->fh = &fh;
        pfh->chunk_start = fh.chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_READ_BYTES, fh.size - pfh->chunk_start);
        fh.chunk_pos += pfh->chunk_size;

        fh.chunks_in_progress++;
        e = nfs_read(nfs->client, fh.handle, fh.offset + pfh->chunk_start,
                     pfh->chunk_size, read_callback, pfh);

        if (e == ERR_MEM) { // internal resource limit in lwip?
            printf("read: error in nfs_read ran out of mem!!!\n");
            printf("read: error chunks %d in progress %d!!!\n",
                    chunks, (int)fh.chunks_in_progress);
            fh.chunk_pos -= pfh->chunk_size;
            free(pfh);
            break;
        }
        assert(e == ERR_OK);
        chunks++;
#ifdef NONBLOCKING_NFS_READ
        check_and_handle_other_events();
#endif // NONBLOCKING_NFS_READ
    }
    lwip_record_event_simple(NFS_READ_1_T, ts);
    uint64_t ts1 = rdtsc();
    wait_for_condition_fair();
    lwip_record_event_simple(NFS_READ_w_T, ts1);

    lwip_mutex_unlock();

    // check result
    if (fh.status != NFS3_OK) {
  //      printf("read:vfs_nfs: fh.status issue %u\n", fh.status);
        return nfsstat_to_errval(fh.status);
    }

    assert(fh.size <= bytes);
    h->u.file.pos += fh.size;
    *bytes_read = fh.size;

    lwip_record_event_simple(NFS_READ_T, ts);
    if (fh.size == 0) {
        /* XXX: assuming this means EOF, but we really do know from NFS */
/*        printf("read:vfs_nfs: EOF marking %"PRIuPTR" < %"PRIuPTR","
                "parallel NFS chunks [%u]\n",
                fh.size, bytes, MAX_NFS_READ_CHUNKS);
*/
        return VFS_ERR_EOF;
    } else {
        return SYS_ERR_OK;
    }
}

static errval_t write(void *st, vfs_handle_t handle, const void *buffer,
                      size_t bytes, size_t *bytes_written)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = handle;
    assert(h != NULL);
    err_t e;

    #if 0
    if((__builtin_return_address(2) < (void *)fclose ||
        __builtin_return_address(2) > (void *)memcpy) &&
       (__builtin_return_address(3) < (void *)fclose ||
        __builtin_return_address(3) > (void *)memcpy) &&
       (__builtin_return_address(4) < (void *)fclose ||
        __builtin_return_address(4) > (void *)memcpy)) {
        printf("vfs_nfs->write called not from fclose()! %p, %p, %p, %p\n",
               __builtin_return_address(0),
               __builtin_return_address(1),
               __builtin_return_address(2),
               __builtin_return_address(3));
    }
    #endif

    assert(!h->isdir);

    // set up the handle
#ifndef ASYNC_WRITES
    struct nfs_file_io_handle the_fh;
    struct nfs_file_io_handle *fh = &the_fh;
#else
    struct nfs_file_io_handle *fh = malloc(sizeof(struct nfs_file_io_handle));
    assert(fh != NULL);
#endif
    memset(fh, 0, sizeof(struct nfs_file_io_handle));

#ifdef ASYNC_WRITES
    fh->data = malloc(bytes);
    assert(fh->data != NULL);
    memcpy(fh->data, buffer, bytes);
    h->inflight++;
#else
    fh->data = (void *)buffer;
#endif
    fh->size = bytes;
    fh->offset = h->u.file.pos;
    fh->status = NFS3_OK;
    fh->handle = h->fh;
    fh->back_fh = h;

    lwip_mutex_lock();

    // start a parallel write of the file, wait for it to complete
    int chunks = 0;
    do {
        struct nfs_file_parallel_io_handle *pfh =
            malloc(sizeof(struct nfs_file_parallel_io_handle));
        pfh->fh = fh;
        pfh->fh->chunks_in_progress = 0;
        pfh->chunk_start = fh->chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_WRITE_BYTES, fh->size - pfh->chunk_start);
        fh->chunk_pos += pfh->chunk_size;
        e = nfs_write(nfs->client, fh->handle, fh->offset + pfh->chunk_start,
                      (char *)fh->data + pfh->chunk_start, pfh->chunk_size,
                      NFS_WRITE_STABILITY, write_callback, pfh);
        assert(e == ERR_OK);
        chunks++;
    } while (fh->chunk_pos < fh->size && chunks < MAX_NFS_WRITE_CHUNKS);
#ifndef ASYNC_WRITES
    wait_for_condition();
#endif

    lwip_mutex_unlock();

#ifndef ASYNC_WRITES
    // check result
    if (fh->status != NFS3_OK && fh->status != NFS3ERR_STALE) {
        printf("NFS Error: %d\n", fh->status);
        return nfsstat_to_errval(fh->status);
    }

    assert(fh->size <= bytes);
    h->u.file.pos += fh->size;
    *bytes_written = fh->size;
#else
    // This always assumes it succeeded. We'll see failures at file close time.
    h->u.file.pos += bytes;
    *bytes_written = bytes;
#endif

    return SYS_ERR_OK;
}


static void setattr_callback(void *arg, struct nfs_client *client,
                             SETATTR3res *result)
{
    assert(result != NULL);

    xdr_SETATTR3res(&xdr_free, result);
    assert(result->status == NFS3_OK);

    signal_condition();
}


static errval_t truncate(void *st, vfs_handle_t handle, size_t bytes)
{
    struct nfs_handle *h = handle;

    assert(!h->isdir);

    struct nfs_state *nfs = st;
    assert(h != NULL);
    err_t e;

    lwip_mutex_lock();
    // We only set the size field for now

    sattr3 new_attributes;
    new_attributes.mode.set_it = FALSE;
    new_attributes.uid.set_it = FALSE;
    new_attributes.gid.set_it = FALSE;
    new_attributes.atime.set_it = FALSE;
    new_attributes.mtime.set_it = FALSE;
    new_attributes.size.set_it = TRUE;
    new_attributes.size.set_size3_u.size = bytes;


    e = nfs_setattr(nfs->client, h->fh,
                    new_attributes, false,
                    setattr_callback, NULL);
    assert(e == ERR_OK);
    wait_for_condition();
    lwip_mutex_unlock();

    return SYS_ERR_OK;
}

static enum vfs_filetype nfs_type_to_vfs_type(enum ftype3 type)
{
    switch(type) {
    case NF3DIR:
        return VFS_DIRECTORY;
    case NF3REG:
        return VFS_FILE;
    default:
        return VFS_FILE; // XXX
    }
}

static void getattr_callback(void *arg, struct nfs_client *client,
                             GETATTR3res *result)
{
    struct vfs_fileinfo *info = arg;

    assert(result != NULL);
    if (result->status == NFS3_OK) {
        fattr3 *res = &result->GETATTR3res_u.resok.obj_attributes;

        info->type = nfs_type_to_vfs_type(res->type);
        info->size = res->size;
    } else {
        // XXX: no error reporting!
        printf("GETATTR Error: %d\n", result->status);
        info->type = -1;
        info->size = -1;
    }

    xdr_GETATTR3res(&xdr_free, result);

    signal_condition();
}

static errval_t tell(void *st, vfs_handle_t handle, size_t *pos)
{
    struct nfs_handle *h = handle;
    assert(!h->isdir);
    *pos = h->u.file.pos;
    return SYS_ERR_OK;
}

static errval_t stat(void *st, vfs_handle_t inhandle, struct vfs_fileinfo *info)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = inhandle;
    assert(h != NULL);
    err_t e;

    lwip_mutex_lock();
    e = nfs_getattr(nfs->client, h->fh, getattr_callback, info);
    assert(e == ERR_OK);
    wait_for_condition();
    lwip_mutex_unlock();

    assert(h->isdir == (info->type == VFS_DIRECTORY));

    return SYS_ERR_OK;
}

static errval_t seek(void *st, vfs_handle_t handle, enum vfs_seekpos whence,
                     off_t offset)
{
    struct nfs_handle *h = handle;
    struct vfs_fileinfo info;
    errval_t err;

    assert(!h->isdir);

    switch (whence) {
    case VFS_SEEK_SET:
        assert(offset >= 0);
        h->u.file.pos = offset;
        break;

    case VFS_SEEK_CUR:
        assert(offset >= 0 || -offset <= h->u.file.pos);
        h->u.file.pos += offset;
        break;

    case VFS_SEEK_END:
        err = stat(st, handle, &info);
        if (err_is_fail(err)) {
            return err;
        }
        assert(offset >= 0 || -offset <= info.size);
        h->u.file.pos = info.size + offset;
        break;

    default:
        USER_PANIC("invalid whence argument to nfs seek");
    }

    return SYS_ERR_OK;
}

static void readdir_callback(void *arg, struct nfs_client *client,
                             READDIR3res *result)
{
    struct nfs_handle *h = arg;
    assert(h->isdir);

    assert(result != NULL && result->status == NFS3_OK); // XXX

    // make copy of result object, as it is allocated on our caller's stack
    assert(h->u.dir.readdir_result == NULL);
    h->u.dir.readdir_result = malloc(sizeof(READDIR3res));
    assert(h->u.dir.readdir_result != NULL);
    memcpy(h->u.dir.readdir_result, result, sizeof(READDIR3res));

    h->u.dir.readdir_next = result->READDIR3res_u.resok.reply.entries;
    h->u.dir.readdir_prev = NULL;

    signal_condition();
}

static void get_info_lookup_cb(void *arg, struct nfs_client *client,
                               LOOKUP3res *result)
{
    assert(result != NULL && result->status == NFS3_OK); // XXX

    LOOKUP3resok *resok = &result->LOOKUP3res_u.resok;

    err_t e = nfs_getattr(client, resok->object, getattr_callback, arg);
    assert(err_is_ok(e));

    xdr_LOOKUP3res(&xdr_free, result);
}

static errval_t dir_read_next(void *st, vfs_handle_t inhandle,
                              char **retname, struct vfs_fileinfo *info)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = inhandle;
    struct entry3 *entry;
    err_t e;

    assert(h->isdir);

    lwip_mutex_lock();

top:
    // do we have a cached result?
    if (h->u.dir.readdir_result != NULL && h->u.dir.readdir_next != NULL) {
        entry = h->u.dir.readdir_next;
        h->u.dir.readdir_prev = entry;
        h->u.dir.readdir_next = entry->nextentry;
    } else if (h->u.dir.readdir_result != NULL
               && h->u.dir.readdir_result->READDIR3res_u.resok.reply.eof) {
        lwip_mutex_unlock();
        return FS_ERR_INDEX_BOUNDS; // end of list
    } else {

        if (h->u.dir.readdir_result != NULL) { // subsequent call
            struct READDIR3res *oldresult = h->u.dir.readdir_result;
            struct entry3 *oldentry = h->u.dir.readdir_prev;
            h->u.dir.readdir_result = NULL;

            e = nfs_readdir(nfs->client, h->fh, oldentry->cookie,
                            oldresult->READDIR3res_u.resok.cookieverf,
                            readdir_callback, h);
            assert(e == ERR_OK);

            xdr_READDIR3res(&xdr_free, oldresult);
            free(oldresult);
        } else { // first call
            e = nfs_readdir(nfs->client, h->fh, NFS_READDIR_COOKIE,
                            NFS_READDIR_COOKIEVERF, readdir_callback, h);
            assert(e == ERR_OK);
        }

        wait_for_condition();

        entry = h->u.dir.readdir_next;
        h->u.dir.readdir_prev = entry;
        h->u.dir.readdir_next = entry != NULL ? entry->nextentry : NULL;
    }

    if (entry == NULL) {
        assert(h->u.dir.readdir_result->READDIR3res_u.resok.reply.eof);
        lwip_mutex_unlock();
        return FS_ERR_INDEX_BOUNDS;
    } else {
        assert(entry->name != NULL);
        if (strcmp(entry->name, ".") == 0 || strcmp(entry->name, "..") == 0) {
            // XXX: hide these from the VFS, because it doesn't understand them
            goto top;
        }
        if (retname != NULL) {
            *retname = strdup(entry->name);
        }
        if (info != NULL) {
            // initiate a lookup/getattr call to find out this information
            e = nfs_lookup(nfs->client, h->fh, entry->name, get_info_lookup_cb,
                           info);
            assert(e == ERR_OK);
            wait_for_condition();
        }
        lwip_mutex_unlock();
        return SYS_ERR_OK;
    }
}

static errval_t close(void *st, vfs_handle_t inhandle)
{
    struct nfs_handle *h = inhandle;
    assert(!h->isdir);

#ifdef ASYNC_WRITES
    while(h->inflight > 0) {
        wait_for_condition();
    }

    // XXX: Errors ignored for now. Will be reported by handler functions though.
#endif

    nfs_freefh(h->fh);
    free(h);
    return SYS_ERR_OK;
}

static errval_t closedir(void *st, vfs_handle_t inhandle)
{
    struct nfs_handle *h = inhandle;
    assert(h->isdir);
    if (h->u.dir.readdir_result != NULL) {
        xdr_READDIR3res(&xdr_free, h->u.dir.readdir_result);
        free(h->u.dir.readdir_result);
    }
    nfs_freefh(h->fh);
    free(h);
    return SYS_ERR_OK;
}

static void remove_callback(void *arg, struct nfs_client *client,
                            REMOVE3res *result)
{
    struct nfs_handle *h = arg;
    assert(result != NULL);

    // XXX: Should find better way to report error
    h->fh.data_len = result->status;

    signal_condition();
}

static void remove_resolve_cont(void *st, errval_t err, struct nfs_fh3 fh,
                                struct fattr3 *fattr)
{
    struct nfs_handle *h = st;

    if (err_is_fail(err) || (fattr != NULL && fattr->type != NF3DIR)) {
        DEBUG_ERR(err, "failure in remove_resolve_cont");
        // FIXME: failed to lookup directory. return meaningful error
        signal_condition();
        return;
    }

    err_t r = nfs_remove(h->nfs->client, fh, h->st, remove_callback, h);
    assert(r == ERR_OK);
}

static errval_t vfs_nfs_remove(void *st, const char *path)
{
    struct nfs_state *nfs = st;

    // find last path component
    char *filename = strrchr(path, VFS_PATH_SEP);
    if (filename == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // get directory name in separate nul-terminated string buffer
    char *dir = malloc(filename - path + 1);
    assert(dir != NULL);
    memcpy(dir, path, filename - path);
    dir[filename - path] = '\0';

    // advance past separator
    filename = filename + 1;

    struct nfs_handle *h = malloc(sizeof(struct nfs_handle));
    assert(h != NULL);

    h->isdir = false;
    h->u.file.pos = 0;
    h->nfs = nfs;
    h->fh = NULL_NFS_FH;
    h->st = filename;
    h->fh.data_len = 0;
#ifdef ASYNC_WRITES
    h->inflight = 0;
#endif

    lwip_mutex_lock();
    initiate_resolve(nfs, dir, remove_resolve_cont, h);
    wait_for_condition();
    lwip_mutex_unlock();

    size_t err = h->fh.data_len;
    free(dir);
    free(h);

    switch(err) {
    case NFS3_OK:
        return SYS_ERR_OK;

    case NFS3ERR_NOENT:
        return FS_ERR_NOTFOUND;

    default:
        // XXX: Unknown error
        return NFS_ERR_TRANSPORT;
    }
}

struct mkdir_state {
    struct nfs_client *client;
    const char *dirname;
    errval_t err;
};

static void mkdir_callback(void *arg, struct nfs_client *client,
                           MKDIR3res *result)
{
    struct mkdir_state *st = arg;

    assert(result != NULL);

    st->err = nfsstat_to_errval(result->status);

    signal_condition();
}

static void mkdir_resolve_cont(void *st, errval_t err, struct nfs_fh3 fh,
                               struct fattr3 *fattr)
{
    struct mkdir_state *s = st;

    if (err_is_fail(err) || (fattr != NULL && fattr->type != NF3DIR)) {
        // failed to lookup directory
        if (err_is_fail(err)) {
            s->err = err;
        } else {
            s->err = FS_ERR_NOTDIR;
        }
        signal_condition();
        return;
    }

    static struct sattr3 nulattr;
    err_t r = nfs_mkdir(s->client, fh, s->dirname, nulattr, mkdir_callback, s);
    if (r != ERR_OK) { // XXX: proper error handling
        debug_printf("error in mkdir_resolve_cont %d\n", r);
    }
    assert(r == ERR_OK);
}

static errval_t mkdir(void *st, const char *path)
{
    struct nfs_state *nfs = st;

    // find last path component
    char *dirname = strrchr(path, VFS_PATH_SEP);
    if (dirname == NULL) {
        return FS_ERR_NOTFOUND;
    }

    // get directory name in separate nul-terminated string buffer
    char *parent = malloc(dirname - path + 1);
    assert(parent != NULL);
    memcpy(parent, path, dirname - path);
    parent[dirname - path] = '\0';

    // advance past separator
    dirname = dirname + 1;

    struct mkdir_state state = {
        .dirname = dirname,
        .client = nfs->client,
        .err = SYS_ERR_OK,
    };

    lwip_mutex_lock();
    initiate_resolve(nfs, parent, mkdir_resolve_cont, &state);
    wait_for_condition();
    lwip_mutex_unlock();

    free(parent);

    return state.err;
}

#ifdef WITH_BUFFER_CACHE

static errval_t get_bcache_key(void *st, vfs_handle_t inhandle,
                               char **retkey, size_t *keylen, size_t *retoffset)
{
    struct nfs_handle *h = inhandle;
    assert(h != NULL);
    size_t filepos = h->u.file.pos;

    assert(!h->isdir);

    size_t blockid = filepos / BUFFER_CACHE_BLOCK_SIZE;
    *retoffset = filepos % BUFFER_CACHE_BLOCK_SIZE;

#if 0
    // XXX: Incredibly slow way to generate a looong hash key
    *retkey = malloc(200);
    *retkey[0] = '\0';
    char str[20];
    for(int i = 0; i < h->fh.data_len; i++) {
        snprintf(str, 20, "%02x", h->fh.data_val[i]);
        strcat(*retkey, str);
        assert(strlen(*retkey) < 200);
    }
    snprintf(str, 20, "/%lx", blockid);
    strcat(*retkey, str);
#else
    *keylen = h->fh.data_len + sizeof(blockid);
    *retkey = malloc(*keylen);
    assert(*retkey != NULL);
    memcpy(*retkey, h->fh.data_val, h->fh.data_len);
    memcpy(&(*retkey)[h->fh.data_len], &blockid, sizeof(blockid));
#endif

    return SYS_ERR_OK;
}

static errval_t read_block(void *st, vfs_handle_t inhandle, void *buffer,
                           size_t *bytes_read)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = inhandle;
    assert(h != NULL);
    err_t e;

    assert(!h->isdir);

    // set up the handle
    struct nfs_file_io_handle fh;
    memset(&fh, 0, sizeof(struct nfs_file_io_handle));

    fh.data = buffer;
    fh.size = BUFFER_CACHE_BLOCK_SIZE;
    fh.offset = (h->u.file.pos / BUFFER_CACHE_BLOCK_SIZE) * BUFFER_CACHE_BLOCK_SIZE;
    fh.status = NFS3_OK;
    fh.handle = h->fh;
    fh.chunks_in_progress = 0;

    lwip_mutex_lock();

    // start a parallel load of the file, wait for it to complete
    int chunks = 0;
    while (fh.chunk_pos < fh.size && chunks < MAX_NFS_READ_CHUNKS) {
//    while (fh.chunk_pos < fh.size) {
        struct nfs_file_parallel_io_handle *pfh =
            malloc(sizeof(struct nfs_file_parallel_io_handle));

        pfh->fh = &fh;
        pfh->chunk_start = fh.chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_READ_BYTES, fh.size - pfh->chunk_start);
        fh.chunk_pos += pfh->chunk_size;
        fh.chunks_in_progress++;
        e = nfs_read(nfs->client, fh.handle, fh.offset + pfh->chunk_start,
                     pfh->chunk_size, read_callback, pfh);
        if (e == ERR_MEM) { // internal resource limit in lwip?
            fh.chunk_pos -= pfh->chunk_size;
            free(pfh);
            break;
        }
        assert(e == ERR_OK);
        chunks++;
    } // end while
    wait_for_condition();

    lwip_mutex_unlock();

    // check result
    if (fh.status != NFS3_OK && fh.status != NFS3ERR_STALE) {
        return nfsstat_to_errval(fh.status);
    }

    assert(fh.size <= BUFFER_CACHE_BLOCK_SIZE);
    *bytes_read = fh.size;

    if (fh.size < BUFFER_CACHE_BLOCK_SIZE) {
        /* XXX: assuming this means EOF, but we really do know from NFS */
        return VFS_ERR_EOF;
    } else {
        return SYS_ERR_OK;
    }
}

#if 0
static errval_t write_block(void *st, vfs_handle_t handle, const void *buffer,
                            size_t bytes, size_t *bytes_written)
{
    struct nfs_state *nfs = st;
    struct nfs_handle *h = handle;
    assert(h != NULL);
    err_t e;

    assert(!h->isdir);

    // set up the handle
    struct nfs_file_io_handle fh;
    memset(&fh, 0, sizeof(struct nfs_file_io_handle));

    assert(bytes <= BUFFER_CACHE_BLOCK_SIZE);

    fh.data = (void *)buffer;
    fh.size = bytes;
    fh.offset = (h->u.file.pos / BUFFER_CACHE_BLOCK_SIZE) * BUFFER_CACHE_BLOCK_SIZE;
    fh.status = NFS3_OK;
    fh.handle = h->fh;

    lwip_mutex_lock();

    // start a parallel write of the file, wait for it to complete
    int chunks = 0;
    do {
        struct nfs_file_parallel_io_handle *pfh =
            malloc(sizeof(struct nfs_file_parallel_io_handle));
        pfh->fh = &fh;
        pfh->chunk_start = fh.chunk_pos;
        pfh->chunk_size = MIN(MAX_NFS_WRITE_BYTES, fh.size - pfh->chunk_start);
        fh.chunk_pos += pfh->chunk_size;
        e = nfs_write(nfs->client, fh.handle, fh.offset + pfh->chunk_start,
                      (char *)fh.data + pfh->chunk_start, pfh->chunk_size,
                      NFS_WRITE_STABILITY, write_callback, pfh);
        assert(e == ERR_OK);
        chunks++;
    } while (fh.chunk_pos < fh.size && chunks < MAX_NFS_WRITE_CHUNKS);
    wait_for_condition();

    lwip_mutex_unlock();

    // check result
    if (fh.status != NFS3_OK) {
        return nfsstat_to_errval(fh.status);
    }

    assert(fh.size <= bytes);
    h->u.file.pos += fh.size;
    *bytes_written = fh.size;

    return SYS_ERR_OK;
}
#endif

#endif

static void
mount_callback (void *arg, struct nfs_client *client, enum mountstat3 mountstat,
                struct nfs_fh3 fhandle)
{
    struct nfs_state *st = arg;

    st->mountstat = mountstat;

    // save the root dir handle
    nfs_copyfh(&st->rootfh, fhandle);
    // signal the waiting code to continue execution
    signal_condition();
}

static struct vfs_ops nfsops = {
    .open = open,
    .create = create,
    .read = read,
    .write = write,
    .seek = seek,
    .truncate = truncate,
    .tell = tell,
    .stat = stat,
    .close = close,
    .opendir = opendir,
    .dir_read_next = dir_read_next,
    .closedir = closedir,
    .remove = vfs_nfs_remove,
    .mkdir = mkdir,
    //.rmdir = rmdir,

#ifdef WITH_BUFFER_CACHE
    .get_bcache_key = get_bcache_key,
    .read_block = read_block,
    //.write_block = write_block,
#endif
};

errval_t vfs_nfs_mount(const char *uri, void **retst, struct vfs_ops **retops)
{
    // skip over protocol part of URI
    char *host = strstr(uri, "://");
    if (host == NULL) {
        return VFS_ERR_BAD_URI;
    }
    host += 3;

    // locate next '/', assume everything before that is an IP address
    char *path = strchr(host, '/');
    if (path == NULL) {
        return VFS_ERR_BAD_URI;
    }

    char host_copy[path - host + 1];
    memcpy(host_copy, host, path - host);
    host_copy[path - host] = '\0';

    struct in_addr server1;
    if (inet_aton(host_copy, &server1) == 0) {
        printf("Invalid host IP: %s\n", host_copy);
        return VFS_ERR_BAD_URI;
    }
    struct ip_addr server2 = { .addr = server1.s_addr }; // XXX

    // init stack if needed
    static bool stack_inited;
    if (!stack_inited) {
        lwip_init_auto();
        stack_inited = true;
    }

    struct nfs_state *st = malloc(sizeof(struct nfs_state));
    assert(st != NULL);

    lwip_mutex_lock();
    st->client = nfs_mount(server2, path, mount_callback, st);
    assert(st->client != NULL);
    wait_for_condition();
    lwip_mutex_unlock();

    if (st->mountstat == MNT3_OK) {
        *retst = st;
        *retops = &nfsops;

#ifdef WITH_BUFFER_CACHE
        return buffer_cache_enable(retst, retops);
#else
        return SYS_ERR_OK;
#endif
    } else {
        errval_t ret = mountstat_to_errval(st->mountstat);
        free(st);
        return ret;
    }
}
