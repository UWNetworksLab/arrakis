/**
 * \file
 * \brief VFS buffer cache.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/bulk_transfer.h>
#include <barrelfish/nameservice_client.h>
#include <vfs/vfs.h>
#include <if/bcache_rpcclient_defs.h>

#ifdef WITH_BUFFER_CACHE

#ifdef WITH_META_DATA_CACHE
#       define CACHE_META_DATA
#endif

//#define FAKE_SCALABLE_CACHE

#include "vfs_backends.h"

#define BCACHE_NAME     "bcache"
#define MAX_CACHES      10

struct bcache_client {
    struct bcache_rpc_client rpc;
    struct bulk_transfer_slave bulk_slave;
    struct capref cache_memory;
    bool bound;
};

struct bcache_state {
    void *orig_st;
    struct vfs_ops *orig_ops;
};

static struct bcache_client *cache[MAX_CACHES];
static size_t num_caches = 0;

enum cacheOps {
    cacheOpen,
    cacheCreate,
    cacheRead,
    cacheWrite,
    cacheClose,
    cacheTruncate,
    cacheStat,
    cacheOpendir,
    cacheDirNext,
    cacheClosedir,
    cacheMkdir,
    cacheRmdir,
    cacheUnlink,
    cacheTell,
    cacheSeek,
    cacheTotalOps
};

static char *cacheOpsName[] = {
    [cacheOpen    ] = "open",
    [cacheCreate  ] = "create",
    [cacheTruncate] = "truncate",
    [cacheStat    ] = "stat",
    [cacheClose   ] = "close",
    [cacheOpendir ] = "opendir",
    [cacheDirNext ] = "dirnext",
    [cacheClosedir] = "closedir",
    [cacheMkdir   ] = "mkdir",
    [cacheRmdir   ] = "rmdir",
    [cacheUnlink  ] = "unlink",
    [cacheRead    ] = "read",
    [cacheWrite   ] = "write",
    [cacheTell    ] = "tell",
    [cacheSeek    ] = "seek",
    [cacheTotalOps] = "___YOU_SHOULD_NOT_SEE_THIS___",
};


static inline void stats_update(enum cacheOps op, int hit, uint64_t ticks);
static inline void start_stats(void);
static inline void end_stats(enum cacheOps op, int hit);

static struct {
    size_t cnt;
   uint64_t ticks;
} Stats[cacheTotalOps][2];

uint64_t ticks0__;
uint64_t tscperms;

static inline void
stats_update(enum cacheOps op, int hit, uint64_t ticks)
{
    hit = !!hit;
    Stats[op][hit].cnt++;
    Stats[op][hit].ticks += ticks;
}

static inline void
start_stats(void)
{
    ticks0__ = rdtsc();
}

static inline void
end_stats(enum cacheOps op, int hit)
{
    stats_update(op, hit, rdtsc() - ticks0__);
}


#include <nfs/nfs.h>
#include "vfs_nfs.h"

#include <stdio.h>

#ifdef CACHE_META_DATA

#include <hashtable/hashtable.h>

#define MAX_DIR         5000

struct dir_entry {
    vfs_handle_t fh;
    bool deleted;
    size_t size;
};

static struct dir_entry directory[MAX_DIR];
static struct hashtable *dir_hash;
static int alloc_ptr = 0;
static size_t meta_hits = 0, meta_misses = 0, meta_overwrites = 0;

static void meta_data_init(void)
{
    /*    dir_hash = create_hashtable2(MAX_DIR, 75);
    assert(dir_hash != NULL);
    */
}

static void meta_data_lookup(const char *fname,
                             bool *haveit, bool *deleted, vfs_handle_t *fh)
{
    ENTRY_TYPE et;
    void *val;

    et = dir_hash->d.get(&dir_hash->d, fname, strlen(fname), &val);
    assert(et == TYPE_WORD || et == 0);

    if(et == TYPE_WORD) {
        /* printf("'%s' is in cache\n", fname); */
        uintptr_t index = (uintptr_t)val;
        struct dir_entry *e = &directory[index];

        *haveit = true;
        *deleted = e->deleted;

        if(!*deleted) {
            *fh = malloc(sizeof(struct nfs_handle));
            assert(*fh != NULL);

            memcpy(*fh, e->fh, sizeof(struct nfs_handle));

            struct nfs_handle *dest = *fh, *src = e->fh;
            nfs_copyfh(&dest->fh, src->fh);
        }

        meta_hits++;
    } else {
        /* printf("'%s' not in cache\n", fname); */
        *haveit = false;
        *deleted = false;
        *fh = NULL;

        meta_misses++;
    }
}

static void meta_data_create(vfs_handle_t fh, const char *fname)
{
    ENTRY_TYPE et;
    void *val;
    struct dir_entry *e;

    et = dir_hash->d.get(&dir_hash->d, fname, strlen(fname), &val);
    assert(et == TYPE_WORD || et == 0);

    if(et == TYPE_WORD) {
        uintptr_t index = (uintptr_t)val;
        e = &directory[index];
        /* printf("overwriting '%s'\n", fname); */
        meta_overwrites++;
    } else {
        /* printf("newly creating '%s'\n", fname); */
        assert(alloc_ptr < MAX_DIR);
        e = &directory[alloc_ptr];
        char *newfname = strdup(fname);
        int r = dir_hash->d.put_word(&dir_hash->d, newfname, strlen(newfname), alloc_ptr);
        assert(r == 0);
        alloc_ptr++;
    }

    e->fh = malloc(sizeof(struct nfs_handle));
    assert(e->fh != NULL);
    memcpy(e->fh, fh, sizeof(struct nfs_handle));
    struct nfs_handle *dest = e->fh, *src = fh;
    nfs_copyfh(&dest->fh, src->fh);
    e->deleted = false;
}

static void meta_data_delete(const char *fname, bool *success)
{
    ENTRY_TYPE et;
    void *val;
    struct dir_entry *e;

    et = dir_hash->d.get(&dir_hash->d, fname, strlen(fname), &val);
    assert(et == TYPE_WORD || et == 0);

    if(et == TYPE_WORD) {
        uintptr_t index = (uintptr_t)val;
        e = &directory[index];
        /* printf("deleting '%s' in cache\n", fname); */
    } else {
        /* printf("deleting '%s' not in cache\n", fname); */
        assert(alloc_ptr < MAX_DIR);
        e = &directory[alloc_ptr];
        int r = dir_hash->d.put_word(&dir_hash->d, fname, strlen(fname), alloc_ptr);
        assert(r == 0);
        alloc_ptr++;
    }

    if(!e->deleted) {
        struct nfs_handle *dest = e->fh;
        if(dest != NULL) {
            nfs_freefh(dest->fh);
            free(e->fh);
        }
        e->deleted = true;
        *success = true;
    } else {
        *success = false;
    }
}
#endif

static errval_t open(void *st, const char *path, vfs_handle_t *rethandle)
{
    start_stats();
    bool hit = false;
    // Hand through...
    errval_t err = SYS_ERR_OK;
#ifdef CACHE_META_DATA
    bool haveit, deleted;

    meta_data_lookup(path, &haveit, &deleted, rethandle);

    hit = haveit;
    if(!haveit) {
#endif

#ifndef FAKE_SCALABLE_CACHE
        struct bcache_state *bst = st;
        err = bst->orig_ops->open(bst->orig_st, path, rethandle);
        if(err_is_fail(err)) {
            goto end;
        }
#else
        *rethandle = malloc(sizeof(struct nfs_handle));
        memset(*rethandle, 0, sizeof(struct nfs_handle));
        struct nfs_handle *h = *rethandle;
        h->filesize_cached = true;
#endif

#ifdef CACHE_META_DATA
        meta_data_create(*rethandle, path);
    } else {
        if(deleted) {
            err = FS_ERR_NOTFOUND;
        }
    }
#endif

end:
    end_stats(cacheOpen, hit);
    return err;
}

static errval_t create(void *st, const char *path, vfs_handle_t *rethandle)
{
    start_stats();
    bool hit = false;
    // Hand through...
    errval_t err = SYS_ERR_OK;
#ifdef CACHE_META_DATA
    bool haveit, deleted;

    meta_data_lookup(path, &haveit, &deleted, rethandle);

    hit = haveit;
    if(!haveit || deleted) {
#endif

#ifndef FAKE_SCALABLE_CACHE
        struct bcache_state *bst = st;
        err = bst->orig_ops->create(bst->orig_st, path, rethandle);
        if(err_is_fail(err)) {
            goto end;
        }
#else
        *rethandle = malloc(sizeof(struct nfs_handle));
        memset(*rethandle, 0, sizeof(struct nfs_handle));
        struct nfs_handle *h = *rethandle;
        h->filesize_cached = true;
#endif

#ifdef CACHE_META_DATA
    }

    if(!haveit || deleted) {
        /* if(haveit && deleted) { */
        /*     assert(!"NYI"); */
        /* } */
        meta_data_create(*rethandle, path);
    }
#endif

end:
    end_stats(cacheCreate, hit);
    return err;
}

static errval_t cache_remove(void *st, const char *path)
{
    errval_t err = SYS_ERR_OK;

    start_stats();
    // Hand through...
#ifdef CACHE_META_DATA
    bool success;

    meta_data_delete(path, &success);

    if(!success) {
        err = FS_ERR_NOTFOUND;
    }
#else
    struct bcache_state *bst = st;
    err = bst->orig_ops->remove(bst->orig_st, path);
#endif

    end_stats(cacheUnlink, false);
    return err;
}

static bool cache_op_start(char *key, size_t key_len, void **retblock,
                           uint64_t *transid, uint64_t *block_length)
{
    struct bcache_client *bcc = cache[0];
    uint64_t index;
    bool haveit;

#ifndef FAKE_SCALABLE_CACHE
    errval_t err;

    err = bcc->rpc.vtbl.get_start(&bcc->rpc, key, key_len, &index, &haveit,
                                  transid, block_length);
    /* err = bcc->rpc.b->tx_vtbl.get_start_call(bcc->rpc.b, key, key_len); */
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_start");
    }
#else
    index = (10 + disp_get_core_id()) * 4096;
    haveit = true;
    *transid = 0;
    *block_length = 4096;
#endif

    assert(*block_length <= BUFFER_CACHE_BLOCK_SIZE);

    *retblock = bulk_slave_buf_get_mem(&bcc->bulk_slave, index, NULL);
    if(haveit) {
        bulk_slave_prepare_recv(&bcc->bulk_slave, index);
    }

    return haveit;
}

static void cache_op_stop(void *block, uint64_t transid, uintptr_t block_length)
{
    struct bcache_client *bcc = cache[0];
    uint64_t index = 0;

    if(block != NULL && transid == 0) {
        // XXX: Hack to resolve block pointer back to ID
        index = block - bcc->bulk_slave.mem;
        bulk_slave_prepare_send(&bcc->bulk_slave, index);
    }

#ifndef FAKE_SCALABLE_CACHE
    errval_t err;
    err = bcc->rpc.vtbl.get_stop(&bcc->rpc, transid, index, block_length);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "get_stop");
    }
#endif
}

void cache_print_stats(void);
void cache_print_stats(void)
{
    struct bcache_client *bcc = cache[0];
    errval_t err = bcc->rpc.vtbl.print_stats(&bcc->rpc);
    assert(err_is_ok(err));

    printf("cache[%d] stats\n", disp_get_core_id());

    double total_misses_time, total_hits_time;
    total_misses_time = total_hits_time = 0;
    for (int i=0; i<cacheTotalOps; i++) {
        size_t misses_cnt = Stats[i][0].cnt;
        size_t hits_cnt = Stats[i][1].cnt;
        double misses_time = (double)Stats[i][0].ticks/(double)tscperms;
        double hits_time =   (double)Stats[i][1].ticks/(double)tscperms;
        double total_time = misses_time + hits_time;
        total_hits_time += hits_time;
        total_misses_time += misses_time;
        printf(" %-12s:  MISSES[cnt:%6zu time:%12.3lf avg:%12.3lf] | HITS[cnt:%6zu time:%12.3lf avg:%12.3lf] TOTAL:%12.3lf\n",
               cacheOpsName[i],
               misses_cnt, misses_time, misses_cnt != 0 ? misses_time/(double)misses_cnt : 0,
               hits_cnt,   hits_time,   hits_cnt   != 0 ? hits_time  /(double)hits_cnt   : 0,
               total_time);
    }
    printf("==== TOTAL:  MISSES[time:%12.3lf] | HITS[time:%12.3lf ] TOTAL:%12.3lf\n",
            total_misses_time, total_hits_time, total_misses_time + total_hits_time);

    #ifdef CACHE_META_DATA
    printf("meta data\n"
           "=========\n"
           "hits        = %lu\n"
           "misses      = %lu\n"
           "overwrites  = %lu\n"
           "allocations = %u\n",
           meta_hits, meta_misses, meta_overwrites, alloc_ptr);
    #endif
}

static errval_t read(void *st, vfs_handle_t handle, void *buffer, size_t bytes,
                     size_t *bytes_read)
{
    struct bcache_state *bst = st;
    errval_t err = SYS_ERR_OK;

    *bytes_read = 0;

    // Divide into blocks and iterate
    size_t block_offset = 0;
    size_t restbytes = bytes;
    size_t didread;
    for(size_t offset = 0; offset < bytes; offset += BUFFER_CACHE_BLOCK_SIZE - block_offset) {

        start_stats();
        char *key;
        size_t key_len;

        bool hit = false;
        start_stats();

        /* get a unique identifier (key) for the block */
        err = bst->orig_ops->get_bcache_key(bst->orig_st, handle, &key,
                                            &key_len, &block_offset);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "get_bcache_key");
        }

        size_t toread = BUFFER_CACHE_BLOCK_SIZE - block_offset < restbytes ?
            BUFFER_CACHE_BLOCK_SIZE - block_offset : restbytes;

        // Check if in cache -- start cache transaction
        void *blockptr;
        uint64_t transid = 0, block_length = 0;

        if(!cache_op_start(key, key_len, &blockptr, &transid, &block_length)) {
            // Cache doesn't have this block -- read it into cache
            err = bst->orig_ops->read_block(bst->orig_st, handle, blockptr,
                                            (size_t *)&block_length);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "orig_ops->read");
            }

            if(block_length < toread) {
                assert(err_no(err) == VFS_ERR_EOF);
                didread = block_length;
            } else {
                didread = toread;
            }
        } else {
            hit = true;
            // Cache has it -- do we need to emulate EOF?
            if(block_length < toread + block_offset) {
                if(block_length < block_offset) {
                    didread = 0;
                } else {
                    didread = block_length - block_offset;
                }
                err = VFS_ERR_EOF;
            } else {
                // Read only as much as was asked for
                didread = toread;
            }
        }

        // XXX: Not sure if using seek() is always safe
        errval_t r = bst->orig_ops->seek(bst->orig_st, handle, VFS_SEEK_CUR,
                                         didread);
        assert(err_is_ok(r));

        // Copy data to user's buffer
        memcpy(buffer + offset, blockptr + block_offset, didread);

        cache_op_stop(blockptr, transid, block_length);
        free(key);
        restbytes -= didread;
        *bytes_read += didread;

        end_stats(cacheRead, hit);
        if(didread < toread) {
            assert(err_no(err) == VFS_ERR_EOF);
            // The file ended prematurely
            break;
        }
    }

    return err;
}

static errval_t write(void *st, vfs_handle_t handle, const void *buffer,
                      size_t bytes, size_t *bytes_written)
{
    struct bcache_state *bst = st;
    errval_t err = SYS_ERR_OK;

    *bytes_written = 0;

    size_t origpos;

    err = bst->orig_ops->tell(bst->orig_st, handle, &origpos);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "orig_ops->tell");
    }

    // Divide into blocks and iterate
    size_t block_offset = 0;
    size_t restbytes = bytes;
    for(size_t offset = 0; offset < bytes; offset += BUFFER_CACHE_BLOCK_SIZE - block_offset) {
        char *key;
        size_t key_len;

        start_stats();

        err = bst->orig_ops->get_bcache_key(bst->orig_st, handle, &key,
                                            &key_len, &block_offset);
        if(err_is_fail(err)) {
            USER_PANIC_ERR(err, "get_bcache_key");
        }

        size_t towrite = BUFFER_CACHE_BLOCK_SIZE - block_offset < restbytes ?
            BUFFER_CACHE_BLOCK_SIZE - block_offset : restbytes;

        // Check if in cache -- start cache transaction
        void *blockptr;
        uint64_t transid = 0, block_length = 0;

        if(!cache_op_start(key, key_len, &blockptr, &transid, &block_length) &&
           towrite < BUFFER_CACHE_BLOCK_SIZE && towrite < restbytes) {
            // Cache doesn't have it and we're not writing an entire block
            // Try to read the block first
            err = bst->orig_ops->read_block(bst->orig_st, handle, blockptr,
                                            (size_t *)&block_length);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "orig_ops->read_block");
            }
        }

        // Write into cache
        memcpy(blockptr + block_offset, buffer + offset, towrite);

        uint64_t new_block_length = block_offset + towrite < block_length ?
            block_length : block_offset + towrite;

        cache_op_stop(blockptr, transid, new_block_length);
        free(key);
        restbytes -= towrite;

        // XXX: Not sure if using seek() is always safe, also it's inefficient
        err = bst->orig_ops->seek(bst->orig_st, handle, VFS_SEEK_CUR,
                                  towrite);
        assert(err_is_ok(err));

        end_stats(cacheWrite, false);
    }

#ifndef WITH_WRITE_BACK_CACHE
    // Seek back and write through to file
    err = bst->orig_ops->seek(bst->orig_st, handle, VFS_SEEK_SET,
                              origpos);
    assert(err_is_ok(err));

    err = bst->orig_ops->write(bst->orig_st, handle, buffer,
                               bytes, bytes_written);
    if(err_is_fail(err) && err_no(err) != NFS_ERR_STALE) {
        USER_PANIC_ERR(err, "orig_ops->write");
    }
#else
    *bytes_written = bytes;
#endif

    if(err_is_ok(err)) {
        assert(bytes == *bytes_written);
    }

#ifdef CACHE_META_DATA
    // Remember new filesize if cached
    struct nfs_handle *nh = handle;
    if(nh->filesize_cached) {
        size_t newsize = origpos + *bytes_written;
        if(nh->cached_filesize < newsize) {
            nh->cached_filesize = newsize;
        }
    }
#endif

    return err;
}

static errval_t truncate(void *st, vfs_handle_t handle, size_t bytes)
{
    start_stats();

#ifdef CACHE_META_DATA
    struct nfs_handle *nh = handle;
    if(nh->filesize_cached) {
        nh->cached_filesize = bytes;
    }
#endif

    // Hand through...
    struct bcache_state *bst = st;
    errval_t ret = bst->orig_ops->truncate(bst->orig_st, handle, bytes);

    end_stats(cacheTruncate, false);
    return ret;
}

static errval_t tell(void *st, vfs_handle_t handle, size_t *pos)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->tell(bst->orig_st, handle, pos);
    end_stats(cacheTell, false);
    return err;
}

static errval_t stat(void *st, vfs_handle_t inhandle, struct vfs_fileinfo *info)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t ret = bst->orig_ops->stat(bst->orig_st, inhandle, info);
    end_stats(cacheStat, false);
    return ret;
}

static errval_t seek(void *st, vfs_handle_t handle, enum vfs_seekpos whence,
                     off_t offset)
{
    start_stats();
    struct bcache_state *bst = st;
    errval_t err;

#ifdef CACHE_META_DATA
    if(whence == VFS_SEEK_END) {
        struct nfs_handle *nh = handle;

        if(!nh->filesize_cached) {
            struct vfs_fileinfo info;
            err = bst->orig_ops->stat(bst->orig_st, handle, &info);
            if(err_is_fail(err)) {
                USER_PANIC_ERR(err, "stat");
            }

            nh->filesize_cached = true;
            nh->cached_filesize = info.size;
            assert((off_t)nh->cached_filesize >= 0);
        }

        // Fake via a setting seek
        if((off_t)nh->cached_filesize < 0) {
            printf("filesize = %lu\n", nh->cached_filesize);
        }
        assert((off_t)nh->cached_filesize + offset >= 0);
        offset = nh->cached_filesize + offset;
    }
#endif

    // Hand through...
    err = bst->orig_ops->seek(bst->orig_st, handle, whence, offset);
    end_stats(cacheSeek, false);
    return err;
}

static errval_t close(void *st, vfs_handle_t inhandle)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->close(bst->orig_st, inhandle);
    end_stats(cacheClose, false);
    return err;
}

static errval_t opendir(void *st, const char *path, vfs_handle_t *rethandle)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->opendir(bst->orig_st, path, rethandle);
    end_stats(cacheOpendir, false);
    return err;
}

static errval_t dir_read_next(void *st, vfs_handle_t inhandle, char **retname,
                              struct vfs_fileinfo *info)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->dir_read_next(bst->orig_st, inhandle, retname, info);
    end_stats(cacheDirNext, false);
    return err;
}

static errval_t closedir(void *st, vfs_handle_t dhandle)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->closedir(bst->orig_st, dhandle);
    end_stats(cacheClosedir, false);
    return err;
}

static errval_t mkdir(void *st, const char *path)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->mkdir(bst->orig_st, path);
    end_stats(cacheMkdir, false);
    return err;
}

static errval_t rmdir(void *st, const char *path)
{
    start_stats();
    // Hand through...
    struct bcache_state *bst = st;
    errval_t err = bst->orig_ops->rmdir(bst->orig_st, path);
    end_stats(cacheRmdir, false);
    return err;
}

static struct vfs_ops bcache_ops = {
    .open = open,
    .create = create,
    .remove = cache_remove,
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

static void bind_cb(void *st, errval_t err, struct bcache_binding *b)
{
    struct bcache_client *cl = st;

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    err = bcache_rpc_client_init(&cl->rpc, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "RPC init failed");
    }

    cl->bound = true;
}

/**
 * \brief Initialize buffer cache.
 */
static errval_t buffer_cache_connect(const char *bcache_name)
{
    errval_t err;
    iref_t iref;

    if(cache[0] != NULL) {
        // XXX: Support max one connection at the moment
        return SYS_ERR_OK;
    }

    if(num_caches >= MAX_CACHES) {
        return VFS_ERR_BCACHE_LIMIT;
    }

    err = nameservice_blocking_lookup(bcache_name, &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_blocking_lookup for '%s'", bcache_name);
        return err;
    }

    struct bcache_client *client = malloc(sizeof(struct bcache_client));
    assert(client != NULL);

    client->bound = false;

    err = bcache_bind(iref, bind_cb, client, get_default_waitset(),
                      IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "bind failed");
        free(client);
        return err; // FIXME
    }

    // XXX: block for bind completion (broken API!)
    while (!client->bound) {
        messages_wait_and_handle_next();
    }

    // Receive bulk transport cap from bcached
    err = client->rpc.vtbl.new_client(&client->rpc, &client->cache_memory);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "new_client");
    }

    // Map the cache's memory
    struct frame_identity fid;
    err = invoke_frame_identify(client->cache_memory, &fid);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "invoke_frame_identify");
    }

    void *pool;
    err = vspace_map_one_frame(&pool, 1 << fid.bits, client->cache_memory,
                               NULL, NULL);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_map_one_frame");
    }

    // Initialize bulk transfer slave
    err = bulk_slave_init(pool, 1 << fid.bits, &client->bulk_slave);
    if(err_is_fail(err)) {
        USER_PANIC_ERR(err, "bulk_slave_init");
    }

    cache[num_caches] = client;
    num_caches++;

    return SYS_ERR_OK;
}

/**
 * \brief Start caching for an existing filesystem
 *
 * \param st    In: State pointer for filesystem, Out: Cached version of filesystem
 * \param ops   In: Operations for filesystem, Out: Cached version of operations
 */
errval_t buffer_cache_enable(void **st, struct vfs_ops **ops)
{
    errval_t err;

    char namebuf[32];
    memset(&Stats, 0, sizeof(Stats));
    err = sys_debug_get_tsc_per_ms(&tscperms);
    if (!err_is_ok(err)) {
       USER_PANIC_ERR("sys_debug_get_tsc_per_ms had problems\n");
    }
#ifndef WITH_SHARED_CACHE
    int name = disp_get_core_id();
#else
    int name = 0;
#endif
    size_t len = snprintf(namebuf, sizeof(namebuf), "%s.%d", BCACHE_NAME,
                          name);
    assert(len < sizeof(namebuf));
    namebuf[sizeof(namebuf) - 1] = '\0';

    printf("VFS_CACHE: %s:%s(): core:%d: Connecting to %s\n",
            __FILE__, __FUNCTION__, disp_get_core_id(), namebuf);
    err = buffer_cache_connect(namebuf);
    if(err_is_fail(err)) {
        return err;
    }

#ifdef CACHE_META_DATA
    meta_data_init();
#endif

    struct bcache_state *bst = malloc(sizeof(struct bcache_state));
    memset(bst, 0, sizeof(struct bcache_state));
    bst->orig_st = *st;
    bst->orig_ops = *ops;

    *st = bst;
    *ops = &bcache_ops;
    return SYS_ERR_OK;
}

#endif
