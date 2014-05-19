/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <string.h>
#include <barrelfish/types.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <vfs/vfs_path.h>
#include <errors/errno.h>
#include <dev/fat_bpb_dev.h>
#include <dev/fat16_ebpb_dev.h>
#include <dev/fat32_ebpb_dev.h>
#include <dev/fat_direntry_dev.h>
#include <if/ata_rw28_defs.h>
#include <if/ata_rw28_ahci_defs.h>
#include <if/ata_rw28_rpcclient_defs.h>
#include <ahci/ahci.h>
#include "vfs_fat_conv.h"
#include "vfs_backends.h"
#include "vfs_ops.h"
#include "vfs_cache.h"

//#define FAT_DEBUG 1
#if defined(FAT_DEBUG) || defined(VFS_DEBUG) || defined(GLOBAL_DEBUG)
#   define FAT_DEBUG_ENABLED

#   ifdef FAT_DEBUG
#       undef FAT_DEBUG
#   endif

#   define FAT_DEBUG_P() printf("fat:%s: ", __func__)
#   define FAT_DEBUG(s) \
        printf("fat:%s: " s "\n", __func__)
#   define FAT_DEBUG_F(s, x...) \
        printf("fat:%s: " s "\n", __func__, x)
#   define TRACE_ENTER \
        printf("fat: entering %s\n", __func__)
#   define TRACE_ENTER_F(s, x...) \
        printf("fat: entering %s: " s "\n", __func__, x)

#else

#   ifdef FAT_DEBUG_ENABLED
#       undef FAT_DEBUG_ENABLED
#   endif

#   define FAT_DEBUG_P() ((void)0)
#   define FAT_DEBUG(s) ((void)0)
#   define FAT_DEBUG_F(s, x...) ((void)0)
#   define TRACE_ENTER ((void)0)
#   define TRACE_ENTER_F(s, x...) ((void)0)

#endif

#define DUMP_DEV(dev_t, dev_p, buf_s) do { \
    char dump_dev_buf__[(buf_s)]; \
    dev_t ## _pr(dump_dev_buf__, (buf_s)-1, (dev_p)); \
    dump_dev_buf__[(buf_s)-1] = 0; \
    printf("%s\n", dump_dev_buf__); \
} while(0)

#ifndef CEIL_DIV
#define CEIL_DIV(x, d) (((x) + ((d)-1)) / (d))
#endif

#define fat_direntry_size 32

#define cluster_for_offset(offset, mount) \
    ((offset) / ((mount)->block_size * (mount)->cluster_size))

#define offset_from_cluster(offset, mount) \
    ((offset) % ((mount)->block_size * (mount)->cluster_size))

#define cluster_to_block(cluster, mount) \
    ((mount)->clusters_start + (((cluster) - 2) * (mount)->cluster_size))

#define cluster_entry_size(mount) \
    ((mount)->fat_type == FAT_TYPE_FAT16 ? sizeof(uint16_t) : sizeof(uint32_t))

#define fat_block_for_cluster(cluster, mount) \
    ((cluster) * cluster_entry_size(mount) / (mount)->block_size)

#define fat_offset_for_cluster(cluster, mount) \
    ((cluster) * cluster_entry_size(mount) % (mount)->block_size)

// NOTE: specification says max 255 chars, but format in principle seems to
// allow 260, so be on the safe side
#define LFN_CHAR_COUNT 260
#define LFN_MAX_LENGTH 255
#define DOSFN_MAX_LEN_UTF8 40

#define ASCII_MAX 127

struct fat_dirsearch {
    size_t index;
    fat_direntry_t *parent_direntry;
    uint8_t *data;
    size_t data_index;
    size_t data_key;
};

struct fat_handle_common {
    struct vfs_handle common;
    uint8_t dirent_data[fat_direntry_size];
    fat_direntry_t dirent;
};

struct fat_handle {
    struct fat_handle_common h;
    size_t offset;
};

struct fat_dirhandle {
    struct fat_handle_common h;
    struct fat_dirsearch search;
};

enum {
    FAT_TYPE_FAT16 = 16,
    FAT_TYPE_FAT32 = 32,
};

struct fat_mount {
    struct ata_rw28_binding *ata_rw28_binding;
    struct ata_rw28_rpc_client ata_rw28_rpc;
    struct ahci_binding *ahci_binding;
    errval_t bind_err;

    int fat_type;
    size_t startblock;
    uint8_t bootsec_data[512];
    fat_bpb_t bpb;
    union {
        fat16_ebpb_t f16;
        fat32_ebpb_t f32;
    } ebpb;

    struct fs_cache *block_cache;
    struct fs_cache *cluster_cache;

    size_t block_count;
    size_t block_size; // bytes
    size_t cluster_size; // blocks
    size_t fat_start; // blocks
    size_t fat_size; // blocks
    size_t rootdir_start; // blocks. 0 implies rootdir in clusters
    size_t rootdir_cluster;
    size_t clusters_start; // blocks
    uint32_t last_cluster_start; // cluster
};

static errval_t
acquire_or_read(struct fat_mount *mount, struct fs_cache *cache,
        uint32_t idx, uint8_t **data, size_t block, size_t size)
{
    TRACE_ENTER_F("idx=%"PRIu32", block=%zu, size=%zu", idx, block, size);

    uint8_t *data_;
    errval_t err;

    err = fs_cache_acquire(cache, idx, (void**)&data_);
    if (err_is_fail(err) && err != FS_CACHE_NOTPRESENT) {
        return err;
    }
    else if (err == FS_CACHE_NOTPRESENT) {
        size_t read_size;
        err = mount->ata_rw28_rpc.vtbl.read_dma(&mount->ata_rw28_rpc,
                size, block, &data_, &read_size);
        if (err_is_fail(err)) {
            return err;
        }
        assert(size == read_size);

        err = fs_cache_put(cache, idx, data_);
        if (err_is_fail(err)) {
            return err;
        }
    }

    *data = data_;
    return SYS_ERR_OK;
}

static errval_t
acquire_block(struct fat_mount *mount, size_t block, uint8_t **data)
{
    return acquire_or_read(mount, mount->block_cache,
            block, data, block, mount->block_size);
}

static errval_t
release_block(struct fat_mount *mount, size_t block)
{
    return fs_cache_release(mount->block_cache, block);
}

static errval_t
acquire_cluster(struct fat_mount *mount, uint32_t cluster, uint8_t **data)
{
    return acquire_or_read(mount, mount->cluster_cache, cluster, data,
            cluster_to_block(cluster, mount),
            mount->block_size * mount->cluster_size);
}

static errval_t
release_cluster(struct fat_mount *mount, uint32_t cluster)
{
    return fs_cache_release(mount->cluster_cache, cluster);
}

static void
dirsearch_initialize(struct fat_dirsearch *search, fat_direntry_t *parent)
{
    memset(search, 0, sizeof(*search));
    search->parent_direntry = parent;
}

static void
dirsearch_wipe(struct fat_dirsearch *search, struct fat_mount *mount)
{
    errval_t err;
    if (search->data) {
        if (!search->parent_direntry && mount->fat_type == FAT_TYPE_FAT16) {
            // the special rootdir area of FAT16 is handled with the block cache
            err = release_block(mount, search->data_key);
        }
        else {
            err = release_cluster(mount, search->data_key);
        }
        if (err_is_fail(err)) {
            // this should not happen
            USER_PANIC_ERR(err, "could not release search data cache");
        }
    }
    memset(search, 0, sizeof(*search));
}

static uint32_t
next_cluster(struct fat_mount *mount, uint32_t cluster, uint32_t *rescluster)
{
    TRACE_ENTER;
    errval_t err;

    // calculate block and offset
    size_t cluster_fat_block = fat_block_for_cluster(cluster, mount)+mount->fat_start;
    size_t cluster_fat_offset = fat_offset_for_cluster(cluster, mount);
    FAT_DEBUG_F("cluster %"PRIu32" is at fat block %zu + %zu",
            cluster, cluster_fat_block, cluster_fat_offset);

    // fetch block data
    uint8_t *data;
    err = acquire_block(mount, cluster_fat_block, &data);
    if (err_is_fail(err)) {
        return err;
    }

    // lookup cluster in found block
    uint32_t result;
    if (mount->fat_type == FAT_TYPE_FAT16) {
        result = *(uint16_t*)(data+cluster_fat_offset);
    }
    else {
        result = *(uint32_t*)(data+cluster_fat_offset);
    }

    // release cache ref
    err = release_block(mount, cluster_fat_block);
    if (err_is_fail(err)) {
        return err;
    }

    FAT_DEBUG_F("next cluster is %"PRIu32, result);
    *rescluster = result;
    return SYS_ERR_OK;
}

static void
update_lfn(const uint8_t *entry_data, fat_direntry_t *entry,
        uint16_t lfn_data[LFN_CHAR_COUNT])
{
    TRACE_ENTER;
    uint8_t seq_nr = entry_data[0];
    FAT_DEBUG_F("updating lfn data from entry with seq_nr 0x%x", (int)seq_nr);
    if (seq_nr & 0x40) {
        // first entry, reset lfn_data
        FAT_DEBUG("first entry, resetting lfn_data");
        memset(lfn_data, 0, LFN_CHAR_COUNT*sizeof(*lfn_data));
    }
    /*
    if (seq_nr & 0x80) {
        // entry is deleted
        printf("entry is deleted");
        return;
    }
    */

    // remove flag bits from sequence number, make 0-based
    seq_nr = (seq_nr & 0x1f) - 1;
    static const size_t chars_per_lfn_entry = 13;

    // chars 0-4 are 16-bit words from offset 0x1
    for (size_t i = 0; i < 5; i++) {
        uint16_t c = *(uint16_t*)(entry_data + 0x1 + i*2);
        lfn_data[seq_nr*chars_per_lfn_entry+0+i] = c;
    }
    // chars 5-10 are 16-bit words from offset 0xe
    for (size_t i = 0; i < 6; i++) {
        uint16_t c = *(uint16_t*)(entry_data + 0xe + i*2);
        lfn_data[seq_nr*chars_per_lfn_entry+5+i] = c;
    }
    // chars 11-12 are 16-bit words from offset 0x1c
    for (size_t i = 0; i < 2; i++) {
        uint16_t c = *(uint16_t*)(entry_data + 0x1c + i*2);
        lfn_data[seq_nr*chars_per_lfn_entry+11+i] = c;
    }
}

static errval_t
next_f16_rootdir_block(struct fat_mount *mount, struct fat_dirsearch *search,
        uint8_t **data)
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;
    assert(!search->parent_direntry);
    assert(mount->fat_type == FAT_TYPE_FAT16);

    // check that index is within number of valid root entries
    size_t search_end = fat_bpb_rtc_rd(&mount->bpb);
    if (search->index >= search_end) {
        return FS_ERR_INDEX_BOUNDS;
    }

    // calculate block index
    size_t bytes_offset = search->index * fat_direntry_size;
    size_t block_index = bytes_offset / mount->block_size;
    size_t block_offset = bytes_offset % mount->block_size;
    FAT_DEBUG_F("search index %zu at block %zu + %zu",
            search->index, block_index, block_offset);

    if (!search->data || search->data_index != block_index) {
        FAT_DEBUG("need new block data");

        // determine block
        size_t block = mount->rootdir_start + block_index;
        FAT_DEBUG_F("file block %zu is block %zu", block_index, block);

        // fetch new block data
        uint8_t *new_data;
        err = acquire_block(mount, block, &new_data);
        if (err_is_fail(err)) {
            return err;
        }

        // free old block data
        if (search->data) {
            err = release_block(mount, search->data_key);
            if (err_is_fail(err)) {
                release_block(mount, block);
                return err;
            }
        }

        search->data = new_data;
        search->data_index = block_index;
        search->data_key = block;
    }

    *data = search->data + block_offset;
    return SYS_ERR_OK;
}

static errval_t
next_subdir_block(struct fat_mount *mount, struct fat_dirsearch *search,
        uint8_t **data)
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;
    assert(search->parent_direntry || mount->fat_type == FAT_TYPE_FAT32);

    // calculate block index
    size_t bytes_offset = search->index * fat_direntry_size;
    size_t block_index = bytes_offset / mount->block_size;
    size_t cluster_index = block_index / mount->cluster_size;
    size_t cluster_offset = bytes_offset %
        (mount->block_size * mount->cluster_size);
    FAT_DEBUG_F("search index %zu at cluster %zu + %zu",
            search->index, cluster_index, cluster_offset);

    if (!search->data || search->data_index != cluster_index) {
        FAT_DEBUG("need new block data");

        // get start cluster of direntry data
        uint32_t cluster = 0;
        if (search->parent_direntry) {
            if (mount->fat_type == FAT_TYPE_FAT32) {
                cluster = (uint32_t)fat_direntry_starth_rd(search->parent_direntry) << 16;
            }
            cluster += fat_direntry_start_rd(search->parent_direntry);
        }
        else {
            cluster = mount->rootdir_cluster;
        }

        // determine cluster corresponding to cluster_index
        for (size_t clsidx = cluster_index;
            cluster < mount->last_cluster_start && clsidx > 0;
            clsidx--)
        {
            err = next_cluster(mount, cluster, &cluster);
            if (err_is_fail(err)) {
                return err;
            }
        }
        FAT_DEBUG_F("dir cluster %zu is cluster %"PRIu32, cluster_index, cluster);
        if (cluster >= mount->last_cluster_start) {
            return FS_ERR_INDEX_BOUNDS;
        }

        // read new cluster data
        uint8_t *new_data;
        err = acquire_cluster(mount, cluster, &new_data);
        if (err_is_fail(err)) {
            return err;
        }

        // free old data
        if (search->data) {
            err = release_cluster(mount, search->data_key);
            if (err_is_fail(err)) {
                USER_PANIC_ERR(err, "could not release dir data cache");
            }
        }

        search->data = new_data;
        search->data_index = cluster_index;
        search->data_key = cluster;
    }

    *data = search->data + cluster_offset;
    return SYS_ERR_OK;
}

static errval_t
read_next_direntry(struct fat_mount *mount, struct fat_dirsearch *search,
        char dosfn[12], uint16_t lfn_data[LFN_CHAR_COUNT], bool *has_lfn,
        uint8_t direntry_data[fat_direntry_size])
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;
    bool has_lfn_ = false;

    for ( ; ; search->index++) {
        FAT_DEBUG_F("checking search index %zu", search->index);

        uint8_t *entry_data = NULL;
        bool special_rootdir = !search->parent_direntry &&
            mount->fat_type == FAT_TYPE_FAT16;
        if (special_rootdir) {
            err = next_f16_rootdir_block(mount, search, &entry_data);
        }
        else {
            err = next_subdir_block(mount, search, &entry_data);
        }
        if (err_is_fail(err)) {
            return err;
        }

        fat_direntry_t entry;
        fat_direntry_initialize(&entry, (char*)entry_data);

        // check for various kinds of non-regular entries
        if (fat_direntry_fn_rd(&entry, 0) == 0xe5) {
            // 0xe5 implies deleted, skip
            FAT_DEBUG("found 0xe5 direntry");
            has_lfn_ = false;
            continue;
        }
        else if (fat_direntry_attr_rd(&entry) == 0xf) {
            // (attr == ro | hidden | system | volume) implies long filename
            FAT_DEBUG("found lfn direntry");
            if (entry_data[0] & 0x40 && !(entry_data[0] & 0x80)) {
                // first lfn entry and not deleted implies next real entry has a lfn
                has_lfn_ = true;
            }
            update_lfn(entry_data, &entry, lfn_data);
            continue;
        }
        else if (!entry_data[0]) {
            // entries with names starting at 0 have never been allocated, so
            // later entries are unused too
            FAT_DEBUG("found clean direntry");
            return FS_ERR_INDEX_BOUNDS;
        }
        else if (fat_direntry_start_rd(&entry) == 0 &&
                !fat_direntry_attr_dir_rdf(&entry)) {
            // entries with starting cluster 0 should only occur in directory
            // and lfn entries.  lfn entries are already handled, so something
            // weird is going on if we get here.
            FAT_DEBUG("found non-lfn/dir direntry with start cluster 0");
            continue;
        }

#ifdef FAT_DEBUG_ENABLED
        FAT_DEBUG_P();
        // dump found names
        printf("8.3 entry: \"");
        for (uint8_t *p = entry_data; p < entry_data+11; p++) {
            putchar(*p);
        }
        printf("\", lfn entry: ");
        if (has_lfn_) {
            putchar('"');
            for (uint16_t *p = lfn_data; *p && p < lfn_data+LFN_CHAR_COUNT; p++) {
                putchar(*p > ASCII_MAX ? '?' : (char)*p);
            }
            putchar('"');
        }
        else {
            printf("(none)");
        }
        putchar('\n');
#endif

        // at this point, we've found a real entry
        memcpy(dosfn, entry_data, 11);
        dosfn[11] = 0;
        *has_lfn = has_lfn_;
        memcpy(direntry_data, entry_data, fat_direntry_size);
        search->index++;
        return SYS_ERR_OK;
    }
    return FS_ERR_INDEX_BOUNDS;
}

static errval_t
find_path(struct fat_mount *mount, const char *path,
        uint8_t direntry[fat_direntry_size])
{
    TRACE_ENTER_F("\"%s\"", path);
    errval_t err = SYS_ERR_OK;
    const char *part_begin = path, *part_end;

    fat_direntry_t parent_entry;
    fat_direntry_t *parent = NULL;

    do {
        if (*part_begin == '/') {
            part_begin++;
        }
        for (part_end = part_begin+1; *part_end && *part_end != '/'; part_end++) ;

        size_t part_len = part_end - part_begin;
#ifdef FAT_DEBUG_ENABLED
        {
            char part[part_len+1];
            strncpy(part, part_begin, part_len);
            part[part_len]=0;
            FAT_DEBUG_F("part \"%s\"", part);
        }
#endif

        struct fat_dirsearch search;
        dirsearch_initialize(&search, parent);

        char dosfn[12];
        uint16_t lfn_data[LFN_CHAR_COUNT];
        bool has_lfn;
        char buf[LFN_CHAR_COUNT];

        do {

            err = read_next_direntry(mount, &search, dosfn, lfn_data,
                    &has_lfn, direntry);
            if (err_is_fail(err)) {
                dirsearch_wipe(&search, mount);
                if (err == FS_ERR_INDEX_BOUNDS) {
                    err = FS_ERR_NOTFOUND;
                }
                return err;
            }

            if (has_lfn) {
                size_t len;
                for (len = 0; len < LFN_CHAR_COUNT; len++) {
                    if (!lfn_data[len] || lfn_data[len] > ASCII_MAX) {
                        break;
                    }
                }
                if (lfn_data[len] > ASCII_MAX) {
                    continue;
                }
                for (size_t i = 0; i < len; ++i) {
                    buf[i] = (char)lfn_data[i];
                }
                buf[len] = 0;
            }
            else {
                if (dos2unixfn((const unsigned char*)dosfn, (unsigned char*)buf, LFN_CHAR_COUNT)) {
                    // TODO: handle error
                }
            }

            FAT_DEBUG_F("comparing part to %s", buf);
            bool match = strncmp(part_begin, buf, part_len) == 0 && buf[part_len] == 0;
            if (match) {
                break;
            }

        } while (1);

        parent = &parent_entry;
        fat_direntry_initialize(parent, (char*)direntry);

        dirsearch_wipe(&search, mount);
        part_begin = part_end;
    } while(*part_begin);

    return SYS_ERR_OK;
}

static errval_t
openhandle(struct fat_mount *mount, const char *path, struct fat_handle_common *handle)
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;

    err = find_path(mount, path, handle->dirent_data);
    if (err_is_fail(err)) {
        return err;
    }

    fat_direntry_initialize(&handle->dirent, (char*)handle->dirent_data);

#ifdef FAT_DEBUG_ENABLED
    /*DUMP_DEV(fat_direntry, &handle->dirent, 4096);*/
#endif

    return SYS_ERR_OK;
}

static errval_t
open(void *st, const char *path, vfs_handle_t *fhandle)
{
    TRACE_ENTER_F("\"%s\"", path);
    errval_t err = SYS_ERR_OK;
    struct fat_handle *handle = NULL;
    struct fat_mount *mount = st;

    handle = calloc(1, sizeof(*handle));
    if (!handle) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    err = openhandle(mount, path, &handle->h);
    if (err_is_fail(err)) {
        goto error;
    }

    if (fat_direntry_attr_dir_rdf(&handle->h.dirent)) {
        err = FS_ERR_NOTFILE;
        goto error;
    }

    *fhandle = handle;
    goto end;

error:
    free(handle);

end:
    return err;
}

static errval_t
create(void *st, const char *path, vfs_handle_t *handle)
{
    TRACE_ENTER;
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
fat_remove(void *st, const char *path)
{
    TRACE_ENTER;
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
read(void *st, vfs_handle_t fhandle, void *buffer, size_t bytes, size_t *bytes_read)
{
    TRACE_ENTER;
    errval_t err;
    struct fat_handle *handle = fhandle;
    struct fat_mount *mount = st;
    size_t file_size = fat_direntry_size_rd(&handle->h.dirent);
    size_t offset = handle->offset;

    assert(bytes_read);
    *bytes_read = 0;

    // limited requested bytes to remaining file size
    size_t file_remainder = file_size - handle->offset;
    if (bytes > file_remainder) {
        bytes = file_remainder;
    }
    if (bytes == 0) {
        return SYS_ERR_OK;
    }
    FAT_DEBUG_F("reading %zu bytes", bytes);

    fat_direntry_t *dirent = &handle->h.dirent;
    int isdir = fat_direntry_attr_dir_rdf(dirent);
    if (isdir) {
        return FS_ERR_NOTFILE;
    }

    size_t remaining = bytes;
    do {
        // split read offset into cluster index and offset within cluster
        size_t cluster_index = cluster_for_offset(offset, mount);
        size_t cluster_offset = offset_from_cluster(offset, mount);
        FAT_DEBUG_F("reading from file cluster %zu + %zu",
                cluster_index, cluster_offset);

        // determin read size, only read single cluster and not beyond end of file
        size_t read_size = remaining;
        size_t cluster_remainder = mount->cluster_size * mount->block_size - cluster_offset;
        if (cluster_remainder < read_size) {
            read_size = cluster_remainder;
        }
        file_remainder = fat_direntry_size_rd(dirent) - offset;
        if (file_remainder < read_size) {
            read_size = file_remainder;
        }
        FAT_DEBUG_F("reading %zu from cluster (clus_rem=%zu, f_rem=%zu)",
                read_size, cluster_remainder, file_remainder);

        // determine cluster corresponding to cluster_index
        uint32_t cluster = fat_direntry_start_rd(dirent);
        if (mount->fat_type == FAT_TYPE_FAT32) {
            cluster += (uint32_t)fat_direntry_starth_rd(dirent) << 16;
        }
        for (size_t clsidx = cluster_index; clsidx > 0; clsidx--) {
            err = next_cluster(mount, cluster, &cluster);
            if (err_is_fail(err)) {
                return err;
            }
        }
        FAT_DEBUG_F("file cluster %zu is cluster %"PRIu32, cluster_index, cluster);
        assert(cluster < mount->last_cluster_start);

        // fetch data and copy into buffer
        uint8_t *data;
        err = acquire_cluster(mount, cluster, &data);
        if (err_is_fail(err)) {
            return err;
        }
        memcpy(buffer, data+cluster_offset, read_size);
        err = release_cluster(mount, cluster);
        if (err_is_fail(err)) {
            // should not happen
            USER_PANIC_ERR(err, "could not release file cluster cache");
        }

        // update variables for successful read
        buffer = (char *)buffer + read_size;
        remaining -= read_size;
        *bytes_read += read_size;
        offset += read_size;

        FAT_DEBUG_F("read of cluster %"PRIu32" completed", cluster);
    } while (remaining);

    // read completed, update handle's offset
    FAT_DEBUG_F("read of %zu bytes completed", bytes);
    handle->offset = offset;
    return SYS_ERR_OK;
}

static errval_t
write(void *st, vfs_handle_t handle, const void *buffer, size_t bytes,
        size_t *bytes_written)
{
    TRACE_ENTER;
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
truncate(void *st, vfs_handle_t handle, size_t bytes)
{
    TRACE_ENTER;
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
seek(void *st, vfs_handle_t fhandle, enum vfs_seekpos whence, off_t offset)
{
    TRACE_ENTER;

    struct fat_handle *handle = fhandle;
    size_t size = fat_direntry_size_rd(&handle->h.dirent);
    size_t base = 0;

    switch (whence) {
    case VFS_SEEK_SET: base = 0; break;
    case VFS_SEEK_CUR: base = handle->offset; break;
    case VFS_SEEK_END: base = size; break;
    default: USER_PANIC("invalid whence argument to fat seek"); break;
    }

    if (offset < 0 && (-offset) > base) {
        handle->offset = 0;
    }
    else if (base + offset > size) {
        handle->offset = size;
    }
    else {
        handle->offset = base + offset;
    }

    return SYS_ERR_OK;
}

static errval_t
tell(void *st, vfs_handle_t fhandle, size_t *pos)
{
    TRACE_ENTER;

    struct fat_handle *handle = fhandle;
    *pos = handle->offset;

    return SYS_ERR_OK;
}

static errval_t
stat(void *st, vfs_handle_t handle, struct vfs_fileinfo *info)
{
    TRACE_ENTER;

    struct fat_handle_common *fhc = handle;

    if (fat_direntry_attr_dir_rdf(&fhc->dirent)) {
        info->type = VFS_DIRECTORY;
        info->size = 0; // TODO: something more useful?
    }
    else {
        info->type = VFS_FILE;
        info->size = fat_direntry_size_rd(&fhc->dirent);
    }

    return SYS_ERR_OK;;
}

static errval_t
close(void *st, vfs_handle_t fhandle)
{
    TRACE_ENTER;
    struct fat_handle *handle = fhandle;

    free(handle);
    return SYS_ERR_OK;
}

static errval_t
mkdir(void *st, const char *path)
{
    TRACE_ENTER_F("\"%s\"", path);
    // fail if already present
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
rmdir(void *st, const char *path)
{
    TRACE_ENTER_F("\"%s\"", path);
    // fail if not empty
    return LIB_ERR_NOT_IMPLEMENTED;
}

static errval_t
opendir(void *st, const char *path, vfs_handle_t *dhandle)
{
    TRACE_ENTER_F("\"%s\"", path);
    errval_t err = SYS_ERR_OK;
    struct fat_dirhandle *new_handle = NULL;
    struct fat_mount *mount = st;

    new_handle = calloc(1, sizeof(*new_handle));
    if (!new_handle) {
        err = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    if (!path[0] || (path[0] == '/' && path[1] == '\0')) {
        FAT_DEBUG("got opendir for root");
        *dhandle = new_handle;
        goto end;
    }

    err = openhandle(mount, path, &new_handle->h);
    if (err_is_fail(err)) {
        goto error;
    }

    if (!fat_direntry_attr_dir_rdf(&new_handle->h.dirent)) {
        err = FS_ERR_NOTDIR;
        goto error;
    }
    FAT_DEBUG_F("directory %s found", path);

    dirsearch_initialize(&new_handle->search, &new_handle->h.dirent);

    *dhandle = new_handle;
    goto end;

error:
    free(new_handle);

end:
    return err;
}

static errval_t
dir_read_next(void *st, vfs_handle_t dhandle, char **name, struct vfs_fileinfo *info)
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;

    struct fat_dirhandle *handle = dhandle;
    struct fat_mount *mount = st;

    FAT_DEBUG_F("handle->search.index = %zu", handle->search.index);

    char dosfn[12];
    uint16_t lfn_data[LFN_CHAR_COUNT];
    bool has_lfn;
    uint8_t dirent_data[fat_direntry_size];
    fat_direntry_t dirent;

    // read next direntry
    err = read_next_direntry(mount, &handle->search, dosfn, lfn_data,
            &has_lfn, dirent_data);
    if (err_is_fail(err)) {
        return err;
    }
    fat_direntry_initialize(&dirent, (char*)dirent_data);

    if (has_lfn) {
        size_t len;
        for (len = 0; len < LFN_CHAR_COUNT; len++) {
            if (!lfn_data[len]) {
                break;
            }
        }
        char *buf = malloc(len+1);
        if (!buf) {
            return LIB_ERR_MALLOC_FAIL;
        }
        for (size_t i = 0; i < len; ++i) {
            if (lfn_data[i] > ASCII_MAX) {
                buf[i] = '?';
            }
            else {
                buf[i] = (char)lfn_data[i];
            }
        }
        buf[len] = 0;
        *name = buf;
    }
    else {
        char *buf = malloc(DOSFN_MAX_LEN_UTF8);
        dos2unixfn((const unsigned char*)dosfn, (unsigned char*)buf, DOSFN_MAX_LEN_UTF8);
        *name = buf;
    }

#ifdef FAT_DEBUG_ENABLED
    /*DUMP_DEV(fat_direntry, &dirent, 4096);*/
#endif

    bool isdir = fat_direntry_attr_dir_rdf(&dirent);
    info->type = isdir ? VFS_DIRECTORY : VFS_FILE;
    info->size = fat_direntry_size_rd(&dirent);
    return SYS_ERR_OK;
}

static errval_t
closedir(void *st, vfs_handle_t dhandle)
{
    TRACE_ENTER;
    errval_t err = SYS_ERR_OK;
    struct fat_mount *mount = st;
    struct fat_dirhandle *handle = dhandle;

    dirsearch_wipe(&handle->search, mount);
    free(handle);

    return err;
}

struct vfs_ops fat_ops = {
    .open = open,
    .create = create,
    .remove = fat_remove,
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

#if defined(__x86_64__) || defined(__i386__)
static void
ahci_init_cb(void *st, errval_t err, struct ahci_binding *b)
{
    TRACE_ENTER;
    struct fat_mount *mount = st;
    
    if (err_is_fail(err)) {
        mount->bind_err = err;
        return;
    }

    mount->ahci_binding = b;
}

static void
ahci_close_cb(void *arg)
{
    *(bool *)arg = true;
}

#elif defined(__pandaboard__)

static void 
bind_cb(void *st, errval_t err, struct ata_rw28_binding *b)
{
    printf("%s:%d\n", __FUNCTION__, __LINE__);

    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    struct fat_mount *mount = (struct fat_mount*) st;
    
    err = ata_rw28_rpc_client_init(&mount->ata_rw28_rpc, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "RPC initialization failed");
    }

    mount->ata_rw28_binding = b;
}
#endif

errval_t
vfs_fat_mount(const char *uri, void **retst, struct vfs_ops **retops)
{
    TRACE_ENTER;
    errval_t err;
    // format: scheme://port[+offset]

    int type;
    if (strncmp(uri, "fat16://", 8) == 0) {
        type = FAT_TYPE_FAT16;
    }
    else if (strncmp(uri, "fat32://", 8) == 0) {
        type = FAT_TYPE_FAT32;
    }
    else {
        return VFS_ERR_BAD_URI;
    }
    // skip scheme
    const char *puri = uri + 8;

    // parse port
    if (*puri < '0' || *puri > '9') {
        return VFS_ERR_BAD_URI;
    }
    size_t port = 0, startblock = 0;
    while (*puri >= '0' && *puri <= '9') {
        port = port*10 + (*puri++ - '0');
    }
    if (*puri == '+') {
        // parse offset
        puri++;
        if (*puri < '0' || *puri > '9') {
            return VFS_ERR_BAD_URI;
        }
        while (*puri >= '0' && *puri <= '9') {
            startblock = startblock*10 + (*puri++ - '0');
        }
    }
    if (*puri != 0) {
        return VFS_ERR_BAD_URI;
    }

    FAT_DEBUG_F("got mount for port %zu, offset %zu", port, startblock);

    struct fat_mount *mount = calloc(1, sizeof(struct fat_mount));
    if (!mount) {
        return LIB_ERR_MALLOC_FAIL;
    }
    mount->fat_type = type;
    mount->startblock = startblock;

    // TODO(gz): We should probably decouple the FAT implementation from
    // all ATA related stuff to avoid these preprocessor hacks
#if defined(__x86_64__) || defined(__i386__)
    err = ahci_init(port, ahci_init_cb, mount, get_default_waitset());
    if (err_is_fail(err)) {
        goto ahci_init_failed;
    }

    while (!mount->ahci_binding && err_is_ok(mount->bind_err)) {
        messages_wait_and_handle_next();
    }
    if (err_is_fail(mount->bind_err)) {
        err = mount->bind_err;
        goto ahci_init_failed;
    }

    FAT_DEBUG("ahci_init completed");

    struct ahci_ata_rw28_binding *ahci_ata_rw28_binding;
    ahci_ata_rw28_binding = calloc(1, sizeof(struct ahci_ata_rw28_binding));
    err = ahci_ata_rw28_init(ahci_ata_rw28_binding,
            get_default_waitset(), mount->ahci_binding);
    if (err_is_fail(err)) {
        goto ata_rw28_init_failed;
    }
    FAT_DEBUG("ahci_ata_rw28_init completed");
    mount->ata_rw28_binding = (struct ata_rw28_binding*)ahci_ata_rw28_binding;
    err = ata_rw28_rpc_client_init(&mount->ata_rw28_rpc, mount->ata_rw28_binding);
    if (err_is_fail(err)) {
        goto ata_rw28_init_failed;
    }
    FAT_DEBUG("ata_rw28_rpc_client_init completed");
#elif defined(__pandaboard__)
    FAT_DEBUG("wait for mmchs service\n");
    iref_t iref;
    err = nameservice_blocking_lookup("mmchs", &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_blocking_lookup failed");
    }
    err = ata_rw28_bind(iref,
                     bind_cb,
                     mount,
                     get_default_waitset(),
                     IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "bind failed");
    }

    while(mount->ata_rw28_binding == NULL) {
        event_dispatch(get_default_waitset());
    }

    FAT_DEBUG("ata_rw28 initialized.\n");
#endif

    // read data from fat boot sector
    uint8_t *data;
    size_t size;
    err = mount->ata_rw28_rpc.vtbl.read_dma_block(&mount->ata_rw28_rpc,
            mount->startblock, &data, &size);
    if (err_is_fail(err)) {
        goto bootsec_read_failed;
    }
    assert(size == 512);
#ifdef FAT_DEBUG_ENABLED
    FAT_DEBUG("dumping sector 0 raw");
    for (size_t i = 0; i < size; i+=16) {
        printf("%02x %02x %02x %02x  %02x %02x %02x %02x    "
                "%02x %02x %02x %02x  %02x %02x %02x %02x\n",
                data[i+0], data[i+1], data[i+2], data[i+3],
                data[i+4], data[i+5], data[i+6], data[i+7],
                data[i+8], data[i+9], data[i+10], data[i+11],
                data[i+12], data[i+13], data[i+14], data[i+15]
                );
    }
    FAT_DEBUG("end sector 0 dump");
#endif
    memcpy(mount->bootsec_data, data, size);
    free(data);
    data = NULL;

    if (memcmp(mount->bootsec_data+0x1FE, "\x55\xAA", 2) != 0) {
        FAT_DEBUG_F("boot sector check bytes do not match, expected 0x55 0xAA,"
                " got 0x%02x 0x%02x", mount->bootsec_data[0x1FE],
                mount->bootsec_data[0x1FF]);
        goto fs_check_failed;
    }

    fat_bpb_initialize(&mount->bpb, (mackerel_addr_t)&mount->bootsec_data);
    mount->block_size = fat_bpb_bps_rd(&mount->bpb);
    mount->cluster_size = fat_bpb_spc_rd(&mount->bpb);
    mount->fat_start = mount->startblock + fat_bpb_rsvs_rd(&mount->bpb);
    size_t ssc = fat_bpb_ssc_rd(&mount->bpb), lsc = fat_bpb_lsc_rd(&mount->bpb);
    if (ssc && lsc) {
        FAT_DEBUG_F("both small and large sector count are nonzero:"
                " ssc=%zu, lsc=%zu", ssc, lsc);
        goto fs_check_failed;
    }
    else if (ssc) {
        mount->block_count = ssc;
    }
    else if (lsc) {
        mount->block_count = lsc;
    }
    else {
        FAT_DEBUG("both small and large sector count are zero");
        goto fs_check_failed;
    }
#ifdef FAT_DEBUG_ENABLED
    DUMP_DEV(fat_bpb, &mount->bpb, 4096);
#endif

    if (mount->fat_type == FAT_TYPE_FAT16) {
        fat16_ebpb_initialize(&mount->ebpb.f16, (char*)&mount->bootsec_data);
#ifdef FAT_DEBUG_ENABLED
        DUMP_DEV(fat16_ebpb, &mount->ebpb.f16, 4096);
#endif
        uint8_t signature = fat16_ebpb_ebs_rd(&mount->ebpb.f16);
        if (signature == 0x28) {
            FAT_DEBUG("FAT16 EBPB signature 0x28 indicates unsupported FAT type");
            goto fs_check_failed;
        }
        if (signature != 0x29) {
            FAT_DEBUG_F("FAT16 EBPB signature does not match, expected 0x29,"
                    " got 0x%02x", signature);
            goto fs_check_failed;
        }
        mount->fat_size = fat_bpb_spf_rd(&mount->bpb);
        mount->rootdir_start = mount->fat_start +
            fat_bpb_fatc_rd(&mount->bpb) * mount->fat_size;
        mount->clusters_start = mount->rootdir_start +
            CEIL_DIV(fat_bpb_rtc_rd(&mount->bpb) * fat_direntry_size,
                    fat_bpb_bps_rd(&mount->bpb));
        mount->last_cluster_start = 0xfff8;
    }
    else {
        fat32_ebpb_initialize(&mount->ebpb.f32, (char*)&mount->bootsec_data);
#ifdef FAT_DEBUG_ENABLED
        DUMP_DEV(fat32_ebpb, &mount->ebpb.f32, 4096);
#endif
        uint8_t signature = fat32_ebpb_ebs_rd(&mount->ebpb.f32);
        if (signature != 0x29) {
            FAT_DEBUG_F("FAT32 EBPB signature does not match, expected 0x29,"
                    " got 0x%02x", signature);
            goto fs_check_failed;
        }
        mount->fat_size = fat32_ebpb_spf_rd(&mount->ebpb.f32);
        mount->rootdir_cluster = fat32_ebpb_rtst_rd(&mount->ebpb.f32);
        mount->clusters_start = mount->fat_start +
            fat_bpb_fatc_rd(&mount->bpb) * mount->fat_size;
        mount->last_cluster_start = 0xfffffff8;

        size_t fs_info_sector = fat32_ebpb_fsis_rd(&mount->ebpb.f32);
        if (fs_info_sector <= 0 || fs_info_sector >= mount->block_count) {
            FAT_DEBUG_F("File System Information Sector out of range,"
                    " block=%zu, block_count=%zu", fs_info_sector,
                    mount->block_count);
            goto fs_check_failed;
        }
        mount->ata_rw28_rpc.vtbl.read_dma_block(&mount->ata_rw28_rpc,
                mount->startblock + fs_info_sector, &data, &size);
        if (memcmp(data+0, "RRaA", 4) != 0 ||
            memcmp(data+0x1e4, "rrAa", 4) != 0)
        {
            FAT_DEBUG_F("File System Information Sector signatures do not match,"
                    " %"PRIx32", %"PRIx32, *(uint32_t*)(data+0),
                    *(uint32_t*)(data+0x1e4));
            goto fs_check_failed;
        }
        if (memcmp(data+0x1fe, "\x55\xAA", 2) != 0) {
            FAT_DEBUG("File System Information Sector check bytes do not match");
            goto fs_check_failed;
        }
#ifdef FAT_DEBUG_ENABLED
        FAT_DEBUG("dumping FSIS");
        printf("nr of free clusters: %"PRIu32"\n", *(uint32_t*)(data+0x1e8));
        printf("most recently allocated cluster: %"PRIu32"\n",
                *(uint32_t*)(data+0x1ec));
        printf("----------------\n");
#endif
        free(data);
        data = NULL;
    }

#ifdef FAT_DEBUG_ENABLED
    FAT_DEBUG("dumping mount variables");
#define D(v) printf("%-18s %zu\n", #v ":", mount-> v )
    D(block_size);
    D(cluster_size);
    D(fat_start);
    D(fat_size);
    D(rootdir_start);
    D(rootdir_cluster);
    D(clusters_start);
#undef D
    printf("----------------\n");
#endif

    err = fs_cache_init(1<<7, 1<<8, &mount->block_cache);
    if (err_is_fail(err)) {
        goto cache_init_failed;
    }
    err = fs_cache_init(1<<7, 1<<8, &mount->cluster_cache);
    if (err_is_fail(err)) {
        goto cache_init_failed;
    }

    *retops = &fat_ops;
    *retst = mount;
    return SYS_ERR_OK;

cache_init_failed:
    if (mount->block_cache) {
        fs_cache_free(mount->block_cache);
    }
    if (mount->cluster_cache) {
        fs_cache_free(mount->cluster_cache);
    }

    goto bootsec_read_failed;

fs_check_failed:
    err = FAT_ERR_BAD_FS;

bootsec_read_failed:
    if (data) {
        free(data);
    }
    data = NULL;

#if defined(__x86_64__) || defined(__i386__)
ata_rw28_init_failed:
    free(ahci_ata_rw28_binding);
    bool closed = false;
    errval_t err2 = ahci_close(mount->ahci_binding, MKCLOSURE(ahci_close_cb, &closed));
    if (err_is_ok(err2)) {
        while (!closed) {
            event_dispatch(mount->ahci_binding->waitset);
        }
    }
    // TODO: bindings leak? how to close flounder connection?

ahci_init_failed:
    free(mount);
#endif

    return err;

}
