#ifndef VFS_NFS_H
#define VFS_NFS_H

// per-mount state
struct nfs_state {
    struct nfs_client *client;
    struct nfs_fh3 rootfh;
    mountstat3 mountstat;
};

// file handle
struct nfs_handle {
    struct vfs_handle common;
    struct nfs_state *nfs;
    bool isdir;
    struct nfs_fh3 fh;
    enum ftype3 type;
    void *st;
#ifdef ASYNC_WRITES
    int inflight;
#endif
#ifdef WITH_META_DATA_CACHE
    size_t cached_filesize;
    bool filesize_cached;
#endif
    union {
        struct {
            size_t pos;
        } file;
        struct {
            struct READDIR3res *readdir_result;
            struct entry3 *readdir_prev;
            struct entry3 *readdir_next;
        } dir;
    } u;
};

#endif
