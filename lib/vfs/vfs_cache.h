#ifndef VFS_CACHE_H
#define VFS_CACHE_H

#include <stddef.h>
#include <errors/errno.h>

struct fs_cache;

// Initialize cache.
// Errors:
//   - LIB_ERR_MALLOC_FAIL: Allocation failed / out of heap memory.
errval_t fs_cache_init(size_t max_capacity, size_t map_size, struct fs_cache **cache);

// Free cache.
void fs_cache_free(struct fs_cache *cache);

// Acquire a reference to a cache entry. The reference must be released with
// fs_cache_release.
// Errors:
//   - FS_CACHE_NOTPRESENT: There is no item with the given key in the cache.
errval_t fs_cache_acquire(struct fs_cache *cache, uint32_t key, void **item);

// Put an entry into the cache. The reference must be released with
// fs_cache_release. Calling put multiple times with the same (key, item) pair
// is not an error.
// Errors:
//   - FS_CACHE_CONFLICT: An item with same key but different data pointer is
//     already present.
//   - FS_CACHE_FULL: Cache is at max capacity and all entries are referenced.
//   - LIB_ERR_MALLOC_FAIL: Increasing capacity failed.
errval_t fs_cache_put(struct fs_cache *cache, uint32_t key, void *item);

// Release an acquired reference. Every call to fs_cache_acquire and
// fs_cache_put should have a matching fs_cache_release.
// Errors:
//   - FS_CACHE_NOTPRESENT: There is no item with the given key in the cache.
errval_t fs_cache_release(struct fs_cache *cache, uint32_t key);

#endif
