/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "vfs_cache.h"
#include <barrelfish/barrelfish.h>
#include <string.h>
#include <stdio.h>

#if defined(CACHE_DEBUG) || defined(VFS_DEBUG) || defined(GLOBAL_DEBUG)
#   define CACHE_DEBUG_ENABLED

#   ifdef CACHE_DEBUG
#       undef CACHE_DEBUG
#   endif

#   define CACHE_DEBUG_P() printf("fscache:%s: ", __func__)
#   define CACHE_DEBUG(s) \
        printf("fscache:%s: %s\n", __func__, (s))
#   define CACHE_DEBUG_F(s, x...) \
        printf("fscache:%s: " s "\n", __func__, x)
#   define TRACE_ENTER() \
        printf("fscache: entering %s\n", __func__)
#   define TRACE_ENTER_F(s, x...) \
        printf("fscache: entering %s: " s "\n", __func__, x)

#else
#   ifdef CACHE_DEBUG_ENABLED
#       undef CACHE_DEBUG_ENABLED
#   endif

#   define CACHE_DEBUG_P() ((void)0)
#   define CACHE_DEBUG(s) ((void)0)
#   define CACHE_DEBUG_F(s, x...) ((void)0)
#   define TRACE_ENTER() ((void)0)
#   define TRACE_ENTER_F(s, x...) ((void)0)

#endif

#define ERRRET(expr) do { \
    errval_t err__ = (expr); \
    if (err_is_fail(err__)) return err__; \
} while (0)
#define ERRGOTO(lbl, expr) do { \
    err = (expr); \
    if (err_is_fail(err)) goto lbl; \
} while(0)
#define ERRVGOTO(errv, lbl, expr) do { \
    (errv) = (expr); \
    if (err_is_fail(errv)) goto lbl; \
} while(0)

#define MIN_ALLOC 4

/*
 * Important: all entry indices are 1-based as 0 is used to indicate invalid.
 *
 * Cache entries form an array with various internal linked lists:
 * - For each hash, a linked list of entries with matching hashes exists.
 *   The first element is referenced by the index stored in the map.
 * - A free list is stored for free entries, the first element is referenced by
 *   the first_free index.
 * - An independant list of unused entries is stored. These are entries with
 *   valid data that are no longer referenced. The list acts as an LRU queue,
 *   with first and last element referenced by first_unused and last_unused.
 * 
 * unused_count stores the number of entries in the unused list for heuristics
 * in cache_free_unused.
 *
 */

struct cache_entry {
    size_t prev, next;
    size_t prev_unused, next_unused;
    void *item;
    uint32_t key;
    size_t references;
};

struct fs_cache {
    size_t *map;
    struct cache_entry *entries;
    size_t map_size, max_capacity, capacity;

    size_t first_free;
    size_t first_unused, last_unused, unused_count;
};

static size_t
hash_key(uint32_t key, size_t map_size)
{
    TRACE_ENTER();
    assert((map_size & ~(map_size-1)) == map_size);
    // http://burtleburtle.net/bob/hash/integer.html
    uint32_t a = key;
    a += ~(a<<15);
    a ^=  (a>>10);
    a +=  (a<<3);
    a ^=  (a>>6);
    a += ~(a<<11);
    a ^=  (a>>16);
    return a % map_size;
}

static void
free_unused(struct fs_cache *cache, size_t factor, size_t maximum)
{
    TRACE_ENTER_F("factor=%zu, max=%zu", factor, maximum);
    size_t unused_overburden = 0;

    if (factor == 0) {
        if (maximum == 0) {
            unused_overburden = cache->unused_count;
        }
        else if (maximum > 0 && cache->unused_count > maximum) {
            unused_overburden = cache->unused_count - maximum;
        }
    }
    else if (factor == 1) {
        if (maximum > 0) {
            if (cache->unused_count > maximum) {
                unused_overburden = maximum;
            }
            else {
                unused_overburden = cache->unused_count;
            }
        }
    }
    else if (cache->unused_count > cache->capacity / factor) {
        unused_overburden = cache->unused_count - cache->capacity/factor;
        if (maximum > 0 && cache->unused_count > maximum) {
            unused_overburden = maximum;
        }
    }

    for (; unused_overburden > 0; unused_overburden--) {
        assert(cache->unused_count > 0);
        assert(cache->first_unused);
        size_t entry_index = cache->first_unused;
        struct cache_entry *entry = &cache->entries[entry_index-1];
        assert(!entry->prev_unused);

        // remove from beginning of unused list
        if (entry->next_unused) {
            cache->entries[entry->next_unused-1].prev_unused = 0;
        }
        if (entry_index == cache->last_unused) {
            cache->last_unused = 0;
        }
        cache->first_unused = entry->next_unused;
        assert((cache->first_unused == 0) == (cache->last_unused == 0));
        cache->unused_count--;

        // remove from hash collision list
        if (entry->next) {
            cache->entries[entry->next-1].prev = entry->prev;
        }
        if (entry->prev) {
            cache->entries[entry->prev-1].next = entry->next;
        }
        else {
            // no prev must be first entry for hash, so must be in map
            size_t hash = hash_key(entry->key, cache->map_size);
            assert(cache->map[hash] == entry_index);
            cache->map[hash] = entry->next;
        }

        free(entry->item);
        memset(entry, 0, sizeof(*entry));

        if (cache->first_free) {
            entry->next = cache->first_free;
            cache->entries[cache->first_free-1].prev = entry_index;
        }
        cache->first_free = entry_index;
    }
}

static errval_t
cache_make_space(struct fs_cache *cache)
{
    TRACE_ENTER();

    if (cache->capacity == 0) {
        // no memory at all allocated, create initial space
        cache->entries = calloc(MIN_ALLOC, sizeof(*cache->entries));
        if (!cache->entries) {
            return LIB_ERR_MALLOC_FAIL;
        }
        cache->capacity = MIN_ALLOC;
        cache->first_free = 1;
        for (size_t i = 1; i < cache->capacity; ++i) {
            cache->entries[i-1].next = i+1;
            cache->entries[i].prev = i;
        }
        return SYS_ERR_OK;
    }

    size_t new_capacity = 2*cache->capacity;
    if (new_capacity <= cache->max_capacity) {
        // realloc with new capacity
        struct cache_entry *new_entries = NULL;
        new_entries = realloc(cache->entries,
                new_capacity*sizeof(*cache->entries));
        if (!new_entries) {
            return LIB_ERR_MALLOC_FAIL;
        }

        // realloc succeeded, initialize new entries
        cache->entries = new_entries;
        memset(cache->entries + cache->capacity, 0,
                cache->capacity*sizeof(*cache->entries));
        // NOTE: 1-based iteration, leaving out last element!
        for (size_t i = cache->capacity + 1; i < new_capacity; i++) {
            cache->entries[i-1].next = i+1;
            cache->entries[i].prev = i;
        }
        cache->first_free = cache->capacity + 1;
        cache->capacity = new_capacity;

        return SYS_ERR_OK;
    }

    // try to free a bunch of unreferenced entries
    free_unused(cache, 2, 10);
    if (cache->first_free) {
        return SYS_ERR_OK;
    }

    // try slightly more aggressive freeing of unreferenced entries
    for (size_t f = 2; f < 5; f++) {
        free_unused(cache, f, 0);
        if (cache->first_free) {
            return SYS_ERR_OK;
        }
    }

    // try freeing all but 10 unreferenced entries
    free_unused(cache, 0, 10);
    if (cache->first_free) {
        return SYS_ERR_OK;
    }

    // try freeing *all* unreferenced entries
    free_unused(cache, 0, 0);
    if (cache->first_free) {
        return SYS_ERR_OK;
    }

    return FS_CACHE_FULL;
}

static errval_t
get_new_entry(struct fs_cache *cache, size_t hash, struct cache_entry **entry)
{
    TRACE_ENTER();

    // make sure space is available
    if (!cache->first_free) {
        ERRRET(cache_make_space(cache));
        if (!cache->first_free) {
            return FS_CACHE_FULL;
        }
    }

    // retrieve first element from free list
    size_t entry_index = cache->first_free;
    cache->first_free = cache->entries[entry_index-1].next;
    *entry = &cache->entries[entry_index-1];
    memset(*entry, 0, sizeof(**entry));

    return SYS_ERR_OK;
}

static void
remove_from_unused(struct fs_cache *cache, struct cache_entry *entry)
{
    TRACE_ENTER();

    size_t entry_index = (entry - cache->entries) + 1;
    if (entry->prev_unused) {
        assert(cache->first_unused != entry_index);
        cache->entries[entry->prev_unused-1].next_unused = entry->next_unused;
    }
    else {
        assert(cache->first_unused == entry_index);
        cache->first_unused = entry->next_unused;
    }
    if (entry->next_unused) {
        assert(cache->last_unused != entry_index);
        cache->entries[entry->next_unused-1].prev_unused = entry->prev_unused;
    }
    else {
        assert(cache->last_unused == entry_index);
        cache->last_unused = entry->prev_unused;
    }
    entry->next_unused = entry->prev_unused = 0;
    cache->unused_count--;
}

errval_t
fs_cache_acquire(struct fs_cache *cache, uint32_t key, void **item)
{
    TRACE_ENTER_F("key=%"PRIu32, key);
    assert(cache);

    if (!cache->entries) {
        return FS_CACHE_NOTPRESENT;
    }

    size_t hash = hash_key(key, cache->map_size);
    size_t entry_index = cache->map[hash];

    if (!entry_index) {
        return FS_CACHE_NOTPRESENT;
    }

    // find matching entry
    struct cache_entry *entry = &cache->entries[entry_index-1];
    while (entry->key != key && entry->next) {
        entry = &cache->entries[entry->next-1];
    }
    if (entry->key != key) {
        return FS_CACHE_NOTPRESENT;
    }

    if (entry->references == 0) {
        // need to remove entry from unused entries list
        remove_from_unused(cache, entry);
    }

    entry->references++;
    *item = entry->item;

    return SYS_ERR_OK;
}

errval_t
fs_cache_put(struct fs_cache *cache, uint32_t key, void *item)
{
    TRACE_ENTER_F("key=%"PRIu32, key);
    assert(cache);

    errval_t err = SYS_ERR_OK;
    size_t hash = hash_key(key, cache->map_size);
    size_t entry_index = cache->map[hash];
    struct cache_entry *entry = NULL;

    if (!entry_index) {
        // the map entry is free, just get a new entry
        err = get_new_entry(cache, hash, &entry);
        if (err_is_ok(err)) {
            entry_index = (entry - cache->entries) + 1;
            cache->map[hash] = entry_index;
        }
    }
    else {
        // find matching entry or last entry
        entry = &cache->entries[entry_index-1];
        while (entry->key != key && entry->next) {
            entry = &cache->entries[entry->next-1];
        }

        if (entry->key == key) {
            // found matching entry
            if (entry->item == item) {
                // duplicate put, do nothing
                // XXX: warn?
            }
            else if (entry->references == 0) {
                // unused entry with same key, replace item
                remove_from_unused(cache, entry);
                free(entry->item);
                entry->item = NULL;
            }
            else {
                // in-use entry exists with different item, report conflict
                err = FS_CACHE_CONFLICT;
            }
        }
        else {
            // reached last entry, append new entry
            struct cache_entry *prev = entry;
            err = get_new_entry(cache, hash, &entry);
            if (err_is_ok(err)) {
                entry_index = (entry - cache->entries) + 1;
                prev->next = entry_index;
                entry->prev = (prev - cache->entries) + 1;
            }
        }
    }

    if (err_is_ok(err)) {
        entry->key = key;
        entry->item = item;
        entry->references++;
    }

    return err;
}

errval_t
fs_cache_release(struct fs_cache* cache, uint32_t key)
{
    TRACE_ENTER_F("key=%"PRIu32, key);
    assert(cache);

    if (!cache->entries) {
        return FS_CACHE_NOTPRESENT;
    }

    size_t hash = hash_key(key, cache->map_size);
    size_t entry_index = cache->map[hash];

    if (!entry_index) {
        return FS_CACHE_NOTPRESENT;
    }

    // find entry with matching key
    struct cache_entry *entry = &cache->entries[entry_index-1];
    while (entry->key != key && entry->next) {
        entry = &cache->entries[entry->next-1];
    }
    if (entry->key != key) {
        return FS_CACHE_NOTPRESENT;
    }

    assert(entry->references > 0);
    entry->references--;
    if (entry->references == 0) {
        // append to unused list
        entry_index = (entry - cache->entries) + 1;
        if (cache->unused_count > 0) {
            assert(cache->first_unused);
            assert(cache->last_unused);
            cache->entries[cache->last_unused-1].next_unused = entry_index;
        }
        else {
            assert(!cache->first_unused);
            assert(!cache->last_unused);
            cache->first_unused = entry_index;
        }
        entry->prev_unused = cache->last_unused;
        entry->next_unused = 0;
        cache->last_unused = entry_index;
        cache->unused_count++;
    }

    return SYS_ERR_OK;
}

errval_t
fs_cache_init(size_t max_capacity, size_t map_size, struct fs_cache **cache_p)
{
    TRACE_ENTER_F("max_capacity=%zu, map_size=%zu", max_capacity, map_size);
    assert(cache_p);

    struct fs_cache *cache;
    cache = calloc(1, sizeof(*cache));
    if (!cache) {
        return LIB_ERR_MALLOC_FAIL;
    }

    assert(max_capacity >= MIN_ALLOC);
    assert((max_capacity & ~(max_capacity-1)) == max_capacity);
    cache->max_capacity = max_capacity;

    // initialize map. do not need to initialize entries as this is done
    // lazily.
    if (!map_size) {
        map_size = max_capacity;
    }
    assert((map_size & ~(map_size-1)) == map_size);
    cache->map_size = map_size;
    cache->map = calloc(cache->map_size, sizeof(*cache->map));
    if (!cache->map) {
        free(cache);
        return LIB_ERR_MALLOC_FAIL;
    }

    *cache_p = cache;
    return SYS_ERR_OK;
}

void
fs_cache_free(struct fs_cache *cache)
{
    TRACE_ENTER();

    if (!cache) {
        return;
    }

    for (size_t i = 0; i < cache->capacity; i++) {
        if (cache->entries[i].item) {
            free(cache->entries[i].item);
        }
    }
    free(cache->entries);
    free(cache->map);
    free(cache);
}
