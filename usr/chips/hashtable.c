/**
 * \file hashtable.c
 * \brief Hashtable implementation
 */

/*
 * Copyright (c) 2008,2009,2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#include <stdint.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

#include "hashtable.h"
#include "multimap.h"

/**
 * \brief get a hash value for a string
 * \param str the string
 * \return the hash value
 */
static inline int hash(const char *str)
{
    register int _hash = 5381;
    register int _c;

    while ((_c = *str++)) {
        _hash = ((_hash << 5) + _hash) + _c;
    }
    return _hash;
}

/**
 * \brief get the index for an given hash in the bucket table
 * \param table_length the length of the table
 * \param hash_value the hash
 * \return the index
 */
static inline int index_for(int table_length, int hash_value)
{
    return ((unsigned)hash_value % table_length);
}

/**
 * \brief check two keys for equality
 * \param k1 the first string
 * \param k2 the second string
 * \return true if the strings are equal
 */
#define equals(_k1, _k2) (!strcmp((_k1), (_k2)))


/**
 * \brief get the number of entries in a hashtable
 * \param h the hashtable
 * \return the number of entries
 */
static int ht_size(struct dictionary *dict)
{
    assert(dict != NULL);
    struct hashtable *ht = (struct hashtable*) dict;

    return ht->entry_count;
}

/**
 * \brief put a new key/value pair into the hashtable
 * \param ht the hashtable
 * \param key the key. Has to be a string.
 * \param value the value. Can be any pointer. This function does
 *      not copy the value or stores it. The caller is responsible for
 *      maintaining the value, the hashtable only keeps pointers.
 * \return 0 if the operation succeeded, otherwise an error code.
 */
static int ht_put(struct dictionary *dict, struct _ht_entry *entry)
{
    assert(dict != NULL);
    struct hashtable *ht = (struct hashtable*) dict;

    int _hash_value = hash(entry->key);

    // TODO: XXX check for size and increase capacity, if necessary
    ++(ht->entry_count);

    entry->hash_value = _hash_value;
    int _index = index_for(ht->table_length, _hash_value);
    entry->next = ht->entries[_index];
    ht->entries[_index] = entry;
    return 0;
}

static int ht_put_string(struct dictionary *dict, char *key, char *value)
{
    struct _ht_entry *e = malloc(sizeof(struct _ht_entry));
    if (NULL == e) {
        return 1;
    }
    e->key = key;
    e->value = value;
    e->type = TYPE_STRING;

    return ht_put(dict, e);
}

static int ht_put_word(struct dictionary *dict, char *key, uintptr_t value)
{
    struct _ht_entry *e = malloc(sizeof(struct _ht_entry));
    if (NULL == e) {
        return 1;
    }
    e->key = key;
    e->value = (void*) value;
    e->type = TYPE_WORD;

    return ht_put(dict, e);
}

static int ht_put_capability(struct dictionary *dict, char *key,
                             struct capref cap)
{
    struct _ht_entry *e = malloc(sizeof(struct _ht_entry));
    if (NULL == e) {
        return 1;
    }
    e->key = key;
    e->capvalue = cap;
    e->type = TYPE_CAPABILITY;

    return ht_put(dict, e);
}

static int ht_put_opaque(struct dictionary *dict, char *key, void *value)
{
    struct _ht_entry *e = malloc(sizeof(struct _ht_entry));
    if (NULL == e) {
        return 1;
    }
    e->key = key;
    e->value = value;
    e->type = TYPE_OPAQUE;

    return ht_put(dict, e);
}

/**
 * \brief get a value from the hashtable for a given key
 * \param ht the hashtable
 * \param key the key. Has to be a zero-terminated string.
 * \return the value or NULL if there is no such key/value pair
 */
static ENTRY_TYPE ht_get(struct dictionary *dict, char *key, void **value)
{
    assert(dict != NULL);
    assert(key != NULL);
    assert(value != NULL);

    struct hashtable *ht = (struct hashtable*) dict;

    int _hash_value = hash(key);
    int _index = index_for(ht->table_length, _hash_value);
    struct _ht_entry *_e = ht->entries[_index];

    while (NULL != _e) {
        if ((_hash_value == _e->hash_value) && (equals(key, _e->key))) {
            assert(_e->type != TYPE_CAPABILITY);
            *value = _e->value;
            return _e->type;
        }
        _e = _e->next;
    }
    *value = NULL;
    return 0;
}

static ENTRY_TYPE ht_get_capability(struct dictionary *dict, char *key,
                                    struct capref *value)
{
    assert(dict != NULL);
    assert(key != NULL);
    assert(value != NULL);

    struct hashtable *ht = (struct hashtable*) dict;

    int _hash_value = hash(key);
    int _index = index_for(ht->table_length, _hash_value);
    struct _ht_entry *_e = ht->entries[_index];

    while (NULL != _e) {
        if ((_hash_value == _e->hash_value) && (equals(key, _e->key))) {
            assert(_e->type == TYPE_CAPABILITY);
            *value = _e->capvalue;
            return _e->type;
        }
        _e = _e->next;
    }
    *value = NULL_CAP;
    return 0;
}

static int ht_get_key_set(struct dictionary *dict, char** key_set)
{
    assert(dict != NULL);
    assert(key_set != NULL);
    struct hashtable *ht = (struct hashtable*) dict;

    struct _ht_entry *e;
    int pos = 0;
    for (int i = 0; i < ht->capacity; i++) {
        e = ht->entries[i];
        while (e != NULL) {
            assert(e->key != NULL);
            strcpy((char*) (key_set + pos * sizeof (char*)), (char*) e->key);
            pos++;
            e = e->next;
        }
    }
    return 0;
}

void print_hashtable(FILE *stream, struct hashtable *ht)
{
    if (ht == NULL) {
        fprintf(stream, "NULL\n");
        return;
    }

    struct _ht_entry *e;
    fprintf(stream, "hashtable {\n");
    for (int i = 0; i < ht->capacity; i++) {
        e = ht->entries[i];
        while (e != NULL) {
            switch (e->type) {
            case TYPE_STRING:
                fprintf(stream, "\t'%s'='%s'\n", (char*) e->key,
                        (char*) e->value);
                break;
            case TYPE_WORD:
                fprintf(stream, "\t'%s'='%" PRIuPTR "'\n", (char*) e->key,
                        (uintptr_t) e->value);
                break;
            case TYPE_OPAQUE:
                fprintf(stream, "\t'%s'=opaque\n", (char*) e->key);
                break;

            case TYPE_CAPABILITY:
                fprintf(stream, "\t'%s'=capability\n", (char *) e->key);
                break;
            }
            e = e->next;
        }
    }
    fprintf(stream, "}\n");
}

static int ht_remove(struct dictionary *dict, char *key)
{
    assert(dict != NULL);
    struct hashtable *ht = (struct hashtable*) dict;

    int _hash_value = hash(key);
    int _index = index_for(ht->table_length, _hash_value);
    struct _ht_entry *_e = ht->entries[_index];
    struct _ht_entry *_prev = NULL;
    while (NULL != _e) {
        if ((_hash_value == _e->hash_value) && (equals(key, _e->key))) {
            if (_prev == NULL) {
                ht->entries[_index] = _e->next;
            } else {
                _prev->next = _e->next;
            }
            free(_e);
            return 0;
        }
        _prev = _e;
        _e = _e->next;
    }
    return 1;
}

// XXX implement destructors

/**
 * \brief create an empty hashtable with a given capacity and load factor
 * \param capacity the capacity
 * \param the load factor
 * \return an empty hashtable.
 */
static void ht_init(struct hashtable *_ht, int capacity, int load_factor)
{
    _ht->capacity = capacity;
    _ht->load_factor = load_factor;
    _ht->table_length = capacity;
    _ht->entries = calloc(_ht->table_length, sizeof(struct _ht_entry));
    assert(_ht->entries != NULL);
    _ht->threshold = (capacity * load_factor) / 100;
    _ht->d.size = ht_size;
    _ht->d.put_string = ht_put_string;
    _ht->d.put_word = ht_put_word;
    _ht->d.put_opaque = ht_put_opaque;
    _ht->d.put_capability = ht_put_capability;
    _ht->d.get = ht_get;
    _ht->d.get_capability = ht_get_capability;
    _ht->d.remove = ht_remove;
    _ht->d.get_key_set = ht_get_key_set;
}

// XXX TODO: loadFactor should be a float, 0.75 instead of 75
struct hashtable* create_hashtable2(int capacity, int load_factor)
{
    struct hashtable *_ht = malloc(sizeof(struct hashtable));
    assert(_ht != NULL);
    ht_init(_ht, capacity, load_factor);
    return _ht;
}


/**
 * \brief create an empty hashtable with default capacity and load factor
 * \return an empty hashtable
 */
struct hashtable* create_hashtable(void)
{
    return create_hashtable2(11, 75);
}

static int multimap_get_first(struct multimap *mm, char* key, void** value)
{
    assert(mm != NULL);

    return mm->h.d.get((struct dictionary *) mm, key, value);
}

/**
 * \brief get all values from the hashtable that are registered under a certain key.
 */

static int multimap_get_all(struct multimap *mm, char* key,
        void** values, int max_count)
{
    assert(mm != NULL);

    int _hash_value = hash(key);
    int _index = index_for(mm->h.table_length, _hash_value);
    struct _ht_entry *_e = mm->h.entries[_index];
    int count = 0;

    while (NULL != _e) {
        if ((_hash_value == _e->hash_value) && (equals(key, _e->key))) {
            values[count] = _e->value;
            count++;
        }
        _e = _e->next;
    }
    return count;
}

static int multimap_put(struct multimap *mm, char* key, void* value)
{
    assert(mm != NULL);

    return mm->h.d.put_opaque((struct dictionary *) mm, key, value);
}

static int multimap_remove(struct multimap *mm, char* key, void* value)
{
    assert(mm != NULL);

    int _hash_value = hash(key);
    int _index = index_for(mm->h.table_length, _hash_value);
    struct _ht_entry *_e = mm->h.entries[_index];
    struct _ht_entry *_prev = NULL;
    while (NULL != _e) {
        if ((_hash_value == _e->hash_value) && (equals(key, _e->key)) && (value == _e->value)) {
            if (_prev == NULL) {
                mm->h.entries[_index] = _e->next;
            } else {
                _prev->next = _e->next;
            }
            return 0;
        }
        _prev = _e;
        _e = _e->next;
    }
    return 1;
}

struct multimap* create_multimap(void)
{
    struct multimap *_mm = malloc(sizeof(struct multimap));
    assert(_mm != NULL);
    ht_init(&_mm->h, 11, 75);
    _mm->get_first = multimap_get_first;
    _mm->get_all = multimap_get_all;
    _mm->put = multimap_put;
    _mm->remove = multimap_remove;
    return _mm;
}
