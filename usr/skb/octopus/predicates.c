/**
 * \file
 * \brief Definitions for external C predicates used in Prolog code of
 * the octopus server implementation.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#define _USE_XOPEN /* for strdup() */
#include <stdio.h>
#include <string.h>

#include <eclipse.h>
#include <barrelfish/barrelfish.h>
#include <include/skb_server.h>
#include <collections/hash_table.h>

#include <octopus_server/debug.h>
#include <octopus_server/service.h>
#include <octopus/trigger.h> // for trigger modes

#include "predicates.h"
#include "skiplist.h"
#include "bitfield.h"
#include "fnv.h"

#define HASH_INDEX_BUCKETS 6151
static collections_hash_table* record_index = NULL;

static collections_hash_table* trigger_index = NULL;
static struct bitfield* no_attr_triggers = NULL;

static collections_hash_table* subscriber_index = NULL;
static struct bitfield* no_attr_subscriptions = NULL;

static inline void init_index(void) {
    if(record_index == NULL) {
        collections_hash_create_with_buckets(&record_index, HASH_INDEX_BUCKETS, NULL);
    }

    if(subscriber_index == NULL) {
        collections_hash_create_with_buckets(&subscriber_index, HASH_INDEX_BUCKETS, NULL);
        bitfield_create(&no_attr_subscriptions);
    }

    if(trigger_index == NULL) {
        collections_hash_create_with_buckets(&trigger_index, HASH_INDEX_BUCKETS, NULL);
        bitfield_create(&no_attr_triggers);
    }
}


static int skip_index_insert(collections_hash_table* ht, uint64_t key, char* value)
{
    assert(ht != NULL);
    assert(value != NULL);

    struct skip_list* sl = (struct skip_list*) collections_hash_find(ht, key);
    if (sl == NULL) {
        errval_t err = skip_create_list(&sl);
        if (err_is_fail(err)) {
            return PFAIL;
        }
        collections_hash_insert(ht, key, sl);
    }

    skip_insert(sl, value);
    //skip_print_list(sl);

    return PSUCCEED;
}

static char* skip_index_remove(collections_hash_table* ht, uint64_t key, char* value)
{
    assert(ht != NULL);
    assert(value != NULL);

    struct skip_list* sl = (struct skip_list*) collections_hash_find(ht, key);
    if (sl == NULL) {
        return NULL;
    }

    char* record_name = skip_delete(sl, value);

    //skip_print_list(sl);
    return record_name;
}

int p_save_index(void)
{
    OCT_DEBUG("p_save_index\n");
    init_index();

    char* value = NULL;
    int res = ec_get_string(ec_arg(3), &value);
    assert(res == PSUCCEED);

    char* record_name = strdup(value);
    bool inserted = false;

    pword list, cur, rest;
    pword attribute_term;
    for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
        ec_get_arg(1, cur, &attribute_term);

        char* attribute;
        ec_get_string(attribute_term, &attribute);

        OCT_DEBUG("insert %s(%p) into index[%s]=", record_name, record_name, attribute);
        uint64_t key = fnv_64a_str(attribute, FNV1A_64_INIT);
        int res = skip_index_insert(record_index, key, record_name);
        assert(res == PSUCCEED);
        inserted = true;
    }

    if (!inserted) {
        free(record_name);
    }

    return PSUCCEED;
}

int p_remove_index(void)
{
    int res;
    char* to_free = NULL;
    init_index();

    char* name = NULL;
    res = ec_get_string(ec_arg(3), &name);
    assert(res == PSUCCEED);

    pword list, cur, rest;
    pword attribute_term;
    for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
        ec_get_arg(1, cur, &attribute_term);

        char* attribute;
        res = ec_get_string(attribute_term, &attribute);
        assert(res == PSUCCEED);

        uint64_t key = fnv_64a_str(attribute, FNV1A_64_INIT);
        to_free = skip_index_remove(record_index, key, name);
        OCT_DEBUG("removed %s(%p) from index[%s]=", name, to_free, attribute);
        //assert(to_free != NULL);
    }

    free(to_free);
    return PSUCCEED;
}

int p_index_intersect(void) /* p_index_intersect(type, -[Attributes], -Current, +Next) */
{
    OCT_DEBUG("p_index_intersect\n");
    static struct skip_list** sets = NULL;
    static char* next = NULL;
    static size_t elems = 0;

    int res;
    char* key;

    init_index();

    char* index_type = NULL;
    res = ec_get_string(ec_arg(1), &index_type);
    if (res != PSUCCEED) {
        return res;
    }
    collections_hash_table* ht = record_index;

    res = ec_get_string(ec_arg(3), &next);
    if (res != PSUCCEED) {
        OCT_DEBUG("state is not a string, find skip lists\n");
        free(sets);
        pword list, cur, rest;

        elems = 0;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            elems++;
        }
        sets = malloc(sizeof(struct skip_list*) * elems);

        size_t i = 0;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct skip_list* sl = collections_hash_find(ht, hash_key);
            if (sl == NULL) {
                return PFAIL;
            }
            OCT_DEBUG("skip_intersect found skip list for key: %s\n", key);
            //skip_print_list(sl);

            sets[i] = sl;
            i++;
        }
        next = NULL;
    }

    next = skip_intersect(sets, elems, next);
    OCT_DEBUG("skip_intersect found next: %s\n", next);
    if(next != NULL) {
        dident item = ec_did(next, 0);
        return ec_unify_arg(4, ec_atom(item));
    }

    return PFAIL;
}

int p_index_union(void) /* p_index_union(type, -[Attributes], -Current, +Next) */
{
    OCT_DEBUG("p_index_union\n");
    static collections_hash_table* union_ht = NULL;
    static char* next = NULL;

    int res;
    char* key;

    init_index();

    char* index_type = NULL;
    res = ec_get_string(ec_arg(1), &index_type);
    if (res != PSUCCEED) {
        return res;
    }
    collections_hash_table* ht = record_index; // TODO broken

    res = ec_get_string(ec_arg(3), &next);
    if (res != PSUCCEED) {
        OCT_DEBUG("state is not a string, find skip lists\n");
        if (union_ht != NULL) {
            collections_hash_release(union_ht);
            union_ht = NULL;
        }
        collections_hash_create_with_buckets(&union_ht, HASH_INDEX_BUCKETS, NULL);

        pword list, cur, rest;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct skip_list* sl = collections_hash_find(ht, hash_key);

            // Insert all entries in union hash table
            if (sl != NULL) {
                OCT_DEBUG("p_index_union found skip list for key: %s\n", key);
                //skip_print_list(sl);

                struct skip_node* sentry = sl->header->forward[0];
                while(sentry != NULL) {
                    uint64_t hash_key = fnv_64a_str(sentry->element, FNV1A_64_INIT);
                    if(collections_hash_find(union_ht, hash_key) == NULL) {
                        OCT_DEBUG("p_index_union insert: %s\n", sentry->element);
                        collections_hash_insert(union_ht, hash_key, sentry->element);
                    }
                    sentry = sentry->forward[0];
                }
            }

        }
        next = NULL;
        collections_hash_traverse_start(union_ht);
    }

    uint64_t hash_key;
    next = collections_hash_traverse_next(union_ht, &hash_key);
    OCT_DEBUG("skip_union found next: %s\n", next);
    if(next != NULL) {
        dident item = ec_did(next, 0);
        return ec_unify_arg(4, ec_atom(item));
    }
    else {
        collections_hash_traverse_end(union_ht);
        return PFAIL;
    }
}



static int bitfield_index_insert(collections_hash_table* ht, uint64_t key, long int id)
{
    assert(ht != NULL);

    struct bitfield* bf = (struct bitfield*) collections_hash_find(ht, key);
    if (bf == NULL) {
        errval_t err = bitfield_create(&bf);
        if (err_is_fail(err)) {
            return PFAIL;
        }
        collections_hash_insert(ht, key, bf);
    }

    bitfield_on(bf, id);
    return PSUCCEED;
}

static int bitfield_index_remove(collections_hash_table* ht, uint64_t key, long int id)
{
    assert(ht != NULL);

    struct bitfield* bf = (struct bitfield*) collections_hash_find(ht, key);
    if (bf != NULL) {
        bitfield_off(bf, id);
    }

    return PSUCCEED;
}

int p_bitfield_add(void) /* p_bitfield_add(Storage, +Name, +[AttributeList], +Id) */
{
    init_index();
    int res = 0;
    long int id;
    bool inserted = false;

    collections_hash_table* ht = NULL;
    struct bitfield* no_attr_bf = NULL;

    char* storage;
    res = ec_get_string(ec_arg(1), &storage);
    if (strcmp(storage, "trigger") == 0) {
        ht = trigger_index;
        no_attr_bf = no_attr_triggers;
    }
    else {
        ht = subscriber_index;
        no_attr_bf = no_attr_subscriptions;
    }

    res = ec_get_long(ec_arg(3), &id);
    if (res != PSUCCEED) {
        return PFAIL;
    }

    pword list, cur, rest;
    pword attribute_term;
    for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
        ec_get_arg(1, cur, &attribute_term);

        char* attribute;
        ec_get_string(attribute_term, &attribute);
        uint64_t key = fnv_64a_str(attribute, FNV1A_64_INIT);

        int res = bitfield_index_insert(ht, key, id);
        assert(res == PSUCCEED);
        inserted = true;
    }

    if (!inserted) {
        bitfield_on(no_attr_bf, id);
    }

    return PSUCCEED;
}

int p_bitfield_remove(void) /* p_bitfield_remove(Storage, +Name, +[AttributeList], +Id) */
{
    init_index();

    int res = 0;
    long int id;

    collections_hash_table* ht = NULL;
    struct bitfield* no_attr_bf = NULL;

    char* storage;
    res = ec_get_string(ec_arg(1), &storage);
    if (strcmp(storage, "trigger") == 0) {
        ht = trigger_index;
        no_attr_bf = no_attr_triggers;
    }
    else {
        ht = subscriber_index;
        no_attr_bf = no_attr_subscriptions;
    }

    res = ec_get_long(ec_arg(3), &id);
    if (res != PSUCCEED) {
        return PFAIL;
    }

    pword list, cur, rest;
    pword attribute_term;
    for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
        ec_get_arg(1, cur, &attribute_term);

        char* attribute;
        res = ec_get_string(attribute_term, &attribute);
        assert(res == PSUCCEED);

        uint64_t key = fnv_64a_str(attribute, FNV1A_64_INIT);
        bitfield_index_remove(ht, key, id);
    }

    bitfield_off(no_attr_bf, id);
    return PSUCCEED;
}

int p_bitfield_union(void) /* p_index_union(Storage, -[Attributes], -Current, +Next) */
{
    OCT_DEBUG("p_bitfield_union\n");
    static struct bitfield** sets = NULL;
    static long int next = -1;
    static size_t elems = 0;

    int res;
    char* key;

    init_index();
    collections_hash_table* ht = NULL;
    struct bitfield* no_attr_bf = NULL;

    char* storage = NULL;
    res = ec_get_string(ec_arg(1), &storage);
    if (strcmp(storage, "trigger") == 0) {
        ht = trigger_index;
        no_attr_bf = no_attr_triggers;
    }
    else {
        ht = subscriber_index;
        no_attr_bf = no_attr_subscriptions;
    }

    res = ec_get_long(ec_arg(3), &next);
    if (res != PSUCCEED) {
        OCT_DEBUG("state is not a id, find bitmaps\n");
        free(sets);
        pword list, cur, rest;

        elems = 0;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            elems++;
        }
        sets = calloc(elems+1, sizeof(struct bitfield*));
        sets[0] = no_attr_bf;

        elems = 1;
        for (list = ec_arg(2); ec_get_list(list, &cur, &rest) == PSUCCEED; list = rest) {
            res = ec_get_string(cur, &key);
            if (res != PSUCCEED) {
                return res;
            }

            uint64_t hash_key = fnv_64a_str(key, FNV1A_64_INIT);
            struct bitfield* sl = collections_hash_find(ht, hash_key);
            if (sl != NULL) {
                OCT_DEBUG("bitfield_union found bitfield for key: %s\n", key);
                sets[elems++] = sl;
            }
            // else: no record with this attribute, just ignore

        }
        next = -1;
    }

    next = bitfield_union(sets, elems, next);
    OCT_DEBUG("bitfield_union found next: %ld\n", next);
    if(next != -1) {
        pword item = ec_long(next);
        return ec_unify_arg(4, item);
    }

    return PFAIL;
}

void oct_rpc_enqueue_reply(struct octopus_binding *b, struct oct_reply_state* st);
extern struct bitfield* trigger_ids;

int p_trigger_watch(void) /* p_trigger_watch(+String, +Mode, +Recipient, +WatchId, -Retract) */
{
    int res;
    OCT_DEBUG("\n*** p_trigger_watch: start\n");

    // Get arguments
    char* record = NULL;
    res = ec_get_string(ec_arg(1), &record);
    if (res != PSUCCEED) {
        assert(ec_is_var(ec_arg(1)) == PSUCCEED);
        // record will be null
        // can happen in case we send OCT_REMOVED
    }

    // Action that triggered the event
    long int action = 0;
    res = ec_get_long(ec_arg(2), &action);
    if (res != PSUCCEED) {
        return res;
    }

    // Mode of watch
    long int watch_mode = 0;
    res = ec_get_long(ec_arg(3), &watch_mode);
    if (res != PSUCCEED) {
        return res;
    }

    struct oct_reply_state* drs = NULL;
    res = ec_get_long(ec_arg(4), (long int*) &drs);
    if (res != PSUCCEED) {
        return res;
    }
    assert(drs != NULL);
    OCT_DEBUG("drs is: %p\n", drs);

    long int watch_id = 0;
    res = ec_get_long(ec_arg(5), &watch_id);
    if (res != PSUCCEED) {
        return res;
    }

    OCT_DEBUG("p_trigger_watch: %s\n", record);
    OCT_DEBUG("drs->binding: %p\n", drs->binding);
    OCT_DEBUG("drs->reply: %p\n", drs->reply);


    drs->error = SYS_ERR_OK;
    bool retract = !(watch_mode & OCT_PERSIST);
    if (record != NULL) {
        assert(strlen(record)+1 < MAX_QUERY_LENGTH);
        strcpy(drs->query_state.std_out.buffer, record);
    }
    else {
        drs->query_state.std_out.buffer[0] = '\0';
        drs->query_state.std_out.length = 0;
    }

    if (drs->binding != NULL && drs->reply != NULL) {

        if (!retract) {
            // Copy reply state because the trigger will stay intact
            struct oct_reply_state* drs_copy = NULL;
            errval_t err = new_oct_reply_state(&drs_copy, NULL);
            assert(err_is_ok(err));
            memcpy(drs_copy, drs, sizeof(struct oct_reply_state));
            drs = drs_copy; // overwrite drs
        }
        else {
            assert(trigger_ids != NULL);
            OCT_DEBUG("turn off trigger id: %lu\n", watch_id);
            bitfield_off(trigger_ids, watch_id);
        }

        drs->mode = (retract) ? (action | OCT_REMOVED) : action;

        if (drs->binding->st != NULL) {
            oct_rpc_enqueue_reply(drs->binding, drs);
        }
        else {
            drs->reply(drs->binding, drs);
        }
    }
    else {
        USER_PANIC("No binding set for watch_id: %lu", watch_id);
    }

    OCT_DEBUG("p_trigger_watch: done");
    return ec_unify_arg(6, ec_long(retract));
}
