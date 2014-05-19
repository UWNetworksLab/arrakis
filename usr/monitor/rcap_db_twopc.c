/** \file
 *  \brief Inter-core capability database, two phase commit version.
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */
#define CAP_PREDICATE_HACK
#include "monitor.h"
#include <collections/hash_table.h>
#include <barrelfish/cap_predicates.h>
#include <inttypes.h>

static hash_table * local_rcap_db;
static coremask_t my_coremask;

/*----------------------- Local database maintenance code -------------------*/

static errval_t local_db_add(struct cap_db_node **cap_node, coremask_t cores,
                             bool has_desc, struct capability * cap)  
{
    *cap_node = malloc(sizeof(struct cap_db_node));
    assert (*cap_node);
    (*cap_node)->cap      = *cap;
    (*cap_node)->has_descendants= has_desc;
    (*cap_node)->locked         = false;
    (*cap_node)->core_with_lock = -1;
    (*cap_node)->on_cores       = cores;
    (*cap_node)->lock_chain     = NULL;
    uint64_t key = hash_cap(cap);
    hash_insert(local_rcap_db, key, *cap_node);

    return SYS_ERR_OK;
}

static errval_t local_db_delete(struct cap_db_node *cap_node) {
    // remove from hashtable
    uint64_t key = hash_cap(&cap_node->cap);
    hash_delete(local_rcap_db, key);  // this will also free the cap_node

    return SYS_ERR_OK;
}


static errval_t mark_non_remote(struct cap_db_node *cap_node)
{
    // mark cap as non-remote in kernel (will mark all local copies)
    errval_t err;
    struct capref cap;
    
    err = slot_alloc(&cap);
    assert(err_is_ok(err));
    
    err = monitor_cap_create(cap, &cap_node->cap, my_core_id);
    assert(err_is_ok(err));
    
    err = monitor_cap_remote(cap, false, &cap_node->has_descendants);
    assert(err_is_ok(err));

    err = cap_destroy(cap);
    assert(err_is_ok(err));
    return err;
}

static struct cap_db_node* local_db_get(struct capability *cap) 
{
    uint64_t key = hash_cap(cap);
    struct cap_db_node* cap_node = hash_find(local_rcap_db, key);
    return cap_node;
}

static errval_t local_db_lock(struct cap_db_node *cap_node, coreid_t core) 
{
    if (cap_node->locked) {
        // wait for in-progress cap operation to complete first
        return MON_ERR_REMOTE_CAP_RETRY;
    }
    
    cap_node->locked = true;
    cap_node->core_with_lock = core;
    return SYS_ERR_OK;
}

static void local_db_unlock(struct cap_db_node *cap_node, coreid_t core) 
{
    assert (cap_node->locked && cap_node->core_with_lock == core);
    
    cap_node->locked = false;
    cap_node->core_with_lock = -1;
}


struct rec_lock_st {
    struct cap_db_node *cap_node;
    coreid_t locking_core;
    errval_t err;
};

static int recursive_lock_visitor(uint64_t key, void *data, void *st_arg)
{
    struct rec_lock_st *st         = (struct rec_lock_st *) st_arg;
    struct cap_db_node *curr_cap_node = (struct cap_db_node *) data;

    if ((curr_cap_node != st->cap_node) && 
        (is_ancestor(&curr_cap_node->cap, &st->cap_node->cap) ||
         is_copy(&curr_cap_node->cap, &st->cap_node->cap))) {
        st->err = local_db_lock(curr_cap_node, st->locking_core);

        // insert into lock chain, keeping locking capability at head of chain
        curr_cap_node->lock_chain = st->cap_node->lock_chain;
        st->cap_node->lock_chain  = curr_cap_node;
    }
    return err_is_ok(st->err);
}

static errval_t local_db_recursive_lock(struct cap_db_node *cap_node,
                                        coreid_t locking_core)
{
    // try to lock given capability
    errval_t err = local_db_lock(cap_node, locking_core);
    if (err_is_fail(err)) {
        return err;
    }

    // traverse the database, locking any descendents of the capability
    struct rec_lock_st st;
    st.cap_node = cap_node;
    st.locking_core = locking_core;
    st.err = SYS_ERR_OK;

    hash_visit(local_rcap_db, recursive_lock_visitor, &st);

    return st.err;
}

static void local_db_recursive_unlock(struct cap_db_node *cap_node,
                                          coreid_t locking_core)
{
    // unlock given capability if it was locked
    if (cap_node->locked) {
        local_db_unlock(cap_node, locking_core);
    }

    // unlock chain of locked capabilities
    while (cap_node->lock_chain) {
        struct cap_db_node * next = cap_node->lock_chain;
        cap_node->lock_chain = NULL;
        cap_node = next;
        local_db_unlock(cap_node, locking_core);
    }
}

/*--------------------------------- Rcap_db API  ----------------------------*/

errval_t rcap_db_init(void)
{
    hash_create(&local_rcap_db, free);
    my_coremask = get_coremask(my_core_id);
    return SYS_ERR_OK;
}

errval_t rcap_db_add(struct capability * cap, bool has_desc)
{
    errval_t err = SYS_ERR_OK;

    struct cap_db_node * cap_node = local_db_get(cap);

    if (!cap_node) {
        // First time we've seen this cap, add it to our db
        err = local_db_add(&cap_node, my_coremask, has_desc, cap);
        if (err_is_fail(err)) {
            err_push(err, MON_ERR_RCAP_DB_ADD);
        }
    }
    return err;
}

bool rcap_db_exists(struct capability *cap) 
{
    return (local_db_get(cap) != NULL);
}

errval_t rcap_db_get_info(struct capability *cap, bool * has_desc, 
                          coremask_t *on_cores) 
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);

    *has_desc = cap_node->has_descendants;
    *on_cores = cap_node->on_cores;

    return SYS_ERR_OK;
}

errval_t rcap_db_update_on_recv (struct capability * cap, bool has_desc,
                                 coremask_t on_cores, coreid_t from_core) {
    errval_t err = SYS_ERR_OK;

    struct cap_db_node * cap_node = local_db_get(cap);

    if (!cap_node) {
        assert ((on_cores & my_coremask) == 0);
        // add to db
        err = local_db_add(&cap_node, on_cores | my_coremask, has_desc, cap);
        if (err_is_fail(err)) {
            err_push(err, MON_ERR_RCAP_DB_ADD);
            goto reply;
        }
        // signal other cores that we know about this cap,
        // unlocking cap in their db in the process
        err = route_rcap_new_core(cap, on_cores, from_core, my_core_id);

    } else {
        assert(cap_node->has_descendants == has_desc && 
               cap_node->on_cores == on_cores);
    }

reply:
    return err;  
}

errval_t rcap_db_acquire_lock(struct capability *cap, struct rcap_st * st)
{
    errval_t err = SYS_ERR_OK;

    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);

    err = local_db_lock(cap_node, my_core_id);
    st->err          = err;

    if (err_is_fail(err)) {
        return err;
    }

    st->cores_locked = my_coremask;

    // lock cap_node on other cores
    coremask_t othercores = cap_node->on_cores &~ my_coremask;
    if (othercores) {
        err = route_rcap_lock_req(cap, othercores, my_core_id, st, false);
        if (err_is_fail(err)) {
            local_db_unlock(cap_node, my_core_id);
            err_push(err, MON_ERR_RCAP_DB_LOCK);
        }
    } else {
        // immediatly call callback
        assert (st->cb);
        st->cb(st);
    }    
    return err;
}

errval_t rcap_db_remote_lock_req(struct capability *cap, coreid_t from_core, 
                                 recordid_t ccast_recordid)
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);

    errval_t reply_err = local_db_lock(cap_node, from_core);
    coremask_t locked_cores = err_is_ok(reply_err) ? my_coremask : 0;
    // reply with result
    return route_rcap_lock_reply(reply_err, locked_cores,
                                 cap_node->has_descendants, ccast_recordid);
}

errval_t rcap_db_release_lock(struct capability *cap, coremask_t to_cores)
{
    errval_t err = SYS_ERR_OK;
    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);

    if (to_cores & my_coremask) {
        local_db_unlock(cap_node, my_core_id);
    }

    coremask_t othercores = to_cores &~ my_coremask;
    // unlock cap_node on other cores
    if (to_cores) {
        err = route_rcap_unlock(cap, othercores, my_core_id, false);
        if (err_is_fail(err)) {
            err_push(err, MON_ERR_RCAP_DB_UNLOCK);
        }
    }
    return err;
}

errval_t rcap_db_remote_unlock(struct capability *cap, coreid_t from_core)
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);
    local_db_unlock(cap_node, from_core);
    return SYS_ERR_OK;
}
 
errval_t rcap_db_acquire_recursive_lock(struct capability *cap,
                                        struct rcap_st * st)
{
    errval_t err = SYS_ERR_OK;

    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);

    err = local_db_recursive_lock(cap_node, my_core_id);
    st->err          = err;

    if (err_is_fail(err)) {
        local_db_recursive_unlock(cap_node, my_core_id);
        return err;
    }

    st->cores_locked = my_coremask;

    //signal ALL other cores to also recursivly lock the cap and its descendents
    err = route_rcap_lock_req(cap, 0, my_core_id, st, true);
    if (err_is_fail(err)) {
        local_db_recursive_unlock(cap_node, my_core_id);
        err_push(err, MON_ERR_RCAP_DB_LOCK);
    }
    return err;
}

errval_t rcap_db_remote_recursive_lock_req(struct capability *cap,
                                           coreid_t from_core, 
                                           recordid_t ccast_recordid)
{
    errval_t err;
    struct cap_db_node * cap_node = local_db_get(cap);
    if (!cap_node) {
        // we don't know about this cap, but might have its descendents, 
        // add to db, but with coremask of 0 so we know to delete it
        err = local_db_add(&cap_node, 0, false, cap); 
        assert(err_is_ok(err));
    }
    assert(cap_node);

    
    errval_t reply_err = local_db_recursive_lock(cap_node, from_core);
    if (err_is_fail(reply_err)) {
        local_db_recursive_unlock(cap_node, my_core_id);
    }
    coremask_t locked_cores = err_is_ok(reply_err) ? my_coremask : 0;

    // reply with result
    return route_rcap_lock_reply(reply_err, locked_cores, 
                                 cap_node->has_descendants, ccast_recordid);
}

errval_t rcap_db_release_recursive_lock(struct capability *cap, 
                                        coremask_t to_cores)
{
    
    errval_t err = SYS_ERR_OK;
    struct cap_db_node * cap_node = local_db_get(cap);
    assert (cap_node);

    if (to_cores & my_coremask) {
        local_db_recursive_unlock(cap_node, my_core_id);
    }

    coremask_t othercores = to_cores &~ my_coremask;
    // unlock cap_node on other cores
    if (to_cores) {
        err = route_rcap_unlock(cap, othercores, my_core_id, true);
        if (err_is_fail(err)) {
            err_push(err, MON_ERR_RCAP_DB_UNLOCK);
        }
    }
    return err;
}

errval_t rcap_db_remote_recursive_unlock(struct capability *cap, 
                                         coreid_t from_core)
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);  // must have locked it before, so should know about it
    local_db_recursive_unlock(cap_node, from_core);
    
    // if this node was only created for the purposes of the recursive lock 
    // operation, then delete it here
    if (!cap_node->on_cores) {
        local_db_delete(cap_node);
    }
    return SYS_ERR_OK;
}

errval_t rcap_db_remote_new_core(struct capability * cap, coreid_t send_core, 
                                 coreid_t recv_core)
{
    struct cap_db_node * cap_node = local_db_get(cap);

    // we should know about this cap if we are receiving updates about it
    assert(cap_node);

    // update the set of cores that know about this cap
    cap_node->on_cores |= get_coremask(recv_core);
    
    // unlock the cap
    local_db_unlock(cap_node, send_core);

    return SYS_ERR_OK;
}

errval_t rcap_db_remote_recv_details(struct capability * cap, 
                                     coreid_t from_core, bool has_desc)
{
    return SYS_ERR_OK;  // only used in rcap_central
}

errval_t rcap_db_remote_details_req(struct capability * cap, 
                                     coreid_t from_core)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

/** Cap deleted on our local core */
errval_t rcap_db_delete (struct capability * cap)
{
    errval_t err;

    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);

    // tell other cores that I don't hold a replica of this cap any longer
    coremask_t othercores = cap_node->on_cores &~ my_coremask;
    err = route_rcap_delete(cap, othercores);
    assert(err_is_ok(err));
    
    err = local_db_delete(cap_node);

    return err;
}

/** Cap deleted on another core */
errval_t rcap_db_remote_delete (struct capability * cap, coreid_t from_core)
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);

    // remove this core from the set of cores that know about this cap
    cap_node->on_cores &= ~get_coremask(from_core);
    local_db_unlock(cap_node, from_core);
    
    if (cap_node->on_cores == my_coremask) {
        // cap is no longer remote
        mark_non_remote(cap_node);
        local_db_delete(cap_node);
    }

    return SYS_ERR_OK;
}

/** Cap retyped on our local core */
errval_t rcap_db_retype(struct capability * cap, bool has_desc)
{
    errval_t err;
    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);

    cap_node->has_descendants = has_desc;

    coremask_t othercores = cap_node->on_cores &~ my_coremask;
    err = route_rcap_retype(cap, has_desc, othercores);
    assert(err_is_ok(err));

    local_db_unlock(cap_node, my_core_id);
    
    return err;
}

/** Retyped on another core */
errval_t rcap_db_remote_retype (struct capability * cap, bool has_desc,
                                coreid_t from_core)
{
    struct cap_db_node * cap_node = local_db_get(cap);
    assert(cap_node);
    cap_node->has_descendants = has_desc;
    local_db_unlock(cap_node, from_core);
    return SYS_ERR_OK;
}


/** Cap revoked on our local core */
errval_t rcap_db_revoke(struct capability * cap)
{
    errval_t err;

    struct cap_db_node *cap_node = local_db_get(cap);
    assert(cap_node);

    // tell other cores to revoke this cap
    err = route_rcap_revoke(cap);
    assert(err_is_ok(err));
    
    // cap is no longer remote, mark it as such
    mark_non_remote(cap_node);

    // remove this cap and any of its descendents from local_db
    do {
        struct cap_db_node * next = cap_node->lock_chain;
        assert (cap_node->locked && cap_node->core_with_lock == my_core_id);
        local_db_delete(cap_node);
        cap_node = next;
    } while (cap_node);

    return err;
}

/** Revoked on another core */
errval_t rcap_db_remote_revoke (struct capability * cap, coreid_t from_core)
{
    errval_t err;

    struct cap_db_node *cap_node = local_db_get(cap);
    assert(cap_node);

    // create our own reference to the cap
    struct capref capref;
    err = slot_alloc(&capref);
    assert(err_is_ok(err));
    
    err = monitor_cap_create(capref, &cap_node->cap, my_core_id);
    assert(err_is_ok(err));

    // perform revoke operation on this cap
    uint8_t vbits = get_cap_valid_bits(capref);
    capaddr_t caddr = get_cap_addr(capref) >> (CPTR_BITS - vbits);
    err = monitor_revoke_remote_cap(cap_root, caddr, vbits);
    assert(err_is_ok(err));

    // destroy our copy of cap (mark as non remote first to prevent destroy 
    // causing remote cap operations)
    err = monitor_cap_remote(capref, false, &cap_node->has_descendants);
    cap_destroy(capref);

    // now there are no copies or descendents of this cap on this core
    // remove this cap and any of its descendents from local rcap_db
    do {
        struct cap_db_node * next = cap_node->lock_chain;
        local_db_delete(cap_node);
        cap_node = next;
    } while (cap_node);

    return err;
}
