/** \file
 *  \brief Inter-core capability database, centralized server
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include "monitor.h"
#include <collections/hash_table.h>

#define BSP_CORE_MASK 0x1

static hash_table * bsp_rcap_db; // only created on the bsp monitor


/*------------------------- BSP database maintenance code -------------------*/

static errval_t _bsp_db_add(uint64_t key, struct capability * cap, bool locked,
                         coreid_t locking_core, bool has_descendants) 
{
    assert(bsp_monitor);

    struct cap_db_node * cap_node = malloc(sizeof(struct cap_db_node));
    assert (cap_node);
    cap_node->cap   = *cap;
    cap_node->has_descendants = has_descendants;
    cap_node->locked  = locked;
    cap_node->core_with_lock = locking_core;

    hash_insert(bsp_rcap_db, key, cap_node);

    return SYS_ERR_OK;
}

static errval_t bsp_db_add(struct capability *cap, bool locked,
                           coreid_t locking_core, bool has_descendants) 
{
    assert(bsp_monitor);

    uint64_t key = hash_cap(cap);
    return _bsp_db_add(key, cap, locked, locking_core, has_descendants);
}

static struct cap_db_node * bsp_db_get(struct capability * cap) 
{
    assert(bsp_monitor);

    uint64_t key = hash_cap(cap);
    return hash_find(bsp_rcap_db, key);
}

static errval_t bsp_db_lock(struct capability * cap, coreid_t core) 
{
    struct cap_db_node * cap_node = bsp_db_get(cap);
    assert(cap_node);
    if (cap_node->locked) {
        printf("%d already locked\n", cap_node->core_with_lock);
        // wait for in-progress cap operation to complete first
        return MON_ERR_REMOTE_CAP_RETRY;
    }
    
    cap_node->locked = true;
    cap_node->core_with_lock = core;
    return SYS_ERR_OK;
}

static void bsp_db_unlock(struct capability *cap, coreid_t core) 
{
    struct cap_db_node * cap_node = bsp_db_get(cap);
    assert (cap_node && cap_node->locked && cap_node->core_with_lock == core);

    cap_node->locked = false;
    cap_node->core_with_lock = -1;
}

static errval_t bsp_db_retyped(struct capability *cap, bool has_descendents, 
                                coreid_t from_core)
 {
    struct cap_db_node * cap_node = bsp_db_get(cap);
    assert(cap_node);
    cap_node->has_descendants = has_descendents;
    bsp_db_unlock(cap, from_core);
    return SYS_ERR_OK;
}

/*--------------------------------- Rcap_db API  ----------------------------*/

errval_t rcap_db_init(void)
{
    if (bsp_monitor) {
        hash_create(&bsp_rcap_db, free);
    }
    return SYS_ERR_OK;
}

bool rcap_db_exists(struct capability *cap) {
    if (bsp_monitor) {
        return bsp_db_get(cap) != NULL;
    } else {
        return false;  // don't know without asking bsp core, just add anyway
    }
}

errval_t rcap_db_add(struct capability * cap, bool has_desc)
{
    if (bsp_monitor) {
        // directly interact with db
        return bsp_db_add(cap, false, -1, has_desc);
    } else {
        // send update to bsp core
        return route_rcap_send_details(cap, BSP_CORE_MASK, has_desc);
    }
}

static bool curr_req_has_desc;
static uint64_t curr_req_hash;

errval_t rcap_db_get_info(struct capability *cap, bool * has_desc, 
                          coremask_t *on_cores)
{
    if (bsp_monitor) {
        struct cap_db_node * cap_node = bsp_db_get(cap);
        assert(cap_node);
        *has_desc = cap_node->has_descendants;
        return SYS_ERR_OK;
    } else {
        // XXX hacky, but done to avoid deadlock (rcap_central should only 
        // be used as a point of comparion anyway, not for production)
        if (curr_req_hash == hash_cap(cap)) {
            assert(curr_req_hash == hash_cap(cap));
            *has_desc = curr_req_has_desc;  
        }
        return SYS_ERR_OK;
    }
}

errval_t rcap_db_remote_recv_details(struct capability * cap, 
                                     coreid_t from_core, bool has_desc)
{
    if (bsp_monitor) {
        // other core sent us details on a new remote cap
        if (!rcap_db_exists(cap)) {
            return rcap_db_add(cap, has_desc);
        }
    } else {
        // reply from bsp core for a request we made
        curr_req_hash     = hash_cap(cap);
        curr_req_has_desc = has_desc;
    }
    return SYS_ERR_OK;
}

errval_t rcap_db_remote_details_req(struct capability * cap, 
                                    coreid_t from_core)
{
    assert(bsp_monitor);
    struct cap_db_node * cap_node = bsp_db_get(cap);
    assert(cap_node);

    return route_rcap_send_details(cap, get_coremask(from_core), 
                                   cap_node->has_descendants);
}

errval_t rcap_db_update_on_recv (struct capability * cap, bool has_desc,
                                 coremask_t on_cores, coreid_t from_core) {
    if (bsp_monitor) {
        bsp_db_unlock(cap, from_core);
        return SYS_ERR_OK;
    } else {
        return route_rcap_unlock(cap, BSP_CORE_MASK, from_core, false);
    }
}


errval_t rcap_db_acquire_lock(struct capability *cap, struct rcap_st * st)
{
    errval_t err;
    if (bsp_monitor) {
        // directly interact with db
        err = bsp_db_lock(cap, my_core_id);
        // and callback immediatly
        if (err_is_ok(err)) {
            assert (st->cb);
            st->err = err;
            st->cb(st);
        }
    } else {
        err = route_rcap_lock_req(cap, BSP_CORE_MASK, my_core_id, st, false);
    }   
    return err;
}

errval_t rcap_db_remote_lock_req(struct capability *cap, coreid_t from_core, 
                                 recordid_t ccast_recordid) 
{
    assert(bsp_monitor);
    errval_t reply_err = bsp_db_lock(cap, from_core);

    bool has_desc;
    coremask_t on_cores;
    errval_t err = rcap_db_get_info(cap, &has_desc, &on_cores);
    assert(err_is_ok(err));
        
    return route_rcap_lock_reply(reply_err, 
                                 err_is_ok(reply_err) ? BSP_CORE_MASK : 0,
                                 has_desc, ccast_recordid);
}

errval_t rcap_db_release_lock(struct capability *cap, coremask_t to_cores)
{
    errval_t err;

    if (bsp_monitor) {
        // directly interact with db
        bsp_db_unlock(cap, my_core_id);
        err = SYS_ERR_OK;
    } else {
        err = route_rcap_unlock(cap, BSP_CORE_MASK, my_core_id, false);
    }
        
    return err;   
}

errval_t rcap_db_remote_unlock(struct capability *cap, coreid_t from_core)
{
    assert(bsp_monitor);
    bsp_db_unlock(cap, from_core);
    return SYS_ERR_OK;
}

errval_t rcap_db_acquire_recursive_lock(struct capability *cap,
                                        struct rcap_st * st)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}
            
errval_t rcap_db_remote_recursive_lock_req(struct capability *cap,
                                           coreid_t from_core, 
                                           recordid_t ccast_recordid)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_release_recursive_lock(struct capability *cap, 
                                        coremask_t to_cores)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_remote_recursive_unlock(struct capability *cap, 
                                         coreid_t from_core)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_remote_new_core(struct capability * cap, coreid_t send_core, 
                                 coreid_t recv_core)
{
    USER_PANIC("Should not be called in rcap_central configuration");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_delete (struct capability * cap)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_remote_delete (struct capability * cap, coreid_t from_core)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_retype(struct capability * cap, bool has_descendents)
{
    if (bsp_monitor) {
        return bsp_db_retyped(cap, has_descendents, my_core_id);
    } else {
        return route_rcap_retype(cap, has_descendents, BSP_CORE_MASK);
    }
}

errval_t rcap_db_remote_retype (struct capability * cap, bool has_descendents,
                                coreid_t from_core)
{
    assert(bsp_monitor);
    return bsp_db_retyped(cap, has_descendents, from_core);
}

errval_t rcap_db_revoke(struct capability * cap)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}

errval_t rcap_db_remote_revoke (struct capability * cap, coreid_t from_core)
{
    USER_PANIC("NYI");
    return LIB_ERR_NOT_IMPLEMENTED;
}
