/**
 * \file
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

struct monitor_binding;
struct intermon_binding;

struct lmp_conn_state {
    uintptr_t domain_id;
    struct monitor_binding *domain_binding;
};

static inline errval_t
lmp_conn_alloc(struct lmp_conn_state **con, uintptr_t *con_id)
{
    struct lmp_conn_state *mem = malloc(sizeof(struct lmp_conn_state));
    assert(mem != NULL);

    *con = mem;
    *con_id = (uintptr_t)mem;
    return SYS_ERR_OK;
}

static inline errval_t
lmp_conn_free(uintptr_t con_id)
{
    struct lmp_conn_state *mem = (struct lmp_conn_state*)con_id;
    assert(mem != NULL);

    free(mem);
    return SYS_ERR_OK;
}

static inline struct lmp_conn_state *
lmp_conn_lookup(uintptr_t con_id)
{
    return (struct lmp_conn_state *)con_id;
}

struct remote_conn_state {
    uintptr_t domain_id;
    struct monitor_binding *domain_binding;
    uintptr_t mon_id;
    struct intermon_binding *mon_binding;
    coreid_t core_id;                       // core id of other side of channel
    
    // type-specific data
    enum remote_conn_type { REMOTE_CONN_UMP } type;
    union {
        struct {
            struct capref frame; // shared frame
        } ump;
    } x;
};

static inline errval_t
remote_conn_alloc(struct remote_conn_state **con, uintptr_t *con_id,
                  enum remote_conn_type type)
{
    struct remote_conn_state *mem = malloc(sizeof(struct remote_conn_state));
    assert(mem != NULL);

    *con = mem;
    *con_id = (uintptr_t)mem;
    mem->type = type;

    return SYS_ERR_OK;
}

static inline errval_t
remote_conn_free(uintptr_t con_id)
{
    struct remote_conn_state *mem = (struct remote_conn_state*)con_id;
    assert(mem != NULL);

    free(mem);
    return SYS_ERR_OK;
}

static inline struct remote_conn_state *
remote_conn_lookup(uintptr_t con_id)
{
    return (struct remote_conn_state *)con_id;
}


struct span_state {
    uint8_t core_id;
    struct capref vroot;
    struct monitor_binding *mb;
    uintptr_t domain_id;
};

static inline errval_t
span_state_alloc(struct span_state **state, uintptr_t *id)
{
    struct span_state *mem = malloc(sizeof(struct span_state));
    assert(mem != NULL);

    *state = mem;
    *id = (uintptr_t)mem;
    return SYS_ERR_OK;
}

static inline errval_t
span_state_free(uintptr_t id)
{
    struct span_state *mem = (struct span_state*)id;
    free(mem);
    return SYS_ERR_OK;
}

static inline struct span_state *
span_state_lookup(uintptr_t id)
{
    return (struct span_state *)id;
}
