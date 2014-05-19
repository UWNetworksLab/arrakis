/**
 * \file  Common operations on remote cap database
 * \brief
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef RCAP_COMMON_H
#define RCAP_COMMON_H

typedef uint16_t version_t;

struct MemType {
    genpaddr_t base;
    uint8_t bits;
};

struct VNodeType {
    genpaddr_t base;
};

union capident_union {
    struct MemType mem_cap;
    struct VNodeType vnode_cap;
    // TODO others
};

enum captype {
    CapIdent_Mem,
    CapIdent_Vnode,
};

// structure used to describe cap's in remote cap db
struct cap_ident {
    enum captype type;
    union capident_union u;
};

// struture saved in cap database
struct cap_db_node;
struct cap_db_node {
    struct capability cap;
    bool has_descendants;
    bool locked;
    coreid_t core_with_lock;
    coremask_t on_cores;
    struct cap_db_node * lock_chain;
};

struct rcap_st;

typedef void(*rcap_ccast_cb_t)(void *);

struct rcap_st {
    rcap_ccast_cb_t cb;
    coremask_t cores_locked;
    errval_t err;
    bool free_at_ccast;
    struct capability capability;
};
    

uint64_t hash_cap(struct capability * cap);

static inline coremask_t get_coremask(coreid_t core)
{
    coremask_t mask;
    memset(mask.bits, 0, sizeof(mask.bits));
    mask.bits[core % _COREMASK_BITS_PER_WORD]
        = (_coremask_word_t)1 << (core / _COREMASK_BITS_PER_WORD);
    return mask;
}

static inline void coremask_or(coremask_t *dest, coremask_t *m1, coremask_t *m2)
{
    for (int i = 0; i < _COREMASK_WORDS; i++) {
        dest->bits[i] = m1->bits[i] | m2->bits[i];
    }
}

#endif
