/**
 * \file
 * \brief Base capability/cnode handling functions.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_CAPABILITIES_H
#define INCLUDEBARRELFISH_CAPABILITIES_H

#include <stdint.h>
#include <sys/cdefs.h>

#include <barrelfish_kpi/types.h>
#include <barrelfish_kpi/capabilities.h>
#include <barrelfish_kpi/dispatcher_shared.h>
#include <barrelfish/invocations_arch.h>

__BEGIN_DECLS

errval_t cnode_create(struct capref *ret_dest, struct cnoderef *cnoderef,
                 cslot_t slots, cslot_t *retslots);
errval_t cnode_create_raw(struct capref dest, struct cnoderef *cnoderef,
                     cslot_t slots, cslot_t *retslots);
errval_t cnode_create_with_guard(struct capref dest, struct cnoderef *cnoderef,
                            cslot_t slots, cslot_t *retslots,
                            uint64_t guard, uint8_t guard_size);
errval_t cnode_create_from_mem(struct capref dest, struct capref src,
                          struct cnoderef *cnoderef, uint8_t slot_bits);

errval_t cap_retype(struct capref dest_start, struct capref src,
               enum objtype new_type, uint8_t size_bits);
errval_t cap_create(struct capref dest, enum objtype type, uint8_t size_bits);
errval_t cap_delete(struct capref cap);
errval_t cap_revoke(struct capref cap);
struct cspace_allocator;
errval_t cap_destroy(struct capref cap);

errval_t vnode_create(struct capref dest, enum objtype type);
errval_t frame_create(struct capref dest, size_t bytes, size_t *retbytes);
errval_t frame_alloc(struct capref *dest, size_t bytes, size_t *retbytes);
errval_t devframe_type(struct capref *dest, struct capref src, uint8_t bits);
errval_t dispatcher_create(struct capref dest);

typedef void (*handler_func_t)(void *);
struct lmp_endpoint;

errval_t endpoint_create(size_t buflen, struct capref *retcap,
                         struct lmp_endpoint **retep);

errval_t idcap_alloc(struct capref *dest);
errval_t idcap_create(struct capref dest);

errval_t cnode_build_cnoderef(struct cnoderef *cnoder, struct capref capr);

/**
 * \brief Mint (Copy changing type-specific parameters) a capability
 *
 * \param dest    Location of destination slot, which must be empty
 * \param src     Location of source slot
 * \param param1  Type-specific parameter 1
 * \param param2  Type-specific parameter 2
 *
 * Consult the Barrelfish Kernel API Specification for the meaning of the
 * type-specific parameters.
 */
static inline errval_t
cap_mint(struct capref dest, struct capref src, uint64_t param1,
         uint64_t param2)
{
    uint8_t dcn_vbits = get_cnode_valid_bits(dest);
    capaddr_t dcn_addr = get_cnode_addr(dest);
    uint8_t scp_vbits = get_cap_valid_bits(src);
    capaddr_t scp_addr = get_cap_addr(src) >> (CPTR_BITS - scp_vbits);

    return invoke_cnode_mint(cap_root, dcn_addr, dest.slot, scp_addr, dcn_vbits,
                             scp_vbits, param1, param2);
}

/**
 * \brief Perform mapping operation in kernel by minting a cap to a VNode
 *
 * \param dest destination VNode cap
 * \param src  source Frame cap
 * \param slot slot in destination VNode
 * \param attr Architecture-specific page (table) attributes
 * \param off Offset from source frame to map (must be page-aligned)
 */
static inline errval_t
vnode_map(struct capref dest, struct capref src, capaddr_t slot,
          uint64_t attr, uint64_t off, uint64_t pte_count)
{
    uint8_t svbits = get_cap_valid_bits(src);
    capaddr_t saddr = get_cap_addr(src) >> (CPTR_BITS - svbits);

    return invoke_vnode_map(dest, slot, saddr, svbits, attr, off, pte_count);
}

static inline errval_t vnode_unmap(struct capref pgtl, struct capref mapping, size_t entry, size_t num_pages)
{
    uint8_t bits = get_cap_valid_bits(mapping);
    capaddr_t mapping_addr = get_cap_addr(mapping) >> (CPTR_BITS - bits);

    return invoke_vnode_unmap(pgtl, mapping_addr, bits, entry, num_pages);
}

/**
 * \brief Copy a capability between slots in CSpace
 *
 * \param dest    Location of destination slot, which must be empty
 * \param src     Location of source capability
 */
static inline errval_t cap_copy(struct capref dest, struct capref src)
{
    errval_t err;
    uint8_t dcn_vbits = get_cnode_valid_bits(dest);
    capaddr_t dcn_addr = get_cnode_addr(dest);
    uint8_t scp_vbits = get_cap_valid_bits(src);
    capaddr_t scp_addr = get_cap_addr(src) >> (CPTR_BITS - scp_vbits);

    err = invoke_cnode_copy(cap_root, dcn_addr, dest.slot, scp_addr, dcn_vbits,
                            scp_vbits);
    return err;
}

__END_DECLS

#endif //INCLUDEBARRELFISH_CAPABILITIES_H
