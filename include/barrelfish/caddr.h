/**
 * \file
 * \brief Inline functions to allow manipulation of raw capability addresses
 *
 * This file is not part of the standard includes, because most user code should
 * treat #capref as an opaque value.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef INCLUDEBARRELFISH_CADDR_H
#define INCLUDEBARRELFISH_CADDR_H

#include <stdbool.h>
#include <sys/cdefs.h>

#include <barrelfish_kpi/types.h>

__BEGIN_DECLS

#include <stdbool.h>

/**
 * \brief User-level representation of a CNode, its CSpace address and size
 */
struct cnoderef {
    capaddr_t address;        ///< Base address of CNode in CSpace
    uint8_t address_bits;   ///< Number of valid bits in base address
    uint8_t size_bits;      ///< Number of slots in the CNode as a power of 2
    uint8_t guard_size;     ///< Guard size of the CNode
} __attribute__((packed));

#define NULL_CNODE (struct cnoderef){ /*address*/ 0, /*address_bits*/ 0, \
                                      /*size_bits*/ 0, /*guard_size*/ 0 }

/**
 * \brief User-level representation of a capability and its CSpace address
 */

struct capref {
    struct cnoderef cnode;    ///< CNode this cap resides in
    capaddr_t slot;               ///< Slot number within CNode
};

#define NULL_CAP (struct capref){ /*cnode*/ NULL_CNODE, /*slot*/ 0 }

static inline bool capref_is_null(struct capref capref)
{
    return capref.cnode.address == 0 && capref.cnode.address_bits == 0;
}

/* well-known cnodes */
extern struct cnoderef cnode_root, cnode_task, cnode_base,
    cnode_super, cnode_page, cnode_module;

/* well-known capabilities */
extern struct capref cap_root, cap_monitorep, cap_irq, cap_io, cap_dispatcher,
    cap_selfep, cap_kernel, cap_initep, cap_perfmon, cap_dispframe, cap_sessionid, cap_vroot;

/**
 * \brief Returns the number of valid bits in the CSpace address of a cap
 */
static inline uint8_t get_cap_valid_bits(struct capref cap)
{
    uint8_t sum = cap.cnode.address_bits + cap.cnode.guard_size +
        cap.cnode.size_bits;
    if (sum > CPTR_BITS) {
        return sum % CPTR_BITS;
    } else {
        return sum;
    }
}

/**
 * \brief Returns the CSpace address of a cap
 */
// XXX: workaround for an inlining bug in gcc 4.4.1 as shipped with ubuntu 9.10
// XXX: bug still present in 4.4.3
#if defined(__GNUC__) \
    && __GNUC__ == 4 && __GNUC_MINOR__ == 4 && __GNUC_PATCHLEVEL__ <= 3
static __attribute__((noinline)) capaddr_t get_cap_addr(struct capref cap)
#else
static inline capaddr_t get_cap_addr(struct capref cap)
#endif
{
    uint8_t vbits = get_cap_valid_bits(cap);
    if (cap.cnode.address_bits == CPTR_BITS) { // special case for root
        return cap.slot << (CPTR_BITS - vbits);
    } else {
        return cap.cnode.address | (cap.slot << (CPTR_BITS - vbits));
    }
}

/**
 * \brief Returns the number of valid bits in the CSpace address of the CNode
 *        containing the given cap
 */
static inline uint8_t get_cnode_valid_bits(struct capref cap)
{
    return cap.cnode.address_bits;
}

/**
 * \brief Returns the CSpace address of the CNode containing the given cap
 *
 * Returns the valid bits of the address only, in the least significant bits
 * of the result. This is the format needed for CNode invocation parameters.
 */
static inline capaddr_t get_cnode_addr(struct capref cap)
{
    return cap.cnode.address >> (CPTR_BITS - cap.cnode.address_bits);
}

/**
 * \brief Compare two cnoderefs
 *
 * Two cnoderefs are equal if they have the same base address,
 * same number of valid bits and the same guard_size.
 */
static inline bool cnodecmp(struct cnoderef c1, struct cnoderef c2)
{
    return ((c1.address == c2.address) &&
            (c1.address_bits == c2.address_bits) &&
            (c1.guard_size == c2.guard_size));
}

/**
 * \brief Compare two caprefs
 *
 * Two caprefs are equal if they have the same cnoderef and the same
 * slot.
 */
static inline bool capcmp(struct capref c1, struct capref c2)
{
    return (c1.slot == c2.slot) && cnodecmp(c1.cnode, c2.cnode);
}

/**
 * \brief Creates a new #cnoderef struct, performing address calculations.
 */
static inline struct cnoderef build_cnoderef(struct capref cap,
                                             uint8_t size_bits)
{
    struct cnoderef ret;
    ret.address      = get_cap_addr(cap);
    ret.address_bits = (cap.cnode.address_bits + cap.cnode.guard_size +
                            cap.cnode.size_bits) % CPTR_BITS;
    ret.size_bits    = size_bits;
    ret.guard_size   = 0; // XXX
    return ret;
}

__END_DECLS

#endif
