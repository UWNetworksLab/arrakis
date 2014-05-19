/**
 * \file
 * \brief Pmap code common for x86 archs
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/pmap.h>
#include "target/x86/pmap_x86.h"

/*
 * The serialisation format is depressingly ad-hoc, and assumes a depth-first
 * walk of the tree. Each vnode is encoded as an entry in an array.
 *
 * We abuse the slot of the first entry, which is the root and thus is always
 * zero, to store the number of entries in the array.
 */
struct serial_entry {
    uint16_t entry;     ///< Entry #
    uint8_t depth;      ///< Depth of this node (0 = root)
    cslot_t slot;       ///< Slot number (in page cnode) of vnode cap
};

static errval_t serialise_tree(int depth, struct vnode *v,
                               struct serial_entry *out,
                               size_t outlen, size_t *outpos)
{
    assert(v != NULL);
    errval_t err;

    // don't serialise leaf pages (yet!)
    if (!v->is_vnode) {
        return SYS_ERR_OK;
    }

    if (*outpos >= outlen) {
        return LIB_ERR_SERIALISE_BUFOVERFLOW;
    }

    // serialise this node
    out[(*outpos)++] = (struct serial_entry) {
        .depth = depth,
        .entry = v->entry,
        .slot = v->u.vnode.cap.slot,
    };

    // depth-first walk
    for (struct vnode *c = v->u.vnode.children; c != NULL; c = c->next) {
        err = serialise_tree(depth + 1, c, out, outlen, outpos);
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

/**
 * \brief Serialise vtree to a flat structure, for passing to another process
 *
 * This is used by spawn_vspace to communicate the vnode capabilities to the child.
 */
errval_t pmap_x86_serialise(struct pmap *pmap, void *buf, size_t buflen)
{
    errval_t err;
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;

    // XXX: check alignment of buffer
    assert((uintptr_t)buf % sizeof(uintptr_t) == 0);

    struct serial_entry *out = buf;
    size_t outlen = buflen / sizeof(struct serial_entry);
    size_t outpos = 0;

    err = serialise_tree(0, &pmapx->root, out, outlen, &outpos);
    if (err_is_ok(err)) {
        // store length in first entry's slot number
        assert(out[0].slot == 0);
        out[0].slot = outpos;
    }

    return err;
}

static errval_t deserialise_tree(struct pmap *pmap, struct serial_entry **in,
                                 size_t *inlen, int depth, struct vnode *parent)
{
    errval_t err;
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;

    if (*inlen == 0) {
        return SYS_ERR_OK;
    }

    while (*inlen > 0 && (*in)->depth == depth) {
        // ensure slab allocator has sufficient space
        err = pmapx->refill_slabs(pmapx);
        if (err_is_fail(err)) {
            return err_push(err, LIB_ERR_SLAB_REFILL);
        }

        // allocate storage for the new vnode
        struct vnode *n = slab_alloc(&pmapx->slab);
        assert(n != NULL);

        // populate it and append to parent's list of children
        n->is_vnode  = true;
        n->entry     = (*in)->entry;
        n->u.vnode.cap.cnode = cnode_page;
        n->u.vnode.cap.slot  = (*in)->slot;
        n->u.vnode.children  = NULL;
        n->next      = parent->u.vnode.children;
        parent->u.vnode.children = n;

        (*in)++;
        (*inlen)--;

        // is next entry a child of the last node?
        if (*inlen > 0 && (*in)->depth > depth) {
            assert((*in)->depth == depth + 1); // depth-first, no missing nodes
            err = deserialise_tree(pmap, in, inlen, depth + 1, n);
            if (err_is_fail(err)) {
                return err;
            }
        }
    }

    assert((*in)->depth < depth);
    return SYS_ERR_OK;
}

/**
 * \brief Deserialise vtree from a flat structure, for importing from another process
 *
 * This is used in a newly-spawned child
 */
errval_t pmap_x86_deserialise(struct pmap *pmap, void *buf, size_t buflen)
{
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;
    errval_t err;

    // XXX: check alignment of buffer
    assert((uintptr_t)buf % sizeof(uintptr_t) == 0);

    // extract length and sanity-check
    struct serial_entry *in = buf;
    assert(buflen > sizeof(struct serial_entry));
    size_t inlen = in[0].slot;
    assert(inlen * sizeof(struct serial_entry) <= buflen);
    in++;
    inlen--;

    err = deserialise_tree(pmap, &in, &inlen, 1, &pmapx->root);
    if (err_is_ok(err)) {
        // XXX: now that we know where our vnodes are, we can support mappings
        // in the bottom of the address space. However, we still don't know
        // exactly where our text and data are mapped (because we don't yet
        // serialise vregions or memobjs), so instead we pad _end.
        extern char _end;
        pmapx->min_mappable_va = ROUND_UP((lvaddr_t)&_end, 64 * 1024)
                                   + 64 * 1024;
    }

    return err;
}



/**
 * \brief Determine a suitable address for a given memory object
 *
 * \param pmap    The pmap object
 * \param memobj  The memory object to determine the address for
 * \param alignment Minimum alignment
 * \param retvaddr Pointer to return the determined address
 *
 * Relies on vspace.c code maintaining an ordered list of vregions
 */
errval_t pmap_x86_determine_addr(struct pmap *pmap, struct memobj *memobj,
                                 size_t alignment, genvaddr_t *retvaddr)
{
    struct pmap_x86 *pmapx = (struct pmap_x86 *)pmap;
    genvaddr_t vaddr;

    struct vregion *walk = pmap->vspace->head;
    assert(walk != NULL); // assume there's always at least one existing entry

    if (alignment == 0) {
        alignment = BASE_PAGE_SIZE;
    } else {
        alignment = ROUND_UP(alignment, BASE_PAGE_SIZE);
    }
    size_t size = ROUND_UP(memobj->size, alignment);

    // if there's space before the first object, map it there
    genvaddr_t minva = ROUND_UP(pmapx->min_mappable_va, alignment);
    if (minva + size <= vregion_get_base_addr(walk)) {
        vaddr = minva;
        goto out;
    }

    while (walk->next) { // Try to insert between existing mappings
        genvaddr_t walk_base = vregion_get_base_addr(walk);
        genvaddr_t walk_size = ROUND_UP(vregion_get_size(walk), BASE_PAGE_SIZE);
        genvaddr_t walk_end = ROUND_UP(walk_base + walk_size, alignment);
        genvaddr_t next_base = vregion_get_base_addr(walk->next);

        // sanity-check for page alignment
        assert(walk_base % BASE_PAGE_SIZE == 0);
        assert(next_base % BASE_PAGE_SIZE == 0);

        if (next_base > walk_end + size) {
            vaddr = walk_end;
            goto out;
        }

        walk = walk->next;
    }

    // Place beyond last mapping, with alignment
    vaddr = ROUND_UP((vregion_get_base_addr(walk)
                      + ROUND_UP(vregion_get_size(walk), BASE_PAGE_SIZE)),
                     alignment);
 
 out:
    // Ensure that we haven't run out of address space
    if (vaddr + memobj->size > pmapx->max_mappable_va) {
        return LIB_ERR_OUT_OF_VIRTUAL_ADDR;
    }

    assert(retvaddr != NULL);
    *retvaddr = vaddr;

    return SYS_ERR_OK;
}
