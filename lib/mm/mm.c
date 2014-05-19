/**
 * \file
 * \brief Memory manager
 *
 * This code manages memory regions (which may refer to actual RAM, or physical
 * address space, or other spaces) and the capabilities to those regions.
 *
 * The meta-data is structured as a B-tree. Every node has a variable
 * power-of-two-sized number of children (up to a limit fixed at initialisation
 * time), which is stored in the node itself (as "childbits"). Nodes without
 * any children have childbits = -1 (FLAGBITS).
 *
 * The position of a node in the tree exactly determines its size and address
 * relative to the size and address of the region being managed by the
 * allocator (all sizes must be powers of two).
 *
 * A node may be one of four types (see #nodetype):
 *   0. A "dummy" node, which exists structurally in the tree, but for which
 *      we do not have a capability (ie. the region is incomplete).
 *   1. A "chunked" node, for which we have a capability, but which has been
 *      split up into child nodes for smaller allocations.
 *   2. A free node, which is a regular free child node in the tree.
 *   3. An allocated node.
 */

/*
 * Copyright (c) 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <mm/mm.h>
#include <stdio.h>
#include <inttypes.h>

#if 0
bool mm_debug;
# define DEBUG(s, x...) do { if (mm_debug) debug_printf("MM: " s, x); } while(0)
#else
# define DEBUG(s, x...) (void)0
#endif

/* Macros to convert from a size in "bits" (ie. power-of-two) to size in bytes */
#define UNBITS_CA(bits) (((cslot_t)1) << (bits))
#define UNBITS_GENPA(bits) (((genpaddr_t)1) << (bits))
#define FLAGBITS        ((uint8_t)-1)

/// Allocate a new node of given type/size. Does NOT initialise children pointers.
static struct mmnode *new_node(struct mm *mm, enum nodetype type,
                               uint8_t childbits)
{
    assert(childbits == FLAGBITS ||
           (childbits > 0 && childbits <= mm->maxchildbits));

    struct mmnode *node = slab_alloc(&mm->slabs);

    if (node != NULL) {
        node->type = type;
        node->childbits = childbits;
    }

    return node;
}

/// Reduce the number of children of a node by pushing existing children down.
static errval_t resize_node(struct mm *mm, struct mmnode *node,
                            uint8_t newchildbits)
{
    assert(newchildbits != FLAGBITS);
    assert(node->childbits != FLAGBITS);
    assert(newchildbits <= mm->maxchildbits);
    assert(node->childbits > newchildbits);

    DEBUG("resize_node %d -> %d\n", node->childbits, newchildbits);

    /* we want to reduce the number of children, moving the existing ones down
     * for every new child node, we need to construct one intermediate node */
    uint8_t diffchildbits = node->childbits - newchildbits;
    for (cslot_t i = 0; i < UNBITS_CA(newchildbits); i++) {
        struct mmnode *newnode = NULL;
        for (cslot_t j = 0; j < UNBITS_CA(diffchildbits); j++) {
            if (node->children[i * UNBITS_CA(diffchildbits) + j] != NULL) {
                if (newnode == NULL) {
                    newnode = new_node(mm, NodeType_Dummy, diffchildbits);
                    if (newnode == NULL) {
                        return MM_ERR_NEW_NODE;
                    }
                    for (cslot_t k = 0; k < j; k++) {
                        newnode->children[k] = NULL;
                    }
                }
                newnode->children[j] = node->children[i*UNBITS_CA(diffchildbits)+j];
            } else if (newnode != NULL) {
                newnode->children[j] = NULL;
            }
        }
        node->children[i] = newnode;
    }
    node->childbits = newchildbits;

    return SYS_ERR_OK;
}

/// Add a new cap and node to the tree in the appropriate place.
static errval_t add_node(struct mm *mm, genpaddr_t base, uint8_t sizebits,
                         struct mmnode *node, genpaddr_t nodebase,
                         uint8_t nodesizebits, struct mmnode **retnode)
{
    errval_t err;

    assert(base >= nodebase &&
           base + UNBITS_GENPA(sizebits) <= nodebase + UNBITS_GENPA(nodesizebits));

    DEBUG("add_node %" PRIxGENPADDR "-%" PRIxGENPADDR " %" PRIxGENPADDR "-%"
          PRIxGENPADDR " %d %d\n", base, base + UNBITS_GENPA(sizebits),
          nodebase, nodebase + UNBITS_GENPA(nodesizebits), node->type,
          node->childbits);

    /* is it this node? is it included in an existing cap? */
    if ((base == nodebase && sizebits == nodesizebits)
        || node->type != NodeType_Dummy) {
        return MM_ERR_ALREADY_PRESENT;
    }

    /* which child should we recurse into? */
    uint8_t childsizebits = nodesizebits - node->childbits;

    /* is this a non-leaf node? */
    if (node->childbits != FLAGBITS) {
        if (childsizebits < sizebits) {
            /* we need to resize this node to fit ourselves in between */
            childsizebits = sizebits;
            err = resize_node(mm, node, nodesizebits - sizebits);
            if (err_is_fail(err)) {
                return err_push(err, MM_ERR_RESIZE_NODE);
            }
        }

        /* can we recurse into the child node? */
        cslot_t nchild = (base - nodebase) / UNBITS_GENPA(childsizebits);
        assert(nchild < UNBITS_CA(node->childbits));
        if (node->children[nchild] != NULL) {
            return add_node(mm, base, sizebits, node->children[nchild],
                            nodebase + nchild * UNBITS_GENPA(childsizebits),
                            childsizebits, retnode);
        }

        if (childsizebits != sizebits) {
            /* create dummy child here */
            struct mmnode *new = new_node(mm, NodeType_Dummy, FLAGBITS);
            if (new == NULL) {
                return MM_ERR_NEW_NODE;
            }

            /* recalculate for new node*/
            node->children[nchild] = new;
            nodebase += nchild * UNBITS_GENPA(childsizebits);
            childsizebits -= node->childbits;
            nodesizebits -= node->childbits;
            node = new;
        }
    }

    if (node->childbits == FLAGBITS) {
        /* create as many maximum-sized children as necessary */
        while (nodesizebits > sizebits + mm->maxchildbits) {
            node->childbits = mm->maxchildbits;
            childsizebits = nodesizebits - mm->maxchildbits;
            for (cslot_t i = 0; i < UNBITS_CA(node->childbits); i++) {
                node->children[i] = NULL;
            }

            struct mmnode *new = new_node(mm, NodeType_Dummy, FLAGBITS);
            if (new == NULL) {
                return MM_ERR_NEW_NODE;
            }

            /* recurse */
            cslot_t nchild = (base - nodebase) / UNBITS_GENPA(childsizebits);
            node->children[nchild] = new;
            nodebase += nchild * UNBITS_GENPA(childsizebits);
            childsizebits -= mm->maxchildbits;
            nodesizebits -= mm->maxchildbits;
            node = new;
            DEBUG("add_node new dummy node %" PRIuCSLOT " %" PRIxGENPADDR "-%"
                  PRIxGENPADDR "\n",
                  nchild, nodebase, nodebase + UNBITS_GENPA(nodesizebits));
        }

        /* set the size appropriately on the last node */
        node->childbits = nodesizebits - sizebits;
        childsizebits = sizebits;
        for (cslot_t i = 0; i < UNBITS_CA(node->childbits); i++) {
            node->children[i] = NULL;
        }
    }

    /* we've found the right spot. create a real child here */
    cslot_t childslot = (base - nodebase) / UNBITS_GENPA(childsizebits);
    DEBUG("add_node inserting at slot %" PRIuCSLOT "\n", childslot);
    assert(childsizebits == sizebits);
    struct mmnode *new = new_node(mm, NodeType_Free, FLAGBITS);
    node->children[childslot] = new;
    if (new == NULL) {
        return MM_ERR_NEW_NODE;
    }
    assert(retnode != NULL);
    *retnode = new;
    return SYS_ERR_OK;
}

/// Finds an unallocated node at least as big as the given size within a region
static errval_t find_node(struct mm *mm, bool do_realloc, uint8_t sizebits,
                          genpaddr_t minbase, genpaddr_t maxlimit,
                          struct mmnode *node,
                          genpaddr_t nodebase, uint8_t nodesizebits,
                          genpaddr_t *retnodebase, uint8_t *retnodesizebits,
                          struct mmnode **retnode)
{
    DEBUG("find_node %d %d %" PRIxGENPADDR "-%" PRIxGENPADDR " %" PRIxGENPADDR
          "-%" PRIxGENPADDR " %d\n", do_realloc, sizebits, minbase,
          maxlimit, nodebase, nodebase + UNBITS_GENPA(nodesizebits), node->type);

    assert(nodesizebits >= sizebits);
    assert(retnode != NULL);
    errval_t err;

    if (!do_realloc && node->type == NodeType_Allocated) {
        return MM_ERR_ALREADY_ALLOCATED;
    } else if (node->type == NodeType_Free
               || (do_realloc && node->type == NodeType_Allocated)) {
        /* could we allocate within this node */
        if (minbase + UNBITS_GENPA(sizebits) <= nodebase + UNBITS_GENPA(nodesizebits)
            && maxlimit - UNBITS_GENPA(sizebits) >= nodebase) {
            *retnode = node;
            *retnodebase = nodebase;
            *retnodesizebits = nodesizebits;
            return SYS_ERR_OK;
        } else {
            return MM_ERR_NOT_FOUND;
        }
    } else if (do_realloc && node->type == NodeType_Chunked) {
        assert(node->childbits != FLAGBITS);
        /* does this node match our search? */
        if (nodesizebits == sizebits) {
            assert(minbase + UNBITS_GENPA(sizebits) <= nodebase + UNBITS_GENPA(nodesizebits)
                   && maxlimit - UNBITS_GENPA(sizebits) >= nodebase);
            *retnode = node;
            *retnodebase = nodebase;
            *retnodesizebits = nodesizebits;
            return SYS_ERR_OK;
        } else if (nodesizebits - node->childbits < sizebits) {
            /* We have the region in the allocator, but we've split it up too
             * small. We can't satisfy this request without revoking and
             * retyping the existing allocations.
             */
            return MM_ERR_MISSING_CAPS;
        }
    }

    assert(node->childbits != FLAGBITS);

    /* don't try the children if they will be too small */
    if (nodesizebits - node->childbits < sizebits) {
        DEBUG("find_node -> children too small (%u bits)\n",
              nodesizebits - node->childbits);
        return MM_ERR_NOT_FOUND;
    }

    /* find a suitable child node
     * FIXME: this is currently a simple first-fit search */
    cslot_t start = 0, stop = UNBITS_CA(node->childbits);
    if (minbase > nodebase) {
        start = (minbase - nodebase) / UNBITS_GENPA(nodesizebits - node->childbits);
    }
    if (maxlimit < nodebase + UNBITS_GENPA(nodesizebits)) {
        stop = DIVIDE_ROUND_UP(maxlimit - nodebase,
                               UNBITS_GENPA(nodesizebits - node->childbits));
    }
    for (cslot_t i = start; i < stop; i++) {
        if (node->children[i] != NULL) {
            DEBUG("find_node %" PRIxGENPADDR "-%" PRIxGENPADDR " -> trying child %"
                  PRIuCSLOT " (%" PRIxGENPADDR "-%" PRIxGENPADDR ")\n",
                  nodebase, nodebase + UNBITS_GENPA(nodesizebits), i,
                  nodebase + i * UNBITS_GENPA(nodesizebits - node->childbits),
                  nodebase + (i + 1) * UNBITS_GENPA(nodesizebits - node->childbits));
            struct mmnode *n = NULL;
            err = find_node(mm, do_realloc, sizebits, minbase, maxlimit,
                            node->children[i],
                            nodebase + i * UNBITS_GENPA(nodesizebits - node->childbits),
                            nodesizebits - node->childbits,
                            retnodebase, retnodesizebits, &n);
            if (err_is_ok(err) || !(err_no(err) == MM_ERR_NOT_FOUND
                || err_no(err) == MM_ERR_ALREADY_ALLOCATED
                || err_no(err) == MM_ERR_MISSING_CAPS)) {
                if (err_is_ok(err)) {
                    assert(n != NULL);
                    *retnode = n;
                }
                return err;
            }
        }
    }

    DEBUG("find_node %" PRIxGENPADDR "-%" PRIxGENPADDR " -> no suitable children "
          "(tried %" PRIuCSLOT "-%" PRIuCSLOT " %" PRIxGENPADDR "-%" PRIxGENPADDR ")\n",
          nodebase, nodebase + UNBITS_GENPA(nodesizebits), start, stop,
          nodebase + start * UNBITS_GENPA(nodesizebits - node->childbits),
          nodebase + stop * UNBITS_GENPA(nodesizebits - node->childbits));
    return MM_ERR_NOT_FOUND;
}

/**
 * \brief Chunk up a node, returning the chunk including the desired region and size
 *
 * \param mm Memory allocator context
 * \param sizebits Desired chunk size (number of valid bits), must be smaller than node's size
 * \param minbase Base address of desired return chunk
 * \param maxlimit Limit address of desired return chunk
 * \param node Node to chunk up
 * \param nodebase Input: Base address of node, Output: Base address of return chunk
 * \param nodesizebits Input: Size of node, Output: Size of return chunk
 * \param retnode Node to return chunk
 *
 * \return Error status (#SYS_ERR_OK on success).
 */
static errval_t chunk_node(struct mm *mm, uint8_t sizebits,
                           genpaddr_t minbase, genpaddr_t maxlimit,
                           struct mmnode *node,
                           genpaddr_t *nodebase, uint8_t *nodesizebits,
                           struct mmnode **retnode)
{
    errval_t err;

    assert(node->type == NodeType_Free || node->type == NodeType_Allocated);
    assert(node->childbits == FLAGBITS);
    assert(*nodesizebits > sizebits);

    /* split up the source cap into at most maxchildbits sub-caps */
    uint8_t childbits = *nodesizebits - sizebits;
    if (childbits > mm->maxchildbits) {
        childbits = mm->maxchildbits;
    }

    DEBUG("chunk_node %d %" PRIxGENPADDR "-%" PRIxGENPADDR " %" PRIxGENPADDR "-%"
          PRIxGENPADDR " -> %" PRIuCSLOT "\n", sizebits, minbase,
          maxlimit, *nodebase, *nodebase + UNBITS_GENPA(*nodesizebits),
          UNBITS_CA(childbits));

    struct capref cap;
    err = mm->slot_alloc(mm->slot_alloc_inst, UNBITS_CA(childbits), &cap);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_CHUNK_SLOT_ALLOC);
    }

    err = cap_retype(cap, node->cap, mm->objtype, *nodesizebits - childbits);
    if (err_is_fail(err)) {
        // This is only a failure if the node was free. Otherwise,
        // the caller could've deleted the cap already.
        // XXX: I have no way to find out whether cap_retype()
        // failed due to source cap not found. The error stack
        // convolutes the root cause of the error. I'm assuming here
        // that that's the case.
        if(node->type == NodeType_Free) {
            return err_push(err, LIB_ERR_CAP_RETYPE);
        }

        // TODO: Should deallocate the unused slots from mm->slot_alloc()
    }

    /* construct child nodes */
    for (cslot_t i = 0; i < UNBITS_CA(childbits); i++) {
        struct mmnode *new = new_node(mm, node->type, FLAGBITS);
        if (new == NULL) {
            return MM_ERR_NEW_NODE;
        }
        node->children[i] = new;
        new->cap = cap;
        cap.slot++;
    }

    // If configured to delete chunked capabilities, we do so now
    // The slot stays available so we could meld chunks later (NYI)
    if(mm->delete_chunked) {
        err = cap_delete(node->cap);
        // Can fail if node was not free (e.g. deleted already)
        if(err_is_fail(err) && node->type == NodeType_Free) {
            DEBUG_ERR(err, "cap_delete for chunked cap failed?!? Ignoring.");
        }
    }

    node->type = NodeType_Chunked;
    node->childbits = childbits;
    *nodesizebits -= node->childbits;

    /* take the first cap that includes the desired region */
    cslot_t nchild = 0;
    if (minbase > *nodebase) {
        if (*nodesizebits == sizebits) {
            // this is the final size, so it must be wholly within the region!
            nchild = DIVIDE_ROUND_UP(minbase - *nodebase, UNBITS_GENPA(*nodesizebits));
        } else {
            nchild = (minbase - *nodebase) / UNBITS_GENPA(*nodesizebits);
        }
    }
    *nodebase += nchild * UNBITS_GENPA(*nodesizebits);
    DEBUG("-> picked %" PRIuCSLOT " (%" PRIxGENPADDR "-%" PRIxGENPADDR ")\n", nchild,
          *nodebase, *nodebase + UNBITS_GENPA(*nodesizebits));

#ifdef __scc__
    // XXX: Debug in case memory allocation blows up on SCC
    if(minbase + UNBITS_GENPA(sizebits) > *nodebase + UNBITS_GENPA(*nodesizebits)) {
        printf("minbase = 0x%" PRIxGENPADDR ", size = %" PRIuGENPADDR
               ", nodebase = 0x%" PRIxGENPADDR ", nodesize = %" PRIuGENPADDR
               ", left sum = 0x%" PRIxGENPADDR ", right sum = %" PRIxGENPADDR "\n",
               minbase, UNBITS_GENPA(sizebits), *nodebase, UNBITS_GENPA(*nodesizebits),
               minbase + UNBITS_GENPA(sizebits), *nodebase + UNBITS_GENPA(*nodesizebits));
    }
#endif

    assert(minbase + UNBITS_GENPA(sizebits) <= *nodebase + UNBITS_GENPA(*nodesizebits));
    assert(maxlimit - UNBITS_GENPA(sizebits) >= *nodebase);
    assert(retnode != NULL);
    *retnode = node->children[nchild];
    return SYS_ERR_OK;
}

/**
 * \brief Debug printout of the status of all nodes
 *
 * \param mmnode    Struct mmnode to print from
 * \param space     Call with 0, used to track depth for pretty printing
 */
void mm_debug_print(struct mmnode *mmnode, int space)
{
    for(int i = 0; i < space; i++) {
        printf("  ");
    }
    printf("%d. type %d, children %d\n",
           space, mmnode->type, 1<<mmnode->childbits);
    if (mmnode->type == NodeType_Chunked) {
        for(int i = 0; i < (1<<mmnode->childbits); i++) {
            mm_debug_print(mmnode->children[i], space + 1);
        }
    }
}

/**
 * \brief Initialise a memory manager instance
 *
 * \param mm Pointer to memory manager instance, to be filled-in
 * \param objtype Kernel object type to be managed
 * \param base Base address of region to be managed
 * \param sizebits Size (in bits) of region to be managed
 * \param maxchildbits Maximum number of children (in bits) at each node
 * \param slab_refill_func Function to be used to refill slab allocator
 *       If this is NULL, the caller must provide static storage with slab_grow.
 * \param slot_alloc_func Slot allocator function
 * \param slot_alloc_inst Slot allocator opaque instance pointer
 * \param delete_chunked Whether to delete chunked caps
 *
 * \note Setting maxchildbits > 1 saves substantial space, but may lead to the
 * situation where an allocation request cannot be satisfied despite memory
 * being available, because the memory has been chunked up into smaller caps
 * that, while free, cannot be recombined without revoking existing allocations.
 */
errval_t mm_init(struct mm *mm, enum objtype objtype, genpaddr_t base,
                 uint8_t sizebits, uint8_t maxchildbits,
                 slab_refill_func_t slab_refill_func,
                 slot_alloc_t slot_alloc_func, void *slot_alloc_inst,
                 bool delete_chunked)
{
    /* init fields */
    assert(mm != NULL);
    mm->objtype = objtype;
    assert((base & (UNBITS_GENPA(sizebits) - 1)) == 0);
    mm->base = base;
    mm->sizebits = sizebits;
    assert(maxchildbits > 0 && maxchildbits != FLAGBITS);
    mm->maxchildbits = maxchildbits;
    mm->root = NULL;
    mm->slot_alloc = slot_alloc_func;
    mm->slot_alloc_inst = slot_alloc_inst;
    mm->delete_chunked = delete_chunked;

    /* init slab allocator */
    slab_init(&mm->slabs, MM_NODE_SIZE(maxchildbits), slab_refill_func);

    return SYS_ERR_OK;
}

/**
 * \brief Destroy a memory manager instance
 *
 * \param mm Memory manager instance
 */
void mm_destroy(struct mm *mm)
{
    USER_PANIC("NYI");
}

/**
 * \brief Add a new region to the memory manager
 *
 * It is an error if any part of the region has already been added, or the
 * region doesn't fit within the base and size specified for the allocator.
 *
 * \param mm Memory manager instance
 * \param cap Capability to newly-added region
 * \param sizebits Size of region
 * \param base Physical base address of region
 */
errval_t mm_add(struct mm *mm, struct capref cap, uint8_t sizebits, genpaddr_t base)
{
    /* check bounds */
    if (base < mm->base ||
        base + UNBITS_GENPA(sizebits) > mm->base + UNBITS_GENPA(mm->sizebits)) {
        return MM_ERR_OUT_OF_BOUNDS;
    }

    /* check that base is properly aligned to size */
    assert((base & (UNBITS_GENPA(sizebits) - 1)) == 0);

    /* construct root node if we need one */
    if (mm->root == NULL) {
        /* if this cap fills up the whole allocator, we are done */
        if (base == mm->base && sizebits == mm->sizebits) {
            mm->root = new_node(mm, NodeType_Free, FLAGBITS);
            if (mm->root == NULL) {
                return MM_ERR_NEW_NODE;
            }
            mm->root->cap = cap;
            return SYS_ERR_OK;
        } else {
            mm->root = new_node(mm, NodeType_Dummy, FLAGBITS);
            if (mm->root == NULL) {
                return MM_ERR_NEW_NODE;
            }
        }
    }

    struct mmnode *node = NULL;
    errval_t err;
    err = add_node(mm, base, sizebits, mm->root, mm->base, mm->sizebits, &node);
    if (err_is_ok(err)) {
        assert(node != NULL);
        node->cap = cap;
    }
    return err;
}

/**
 * \brief Allocate an arbitrary memory region of a given size
 *
 * \param mm Memory manager instance
 * \param sizebits Size of requested region
 * \param retcap Pointer to capref struct, to be filled-in
 * \param retbase If non-NULL, the base address of the allocated region is
 *                returned here
 */
errval_t mm_alloc(struct mm *mm, uint8_t sizebits, struct capref *retcap,
                  genpaddr_t *retbase)
{
    /* check bounds, before we hit the debug assertion in mm_alloc_range */
    if (sizebits > mm->sizebits) {
        return MM_ERR_NOT_FOUND;
    }

    return mm_alloc_range(mm, sizebits, mm->base,
                          mm->base + UNBITS_GENPA(mm->sizebits), retcap, retbase);
}

/**
 * \brief Allocate memory region of a given size within a given address range
 *
 * If this call succeeds, it must be the case that:
 *     *retbase >= minbase && *retbase + (1UL << sizebits) <= maxlimit
 *
 * \param mm Memory manager instance
 * \param sizebits Size of requested region
 * \param minbase Minimum base address of region to allocate
 * \param maxlimit Maximum limit address of region to allocate
 * \param retcap Pointer to capref struct, to be filled-in
 * \param retbase If non-NULL, the base address of the allocated region is
 *                returned here
 */
errval_t mm_alloc_range(struct mm *mm, uint8_t sizebits, genpaddr_t minbase,
                        genpaddr_t maxlimit, struct capref *retcap,
                        genpaddr_t *retbase)
{
    /* check bounds */
    if(minbase + UNBITS_GENPA(sizebits) > maxlimit) {
        printf("mm_alloc_range: mb %"PRIxGENPADDR" sizebits %x ,  <= max %"PRIxGENPADDR" \n",
            minbase, sizebits, maxlimit);
    }
    assert(minbase + UNBITS_GENPA(sizebits) <= maxlimit);
    if (minbase < mm->base ||
        maxlimit > mm->base + UNBITS_GENPA(mm->sizebits)) {
        return MM_ERR_OUT_OF_BOUNDS;
    }

    if (mm->root == NULL) {
        return MM_ERR_NOT_FOUND; // nothing added
    }

    genpaddr_t nodebase;
    uint8_t nodesizebits;
    struct mmnode *node = NULL;
    errval_t err;

    /* search for closest matching node in the tree */
    err = find_node(mm, false, sizebits, minbase, maxlimit, mm->root, mm->base,
                    mm->sizebits, &nodebase, &nodesizebits, &node);
    if (err_is_fail(err)) {
        return err;
    }

    assert(node != NULL);
    assert(node->type == NodeType_Free);
    assert(nodesizebits >= sizebits);

    /* split up node until it fits */
    while (nodesizebits > sizebits) {
        err = chunk_node(mm, sizebits, minbase, maxlimit, node, &nodebase,
                          &nodesizebits, &node);
        if (err_is_fail(err)) {
            return err;
        }
    }

    assert(nodebase >= minbase && nodebase + UNBITS_GENPA(sizebits) <= maxlimit);
    node->type = NodeType_Allocated;

    assert(retcap != NULL);
    *retcap = node->cap;
    if (retbase != NULL) {
        *retbase = nodebase;
    }

    return SYS_ERR_OK;
}

/**
 * \brief Return cap to a specific region, that may be partially allocated
 *
 * The parameters to this function specify a fixed memory region (base and size)
 * that may overlap one or more already-allocated memory regions. It marks the
 * entire region as allocated, returning a cap to it.
 *
 * \warning It is assumed that the caller knows what they are doing.
 *
 * \param mm Memory manager instance
 * \param sizebits Size of region
 * \param base Base address of region
 * \param retcap Pointer to capref struct, to be filled-in
 */
errval_t mm_realloc_range(struct mm *mm, uint8_t sizebits, genpaddr_t base,
                          struct capref *retcap)
{
    /* check bounds */
    if (base < mm->base ||
        base + UNBITS_GENPA(sizebits) > mm->base + UNBITS_GENPA(mm->sizebits)) {
        return MM_ERR_OUT_OF_BOUNDS;
    }

    /* check that base is properly aligned to size */
    assert((base & (UNBITS_GENPA(sizebits) - 1)) == 0);

    if (mm->root == NULL) {
        return MM_ERR_NOT_FOUND; // nothing added
    }

    genpaddr_t nodebase;
    uint8_t nodesizebits;
    struct mmnode *node = NULL;
    errval_t err;

    /* search for closest matching node in the tree */
    err = find_node(mm, true, sizebits, base, base + UNBITS_GENPA(sizebits),
                    mm->root, mm->base, mm->sizebits, &nodebase, &nodesizebits,
                    &node);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_FIND_NODE);
    }

    assert(node != NULL);
    if (node->type == NodeType_Chunked) {
        assert(nodesizebits == sizebits);
        node->type = NodeType_Allocated;
        /* FIXME: walk child nodes and mark them allocated? or destroy? */
        *retcap = node->cap;
        return SYS_ERR_OK;
    }

    assert(node->type == NodeType_Free || node->type == NodeType_Allocated);
    assert(nodesizebits >= sizebits);

    /* split up node until it fits */
    while (nodesizebits > sizebits) {
        err = chunk_node(mm, sizebits, base, base + UNBITS_GENPA(sizebits), node,
                         &nodebase, &nodesizebits, &node);
        if (err_is_fail(err)) {
            return err_push(err, MM_ERR_CHUNK_NODE);
        }
    }

    assert(nodebase == base && nodesizebits == sizebits);
    node->type = NodeType_Allocated;

    assert(retcap != NULL);
    *retcap = node->cap;

    return SYS_ERR_OK;
}

/**
 * \brief Free an allocated region
 *
 * Marks the region (which must previously have been allocated) as free.
 *
 * \bug The user might not know (or care about) the base address.
 *
 * \param mm Memory manager instance
 * \param cap Cap to re-insert (specify NULL_CAP if delete_chunked == false)
 * \param base Physical base address of region
 * \param sizebits Size of region
 */
errval_t mm_free(struct mm *mm, struct capref cap, genpaddr_t base,
                 uint8_t sizebits)
{
    // find node, then mark it as free
    genpaddr_t nodebase;
    uint8_t nodesizebits;
    struct mmnode *node = NULL;
    errval_t err;

    /* search for closest matching node in the tree */
    err = find_node(mm, true, sizebits, base, base + UNBITS_GENPA(sizebits),
                    mm->root, mm->base, mm->sizebits, &nodebase, &nodesizebits,
                    &node);
    if (err_is_fail(err)) {
        return err_push(err, MM_ERR_NOT_FOUND);
    }

    assert(node != NULL);
    if (node->type != NodeType_Allocated || nodesizebits < sizebits
        || nodebase > base) {
        return MM_ERR_NOT_FOUND;
    }

    /* split up node until it fits */
    // XXX: Is the while-loop still required? chunk_node() should
    // immediately return the right chunk.
    while (nodesizebits > sizebits) {
        err = chunk_node(mm, sizebits, base, base + UNBITS_GENPA(sizebits), node,
                         &nodebase, &nodesizebits, &node);
        if (err_is_fail(err)) {
            return err_push(err, MM_ERR_CHUNK_NODE);
        }
    }

    node->type = NodeType_Free;
    node->cap = cap;

    return SYS_ERR_OK;
}

/**
 * \brief Fills an array with metadata for all free regions
 *
 * The regions returned are marked as allocated.
 *
 * \param mm Memory manager instance
 * \param ret Pointer to return array to be filled-in
 * \param retlen Length of the array, in slots
 *
 * \returns Number of caps that could have been stored in the return array.
 * If this is less than or equal to #retlen, all caps have been returned.
 */
size_t mm_relinquish_all(struct mm *mm, struct mem_cap *ret, size_t retlen)
{
    USER_PANIC("NYI");
    return 0;
}

/**
 * \brief Fills an array with metadata for all free regions within a given range
 *
 * The regions returned are marked as allocated.
 *
 * It must be the case for every returned cap (i) that:
 *     ret[i]->base >= base && ret[i]->base + (1UL << ret[i]->sizebits) <= limit
 *
 * \param mm Memory manager instance
 * \param base Base address of range from which to relinquish memory
 * \param limit Limit address of range from which to relinquish memory
 * \param ret Pointer to return array to be filled-in
 * \param retlen Length of the array, in slots
 *
 * \returns Number of caps that could have been stored in the return array.
 * If this is less than or equal to #retlen, all caps have been returned.
 */
size_t mm_relinquish_range(struct mm *mm, genpaddr_t base, genpaddr_t limit,
                           struct mem_cap *ret, size_t retlen)
{
    USER_PANIC("NYI");
    return 0;
}
