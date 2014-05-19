/**
 * \file
 * \brief Hacky MMAP support for VFS.
 * \bug The current implementation is a thin layer over an anonymous memobj.
 *      It does not share memory, and does not propagate any updates.
 */

/*
 * Copyright (c) 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/memobj.h>
#include <vfs/mmap.h>

/**
 * \brief Page fault handler
 *
 * \param memobj  The memory object
 * \param region  The associated vregion
 * \param offset  Offset into memory object of the page fault
 * \param type    The fault type
 */
static errval_t pagefault(struct memobj *memobj, struct vregion *vregion,
                          genvaddr_t offset, vm_fault_type_t type)
{
    errval_t err;
    assert(memobj->type == MEMOBJ_VFS);
    struct memobj_vfs *mv = (struct memobj_vfs *)memobj;
    struct memobj_anon *anon = &mv->anon;
    struct vspace *vspace = vregion_get_vspace(vregion);
    struct pmap *pmap     = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);

    assert(vregion_off == 0); // not sure if we handle this correctly

    // Walk the ordered list to find the matching frame, but don't map it yet
    struct memobj_frame_list *walk = anon->frame_list;
    while (walk) {
        if (offset >= walk->offset && offset < walk->offset + walk->size) {
            break;
        }
        walk = walk->next;
    }

    if (walk == NULL) {
        return LIB_ERR_MEMOBJ_WRONG_OFFSET;
    }

    genvaddr_t map_offset = vregion_off + walk->offset;
    size_t nbytes = walk->size;

    // how much do we need to read from the file?
    if (map_offset >= mv->filesize) {
        // nothing
        goto do_map;
    } else if (map_offset + nbytes > mv->filesize) {
        // limit size of read to maximum mapping (rest is zero-filled)
        nbytes = mv->filesize - map_offset;
    }

#if 0
    debug_printf("fault at offset %lx, mapping at %lx-%lx from file data %lx-%lx\n",
                 offset, vregion_base + map_offset,
                 vregion_base + map_offset + walk->size,
                 map_offset + mv->offset,
                 map_offset + mv->offset + nbytes);
#endif

    // map frame writable at temporary location so that we can safely fill it
    void *buf;
    struct memobj *tmp_memobj = NULL;
    struct vregion *tmp_vregion = NULL;
    err = vspace_map_one_frame(&buf, walk->size, walk->frame,
                               &tmp_memobj, &tmp_vregion);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "error setting up temp mapping in mmap pagefault handler\n");
        return err; // XXX
    }

    // seek file handle
    err = vfs_seek(mv->vh, VFS_SEEK_SET, map_offset + mv->offset);
    if (err_is_fail(err)) {
        return err;
    }

    // read contents into frame
    size_t rsize, pos = 0;
    do {
        err = vfs_read(mv->vh, (char *)buf + pos, nbytes - pos, &rsize);
        if (err_is_fail(err)) {
            break;
        }
        pos += rsize;
    } while(rsize > 0 && pos < nbytes);

    // destroy temp mappings
    // FIXME: the API for tearing down mappings is really unclear! is this sufficient?
    err = vregion_destroy(tmp_vregion);
    assert(err_is_ok(err));
    err = memobj_destroy_one_frame(tmp_memobj);
    assert(err_is_ok(err));
    //free(tmp_vregion);
    //free(tmp_memobj);

do_map:
    // map at target address with appropriate flags
    err = pmap->f.map(pmap, vregion_base + map_offset, walk->frame, 0,
                      walk->size, vregion_get_flags(vregion), NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_PMAP_MAP);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Initialize
 *
 * \param memobj  The memory object
 * \param size    Size of the memory region
 * \param flags   Memory object specific flags
 * \param vh      VFS handle for underlying file
 * \param offset  Offset within file to start mapping
 * \param filesize Size of file data to map, anything above this is zero-filled
 */
errval_t memobj_create_vfs(struct memobj_vfs *memobj, size_t size,
                           memobj_flags_t flags, vfs_handle_t vh, off_t offset,
                           size_t filesize)
{
    errval_t err;

    // create anon memobj
    err = memobj_create_anon(&memobj->anon, size, flags);
    if (err_is_fail(err)) {
        return err;
    }

    // replace pagefault handler with our own
    ((struct memobj *)memobj)->f.pagefault = pagefault;

    // reset type
    ((struct memobj *)memobj)->type = MEMOBJ_VFS;

    memobj->vh = vh;
    memobj->offset = offset;
    memobj->filesize = filesize;

    return SYS_ERR_OK;
}

// FIXME: why aren't the destructors instance methods? -AB
errval_t memobj_destroy_vfs(struct memobj *memobj)
{
    USER_PANIC("NYI");
}

// Kludge to push changes in VFS memobj back out to disk
errval_t memobj_flush_vfs(struct memobj *memobj, struct vregion *vregion)
{
    errval_t err;

    assert(memobj->type == MEMOBJ_VFS);

    struct memobj_vfs *mv    = (struct memobj_vfs *)memobj;
    struct vspace *vspace    = vregion_get_vspace(vregion);
    struct pmap *pmap        = vspace_get_pmap(vspace);
    genvaddr_t vregion_base  = vregion_get_base_addr(vregion);
    lvaddr_t vregion_lbase   = vspace_genvaddr_to_lvaddr(vregion_base);
    genvaddr_t vregion_off   = vregion_get_offset(vregion);

    assert(vregion_off == 0); // not sure if we handle this correctly

    /* TODO: mv->size instead of BASE_PAGE_SIZE?*/
    for (genvaddr_t off = 0; off < mv->filesize ; off += BASE_PAGE_SIZE){
        genvaddr_t retvaddr;
        size_t retsize;
        vregion_flags_t retflags;

        // For each page check if it's in memory
        err = pmap->f.lookup(pmap, vregion_base + off, &retvaddr, &retsize,
                             NULL, NULL, &retflags);
        if (err_is_fail(err)) {
            continue; // Page not in memory
#if 0 /* this optimisation may not be correct if flags were changed -AB */
        } else if ((retflags & VREGION_FLAGS_WRITE) == 0) {
            continue; // Not writable
#endif
        }

        //TRACE("Flushing page at address: %lx\n", vregion_base + off);

        // seek file handle
        err = vfs_seek(mv->vh, VFS_SEEK_SET, off + mv->offset);
        if (err_is_fail(err)) {
            return err;
        }

        // write contents to file
        size_t rsize, pos = 0;
        size_t nbytes = mv->filesize - off;
        if (nbytes > BASE_PAGE_SIZE) {
            nbytes = BASE_PAGE_SIZE;
        }

        do {
            err = vfs_write(mv->vh, (char *)vregion_lbase + off + pos,
                            nbytes - pos, &rsize);
            if (err_is_fail(err)) {
                return err;
            }
            pos += rsize;
        } while(rsize > 0 && pos < nbytes);
        assert(pos==nbytes);
    }

    return SYS_ERR_OK;
}

/**
 * \brief Wrapper to create and map a file object, optionally at a fixed address
 *
 * The memory object and vregion are returned so the user can call fill and
 * pagefault on it to create actual mappings.
 */
static errval_t vspace_map_file_internal(genvaddr_t opt_base,
                                         size_t opt_alignment,
                                         size_t size, vregion_flags_t flags,
                                         vfs_handle_t file, off_t offset,
                                         size_t filesize,
                                         struct vregion **ret_vregion,
                                         struct memobj **ret_memobj)
{
    errval_t err1, err2;
    struct memobj *memobj = NULL;
    struct vregion *vregion = NULL;

    // Allocate space
    memobj = malloc(sizeof(struct memobj_vfs));
    if (!memobj) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }
    vregion = malloc(sizeof(struct vregion));
    if (!vregion) {
        err1 = LIB_ERR_MALLOC_FAIL;
        goto error;
    }

    // Create a memobj and vregion
    err1 = memobj_create_vfs((struct memobj_vfs *)memobj, size, 0, file, offset,
                             filesize);
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_MEMOBJ_CREATE_VFS);
        goto error;
    }

    if (opt_base != 0) {
        err1 = vregion_map_fixed(vregion, get_current_vspace(), memobj, 0, size,
                                 opt_base, flags);
    } else if (opt_alignment != 0) {
        err1 = vregion_map_aligned(vregion, get_current_vspace(), memobj, 0, size,
                                   flags, opt_alignment);
    } else {
        err1 = vregion_map(vregion, get_current_vspace(), memobj, 0, size, flags);
    }
    if (err_is_fail(err1)) {
        err1 = err_push(err1, LIB_ERR_VREGION_MAP);
        goto error;
    }

    *ret_vregion = vregion;
    *ret_memobj = memobj;

    return SYS_ERR_OK;

 error:
    if (memobj) {
        err2 = memobj_destroy_vfs(memobj);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "memobj_destroy_anon failed");
        }
        free(memobj);
    }
    if (vregion) {
        err2 = vregion_destroy(vregion);
        if (err_is_fail(err2)) {
            DEBUG_ERR(err2, "vregion_destroy failed");
        }
        free(vregion);
    }
    return err1;
}

errval_t vspace_map_file(size_t size, vregion_flags_t flags,
                         vfs_handle_t file, off_t offset, size_t filesize,
                         struct vregion **ret_vregion,
                         struct memobj **ret_memobj)
{
    return vspace_map_file_internal(0, 0, size, flags, file, offset, filesize,
                                    ret_vregion, ret_memobj);
}

errval_t vspace_map_file_fixed(genvaddr_t base, size_t size,
                               vregion_flags_t flags, vfs_handle_t file,
                               off_t offset, size_t filesize,
                               struct vregion **ret_vregion,
                               struct memobj **ret_memobj)
{
    assert(base != 0);
    return vspace_map_file_internal(base, 0, size, flags, file, offset, filesize,
                                    ret_vregion, ret_memobj);
}

errval_t vspace_map_file_aligned(size_t alignment, size_t size,
                                 vregion_flags_t flags, vfs_handle_t file,
                                 off_t offset, size_t filesize,
                                 struct vregion **ret_vregion,
                                 struct memobj **ret_memobj)
{
    return vspace_map_file_internal(0, alignment, size, flags, file, offset,
                                    filesize, ret_vregion, ret_memobj);
}
