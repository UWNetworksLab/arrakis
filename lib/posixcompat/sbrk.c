/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <unistd.h>
#include <barrelfish/barrelfish.h>

#if __SIZEOF_POINTER__ == 8
//need lot of memory...
#define SBRK_REGION_BYTES (1*256*1024UL * BASE_PAGE_SIZE)
#else // still huge, but slightly more achievable in a 32-bit address space!
#define SBRK_REGION_BYTES (256 * 1024 * 1024)
#endif

void *sbrk(intptr_t increment)
{
    errval_t err;
    size_t orig_offset;

    static void *base;
    static size_t offset = 0;
    static size_t goffset = 0;
    static struct memobj_anon memobj_;
    static struct memobj *memobj = NULL;
    static struct vregion vregion_;
    static struct vregion *vregion = NULL;

    if (!memobj) { // Initialize
        err = vspace_map_anon_nomalloc(&base, &memobj_, &vregion_,
                                       SBRK_REGION_BYTES, NULL,
                                       VREGION_FLAGS_READ_WRITE, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "vspace_map_anon_nomalloc failed");
            return (void *)-1;
        }
        memobj = (struct memobj *) &memobj_;
        vregion = &vregion_;
    }

    if (increment < 0) {
      if (-increment > offset) {
        USER_PANIC("sbrk() called with negative increment beyond offset");
      } else {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
      }
    } else if (increment == 0) {
        return base + offset;
    } else if (offset + increment > SBRK_REGION_BYTES) {
        debug_printf("sbrk() exceeded static region limit of %zu bytes, offset: %zu\n",
                     (size_t)SBRK_REGION_BYTES, offset);
        return (void *)-1;
    } else if (offset + increment <= goffset) {
        orig_offset = offset;
        offset += increment;

        void *ret = base + orig_offset;
        return ret;
    }

    size_t inc_bytes = offset + increment - goffset;
    orig_offset = offset;

    struct capref frame;
    err = frame_alloc(&frame, inc_bytes, &inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "frame_alloc failed");
        return (void *)-1;
    }

    err = memobj->f.fill(memobj, goffset, frame, inc_bytes);
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err, "memobj->f.fill failed");
        cap_destroy(frame);
        return (void *)-1;
    }

    err = memobj->f.pagefault(memobj, vregion, goffset, 0);
    goffset += inc_bytes;
    offset = goffset;
    if (err_is_fail(err)) {
        debug_err(__FILE__, __func__, __LINE__, err,
                  "memobj->f.pagefault failed");
        return (void *)-1;
    }

    void *ret = base + orig_offset;
    return ret;
}
