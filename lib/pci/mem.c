/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <pci/mem.h>

#if 0
#define PCI_DEBUG(x...) printf("pci_client: " x)
#else
#define PCI_DEBUG(x...) ((void)0)
#endif

errval_t map_device(struct device_mem *mem)
{
    errval_t err;

    if(mem->type == 1) { // IO
        return SYS_ERR_OK; // XXX
    }

    PCI_DEBUG("map_device: %lu\n", mem->bytes);

    size_t offset = 0;
    size_t cap_size = mem->bytes / mem->nr_caps;

    err = vspace_map_anon_attr(&mem->vaddr, &mem->memobj, &mem->vregion,
                               mem->bytes, NULL,
                               VREGION_FLAGS_READ_WRITE_NOCACHE);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_anon_attr failed");
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    for (int i = 0; i < mem->nr_caps; i++) {
        PCI_DEBUG("mem: map in cap nr %d\n", i);
        err = mem->memobj->f.fill(mem->memobj, offset, mem->frame_cap[i],
                                  cap_size);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "memobj->f.fill failed");
            return err_push(err, LIB_ERR_MEMOBJ_FILL);
        }
        PCI_DEBUG("offset = %lu\n", offset);
        err = mem->memobj->f.pagefault(mem->memobj, mem->vregion, offset, 0);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "memobj->f.pagefault failed");
            return err_push(err, LIB_ERR_MEMOBJ_PAGEFAULT_HANDLER);
        }
        offset += cap_size;
    }

    return SYS_ERR_OK;
}

errval_t map_bars(struct device_mem *bars, int nr_mapped_bars)
{
    errval_t err;

    for (int i = 0; i < nr_mapped_bars; i++) {
        err = map_device(&(bars[i]));
        if (err_is_fail(err)) {
            return err;
        }
    }

    return SYS_ERR_OK;
}

