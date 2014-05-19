/**
 * \file
 * \brief Header file for the driver's part of the PCI memory management
 */

/*
 * Copyright (c) 2007, 2008, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LIB_MEM_H_
#define LIB_MEM_H_

/* XXX: FIXME: this structure is used by both the PCI server and drivers
 *
 * Not all fields are valid for all locations, and many are only
 * filled in after calling map_device() in the driver.
 */
struct device_mem {
    uint8_t type; // 0 = memory BAR, 1 = IO BAR
    void *vaddr;  // assigned by the device driver when calling map_device()
    genpaddr_t paddr; // physical base address of device
    struct capref *phys_cap; // array of phys caps (only if type == 0 and in PCI server)
    struct capref *frame_cap; // array of frame caps
    struct capref io_cap; // IO cap (only valid if type == 1)
    uint8_t bits;    // size of a single cap in bits
    size_t bytes;    // size of entire region in bytes
    uint32_t nr_caps;// number of caps used to map region, length of cap arrays
    /* NB: it should be the case that bytes = (1 << bits) * nr_caps */
    struct memobj *memobj;   // valid after map_device()
    struct vregion *vregion; // valid after map_device()
};

errval_t map_device(struct device_mem *mem);
errval_t map_bars(struct device_mem *bars, int nr_mapped_bars);

#endif // LIB_MEM_H_

