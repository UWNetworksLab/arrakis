/**
 * \file
 * \brief I/O APIC address space access functions implementation.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_IOAPIC_IOAPIC_IMPL_H
#define LPC_IOAPIC_IOAPIC_IMPL_H

static inline uint32_t lpc_ioapic_ioapic_read_32(lpc_ioapic_t *dev,
                                                 size_t offset)
{
    // Select address via index register
    lpc_ioapic_ind_wr(dev, offset);

    // Return value from window register
    return lpc_ioapic_wdw_rd(dev);
}

static inline uint64_t lpc_ioapic_ioapic_read_64(lpc_ioapic_t *dev,
                                                 size_t offset)
{
    uint64_t ret;

    // Read LSW
    lpc_ioapic_ind_wr(dev, offset);
    ret = lpc_ioapic_wdw_rd(dev);

    // Read MSW
    lpc_ioapic_ind_wr(dev, offset + 1);
    ret |= (uint64_t)lpc_ioapic_wdw_rd(dev) << 32;

    return ret;
}

static inline void lpc_ioapic_ioapic_write_32(lpc_ioapic_t *dev, size_t offset,
                                              uint32_t value)
{
    // Select address via index register
    lpc_ioapic_ind_wr(dev, offset);

    // Write value to window register
    lpc_ioapic_wdw_wr(dev, value);
}

static inline void lpc_ioapic_ioapic_write_64(lpc_ioapic_t *dev, size_t offset,
                                              uint64_t value)
{
    // Write LSW
    lpc_ioapic_ind_wr(dev, offset);
    lpc_ioapic_wdw_wr(dev, value & 0xffffffff);

    // Write MSW
    lpc_ioapic_ind_wr(dev, offset + 1);
    lpc_ioapic_wdw_wr(dev, value >> 32);
}

#endif
