/**
 * \file
 * \brief I/O APIC address space access functions.
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef LPC_IOAPIC_IOAPIC_H
#define LPC_IOAPIC_IOAPIC_H

static inline uint32_t lpc_ioapic_ioapic_read_32(lpc_ioapic_t *dev,
                                                 size_t offset);
static inline uint64_t lpc_ioapic_ioapic_read_64(lpc_ioapic_t *dev,
                                                 size_t offset);
static inline void lpc_ioapic_ioapic_write_32(lpc_ioapic_t *dev, size_t offset,
                                              uint32_t value);
static inline void lpc_ioapic_ioapic_write_64(lpc_ioapic_t *dev, size_t offset,
                                              uint64_t value);

#endif
