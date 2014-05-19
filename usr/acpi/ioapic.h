/**
 * \file
 * \brief I/O APIC driver.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef IOAPIC_H
#define IOAPIC_H

#include "lpc_ioapic_dev.h"
#include "lpc_ioapic_ioapic_impl.h"

struct ioapic {
    lpc_ioapic_t        dev;
    int                 nintis;
    uint32_t            irqbase;
};

extern uintptr_t my_apic_id;

errval_t ioapic_init(struct ioapic *a, lvaddr_t base, uint8_t id,
                     uint32_t irqbase);
void ioapic_toggle_inti(struct ioapic *a, int inti, bool enable);
void ioapic_setup_inti(struct ioapic *a, int inti,
                       lpc_ioapic_redir_tbl_t entry);
void ioapic_route_inti(struct ioapic *a, int inti, uint8_t vector,
                       uint8_t dest);
errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector);
int init_all_apics(void);

#endif
