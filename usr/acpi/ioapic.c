/**
 * \file
 * \brief I/O APIC driver.
 *
 * Barrelfish only supports edge triggered interrupts.
 */

/*
 * Copyright (c) 2007, 2008, 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <mm/mm.h>

#include "ioapic.h"

#include "acpi_debug.h"

/**
 * \brief Initialize I/O APIC.
 *
 * Initializes the I/O APIC pointed to by 'a', with virtual base
 * address 'base', ID 'id' and INTI base 'irqbase'. All interrupts are
 * masked initially and have to be unmasked individually upon use.
 *
 * \param a             Pointer to I/O APIC structure.
 * \param base          Virtual base address for I/O APIC registers.
 * \param id            ID of I/O APIC (from ACPI).
 * \param irqbase       INTI base (from ACPI).
 *
 * \return 0 on success.
 */
errval_t ioapic_init(struct ioapic *a, lvaddr_t base, uint8_t id,
                     uint32_t irqbase)
{
    lpc_ioapic_initialize(&a->dev, (void *)base);

    a->irqbase = irqbase;

    // Write I/O APIC ID
    lpc_ioapic_id_wr(&a->dev, (lpc_ioapic_id_t) { .id = id });

    // Check number of supported IRQs
    a->nintis = lpc_ioapic_ver_rd(&a->dev).mre + 1;
    if (a->nintis == 1) {
        ACPI_DEBUG("Warning: I/O APIC claims only to support a single interrupt!"
                  " This is probably going to break...\n");
    } else {
        ACPI_DEBUG("I/O APIC supports %d interrupts\n", a->nintis);
    }

    // Mask out all interrupts
    for(int i = 0; i < a->nintis; i++) {
        ioapic_toggle_inti(a, i, false);
    }

    return SYS_ERR_OK;
}

void ioapic_toggle_inti(struct ioapic *a, int inti, bool enable)
{
    assert(inti >= 0 && inti < a->nintis);
    lpc_ioapic_redir_tbl_t tbl = lpc_ioapic_redirtbl_rd(&a->dev, inti);
    tbl.mask = enable ? 0 : 1;
    lpc_ioapic_redirtbl_wr(&a->dev, inti, tbl);
}

void ioapic_setup_inti(struct ioapic *a, int inti, lpc_ioapic_redir_tbl_t entry)
{
    assert(inti >= 0 && inti < a->nintis);
    lpc_ioapic_redirtbl_wr(&a->dev, inti, entry);
}

void ioapic_route_inti(struct ioapic *a, int inti, uint8_t vector, uint8_t dest)
{
    assert(inti >= 0 && inti < a->nintis);
    lpc_ioapic_redir_tbl_t tbl = lpc_ioapic_redirtbl_rd(&a->dev, inti);
    tbl.vector = vector;
    tbl.dest = dest;
    lpc_ioapic_redirtbl_wr(&a->dev, inti, tbl);
}
