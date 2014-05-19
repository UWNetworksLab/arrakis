/**
 * \file
 * \brief Interrupt management (Local and IOAPICs) and routing
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <acpi.h>
#include <mm/mm.h>

#include <skb/skb.h>
#include <octopus/getset.h>

#include "ioapic.h"
#include "acpi_debug.h"
#include "acpi_shared.h"

//from documentation
#define APIC_BITS 11

/// Assume size of the I/O APIC is one page
#define IOAPIC_PAGE_BITS        BASE_PAGE_BITS
#define IOAPIC_PAGE_SIZE        (1<<IOAPIC_PAGE_BITS)

/// Maximum number of supported I/O APICs
#define IOAPIC_MAX      5

/// Room for all supported I/O APICs
static struct ioapic ioapics[IOAPIC_MAX];

/// Interrupt override table (maps ISA interrupts to GSIs)
#define N_ISA_INTERRUPTS 16
static int interrupt_overrides[N_ISA_INTERRUPTS];

/// I/O APIC redirection table entry template for ISA bus
static lpc_ioapic_redir_tbl_t ioapic_redir_tmpl_isa = {
    .mode = lpc_ioapic_fixed,
    .destmode = lpc_ioapic_physical,
    .polarity = lpc_ioapic_active_high,
    .trigger = lpc_ioapic_edge,
    .mask = 1
};

/// I/O APIC redirection table entry template for PCI bus
static lpc_ioapic_redir_tbl_t ioapic_redir_tmpl_pci = {
    .mode = lpc_ioapic_fixed,
    .destmode = lpc_ioapic_physical,
    .polarity = lpc_ioapic_active_low,
    .trigger = lpc_ioapic_edge, // XXX: Barrelfish cannot handle level
                                // triggered interrupts
    .mask = 1
};

static struct ioapic *find_ioapic(uint32_t gsi)
{
    for(int i = 0; i < IOAPIC_MAX; i++) {
        struct ioapic *a = &ioapics[i];

        if(a->irqbase <= gsi && gsi < a->irqbase + a->nintis) {
            return a;
        }
    }

    return NULL;
}

static errval_t init_one_ioapic(ACPI_MADT_IO_APIC *s)
{
    errval_t            err;
    struct capref       devmem, devframe;
    lvaddr_t            vaddr;
    static int          ioapic_nr = 0;

    assert(ioapic_nr < IOAPIC_MAX);

    // allocate memory backing IOAPIC
    err = mm_realloc_range(&pci_mm_physaddr, IOAPIC_PAGE_BITS, 
                           s->Address, &devmem);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "Failed to allocate I/O APIC register page at 0x%x\n",
                  s->Address);
        return err_push(err, MM_ERR_REALLOC_RANGE);
    }

    /*
    err = devframe_type(&devframe, devmem, BASE_PAGE_BITS);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_DEVFRAME_TYPE);
    }*/

    devframe = devmem;

    // Map registers
    err = vspace_map_one_frame_attr((void**)&vaddr, IOAPIC_PAGE_SIZE, devframe,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        return err_push(err, LIB_ERR_VSPACE_MAP);
    }

    // Initialize driver
    struct ioapic *ioapic = &ioapics[ioapic_nr++];

    err = ioapic_init(ioapic, vaddr, s->Id, s->GlobalIrqBase);
    if (err_is_fail(err)) {
        ACPI_DEBUG("I/O APIC init failed!\n");
        return err_push(err, PCI_ERR_IOAPIC_INIT);
    }

    // Set redirection table entry defaults for managed buses
    for(int i = 0; i < ioapic->nintis; i++) {
        if(s->GlobalIrqBase + i < N_ISA_INTERRUPTS) {
            // ISA interrupts go to GSIs 0 .. 15
            ioapic_setup_inti(ioapic, i, ioapic_redir_tmpl_isa);
        } else {
            // Assume the rest is PCI
            ioapic_setup_inti(ioapic, i, ioapic_redir_tmpl_pci);
        }
    }

    return SYS_ERR_OK;
}

static int init_one_local_apic(ACPI_MADT_LOCAL_APIC *s, lpaddr_t base)
{
    // TODO: Bring up other cores here! But I think we need chips for
    // better integration with other domains booted from init.
    return 0;
}

int init_all_apics(void)
{
    ACPI_STATUS         as;
    ACPI_TABLE_MADT     *madt;
    ACPI_TABLE_HEADER   *ath;
    int                 r;

    // default overrides
    for (int i = 0; i < N_ISA_INTERRUPTS; i++) {
        interrupt_overrides[i] = i;
    }

    // Get the ACPI APIC table (the MADT)
    as = AcpiGetTable("APIC", 1, (ACPI_TABLE_HEADER **)&ath);

    if(ACPI_FAILURE(as)) {
        ACPI_DEBUG("No MADT found in ACPI! Cannot initialize I/O APICs.\n");
        return -1;
    }
    else {
        madt = (ACPI_TABLE_MADT*)ath;
    }

    // Parse main table entries
    ACPI_DEBUG("Local APIC is at 0x%x\n", madt->Address);
    skb_add_fact("memory_region(%" PRIu32 ",%u,%zu, %u,%u).",
                 madt->Address,
                 APIC_BITS, //from documentation
                 ((size_t)1) << APIC_BITS, //from documentation
                 RegionType_LocalAPIC,
                 0);

    if((madt->Flags & ACPI_MADT_PCAT_COMPAT) == ACPI_MADT_MULTIPLE_APIC) {
        ACPI_DEBUG("This system also has dual-8259As.\n");
    }

    // Walk all subtables (after the main table entries)
    void *p = (void *)madt + sizeof(ACPI_TABLE_MADT);
    while(p < (void *)madt + madt->Header.Length) {
        ACPI_SUBTABLE_HEADER *sh = (ACPI_SUBTABLE_HEADER *)p;

        switch(sh->Type) {
        case ACPI_MADT_TYPE_LOCAL_APIC:
            {
                ACPI_MADT_LOCAL_APIC *s = (ACPI_MADT_LOCAL_APIC *)sh;

                ACPI_DEBUG("Found local APIC: CPU = %d, ID = %d, usable = %d\n",
                       s->ProcessorId, s->Id,
                       s->LapicFlags & ACPI_MADT_ENABLED);

                errval_t err = oct_set("hw.apic.%d { cpu_id: %d, id: %d, enabled: %d }",
                                         s->Id, s->ProcessorId, s->Id,
                                         s->LapicFlags & ACPI_MADT_ENABLED);
                assert(err_is_ok(err));

                skb_add_fact("apic(%d,%d,%"PRIu32").",
                       s->ProcessorId, s->Id,
                       s->LapicFlags & ACPI_MADT_ENABLED);

                if(s->LapicFlags & ACPI_MADT_ENABLED) {
                    r = init_one_local_apic(s, madt->Address);
                    assert(r == 0);
                }
            }
            break;

        case ACPI_MADT_TYPE_IO_APIC:
            {
                ACPI_MADT_IO_APIC *s = (ACPI_MADT_IO_APIC *)sh;
                errval_t err;

                ACPI_DEBUG("Found I/O APIC: ID = %d, mem base = 0x%x, "
                       "INTI base = %d\n", s->Id, s->Address, s->GlobalIrqBase);

                skb_add_fact("ioapic(%d,%"PRIu32",%"PRIu32").", s->Id, s->Address, s->GlobalIrqBase);
                skb_add_fact("memory_region(%"PRIu32",%u,%zu, %u,%u).",
                             s->Address,
                             BASE_PAGE_BITS, //as used elswhere in acpi.c
                             ((size_t)1) << BASE_PAGE_BITS, //as used elswhere in acpi.c
                             RegionType_IOAPIC,
                             0);

                err = init_one_ioapic(s);
                if(err_is_fail(err)) {
                    DEBUG_ERR(err, "Unable to initialize I/O APIC (ID = %d)",
                              s->Id);
                    abort();
                }
            }
            break;

        case ACPI_MADT_TYPE_INTERRUPT_OVERRIDE:
            {
                ACPI_MADT_INTERRUPT_OVERRIDE *s =
                    (ACPI_MADT_INTERRUPT_OVERRIDE *)sh;

                ACPI_DEBUG("Found interrupt override: bus = %d, bus_irq = %d, "
                       "GSI = %d, flags = %x\n", s->Bus, s->SourceIrq,
                       s->GlobalIrq, s->IntiFlags);

                skb_add_fact("interrupt_override(%d,%d,%"PRIu32",%d).",
                            s->Bus, s->SourceIrq, s->GlobalIrq, s->IntiFlags);

                // ACPI spec says these are only for ISA interrupts
                assert(s->SourceIrq < N_ISA_INTERRUPTS);

                interrupt_overrides[s->SourceIrq] = s->GlobalIrq;

                if (s->IntiFlags == 0) {
                    break;
                }

                lpc_ioapic_redir_tbl_t entry = ioapic_redir_tmpl_isa;
                struct ioapic *a = find_ioapic(s->GlobalIrq);
                if (a == NULL) {
                    ACPI_DEBUG("Warning: unknown IOAPIC for GSI %d, ignored"
                              " interrupt override flags.\n", s->GlobalIrq);
                    break;
                }

                // Set polarity
                assert((s->IntiFlags & ACPI_MADT_POLARITY_MASK)
                       != ACPI_MADT_POLARITY_RESERVED);

                switch(s->IntiFlags & ACPI_MADT_POLARITY_MASK) {
                case ACPI_MADT_POLARITY_ACTIVE_HIGH:
                    entry.polarity = lpc_ioapic_active_high;
                    break;

                case ACPI_MADT_POLARITY_ACTIVE_LOW:
                    entry.polarity = lpc_ioapic_active_low;
                    break;
                }

                // Set trigger mode
                assert((s->IntiFlags & ACPI_MADT_TRIGGER_MASK)
                       != ACPI_MADT_TRIGGER_RESERVED);

                switch(s->IntiFlags & ACPI_MADT_TRIGGER_MASK) {
                case ACPI_MADT_TRIGGER_EDGE:
                    entry.trigger = lpc_ioapic_edge;
                    break;

                case ACPI_MADT_TRIGGER_LEVEL:
                    // XXX: should be lpc_ioapic_level
                    entry.trigger = lpc_ioapic_edge;
                    break;
                }

                ioapic_setup_inti(a, s->GlobalIrq - a->irqbase, entry);
            }
            break;

        case ACPI_MADT_TYPE_LOCAL_APIC_NMI:
            {
                ACPI_MADT_LOCAL_APIC_NMI *s = (ACPI_MADT_LOCAL_APIC_NMI *)sh;

                ACPI_DEBUG("Found local APIC NMI: CPU ID = %d, flags = %x, "
                       "LINT = %d\n", s->ProcessorId, s->IntiFlags, s->Lint);

                skb_add_fact("apic_nmi(%d,%d,%d).",s->ProcessorId, s->IntiFlags,
                                                   s->Lint);

                ACPI_DEBUG("Ignoring for now.\n");
            }
            break;

        default:
            ACPI_DEBUG("Unknown subtable type %d\n", sh->Type);
            break;
        }

        p += sh->Length;
    }


    /* XXX: Quirk hack for QEMU
     * There is no override for the timer interrupt, although it appears as IRQ2.
     */
    if (strncmp(madt->Header.OemId, "QEMU", 4) == 0
        && interrupt_overrides[0] == 0) {
        interrupt_overrides[0] = 2;
        printf("added missing override from GSI 0 to INTI 2 on QEMU\n");
    }

    return 0;
}

errval_t enable_and_route_interrupt(int gsi, coreid_t dest, int vector)
{
    /* sanity-check vector */
    // XXX: this check matches the use of vectors in the kernel's irq.c
    if (vector < 0 || vector >= (250 - 32)) {
        return PCI_ERR_INVALID_VECTOR;
    }

    /* convert to CPU vector */
    vector += 32;

    /* lookup override table */
    int gsi_mapped;
    if (gsi < N_ISA_INTERRUPTS) {
        gsi_mapped = interrupt_overrides[gsi];
    } else {
        gsi_mapped = gsi;
    }

    /* find the correct IOAPIC */
    struct ioapic *i = find_ioapic(gsi_mapped);
    if (i == NULL) {
        return PCI_ERR_UNKNOWN_GSI;
    }

    // Resolve destination core ID to APIC ID
    char *result = NULL, *str_error = NULL, query[256];
    int32_t int_error = 0;
    int r = snprintf(query, 256, "corename(%d, _, apic(A)), write(A).", dest);
    assert(r >= 0 && r < 256);
    errval_t err = skb_evaluate(query, &result, &str_error, &int_error);
    assert(err_is_ok(err));
    assert(result != NULL);
    free(str_error);

    // If SKB didn't have an answer, we assume we're not done setting up
    // mappings yet. In this case, we can only resolve our own core ID.
    uint8_t dest_apicid;
    if(*result == '\0') {
        if(dest != disp_get_core_id()) {
            USER_PANIC("SKB couldn't resolve core ID and it's not this core's "
                       "ID, giving up.");
        }

        dest_apicid = my_apic_id;
    } else {
        dest_apicid = strtol(result, &str_error, 10);
        assert(*str_error == '\0');
    }
    free(result);

    /* route to the given core */
    int inti = gsi_mapped - i->irqbase;
    ioapic_route_inti(i, inti, vector, dest_apicid);

    ACPI_DEBUG("routing GSI %d -> %d -> INTI %d -> APIC %d (coreid %d) "
              "vector %d\n", gsi, gsi_mapped, inti, dest_apicid, dest, vector);

    /* enable */
    ioapic_toggle_inti(i, inti, true);

    return SYS_ERR_OK;
}
