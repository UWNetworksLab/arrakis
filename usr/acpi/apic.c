#include <stdio.h>

#include <barrelfish/barrelfish.h>

#include <acpi.h>
#include <mm/mm.h>
#include <skb/skb.h>
#include <octopus/getset.h>

#include "acpi_shared.h"
#include "acpi_debug.h"

enum user_region_type {
    RegionType_LocalAPIC = RegionType_Max,  ///< local APIC start address
    RegionType_IOAPIC                       ///< I/O APIC start address
};

//from documentation
#define APIC_BITS 11

errval_t find_all_apics(void)
{
    ACPI_STATUS         as;
    ACPI_TABLE_MADT     *madt;
    ACPI_TABLE_HEADER   *ath;

    // Get the ACPI APIC table (the MADT)
    as = AcpiGetTable("APIC", 1, (ACPI_TABLE_HEADER **)&ath);
    if(ACPI_FAILURE(as)) {
        ACPI_DEBUG("No MADT found in ACPI! Cannot initialize I/O APICs.\n");
        return ACPI_ERR_NO_MADT_TABLE;
    }

    madt = (ACPI_TABLE_MADT*)ath;

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

            skb_add_fact("apic(%d,%d,%d).",
                   s->ProcessorId, s->Id,
                   s->LapicFlags & ACPI_MADT_ENABLED);

            errval_t err = oct_set("hw.apic.%d { cpu_id: %d, id: %d, enabled: %d }",
                                     s->Id, s->ProcessorId, s->Id,
                                     s->LapicFlags & ACPI_MADT_ENABLED);
            assert(err_is_ok(err));
        }
        break;

        case ACPI_MADT_TYPE_IO_APIC:
        {
            ACPI_MADT_IO_APIC *s = (ACPI_MADT_IO_APIC *)sh;
            ACPI_DEBUG("Found I/O APIC: ID = %d, mem base = 0x%x, "
                   "INTI base = %d\n", s->Id, s->Address, s->GlobalIrqBase);

            skb_add_fact("ioapic(%d,%u,%d).", s->Id, s->Address, s->GlobalIrqBase);
            skb_add_fact("memory_region(%u,%u,%zu, %u,%u).",
                         s->Address,
                         BASE_PAGE_BITS, //as used elswhere in acpi.c
                         ((size_t)1) << BASE_PAGE_BITS, //as used elswhere in acpi.c
                         RegionType_IOAPIC,
                         0);

            errval_t err = oct_mset(SET_SEQUENTIAL,
                                     "hw.ioapic. { id: %d, address: %u, irqbase: %d }",
                                     s->Id, s->Address, s->GlobalIrqBase);
            assert(err_is_ok(err));
        }
        break;

        case ACPI_MADT_TYPE_INTERRUPT_OVERRIDE:
        {
            ACPI_MADT_INTERRUPT_OVERRIDE *s =
                (ACPI_MADT_INTERRUPT_OVERRIDE *)sh;

            ACPI_DEBUG("Found interrupt override: bus = %d, bus_irq = %d, "
                   "GSI = %d, flags = %x\n", s->Bus, s->SourceIrq,
                   s->GlobalIrq, s->IntiFlags);

            skb_add_fact("interrupt_override(%d,%d,%d,%d).",
                        s->Bus, s->SourceIrq, s->GlobalIrq, s->IntiFlags);
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

    return SYS_ERR_OK;
}
