/**
 * \file
 * \brief ACPI management
 */

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
#include <barrelfish/nameservice_client.h>
#include <barrelfish_kpi/types.h>
#include <acpi.h>
#include <mm/mm.h>
#include <octopus/getset.h>
#include <octopus/barrier.h>
#include <skb/skb.h>
#include <pci/confspace/pci_confspace.h>
#include "acpi_shared.h"
#include "acpi_debug.h"
#include "ioapic.h"
#include "intel_vtd.h"

struct pci_resources {
    uint8_t minbus, maxbus;
    lpaddr_t minmem, maxmem;
    struct pci_address addr;
};

struct memrange {
    lpaddr_t min;
    lpaddr_t limit;
};

#define MAX_RESERVED_MEM_REGIONS 32
// Hack: reserved memory regions (eg. PCIe config space mapping)
static struct memrange reserved_memory[MAX_RESERVED_MEM_REGIONS];
static int n_reserved_memory_regions;

static ACPI_STATUS pci_resource_walker(ACPI_RESOURCE *resource, void *context)
{
    struct pci_resources *ret = context;
//    uint64_t granularity, min, max, translationoffset, addrlength;
    lpaddr_t granularity, min, max, translationoffset, addrlength;

    switch (resource->Type) {
    case ACPI_RESOURCE_TYPE_ADDRESS16:
        granularity = resource->Data.Address16.Granularity;
        min = resource->Data.Address16.Minimum;
        max = resource->Data.Address16.Maximum;
        translationoffset = resource->Data.Address16.TranslationOffset;
        addrlength = resource->Data.Address16.AddressLength;
        break;

    case ACPI_RESOURCE_TYPE_ADDRESS32:
        granularity = resource->Data.Address32.Granularity;
        min = resource->Data.Address32.Minimum;
        max = resource->Data.Address32.Maximum;
        translationoffset = resource->Data.Address32.TranslationOffset;
        addrlength = resource->Data.Address32.AddressLength;
        break;

    case ACPI_RESOURCE_TYPE_ADDRESS64:
        granularity = resource->Data.Address64.Granularity;
        min = resource->Data.Address64.Minimum;
        max = resource->Data.Address64.Maximum;
        translationoffset = resource->Data.Address64.TranslationOffset;
        addrlength = resource->Data.Address32.AddressLength;
        break;

    default:
        return AE_OK;
    }

    /* Ignore bogus entries with zero length */
    if (addrlength == 0) {
        ACPI_DEBUG("Warning: ignoring zero-length address resource\n");
        return AE_OK;
    }

    /* TODO: handle non-fixed regions. Does anything other than QEMU do this? */
    if (resource->Data.Address.MinAddressFixed != ACPI_ADDRESS_FIXED ||
        resource->Data.Address.MaxAddressFixed != ACPI_ADDRESS_FIXED) {
        ACPI_DEBUG("Warning: Treating non-fixed address range resource as fixed\n");
    }

    switch (resource->Data.Address.ResourceType) {
    case ACPI_BUS_NUMBER_RANGE:
        assert(max > 0);
        assert(ret->maxbus == 0); /* shouldn't have more than one of these */
        ret->minbus = min;
        ret->maxbus = max;
        break;

    case ACPI_MEMORY_RANGE:
        ACPI_DEBUG("PCI mem range %lx-%lx granularity 0x%lx translation offset "
                  " 0x%lx length 0x%lx prodcons %u decode %u writeprot %u"
                  " caching %u rangetype %u translation %u\n", min, max,
                  granularity, translationoffset, addrlength,
                  resource->Data.Address.ProducerConsumer,
                  resource->Data.Address.Decode,
                  resource->Data.Address.Info.Mem.WriteProtect,
                  resource->Data.Address.Info.Mem.Caching,
                  resource->Data.Address.Info.Mem.RangeType,
                  resource->Data.Address.Info.Mem.Translation);
/*
        // check for overlaps with reserved memory regions
        for (int i = 0; i < n_reserved_memory_regions; i++) {
            struct memrange *range = &reserved_memory[i];
            if (min < range->limit && max >= range->min) {
                if (min < range->limit && min >= range->min) {
                    // overlaps with min: take top part
                    min = range->limit;
                } else if (max - range->limit > range->min - min) {
                    // take top part
                    min = range->limit;
                } else {
                    // take bottom part
                    max = range->min - 1;
                }
                if (min > max) {
                    min = max = 0;
                }
                ACPI_DEBUG("mem range overlaps reserved space [%lx,%lx], truncated"
                          " to %lx-%lx\n", range->min, range->limit, min, max);
            }
        }
*/
        skb_add_fact("rootbridge_address_window(addr(%u, %u, %u), mem(%"PRIuLPADDR", %"PRIuLPADDR")).",
            ret->addr.bus, ret->addr.device, ret->addr.function,
            min, max);
        if (ret->minmem == ret->maxmem) {
            /* this is the first region we've seen */
            ret->minmem = min;
            ret->maxmem = max;
        } else if (min == ret->maxmem + 1) {
            /* this region extends the existing region */
            ret->maxmem = max;
        } else if (max - min > ret->maxmem - ret->minmem) {
            /* this region is bigger than the existing region */
            ret->minmem = min;
            ret->maxmem = max;
        }
        break;
    }

    return AE_OK;
}

#ifdef PCI_SERVICE_DEBUG
static ACPI_STATUS resource_printer(ACPI_RESOURCE *res, void *context)
{
    switch(res->Type) {
    case ACPI_RESOURCE_TYPE_END_TAG:
        return AE_OK;

    case ACPI_RESOURCE_TYPE_ADDRESS16:
        printf("addr16\n");
        break;
    case ACPI_RESOURCE_TYPE_ADDRESS32:
        printf("addr32\n");
        break;
    case ACPI_RESOURCE_TYPE_ADDRESS64:
        printf("length = %"PRIu32", gran = %lx, min = %lx, max = %lx, transoff "
               "= %lx, addrlen = %lx, index = %hhu, strlen = %hu, string = %s",
               res->Length, res->Data.Address64.Granularity, 
               res->Data.Address64.Minimum,
               res->Data.Address64.Maximum,
               res->Data.Address64.TranslationOffset,
               res->Data.Address64.AddressLength,
               res->Data.Address64.ResourceSource.Index,
               res->Data.Address64.ResourceSource.StringLength,
               res->Data.Address64.ResourceSource.StringPtr
               );
        break;

    case ACPI_RESOURCE_TYPE_IRQ:
        {
            ACPI_RESOURCE_IRQ *irqres = &res->Data.Irq;

            printf("%s, %s triggered, active %s, ",
                   irqres->Sharable ? "shared" : "exclusive",
                   irqres->Triggering ? "edge" : "level",
                   irqres->Polarity ? "low" : "high");

            if (irqres->InterruptCount > 0) {
                printf("IRQs:");
                for (int i = 0; i < irqres->InterruptCount; i++) {
                    printf(" %d", irqres->Interrupts[i]);
                }
                printf(".\n");
            } else {
                printf("no IRQ.\n");
            }
        }
        break;

    case ACPI_RESOURCE_TYPE_EXTENDED_IRQ:
        {
            ACPI_RESOURCE_EXTENDED_IRQ *irqres = &res->Data.ExtendedIrq;

            printf("%s, %s triggered, active %s, ",
                   irqres->Sharable ? "shared" : "exclusive",
                   irqres->Triggering ? "edge" : "level",
                   irqres->Polarity ? "low" : "high");

            if (irqres->InterruptCount > 0) {
                printf("IRQs:");
                for (int i = 0; i < irqres->InterruptCount; i++) {
                    printf(" %d", irqres->Interrupts[i]);
                }
            } else {
                printf("no IRQ");
            }

            ACPI_RESOURCE_SOURCE *src = &irqres->ResourceSource;

            if(src->StringLength > 0) {
                printf(", resource index %d, source %.*s\n", src->Index,
                       src->StringLength, src->StringPtr);
            } else {
                printf(".\n");
            }
        }
        break;

    default:
        printf("resource_printer: Unexpected resource type %d\n", res->Type);
        break;
    }

    return AE_OK;
}
#endif

ACPI_STATUS acpi_eval_integer(ACPI_HANDLE handle, char *name, ACPI_INTEGER *ret)
{
    assert(ret != NULL);
    ACPI_STATUS as;
    char intbuf[sizeof(ACPI_OBJECT)];
    ACPI_BUFFER intbufobj = {.Length = sizeof(intbuf), .Pointer = intbuf};

    as = AcpiEvaluateObjectTyped(handle, name, NULL, &intbufobj, ACPI_TYPE_INTEGER);
    if (ACPI_SUCCESS(as)) {
        ACPI_OBJECT *obj = intbufobj.Pointer;
        *ret = obj->Integer.Value;
    }

    return as;
}

static ACPI_STATUS fixed_resource_walker(ACPI_RESOURCE *resource, void *context)
{
    if (resource->Type == ACPI_RESOURCE_TYPE_FIXED_MEMORY32) {
        struct memrange range = {
            .min = resource->Data.FixedMemory32.Address,
            .limit = resource->Data.FixedMemory32.Address
                + resource->Data.FixedMemory32.AddressLength
        };
        ACPI_DEBUG("fixed memory resource claimed: 0x%"PRIxLPADDR"-%"PRIxLPADDR"\n",
                  range.min, range.limit);

        /* XXX: TODO: insert something in the SKB */
        assert(n_reserved_memory_regions < MAX_RESERVED_MEM_REGIONS);
        reserved_memory[n_reserved_memory_regions++] = range;
        skb_add_fact("fixed_memory(%"PRIuLPADDR",%"PRIuLPADDR").", range.min,
            range.limit);
    }

    return AE_OK;
}

static ACPI_STATUS reserve_resources(ACPI_HANDLE handle, UINT32 level,
                                     void *context, void **retval)
{
    ACPI_STATUS as;

    /* walk _CRS resources looking for fixed resources */
    as = AcpiWalkResources(handle, METHOD_NAME__CRS, fixed_resource_walker, NULL);
    if (ACPI_FAILURE(as)) {
        return as;
    }

    return AE_OK;
}

/**
 * \brief Get IRQ routing table by querying _PRT method.
 *
 * \param handle        Handle to _PRT method.
 * \param bus           Bus number this _PRT method is for.
 */
static void get_irq_routing(ACPI_HANDLE handle, uint8_t bus)
{
    ACPI_STATUS as;
    char prtbuf[2048];
    ACPI_BUFFER bufobj = {.Length = sizeof(prtbuf), .Pointer = prtbuf};

    /* do we have an interrupt routing table? */
    as = AcpiGetIrqRoutingTable(handle, &bufobj);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("No IRQ routing table found: %s\n", AcpiFormatException(as));
        return;
    }

    //printf("PCI IRQ routing table:\n");
    ACPI_PCI_ROUTING_TABLE *prt = bufobj.Pointer;
    for (; prt->Length; prt = (void *)prt + prt->Length) {
        uint16_t device = (prt->Address >> 16) & 0xffff;
        assert((prt->Address & 0xffff) == 0xffff); // any function
        ACPI_DEBUG(" device %u pin %u %s (index %u)\n",
               device, prt->Pin, *(prt->Source) ? prt->Source : "GSI",
               prt->SourceIndex);

        if (*prt->Source == 0) {
            /* this is a global interrupt number */
            skb_add_fact("prt(addr(%"PRIu8", %"PRIu16", _), %"PRIu32", gsi(%"PRIu32")).",
                         bus, device, prt->Pin, prt->SourceIndex);
            continue;
        }

        ACPI_HANDLE source;
        as = AcpiGetHandle(handle, prt->Source, &source);
        if (ACPI_FAILURE(as)) {
            ACPI_DEBUG("  failed lookup: %s\n", AcpiFormatException(as));
            continue;
        }

        assert(device < PCI_NDEVICES);
        assert(prt->Pin >= 0 && prt->Pin < PCI_NINTPINS);

        char *esource = calloc(strlen(prt->Source) * 2, 1);
        for(int i = 0, j = 0; i < strlen(prt->Source) + 1; i++, j++) {
            esource[j] = prt->Source[i];
            if(prt->Source[i] == '\\') {
                esource[++j] = '\\';
            }
        }
        skb_add_fact("prt(addr(%"PRIu8", %"PRIu16", _), %"PRIu32", pir(\"%s\")).",
                     bus, device, prt->Pin, esource);

#ifdef PCI_SERVICE_DEBUG /* debug code to dump resources */
        ACPI_DEBUG("  INITIAL:  ");
        as = AcpiWalkResources(source, METHOD_NAME__CRS,
                               resource_printer, NULL);
        if (ACPI_FAILURE(as)) {
            ACPI_DEBUG("  failed walking _CRS: %s\n", AcpiFormatException(as));
        }

        ACPI_DEBUG("  POSSIBLE: ");
        as = AcpiWalkResources(source, METHOD_NAME__PRS,
                               resource_printer, NULL);
        if (ACPI_FAILURE(as)) {
            ACPI_DEBUG("  failed walking _PRS: %s\n", AcpiFormatException(as));
        }
#endif

        uint8_t data[512];
        ACPI_BUFFER buf = { .Length = sizeof(data), .Pointer = &data };
        as = AcpiGetPossibleResources(source, &buf);
        if (ACPI_FAILURE(as)) {
            ACPI_DEBUG("  failed retrieving _PRS: %s\n",
                      AcpiFormatException(as));
            free(esource);
            continue;
        }

        for(ACPI_RESOURCE *res = buf.Pointer;
            (void *)res < buf.Pointer + buf.Length;
            res = (ACPI_RESOURCE *)(((char *)res) + res->Length)) {

            if(res->Type == ACPI_RESOURCE_TYPE_END_TAG) {
                break;
            }

            switch(res->Type) {
            case ACPI_RESOURCE_TYPE_IRQ:
            {
                ACPI_RESOURCE_IRQ *irqres = &res->Data.Irq;
                //printf("IRQs:");
                for (int i = 0; i < irqres->InterruptCount; i++) {
                    skb_add_fact("pir(\"%s\", %u).",
                                 esource, irqres->Interrupts[i]);
                    //printf(" %d", irqres->Interrupts[i]);
                }
                //printf("\n");
                break;
            }

            case ACPI_RESOURCE_TYPE_EXTENDED_IRQ:
            {
                ACPI_RESOURCE_EXTENDED_IRQ *irqres = &res->Data.ExtendedIrq;
                //printf("Extended IRQs:");
                for (int i = 0; i < irqres->InterruptCount; i++) {
                    //printf(" %d", irqres->Interrupts[i]);
                    skb_add_fact("pir(\"%s\", %"PRIu32").",
                                 esource, irqres->Interrupts[i]);
                }
                //printf("\n");
                break;
            }

            default:
                printf("Unknown resource type: %"PRIu32"\n", res->Type);
                USER_PANIC("NYI");
                break;
            }
        }

        free(esource);
    }
}

void acpi_get_irqtable_device(ACPI_HANDLE parent,
        acpi_pci_address_t device, ACPI_HANDLE *child, uint8_t bus)
{
/*     char b[128]; */
/*     ACPI_BUFFER buf = { .Length = 128, .Pointer = b }; */
/*     ACPI_STATUS s; */

    *child = NULL;

    if(parent == NULL) {
        return;
    }

/*     s = AcpiGetName(parent, ACPI_FULL_PATHNAME, &buf); */
/*     assert(ACPI_SUCCESS(s)); */
/*     printf("Parent: %s\n", b); */

    for(;;) {
        ACPI_STATUS as =
            AcpiGetNextObject(ACPI_TYPE_DEVICE, parent, *child, child);

        if(as == AE_NOT_FOUND || *child == NULL) {
            return;
        }

        if(ACPI_FAILURE(as)) {
            ACPI_DEBUG("Error looking up ACPI children\n");
            abort();
        }

/*         s = AcpiGetName(*child, ACPI_FULL_PATHNAME, &buf); */
/*         if(ACPI_FAILURE(s)) { */
/*             printf("Name lookup failure: %d\n", s); */
/*         } else { */
/*             printf("Current: %s\n", b); */
/*         } */

        /* look for a _ADR node, which tells us the bridge's configuration space */
        ACPI_INTEGER addr;
        as = acpi_eval_integer(*child, "_ADR", &addr);
        if (ACPI_FAILURE(as)) {
            continue;
        }

        acpi_pci_address_t bridgeaddr;
        bridgeaddr.bus = 0;
        bridgeaddr.device = (addr >> 16) & 0xffff;
        bridgeaddr.function = addr & 0xffff;

        if(device.device == bridgeaddr.device
           && device.function == bridgeaddr.function) {
/*             printf("Found corresponding ACPI bridge device!\n"); */
            get_irq_routing(*child, bus);
        }
    }
}

static ACPI_STATUS add_pci_device(ACPI_HANDLE handle, UINT32 level,
                                  void *context, void **retval)
{
    ACPI_STATUS as;
    char namebuf[128];
    ACPI_BUFFER bufobj = {.Length = sizeof(namebuf), .Pointer = namebuf};

    /* get the node's name */
    as = AcpiGetName(handle, ACPI_FULL_PATHNAME, &bufobj);
    if (ACPI_FAILURE(as)) {
        return as;
    }
    assert(bufobj.Pointer == namebuf);

    ACPI_HANDLE handle2;
    as = AcpiGetHandle(NULL, namebuf, &handle2);
    ACPI_DEBUG("acpi get handle for %s\n", namebuf);
    assert(ACPI_SUCCESS(as) && handle == handle2);


    /* look for a _ADR node, which tells us the bridge's configuration space */
    ACPI_INTEGER addr;
    as = acpi_eval_integer(handle, "_ADR", &addr);
    if (ACPI_FAILURE(as)) {
        return as;
    }

    struct pci_address bridgeaddr;
    bridgeaddr.bus = 0;
    bridgeaddr.device = (addr >> 16) & 0xffff;
    bridgeaddr.function = addr & 0xffff;

    /* look for a _BBN node, which tells us the bus number on a multi-root box */
    ACPI_INTEGER busnum;
    as = acpi_eval_integer(handle, "_BBN", &busnum);
    if (ACPI_SUCCESS(as)) {
        bridgeaddr.bus = busnum;
    }

    /* walk resources looking for the child bus ranges */
    struct pci_resources resources;
    memset(&resources, 0, sizeof(resources));

    resources.addr = bridgeaddr;

#ifdef PCI_SERVICE_DEBUG
    printf("\nstart PRS\n");
    as = AcpiWalkResources(handle, METHOD_NAME__PRS, resource_printer,
                           NULL);
    printf("\nPRS finished\n");
    if (ACPI_FAILURE(as)) {
        printf("\nPRS failed. Status = %d\n", as);
//        return as;
    }
#endif
    as = AcpiWalkResources(handle, METHOD_NAME__CRS, pci_resource_walker,
                           &resources);
    if (ACPI_FAILURE(as)) {
        return as;
    }

    if (resources.maxbus == 0) {
        ACPI_DEBUG("%s: invalid PCI root at %u:%u:%u? Ignored.\n",
               namebuf, bridgeaddr.bus, bridgeaddr.device, bridgeaddr.function);
        return AE_OK;
    }

    get_irq_routing(handle, bridgeaddr.bus);

    ACPI_DEBUG("%s: root at %u:%u:%u child buses %u-%u memory 0x%lx-%lx\n",
           namebuf, bridgeaddr.bus, bridgeaddr.device, bridgeaddr.function,
           resources.minbus, resources.maxbus, resources.minmem,
           resources.maxmem + 1);

    skb_add_fact("rootbridge(addr(%u,%u,%u),childbus(%u,%u),mem(%" PRIuPTR ",%" PRIuPTR ")).",
           bridgeaddr.bus, bridgeaddr.device, bridgeaddr.function,
           resources.minbus, resources.maxbus, resources.minmem,
           resources.maxmem);

    // octopus record for rootbridge
    ACPI_DEBUG("acpi_node: %s\n", namebuf);
    static char* format = "hw.pci.rootbridge. { bus: %lu, device: %lu, function: %lu, maxbus: %lu, acpi_node: '%s' }";
    errval_t err = oct_mset(SET_SEQUENTIAL, format,
            bridgeaddr.bus, bridgeaddr.device, bridgeaddr.function,
            resources.maxbus, namebuf);
    assert(err_is_ok(err));
    // end

    // XXX: enable PCIe for bridge programming
    /*
    pcie_enable();
    pci_add_root(bridgeaddr, resources.maxbus, handle);
    pcie_disable();*/

    return AE_OK;
}

static int acpi_init(void)
{
    AcpiDbgLevel = 0; // ACPI_DEBUG_DEFAULT | ACPI_LV_INFO | ACPI_LV_EXEC;

    // enable workarounds for non-compliant ACPI bytecode
    AcpiGbl_EnableInterpreterSlack = TRUE;

    ACPI_STATUS as;
    as = AcpiInitializeSubsystem();
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiInitializeSubsystem failed\n");
        return -1;
    }

    as = AcpiInitializeTables(NULL, 0, false);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiInitializeTables failed\n");
        return -1;
    }

    as = AcpiLoadTables();
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiLoadTables failed %s\n", AcpiFormatException(as));
        return -1;
    }

    ACPI_DEBUG("Scanning local and I/O APICs...\n");
    int r = init_all_apics();
    assert(r == 0);

#ifdef USE_KALUGA_DVM
    char* record;
    errval_t err = oct_barrier_enter("barrier.acpi", &record, 2);
    assert(err_is_ok(err));
#endif

    as = AcpiEnableSubsystem(ACPI_FULL_INITIALIZATION);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiEnableSubsystem failed %s\n", AcpiFormatException(as));
        return -1;
    }

    // find and init any embedded controller drivers
    // we do this early, because control methods may need to access the EC space
    ec_probe_ecdt();

    as = AcpiInitializeObjects(ACPI_FULL_INITIALIZATION);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("AcpiInitializeObjects failed\n");
        return -1;
    }

    return 0;
}

/**
 * \brief Sets system into APIC mode.
 *
 * Evaluates _PIC method to argument 1 and disables dual-8259As.
 * This changes the IRQs reported/set with the interrupt routing (PRT) methods.
 */
static ACPI_STATUS set_apic_mode(void)
{
    ACPI_OBJECT arg1;
    ACPI_OBJECT_LIST args;
    ACPI_STATUS as;

    // Evaluate _PIC method to argument 1
    arg1.Type = ACPI_TYPE_INTEGER;
    arg1.Integer.Value = 1;
    args.Count = 1;
    args.Pointer = &arg1;

    as = AcpiEvaluateObject(ACPI_ROOT_OBJECT, "_PIC", &args, NULL);

    // Bail out if this didn't work
    if(ACPI_FAILURE(as)) {
        return as;
    }

    return AE_OK;
}

static void process_srat(ACPI_TABLE_SRAT *srat)
{
    assert(!strncmp(srat->Header.Signature, "SRAT", ACPI_NAME_SIZE));
    assert(srat->TableRevision == 1);

    void *pos = (void *)srat + sizeof(ACPI_TABLE_SRAT);

    // Scan subtables
    while(pos < (void *)srat + srat->Header.Length) {
        ACPI_SUBTABLE_HEADER *shead = pos;

        switch(shead->Type) {
        case ACPI_SRAT_TYPE_CPU_AFFINITY:
            {
                ACPI_SRAT_CPU_AFFINITY *a = (ACPI_SRAT_CPU_AFFINITY *)shead;

                assert(a->Header.Length == 16);

                if(a->Flags & ACPI_SRAT_MEM_ENABLED) {
                    uint32_t proximitydomain = (a->ProximityDomainHi[0] << 24) +
                        (a->ProximityDomainHi[1] << 16) +
                        (a->ProximityDomainHi[2] << 8) +
                        a->ProximityDomainLo;

                    ACPI_DEBUG("CPU affinity table:\n");
                    ACPI_DEBUG("Proximity Domain: %"PRIu32"\n", proximitydomain);
                    ACPI_DEBUG("CPU local APIC ID: %d\n", a->ApicId);
                    ACPI_DEBUG("CPU local SAPIC EID: %d\n", a->LocalSapicEid);

                    skb_add_fact("cpu_affinity(%d,%d,%"PRIu32").",
                        a->ApicId, a->LocalSapicEid, proximitydomain);
                } else {
                    ACPI_DEBUG("CPU affinity table disabled!\n");
                }

                pos += sizeof(ACPI_SRAT_CPU_AFFINITY);
            }
            break;

        case ACPI_SRAT_TYPE_MEMORY_AFFINITY:
            {
                ACPI_SRAT_MEM_AFFINITY *a = (ACPI_SRAT_MEM_AFFINITY *)shead;

                assert(a->Header.Length == 40);

                if(a->Flags & ACPI_SRAT_MEM_ENABLED) {
                    ACPI_DEBUG("Memory affinity table:\n");
                    ACPI_DEBUG("Proximity Domain: %d\n", a->ProximityDomain);
                    ACPI_DEBUG("Base address: 0x%lx\n", a->BaseAddress);
                    ACPI_DEBUG("Length: 0x%lx\n", a->Length);

                    bool hotpluggable = false, nonvolatile = false;
                    if(a->Flags & ACPI_SRAT_MEM_HOT_PLUGGABLE) {
                        hotpluggable = true;
                    }
                    if(a->Flags & ACPI_SRAT_MEM_NON_VOLATILE) {
                        nonvolatile = true;
                    }
                    ACPI_DEBUG("Flags:%s%s\n",
                              hotpluggable ? " Hot-pluggable" : "",
                              nonvolatile ? " Non-volatile" : "");

                    skb_add_fact("memory_affinity(%" PRIu64 ", %" PRIu64 ", %"PRIu32").",
                        a->BaseAddress, a->Length, a->ProximityDomain);

                } else {
                    ACPI_DEBUG("Memory affinity table disabled!\n");
                }

                pos += sizeof(ACPI_SRAT_MEM_AFFINITY);
            }
            break;

        case ACPI_SRAT_TYPE_X2APIC_CPU_AFFINITY:
            ACPI_DEBUG("Ignoring unsupported x2APIC CPU affinity table.\n");
            break;

        default:
            ACPI_DEBUG("Ignoring unknown SRAT subtable ID %d.\n", shead->Type);
            break;
        }
    }
}

int init_acpi(void)
{
    ACPI_STATUS as;
    int r;

    ACPI_DEBUG("Initialising ACPI...\n");
    r = acpi_init();
    assert(r == 0);

    // Put system into APIC mode
    ACPI_DEBUG("Switching to APIC mode...\n");
    as = set_apic_mode();
    if(ACPI_FAILURE(as)) {
        ACPI_DEBUG("Warning: Could not set system to APIC mode! "
                  "Continuing anyway...\n");
    }

    /* look for an MCFG table
     * this tells us where the PCI express memory-mapped configuration area is
     */

    /*
    ACPI_TABLE_HEADER *mcfg_header;
    as = AcpiGetTable("MCFG", 1, &mcfg_header);
    if (ACPI_SUCCESS(as) && mcfg_header->Length >=
            sizeof(ACPI_TABLE_MCFG) + sizeof(ACPI_MCFG_ALLOCATION)) {
        ACPI_MCFG_ALLOCATION *mcfg = (void *)mcfg_header + sizeof(ACPI_TABLE_MCFG);
        ACPI_DEBUG("PCIe enhanced configuration region at 0x%lx "
                   "(segment %u, buses %u-%u)\n", mcfg->Address,
                   mcfg->PciSegment, mcfg->StartBusNumber, mcfg->EndBusNumber);

        skb_add_fact("pcie_confspace(%"PRIu64", %"PRIu16", %"PRIu8", %"PRIu8").",
                mcfg->Address, mcfg->PciSegment, mcfg->StartBusNumber,
                mcfg->EndBusNumber);

        //XXX: Not needed as long as PCIe walking is disabled
        r = pcie_confspace_init(mcfg->Address, mcfg->PciSegment,
                                mcfg->StartBusNumber, mcfg->EndBusNumber);
        if (r == 0) {
            // XXX: compute physical address region used by conf space
            struct memrange confspace = {
                .min = mcfg->Address,
                .limit = mcfg->Address +
               ((lpaddr_t)(mcfg->EndBusNumber + 1 - mcfg->StartBusNumber) << 20)
            };
            reserved_memory[n_reserved_memory_regions++] = confspace;
        }

    } else {
        ACPI_DEBUG("No MCFG table found -> no PCIe enhanced configuration\n");
    }*/

    // XXX: disable PCIe memory-mapped config space until after we walk and
    // prescan all the buses. This is necessary on some AMD boxes, because the
    // ACPI table includes code to read registers in the HyperTransport config,
    // and this only appears in IO space.
    // We need a cleaner way of determining when to use PCIe config space!
    pcie_disable();

    /* Find and reserve all memory regions claimed by non-PCI devices */
    ACPI_DEBUG("Reserving fixed resources\n");
    as = AcpiGetDevices("PNP0C02", reserve_resources, NULL, NULL);
    if (ACPI_FAILURE(as) && as != AE_NOT_FOUND) {
        printf("WARNING: AcpiGetDevices failed with error %"PRIu32"\n", as);
    }
    assert(ACPI_SUCCESS(as) || as == AE_NOT_FOUND);

    // XXX: PCIe walking disabled, as these also show up as PCI buses,
    // and we don't currently distinguish between them
    //ACPI_DEBUG("Walking for PCIe buses\n");
    //as = AcpiGetDevices(PCI_EXPRESS_ROOT_HID_STRING, add_pci_device, NULL, NULL);
    //assert(ACPI_SUCCESS(as));

    ACPI_DEBUG("Walking for PCI buses\n");
    as = AcpiGetDevices(PCI_ROOT_HID_STRING, add_pci_device, NULL, NULL);
    assert(ACPI_SUCCESS(as));

    //ACPI_DEBUG("Programming PCI BARs and bridge windows\n");
    //pci_program_bridges();
    //ACPI_DEBUG("PCI programming completed\n");

    ACPI_TABLE_HEADER *srat_header;
    as = AcpiGetTable("SRAT", 1, &srat_header);
    if(ACPI_SUCCESS(as)) {
        process_srat((ACPI_TABLE_SRAT *)srat_header);
    }

    if (!vtd_force_off) {
        vtd_init();
    }

    return 0;
}
