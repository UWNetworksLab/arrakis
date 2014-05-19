/**
 * \file
 * \brief ACPI daemon Flounder handler functions
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
#include <barrelfish/nameservice_client.h>
#include <if/acpi_defs.h>
#include <acpi.h>
#include <mm/mm.h>
#include "acpi_shared.h"
#include "acpi_debug.h"
#include "ioapic.h"
#include "intel_vtd.h"

// XXX: proper cap handling (del etc.)
static void mm_alloc_range_proxy_handler(struct acpi_binding* b, uint8_t sizebits,
		                                 genpaddr_t minbase, genpaddr_t maxlimit)
{
    ACPI_DEBUG("mm_alloc_range_proxy_handler: sizebits: %d, minbase: 0x%lx maxlimit: 0x%lx\n",
	       sizebits, minbase, maxlimit);

    struct capref devframe = NULL_CAP;
    /* errval_t err = mm_alloc_range(&pci_mm_physaddr, sizebits, minbase, maxlimit, &devframe, NULL); */
    errval_t err = mm_realloc_range(&pci_mm_physaddr, sizebits, minbase, &devframe);
    if (err_is_fail(err)) {
    	DEBUG_ERR(err, "mm realloc range failed...\n");
    }

    err = b->tx_vtbl.mm_alloc_range_proxy_response(b, NOP_CONT, devframe, err);
    assert(err_is_ok(err));
}

// XXX: proper cap handling
static void mm_free_proxy_handler(struct acpi_binding* b, struct capref devframe,
		                          uint64_t base, uint8_t sizebits)
{
    ACPI_DEBUG("mm_free_proxy_handler: base: %lu, sizebits: %d\n", base, sizebits);

    errval_t err = mm_free(&pci_mm_physaddr, devframe, base, sizebits);
    if (err_is_fail(err)) {
    	DEBUG_ERR(err, "mm free failed...\n");
    }

    err = b->tx_vtbl.mm_free_proxy_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void enable_interrupt_handler(struct acpi_binding* b, uint32_t gsi,
        coreid_t dest, uint32_t vector)
{
    errval_t err = SYS_ERR_OK;
    err = enable_and_route_interrupt(gsi, dest, vector);

    err = b->tx_vtbl.enable_and_route_interrupt_response(b, NOP_CONT, err);
    assert(err_is_ok(err));

}

static inline bool mcfg_correct_length(uint32_t header_len)
{
    return header_len >=
            sizeof(ACPI_TABLE_MCFG) + sizeof(ACPI_MCFG_ALLOCATION);
}

static void get_pcie_confspace(struct acpi_binding* b)
{
    ACPI_DEBUG("get_pcie_confspace\n");

    errval_t err;
    ACPI_STATUS as;
    ACPI_TABLE_HEADER *mcfg_header;

    as = AcpiGetTable("MCFG", 1, &mcfg_header);
    if (ACPI_SUCCESS(as) && mcfg_correct_length(mcfg_header->Length)) {

        ACPI_MCFG_ALLOCATION *mcfg = (void*) mcfg_header
                + sizeof(ACPI_TABLE_MCFG);
        ACPI_DEBUG(
                "PCIe enhanced configuration region at 0x%lx "
                "(segment %u, buses %u-%u)\n", mcfg->Address,
                mcfg->PciSegment, mcfg->StartBusNumber, mcfg->EndBusNumber);

        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT, SYS_ERR_OK,
                mcfg->Address, mcfg->PciSegment, mcfg->StartBusNumber,
                mcfg->EndBusNumber);

    } else {
        ACPI_DEBUG("No MCFG table found -> no PCIe enhanced configuration\n");
        err = b->tx_vtbl.get_pcie_confspace_response(b, NOP_CONT,
                ACPI_ERR_NO_MCFG_TABLE, 0, 0, 0, 0);
    }

    assert(err_is_ok(err));
}

static void get_path_name(ACPI_HANDLE handle, char* name, size_t len)
{
    ACPI_BUFFER buf = { .Length = len, .Pointer = name };
    ACPI_STATUS s;

    s = AcpiGetName(handle, ACPI_FULL_PATHNAME, &buf);
    assert(ACPI_SUCCESS(s));
}

static void read_irq_table(struct acpi_binding* b, char* pathname,
        acpi_pci_address_t addr, uint8_t bus)
{
    ACPI_DEBUG("read_irq_table: %s\n", pathname);

    errval_t err;
    ACPI_STATUS as;
    ACPI_HANDLE handle;

    as = AcpiGetHandle(NULL, pathname, &handle);
    if (ACPI_SUCCESS(as)) {
        ACPI_HANDLE child;
        acpi_get_irqtable_device(handle, addr, &child, bus);

        char name[128];
        get_path_name(child, name, 128);
        ACPI_DEBUG("Sending back path name: %s\n", name);

        err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT, SYS_ERR_OK, name);
        assert(err_is_ok(err));
    }
    else {
        ACPI_DEBUG("Unknown ACPI Handle for path: %s\n", pathname);
        err = b->tx_vtbl.read_irq_table_response(b, NOP_CONT,
                ACPI_ERR_INVALID_PATH_NAME, NULL);
        assert(err_is_ok(err));
    }

    free(pathname);
}

static void set_device_irq(struct acpi_binding *b, char* device, uint32_t irq)
{
    ACPI_DEBUG("Setting link device '%s' to GSI %u\n", device, irq);

    errval_t err = SYS_ERR_OK;

    ACPI_HANDLE source;
    ACPI_STATUS as = AcpiGetHandle(NULL, device, &source);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed lookup: %s\n", AcpiFormatException(as));
        err = ACPI_ERR_INVALID_PATH_NAME;
        goto reply;
    }

    uint8_t data[512];
    ACPI_BUFFER buf = { .Length = sizeof(data), .Pointer = &data };
    as = AcpiGetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed getting _CRS: %s\n", AcpiFormatException(as));
        err = ACPI_ERR_GET_RESOURCES;
        goto reply;
    }

    // set chosen IRQ in first IRQ resource type
    ACPI_RESOURCE *res = buf.Pointer;
    switch(res->Type) {
    case ACPI_RESOURCE_TYPE_IRQ:
        res->Data.Irq.Interrupts[0] = irq;
        break;

    case ACPI_RESOURCE_TYPE_EXTENDED_IRQ:
        res->Data.ExtendedIrq.Interrupts[0] = irq;
        break;

    default:
        printf("Unknown resource type: %"PRIu32"\n", res->Type);
        ACPI_DEBUG("NYI");
        break;
    }

    //pcie_enable(); // XXX
    as = AcpiSetCurrentResources(source, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("  failed setting current IRQ: %s\n",
                  AcpiFormatException(as));
        err = ACPI_ERR_SET_IRQ;
        goto reply;
    }

reply:
    err = b->tx_vtbl.set_device_irq_response(b, NOP_CONT, err);
    assert(err_is_ok(err));

    free(device);
}

static void reset_handler(struct acpi_binding *b)
{
    if (AcpiGbl_FADT.Flags & ACPI_FADT_RESET_REGISTER) {
        printf("Resetting machine via ACPI...\n");
        ACPI_STATUS as = AcpiReset();
        if (ACPI_FAILURE(as)) {
            printf("ACPI reset failed\n");
        }
    }

    printf("Resetting machine via syscall...\n");
    errval_t err = sys_reboot();
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "reboot syscall failed");
    }
}

static void sleep_handler(struct acpi_binding *b, uint32_t state)
{
    printf("Entering S%"PRIu32" sleep state via ACPI...\n", state);
    ACPI_STATUS as = AcpiEnterSleepStatePrep(state);
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepStatePrep failed\n");
        return;
    }

    as = AcpiEnterSleepState(state);
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepState failed\n");
    }
}

extern struct capref biosmem;
static void get_vbe_bios_cap(struct acpi_binding *b)
{
    errval_t err;
    err = b->tx_vtbl.get_vbe_bios_cap_response(b, NOP_CONT, SYS_ERR_OK, biosmem,
                                               1UL << BIOS_BITS);
    assert(err_is_ok(err));
}

static void create_domain(struct acpi_binding *b, struct capref pml4)
{
    errval_t err;
    err = vtd_create_domain(pml4);
    err = b->tx_vtbl.create_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void delete_domain(struct acpi_binding *b, struct capref pml4)
{
    errval_t err;
    err = vtd_remove_domain(pml4);
    err = b->tx_vtbl.delete_domain_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_add_device(struct acpi_binding *b, uint32_t seg, uint32_t bus, 
			   uint32_t dev, uint32_t func, struct capref pml4)
{
    errval_t err;
    err = vtd_domain_add_device(seg, bus, dev, func, pml4);
    err = b->tx_vtbl.vtd_add_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_remove_device(struct acpi_binding *b, uint32_t seg, uint32_t bus, 
			      uint32_t dev, uint32_t func, struct capref pml4)
{
    errval_t err;
    err = vtd_domain_remove_device(seg, bus, dev, func, pml4);
    err = b->tx_vtbl.vtd_remove_device_response(b, NOP_CONT, err);
    assert(err_is_ok(err));
}

static void vtd_id_dom_add_devices(struct acpi_binding *b)
{
    errval_t err;
    vtd_identity_domain_add_devices();
    err = b->tx_vtbl.vtd_id_dom_add_devices_response(b, NOP_CONT, SYS_ERR_OK);
    assert(err_is_ok(err));
}

struct acpi_rx_vtbl acpi_rx_vtbl = {
    .get_pcie_confspace_call = get_pcie_confspace,
    .read_irq_table_call = read_irq_table,
    .set_device_irq_call = set_device_irq,
    .enable_and_route_interrupt_call = enable_interrupt_handler,

    .mm_alloc_range_proxy_call = mm_alloc_range_proxy_handler,
    .mm_free_proxy_call = mm_free_proxy_handler,

    .reset_call = reset_handler,
    .sleep_call = sleep_handler,

    .get_vbe_bios_cap_call = get_vbe_bios_cap,

    .create_domain_call = create_domain,
    .delete_domain_call = delete_domain,
    .vtd_add_device_call = vtd_add_device,
    .vtd_remove_device_call = vtd_remove_device,
    .vtd_id_dom_add_devices_call = vtd_id_dom_add_devices,
};

static void export_callback(void *st, errval_t err, iref_t iref)
{
    assert(err_is_ok(err));

    err = nameservice_register("acpi", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
    ACPI_DEBUG("acpi service exported\n");
}

static errval_t connect_callback(void *cst, struct acpi_binding *b)
{
    ACPI_DEBUG("acpi service get connection\n");
    b->rx_vtbl = acpi_rx_vtbl;
    b->st = NULL;

    return SYS_ERR_OK;
}

void start_service(void)
{
    ACPI_DEBUG("start_service\n");
    errval_t r = acpi_export(NULL, export_callback, connect_callback,
                            get_default_waitset(), IDC_EXPORT_FLAGS_DEFAULT);
    assert(err_is_ok(r));

    ACPI_DEBUG("start_service: terminated\n");
}
