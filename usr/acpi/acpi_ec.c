/**
 * \file
 * \brief ACPI embedded controller (EC) driver
 */

/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

/*
 * Portions based on the FreeBSD driver, which carries the following notice:
 *
 * Copyright (c) 2003-2007 Nate Lawson
 * Copyright (c) 2000 Michael Smith
 * Copyright (c) 2000 BSDi
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <acpi.h>

#include <dev/acpi_ec_dev.h>

#include "acpi_shared.h"
#include "acpi_debug.h"

struct ec {
    ACPI_HANDLE handle; ///< Handle to EC object
    ACPI_INTEGER uid;   ///< UID of this EC object
    bool use_glk;       ///< Whether to use the ACPI global lock
    acpi_ec_t dev;           ///< Mackerel device status
};

static ACPI_STATUS doread(struct ec *ec, uint8_t addr, uint8_t *data)
{
    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    // send the read command
    acpi_ec_cmd_wr(&ec->dev, acpi_ec_read);

    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    // send the address
    acpi_ec_data_wr(&ec->dev, addr);

    // spinwait for output buffer full
    while (!acpi_ec_status_obf_rdf(&ec->dev)) ;

    // read byte
    *data = acpi_ec_data_rd(&ec->dev);

    return AE_OK;
}

static ACPI_STATUS dowrite(struct ec *ec, uint8_t addr, uint8_t data)
{
    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    // send the write command
    acpi_ec_cmd_wr(&ec->dev, acpi_ec_write);

    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    acpi_ec_data_wr(&ec->dev, addr);

    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    // write byte
    acpi_ec_data_wr(&ec->dev, data);

    // spinwait for input buffer empty
    while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

    return AE_OK;
}

static uint32_t gpe_handler(void *arg)
{
    struct ec *ec = arg;
    ACPI_STATUS as;

    /* check if an SCI is pending */
    if (acpi_ec_status_sci_evt_rdf(&ec->dev)) {
        // spinwait for input buffer empty
        while (acpi_ec_status_ibf_rdf(&ec->dev)) ;

        // send query command
        acpi_ec_cmd_wr(&ec->dev, acpi_ec_query);

        // spinwait for output buffer full
        while (!acpi_ec_status_obf_rdf(&ec->dev)) ;

        // read data
        uint8_t data = acpi_ec_data_rd(&ec->dev);

        printf("EC: GPE query %X\n", data);

        if (data != 0) {
            // evaluate query method _QXX to respond
            char method[5];
            snprintf(method, sizeof(method), "_Q%02X", data);
            as = AcpiEvaluateObject(ec->handle, method, NULL, NULL);
            if (ACPI_FAILURE(as)) {
                ACPI_DEBUG("EC: error 0x%x in query method %s\n", as, method);
            }
        }
    }

    return 0; // noop
}

static ACPI_STATUS space_handler(uint32_t function, ACPI_PHYSICAL_ADDRESS xaddr,
                                 uint32_t bitwidth, ACPI_INTEGER *value,
                                 void *handler_context, void *region_context)
{
    struct ec *ec = handler_context;
    uint8_t addr = xaddr, data;
    ACPI_INTEGER retval = 0;
    ACPI_STATUS as = AE_OK;

    assert(bitwidth % 8 == 0);
    assert(xaddr + (bitwidth / 8) <= 256);

    for (int i = 0; i < bitwidth; i += 8, addr++) {
        switch(function) {
        case ACPI_READ:
            as = doread(ec, addr, &data);
            if (ACPI_SUCCESS(as)) {
                retval |= (ACPI_INTEGER)data << i;
            }
            break;

        case ACPI_WRITE:
            data = *value >> i;
            as = dowrite(ec, addr, data);
            break;

        default:
            USER_PANIC("NYI");
            as = AE_ERROR;
        }

        if (ACPI_FAILURE(as)) {
            ACPI_DEBUG("error 0x%x in EC space handler\n", as);
            break;
        }
    }

    return as;
}

static ACPI_STATUS ec_probe(ACPI_HANDLE handle, uint32_t nestlevel,
                            void *context, void **retval)
{
    ACPI_STATUS as;

    struct ec *ec = malloc(sizeof(struct ec));
    assert(ec != NULL);

    ec->handle = handle;

    // store UID of object
    as = acpi_eval_integer(handle, "_UID", &ec->uid);
    if (ACPI_FAILURE(as)) {
        ec->uid = 0;
    }

    // do we need to use the global lock when accessing?
    ACPI_INTEGER tmp;
    as = acpi_eval_integer(handle, "_GLK", &tmp);
    ec->use_glk = (ACPI_SUCCESS(as) && tmp);
    assert(!ec->use_glk); // NYI by this driver

    // evaluate _GPE to find the GPE used by this EC
    ACPI_BUFFER buf = {
        .Pointer = NULL,
        .Length = ACPI_ALLOCATE_BUFFER,
    };
    as = AcpiEvaluateObject(handle, "_GPE", NULL, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("_GPE failed: 0x%x\n", as);
        return as;
    }

    ACPI_OBJECT *obj = (ACPI_OBJECT *)buf.Pointer;
    assert(obj != NULL);

    ACPI_INTEGER gpe;
    if (obj->Type == ACPI_TYPE_INTEGER) {
        gpe = obj->Integer.Value;
    } else if (obj->Type == ACPI_TYPE_PACKAGE) {
        // see 12.11
        USER_PANIC("NYI");
        return AE_ERROR;
    } else {
        ACPI_DEBUG("_GPE returned unexpected object type %d\n", obj->Type);
        free(buf.Pointer);
        return AE_TYPE;
    }

    // Free returned buffer
    free(buf.Pointer);

    // locate device ports
    buf.Pointer = NULL;
    buf.Length = ACPI_ALLOCATE_BUFFER;
    as = AcpiGetCurrentResources(handle, &buf);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("error calling _CRS\n");
        return as;
    }

    // walk resources
    uint16_t ioports[2];
    int nports = 0;

    ACPI_RESOURCE *resource = buf.Pointer;
    assert(resource != NULL);
    do {
        switch(resource->Type) {
        case ACPI_RESOURCE_TYPE_IO:
            if (nports < 2) {
                ioports[nports++] = resource->Data.Io.Minimum;
            }
            break;
        default:
            ACPI_DEBUG("unhandled EC resource type %d\n", resource->Type);
        }

        resource = ACPI_ADD_PTR(ACPI_RESOURCE, resource, resource->Length);
    } while (resource->Type != ACPI_RESOURCE_TYPE_END_TAG);

    free(buf.Pointer);

    if (nports < 2) {
        ACPI_DEBUG("insufficient IO ports included in EC resource set\n");
        return AE_ERROR;
    }

    // init mackerel state
    acpi_ec_initialize(&ec->dev, ioports[0], ioports[1]);

    // register GPE handler
    as = AcpiInstallGpeHandler(NULL, gpe, ACPI_GPE_EDGE_TRIGGERED, gpe_handler,
                               ec);
    assert(ACPI_SUCCESS(as));

    // install address space handler
    as = AcpiInstallAddressSpaceHandler(handle, ACPI_ADR_SPACE_EC,
                                        space_handler, NULL, ec);
    assert(ACPI_SUCCESS(as));

    // enable GPE
    as = AcpiSetGpeType(NULL, gpe, ACPI_GPE_TYPE_RUNTIME);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("Failed to set GPE %ld type: 0x%x\n", gpe, as);
        return as;
    }

    as = AcpiEnableGpe(NULL, gpe, ACPI_NOT_ISR);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("Failed to enable GPE %ld: 0x%x\n", gpe, as);
        return as;
    }

    return AE_OK;
}

/* init first EC in an ECDT table, if present */
void ec_probe_ecdt(void)
{
    ACPI_STATUS as;
    ACPI_TABLE_ECDT *ecdt;
    ACPI_TABLE_HEADER *th;

    /* parse ECDT table */
    as = AcpiGetTable("ECDT", 1, (ACPI_TABLE_HEADER **)&th);
    if (ACPI_FAILURE(as)) {
        return;
    }
    else {
        ecdt = (ACPI_TABLE_ECDT*)th;
    }

    assert(ecdt->Control.BitWidth == 8 && ecdt->Data.BitWidth == 8);

    printf("Initialising EC driver from ECDT...\n");

    struct ec *ec = malloc(sizeof(struct ec));
    assert(ec != NULL);

    // lookup handle
    as = AcpiGetHandle(NULL, (char *) ecdt->Id, &ec->handle);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("EC: can't get handle for %s\n", ecdt->Id);
        return;
    }

    // store UID of object
    ec->uid = ecdt->Uid;

    // do we need to use the global lock when accessing?
    ACPI_INTEGER tmp;
    as = acpi_eval_integer(ec->handle, "_GLK", &tmp);
    ec->use_glk = (ACPI_SUCCESS(as) && tmp);
    assert(!ec->use_glk); // NYI by this driver

    // init mackerel state
    assert(ecdt->Control.SpaceId == ACPI_ADR_SPACE_SYSTEM_IO);
    assert(ecdt->Data.SpaceId == ACPI_ADR_SPACE_SYSTEM_IO);
    acpi_ec_initialize(&ec->dev, ecdt->Data.Address, ecdt->Control.Address);

    // register GPE handler
    as = AcpiInstallGpeHandler(NULL, ecdt->Gpe, ACPI_GPE_EDGE_TRIGGERED,
                               gpe_handler, ec);
    assert(ACPI_SUCCESS(as));

    // install address space handler
    as = AcpiInstallAddressSpaceHandler(ec->handle, ACPI_ADR_SPACE_EC,
                                        space_handler, NULL, ec);
    assert(ACPI_SUCCESS(as));

    // enable GPE
    as = AcpiSetGpeType(NULL, ecdt->Gpe, ACPI_GPE_TYPE_RUNTIME);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("Failed to set GPE %d type: 0x%x\n", ecdt->Gpe, as);
        return;
    }

    as = AcpiEnableGpe(NULL, ecdt->Gpe, ACPI_NOT_ISR);
    if (ACPI_FAILURE(as)) {
        ACPI_DEBUG("Failed to enable GPE %d: 0x%x\n", ecdt->Gpe, as);
        return;
    }
}

/* probe and init all other EC devices
 * FIXME: doesn't protect against re-initing devices listed in ECDT! */
void ec_init(void)
{
    AcpiGetDevices("PNP0C09", ec_probe, NULL, NULL);
}
