/**
 * \file
 * \brief ACPI button event handlers
 */

/*
 * Copyright (c) 2009, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <acpi.h>

#include "acpi_shared.h"
#include "acpi_debug.h"

#define OFF_STATE 5 // S5 state (really off!)

static uint32_t power_button_handler(void *arg)
{
    ACPI_STATUS as;

    printf("Power button pressed, switching off...\n");
    as = AcpiEnterSleepStatePrep(OFF_STATE);
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepStatePrep failed\n");
        return 0;
    }

    as = AcpiEnterSleepState(OFF_STATE);
    printf("Well, that sure didn't work!\n");
    if (!ACPI_SUCCESS(as)) {
        printf("AcpiEnterSleepState failed\n");
    }

    return 0;
}

static void power_button_notify_handler(ACPI_HANDLE handle, uint32_t value,
                                        void *context)
{
    printf("Power button notify 0x%"PRIx32"\n", value);
    if (value == 0x80) { // switch off!
        power_button_handler(context);
    }
}

static ACPI_STATUS power_button_probe(ACPI_HANDLE handle, uint32_t nestlevel,
                                      void *context, void **retval)
{
    // install handler
    printf("Installing notify handler for power/sleep button\n");
    return AcpiInstallNotifyHandler(handle, ACPI_DEVICE_NOTIFY,
                                    power_button_notify_handler, NULL);
}

void buttons_init(void)
{
    ACPI_STATUS as;

    if ((AcpiGbl_FADT.Flags & ACPI_FADT_POWER_BUTTON) == 0) {
        printf("Installing fixed event handler for power button\n");
        as = AcpiInstallFixedEventHandler(ACPI_EVENT_POWER_BUTTON,
                                          power_button_handler, NULL);
        assert(ACPI_SUCCESS(as));
    }

    // install handlers for notify events on other button objects
    AcpiGetDevices("PNP0C0C", power_button_probe, NULL, NULL); // power buttons
    AcpiGetDevices("PNP0C0E", power_button_probe, NULL, NULL); // sleep buttons
}
