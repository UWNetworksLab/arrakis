/**
 * \file
 * \brief Code responsible for booting application cores
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <barrelfish/barrelfish.h>

#include <octopus/octopus.h>
#include <skb/skb.h>

#include "kaluga.h"

static void pci_change_event(octopus_mode_t mode, char* device_record, void* st);

static void spawnd_up_event(octopus_mode_t mode, char* spawnd_record, void* st)
{
    assert(mode & OCT_ON_SET);
    uint64_t iref;
    errval_t err = oct_read(spawnd_record, "_ { iref: %d }", &iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "Failed to read iref from spawnd record?");
    }

    // Pass the iref as state, this tells pci_change_event that we
    // don't need to look again for the spawnd iref
    // XXX: Pointer
    pci_change_event(OCT_ON_SET, st, (void*)(uintptr_t)iref);
    free(spawnd_record);
}

static errval_t wait_for_spawnd(coreid_t core, void* state)
{
    // Check if the core we're spawning on is already up...
    struct octopus_thc_client_binding_t* cl = oct_get_thc_client();
    char* iref_record = NULL;
    octopus_trigger_id_t tid;
    errval_t error_code;
    octopus_trigger_t t = oct_mktrigger(OCT_ERR_NO_RECORD,
            octopus_BINDING_EVENT, OCT_ON_SET, spawnd_up_event, state);

    // Construct service name
    static char* format = "spawn.%hhu { iref: _ }";
    int length = snprintf(NULL, 0, format, core);
    char* query = malloc(length+1);
    snprintf(query, length+1, format, core);

    errval_t err = cl->call_seq.get(cl, query, t, &iref_record, &tid, &error_code);
    free(query);
    free(iref_record);

    if (err_is_fail(err)) {
        return err;
    }

    return error_code;
}

static void pci_change_event(octopus_mode_t mode, char* device_record, void* st)
{
    errval_t err;
    if (mode & OCT_ON_SET) {
        uint64_t vendor_id, device_id;
        err = oct_read(device_record, "_ { vendor: %d, device_id: %d }",
                &vendor_id, &device_id);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "Got malformed device record?");
        }

        // Ask the SKB which binary and where to start it...
        static char* query = "find_pci_driver(pci_card(%"PRIu64", %"PRIu64", _, _, _), Driver),"
                             "writeln(Driver).";
        err = skb_execute_query(query, vendor_id, device_id);
        if (err_no(err) == SKB_ERR_EXECUTION) {
            KALUGA_DEBUG("No PCI driver found for: VendorId=0x%"PRIx64", "
                         "DeviceId=0x%"PRIx64"\n",
                    vendor_id, device_id);
            goto out;
        }
        else if (err_is_fail(err)) {
            DEBUG_ERR(err, "Failed to query SKB.\n");
            goto out;
        }

        // XXX: Find better way to parse binary name from SKB
        char* binary_name = malloc(strlen(skb_get_output()));
        coreid_t core;
        skb_read_output("driver(%"SCNu8", %s)", &core, binary_name);
        *strrchr(binary_name, ')') = '\0';

        struct module_info* mi = find_module(binary_name);
        free(binary_name);
        if (mi == NULL) {
            KALUGA_DEBUG("Driver %s not loaded. Ignore.", binary_name);
            goto out;
        }

        // Wait until the core where we start the driver
        // is ready
        if (st == NULL && core != my_core_id) {
            err = wait_for_spawnd(core, device_record);
            if (err_no(err) == OCT_ERR_NO_RECORD) {
                KALUGA_DEBUG("Core where driver %s runs is not up yet.\n",
                        mi->binary);
                // Don't want to free device record yet...
                return;
            }
            else if (err_is_fail(err)) {
                DEBUG_ERR(err, "Waiting for core %d failed?\n", core);
                goto out;
            }
        }

        // If we've come here the core where we spawn the driver
        // is already up
        err = mi->start_function(core, mi, device_record);
        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned PCI driver: %s\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_ALREADY_STARTED:
            KALUGA_DEBUG("%s already running.\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
            break;
        }
    }

out:
    free(device_record);
}

errval_t watch_for_pci_devices(void)
{
    static char* pci_device  = "r'hw\\.pci\\.device\\.[0-9]+' { "
                               " bus: _, device: _, function: _, vendor: _,"
                               " device_id: _, class: _, subclass: _, "
                               " prog_if: _ }";
    octopus_trigger_id_t tid;
    return trigger_existing_and_watch(pci_device, pci_change_event, NULL, &tid);
}

static void bridge_change_event(octopus_mode_t mode, char* bridge_record, void* st)
{
    if (mode & OCT_ON_SET) {
        // No need to ask the SKB as we always start pci for
        // in case we find a root bridge
        struct module_info* mi = find_module("pci");
        if (mi == NULL) {
            KALUGA_DEBUG("PCI driver not found or not declared as auto.");
            goto out;
        }

        // XXX: always spawn on my_core_id; otherwise we need to check that
        // the other core is already up
        errval_t err = mi->start_function(my_core_id, mi, bridge_record);
        switch (err_no(err)) {
        case SYS_ERR_OK:
            KALUGA_DEBUG("Spawned PCI bus driver: %s\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_ALREADY_STARTED:
            KALUGA_DEBUG("%s already running.\n", mi->binary);
            break;

        case KALUGA_ERR_DRIVER_NOT_AUTO:
            KALUGA_DEBUG("%s not declared as auto, ignore.\n", mi->binary);
            break;

        default:
            DEBUG_ERR(err, "Unhandled error while starting %s\n", mi->binary);
            break;
        }
    }

out:
    free(bridge_record);
}

errval_t watch_for_pci_root_bridge(void)
{
    static char* root_bridge = "r'hw\\.pci\\.rootbridge\\.[0-9]+' { "
                               " bus: _, device: _, function: _, maxbus: _,"
                               " acpi_node: _ }";
    octopus_trigger_id_t tid;
    return trigger_existing_and_watch(root_bridge, bridge_change_event,
            NULL, &tid);
}
