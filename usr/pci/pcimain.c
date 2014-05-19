/**
 * \file
 * \brief PCI
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, 2014, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>

#include <if/monitor_blocking_rpcclient_defs.h>

#include <mm/mm.h>
#include <octopus/init.h>
#include <skb/skb.h>
#include <acpi_client/acpi_client.h>

#include "pci.h"
#include "pci_debug.h"

static errval_t init_io_ports(void)
{
    errval_t err;

    struct monitor_blocking_rpc_client *cl = get_monitor_blocking_rpc_client();
    assert(cl != NULL);

    // Request I/O Cap
    struct capref requested_caps;
    errval_t error_code;
    err = cl->vtbl.get_io_cap(cl, &requested_caps, &error_code);
    assert(err_is_ok(err) && err_is_ok(error_code));

    // Copy into correct slot
    struct capref caps_io = {
        .cnode = cnode_task,
        .slot  = TASKCN_SLOT_IO
    };
    err = cap_copy(caps_io, requested_caps);

    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;

    // Parse commandline arguments
    for(int i = 1; i < argc; i++) {
        if(!strncmp(argv[i], "skb_bridge_program=", strlen("skb_bridge_program="))) {
            skb_bridge_program = argv[i] + strlen("skb_bridge_program=");
        } else if(!strncmp(argv[i], "numvfs=", strlen("numvfs="))) {
            max_numvfs = atoi(argv[i] + strlen("numvfs="));
        } else {
            printf("%s: Unknown commandline option \"%s\" -- skipping.\n", argv[0], argv[i]);
        }
    }

    err = oct_init();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "dist initialization failed.");
    }

    err = skb_client_connect();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Connecting to SKB failed.");
    }

    err = connect_to_acpi();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ACPI Connection failed.");
    }

    err = init_io_ports();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Init memory allocator failed.");
    }

    err = pcie_setup_confspace();
    if (err_is_fail(err)) {
        if (err_no(err) == ACPI_ERR_NO_MCFG_TABLE) {
            debug_printf("No PCIe found, continue.\n");
        }
        else {
            USER_PANIC_ERR(err, "Setup PCIe confspace failed.");
        }
    }

    err = pci_setup_root_complex();
    if (err_is_fail(err)) {
    	USER_PANIC_ERR(err, "Setup PCI root complex failed.");
    }

    // Start configuring PCI
    PCI_DEBUG("Programming PCI BARs and bridge windows\n");
    pci_program_bridges();
    PCI_DEBUG("PCI programming completed\n");
    pci_init_datastructures();
    pci_init();


#if 0 // defined(PCI_SERVICE_DEBUG) || defined(GLOBAL_DEBUG)
//output all the facts stored in the SKB to produce a sample data file to use
//for debugging on linux
    skb_execute("listing.");
    while (skb_read_error_code() == SKB_PROCESSING) messages_wait_and_handle_next();
    PCI_DEBUG("\nSKB returned: \n%s\n", skb_get_output());
    const char *errout = skb_get_error_output();
    if (errout != NULL && *errout != '\0') {
        PCI_DEBUG("\nSKB error returned: \n%s\n", errout);
    }
#endif

    skb_add_fact("pci_discovery_done.");

    /* Using the name server as a lock server,
       register a service with it so that other domains can do a blocking wait
       on pci to finish enumeration */
    err = nameservice_register("pci_discovery_done", 0);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_register failed");
        abort();
    }

    vtd_add_devices();

    messages_handler_loop();
}
