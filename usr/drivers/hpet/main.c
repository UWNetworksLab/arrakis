/**
 * \file
 * \brief Beginnings of an HPET driver
 */

/*
 * Copyright (c) 2007, 2008, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <pci/pci.h>
#include <if/hpet_defs.h>

#define COUNTER_INDEX 0xF0
static struct capref hpet_cap;

/* Request the counter register from the hpet
 * It replies by sending a cap for the entire hpet space
 * and an index for where the counter register lies.
 */
static void counter_request(struct hpet_binding *b)
{
    errval_t err;

    err = b->tx_vtbl.counter_reply(b, NOP_CONT, hpet_cap, COUNTER_INDEX);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "sending counter_reply failed");
    }
}

static struct hpet_rx_vtbl rx_vtbl = {
    .counter_request = counter_request,
};

static void export_cb(void *st, errval_t err, iref_t iref)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    // register this iref with the name service
    err = nameservice_register("hpet", iref);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "nameservice_register failed");
    }
}

static errval_t connect_cb(void *st, struct hpet_binding *b)
{
    b->rx_vtbl = rx_vtbl;
    return SYS_ERR_OK;
}

int main(int argc, char *argv[])
{
    errval_t err;

    /* Connect to the pci server */
    err = pci_client_connect();
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "pci_client_connect failed");
    }

    /* Get the device frame cap for the HPET registers */
    /* XXX: this used an old unsupported mechanism which has now been removed.
     * The HPET should be located through SKB/ACPI techniques, not a PCI
     * backdoor. */
    USER_PANIC("pci_register_hpet_driver(&hpet_cap); not implemented\n");
    // err = pci_register_hpet_driver(&hpet_cap);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "pci_register_hpet_driver failed");
    }

    /* Map them in */
    void* buf;
    err = vspace_map_one_frame_attr(&buf, BASE_PAGE_SIZE, hpet_cap,
                                    VREGION_FLAGS_READ_WRITE_NOCACHE,
                                    NULL, NULL);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "vspace_map_one_frame_attr failed");
    }

    /* Config register */
    uint64_t *config = (uint64_t*)(buf + 0x010);
    // Enable the counter
    *config = *config | 0x1;

    /* Enable server listening */
    err = hpet_export(NULL, export_cb, connect_cb, get_default_waitset(),
                      IDC_EXPORT_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "export failed");
    }

    messages_handler_loop();
}
