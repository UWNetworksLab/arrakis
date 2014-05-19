/**
 * \file
 * \brief ACPI RPC Client
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/caddr.h>
#include <barrelfish/nameservice_client.h>

#include <if/acpi_defs.h>
#include <if/acpi_rpcclient_defs.h>

#include <acpi_client/acpi_client.h>

static struct acpi_connection {
    bool is_done;
    errval_t err;
} state;

static struct acpi_rpc_client* rpc_client;

errval_t acpi_reset(void)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.reset(rpc_client, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t acpi_sleep(int st)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.sleep(rpc_client, st, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

// Kludge for VBE driver
errval_t acpi_get_vbe_bios_cap(struct capref *retcap, size_t *retsize)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    assert(retcap != NULL);
    assert(retsize != NULL);
    uint32_t s;
    err = rpc_client->vtbl.get_vbe_bios_cap(rpc_client, &msgerr, retcap, &s);
    *retsize = s;
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_create_domain(struct capref pml4)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.create_domain(rpc_client, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_delete_domain(struct capref pml4)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.delete_domain(rpc_client, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_domain_add_device(int seg, int bus, int dev, int func, struct capref pml4) 
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.vtd_add_device(rpc_client, seg, bus, dev, func, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_domain_remove_device(int seg, int bus, int dev, int func, struct capref pml4) 
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.vtd_remove_device(rpc_client, seg, bus, dev, func, pml4, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t vtd_add_devices(void)
{
    assert(rpc_client != NULL);
    errval_t err, msgerr;
    err = rpc_client->vtbl.vtd_id_dom_add_devices(rpc_client, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

struct acpi_rpc_client* get_acpi_rpc_client(void)
{
    assert(rpc_client != NULL);
    return rpc_client;
}

static void rpc_bind_cb(void *st, errval_t err, struct acpi_binding* b)
{
    if (err_is_ok(err)) {
        rpc_client = malloc(sizeof(struct acpi_rpc_client));
        assert(rpc_client != NULL);

        err = acpi_rpc_client_init(rpc_client, b);
        if (err_is_fail(err)) {
            free(rpc_client);
        }
    } // else: Do nothing

    assert(!state.is_done);
    state.is_done = true;
    state.err = err;
}

errval_t connect_to_acpi(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup("acpi", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    state.is_done = false;
    err = acpi_bind(iref, rpc_bind_cb, NULL, get_default_waitset(),
            IDC_BIND_FLAGS_DEFAULT);
    if (err_is_fail(err)) {
        return err_push(err, FLOUNDER_ERR_BIND);
    }

    //  Wait for callback to complete
    while (!state.is_done) {
        messages_wait_and_handle_next();
    }

    return state.err;

}
