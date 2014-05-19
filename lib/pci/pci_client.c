/**
 * \file
 * \brief PCI service client-side logic
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <stdlib.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <barrelfish/dispatch.h>
#include <barrelfish/inthandler.h>
#include <pci/pci.h>
#include <if/pci_defs.h>
#include <if/pci_rpcclient_defs.h>

#define INVALID_VECTOR ((uint32_t)-1)

static struct pci_rpc_client *pci_client = NULL;

errval_t pci_register_driver_irq(pci_driver_init_fn init_func, uint32_t class,
                                 uint32_t subclass, uint32_t prog_if,
                                 uint32_t vendor, uint32_t device,
                                 uint32_t bus, uint32_t dev, uint32_t fun,
                                 interrupt_handler_fn handler,
                                 void *handler_arg)
{
    uint32_t vector = INVALID_VECTOR;
    pci_caps_per_bar_t *caps_per_bar = NULL;
    uint8_t nbars;
    errval_t err, msgerr;

    if (handler != NULL) {
        err = inthandler_setup(handler, handler_arg, &vector);
        if (err_is_fail(err)) {
            return err;
        }
        printf("pci_client.c: got vector %"PRIu32"\n", vector);
        assert(vector != INVALID_VECTOR);
    }

    err = pci_client->vtbl.
        init_pci_device(pci_client, class, subclass, prog_if, vendor,
                        device, bus, dev, fun, disp_get_core_id(), vector,
                        &msgerr, &nbars, &caps_per_bar);
    if (err_is_fail(err)) {
        return err;
    } else if (err_is_fail(msgerr)) {
        free(caps_per_bar);
        return msgerr;
    }

    assert(nbars > 0); // otherwise we should have received an error!

    // FIXME: leak
    struct device_mem *bars = calloc(nbars, sizeof(struct device_mem));
    assert(bars != NULL);

    // request caps for all bars of device
    for (int nb = 0; nb < nbars; nb++) {
        struct device_mem *bar = &bars[nb];

        int ncaps = (*caps_per_bar)[nb];
        if (ncaps != 0) {
            bar->nr_caps = ncaps;
            bar->frame_cap = malloc(ncaps * sizeof(struct capref)); // FIXME: leak
            assert(bar->frame_cap != NULL);
        }

        for (int nc = 0; nc < ncaps; nc++) {
            struct capref cap;
            uint8_t type;

            err = pci_client->vtbl.get_cap(pci_client, nb, nc, &msgerr, &cap,
                                           &type);
            if (err_is_fail(err) || err_is_fail(msgerr)) {
                if (err_is_ok(err)) {
                    err = msgerr;
                }
                DEBUG_ERR(err, "requesting cap %d for BAR %d of device", nc, nb);
                goto out;
            }

            if (type == 0) { // Frame cap BAR
                bar->frame_cap[nc] = cap;
                if (nc == 0) {
                    struct frame_identity id = { .base = 0, .bits = 0 };
                    invoke_frame_identify(cap, &id);
                    bar->paddr = id.base;
                    bar->bits = id.bits;
                    bar->bytes = (1ul << id.bits) * ncaps;
                }
            } else { // IO BAR
                bar->io_cap = cap;
                err = cap_copy(cap_io, cap);
                if(err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
                    DEBUG_ERR(err, "cap_copy for IO cap");
                    goto out;
                }
            }
        }
    }

    // initialize the device. We have all the caps now
    init_func(bars, nbars);

    err = SYS_ERR_OK;

 out:
    free(caps_per_bar);
    return err;
}

errval_t pci_register_driver_noirq(pci_driver_init_fn init_func, uint32_t class,
                                   uint32_t subclass, uint32_t prog_if,
                                   uint32_t vendor, uint32_t device,
                                   uint32_t bus, uint32_t dev, uint32_t fun)
{
    return pci_register_driver_irq(init_func, class, subclass, prog_if, vendor,
                                   device, bus, dev, fun, NULL, NULL);
}

errval_t pci_register_legacy_driver_irq(legacy_driver_init_fn init_func,
                                        uint16_t iomin, uint16_t iomax, int irq,
                                        interrupt_handler_fn handler,
                                        void *handler_arg)
{
    errval_t err, msgerr;
    struct capref iocap;

    uint32_t vector = INVALID_VECTOR;
    err = inthandler_setup(handler, handler_arg, &vector);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "inthandler_setup()\n");
        return err;
    }

    err = pci_client->vtbl.init_legacy_device(pci_client, iomin, iomax, irq,
                                              disp_get_core_id(), vector,
                                              &msgerr, &iocap);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "pci_client->init_legacy_device()\n");
        return err;
    } else if (err_is_fail(msgerr)) {
        DEBUG_ERR(msgerr, "pci_client->init_legacy_device()\n");
        return msgerr;
    }

    /* copy IO cap to default location */
    err = cap_copy(cap_io, iocap);
    if (err_is_fail(err) && err_no(err) != SYS_ERR_SLOT_IN_USE) {
        DEBUG_ERR(err, "failed to copy legacy io cap to default slot\n");
    }

    err = cap_destroy(iocap);
    assert(err_is_ok(err));

    /* run init func */
    init_func();

    return msgerr;
}

errval_t pci_setup_inthandler(interrupt_handler_fn handler, void *handler_arg,
                                      uint8_t *ret_vector)
{
    errval_t err;
    uint32_t vector = INVALID_VECTOR;
    *ret_vector = 0;
    err = inthandler_setup(handler, handler_arg, &vector);
    if (err_is_ok(err)) {
        *ret_vector = vector + 32; // FIXME: HACK
    }
    return err;
}

errval_t pci_read_conf_header(uint32_t dword, uint32_t *val)
{
    errval_t err, msgerr;
    err = pci_client->vtbl.read_conf_header(pci_client, dword, &msgerr, val);
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_write_conf_header(uint32_t dword, uint32_t val)
{
    errval_t err, msgerr;
    err = pci_client->vtbl.write_conf_header(pci_client, dword, val, &msgerr);
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_msix_enable(uint16_t *count)
{
    errval_t err, msgerr;
    err = pci_client->vtbl.msix_enable(pci_client, &msgerr, count);
    return err_is_fail(err) ? err : msgerr;
}

errval_t pci_msix_vector_init(uint16_t idx, uint8_t destination,
                              uint8_t vector)
{
    errval_t err, msgerr;
    err = pci_client->vtbl.msix_vector_init(pci_client, idx, destination,
                                            vector, &msgerr);
    return err_is_fail(err) ? err : msgerr;

}

static void bind_cont(void *st, errval_t err, struct pci_binding *b)
{
    errval_t *reterr = st;
    if (err_is_ok(err)) {
        struct pci_rpc_client *r = malloc(sizeof(*r));
        assert(r != NULL);
        err = pci_rpc_client_init(r, b);
        if (err_is_ok(err)) {
            pci_client = r;
        } else {
            free(r);
        }
    }
    *reterr = err;
}

errval_t pci_client_connect(void)
{
    iref_t iref;
    errval_t err, err2 = SYS_ERR_OK;

    /* Connect to the pci server */
    err = nameservice_blocking_lookup("pci", &iref);
    if (err_is_fail(err)) {
        return err;
    }

    assert(iref != 0);

    /* Setup flounder connection with pci server */
    err = pci_bind(iref, bind_cont, &err2, get_default_waitset(),
                   IDC_BIND_FLAG_RPC_CAP_TRANSFER);
    if (err_is_fail(err)) {
        return err;
    }

    /* XXX: Wait for connection establishment */
    while (pci_client == NULL && err2 == SYS_ERR_OK) {
        messages_wait_and_handle_next();
    }

    return err2;
}
