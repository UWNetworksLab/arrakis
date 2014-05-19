/*
 * Copyright (c) 2011, 2012, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <barrelfish/waitset_chan.h>
#include <barrelfish/nameservice_client.h>
#include <if/ahci_mgmt_defs.h>
#include <dev/ahci_hba_dev.h> // this is used to parse HBA capabilities
#include <dev/ahci_port_dev.h>
#include <ahci/ahci.h>
#include <ahci/ahci_dma_pool.h>
#include <ahci/ahci_util.h>
#include <string.h>
#include "ahci_debug.h"
#include "ahci_internal.h"

static struct ahci_mgmt_binding *mgmt_binding;

struct bind_st { // this is to be able to pass 3 params as user state
    size_t num_ports;
    struct ahci_binding **ahci_binding;
    ahci_bind_continuation_fn *cont;
    void *st;
    uint8_t port; // only valid in open and identify cb
};

struct mgmt_close_call_st
{
    struct ahci_binding *binding;
    struct event_closure continuation;
};

static void ahci_mgmt_identify_response_cb(struct ahci_mgmt_binding *b,
        uint8_t *identify_data, size_t data_len)
{
    struct bind_st *bst = b->st;
    struct ahci_binding *ahci_binding = bst->ahci_binding[bst->port];

    if (!ahci_binding) {
        AHCI_DEBUG("got identify data for unbound port\n");
        return;
    }

    bool has_identify = ahci_binding->identify_data;
    if (has_identify) {
        free(ahci_binding->identify_data);
    }
    ahci_binding->identify_data = identify_data;
    ahci_binding->identify_length = data_len;
    ata_identify_initialize(&ahci_binding->identify,
            (void*)ahci_binding->identify_data);

    if (!has_identify) {
        struct ahci_port_info *port = &ahci_binding->port_info;

        // enable rFIS area and start running commands
        ahci_port_cmd_t cmd = ahci_port_cmd_rd(&port->port);
        cmd = ahci_port_cmd_fre_insert(cmd, 1);
        cmd = ahci_port_cmd_st_insert(cmd, 1);
        ahci_port_cmd_wr(&port->port, cmd);

        // enable all interrupts of this port
        AHCI_DEBUG("enabling all port interrupts\n");
        ahci_port_ie_wr(&port->port, -1);

        // fire off the original binding callback
        if (bst->cont) {
            bst->cont(bst->st, SYS_ERR_OK, ahci_binding);
        }
    }

    AHCI_DEBUG("ahci_mgmt_identify_response_cb: exiting\n");
}

static void ahci_mgmt_open_cb(struct ahci_mgmt_binding *b, errval_t status,
        struct capref controller_mem, uint64_t offset, uint32_t capabilities)
{
    AHCI_DEBUG("open cb\n");
    struct bind_st *bst = b->st;
    errval_t err = status, cleanup_err;
    struct ahci_binding *ahci_binding;

    if (bst->num_ports == 0) {
        // init bind_st properly
        ahci_binding = bst->ahci_binding[0];
        bst->num_ports = ahci_hba_cap_np_extract(capabilities) + 1;
        void *tmp = calloc(bst->num_ports, sizeof(struct ahci_binding *));
        if (tmp == NULL) {
            err = LIB_ERR_MALLOC_FAIL;
            goto error_status;
        }
        bst->ahci_binding = tmp;
        bst->ahci_binding[bst->port] = ahci_binding;
    }
    else {
        // get correct ahci binding
        ahci_binding = bst->ahci_binding[bst->port];
    }

    struct ahci_port_info *port = &ahci_binding->port_info;

    // check status code from management daemon
    if (err_is_fail(status)) {
        DEBUG_ERR(status, "failed to open port: '%s'", err_getstring(status));
        goto error_status;
    }

    // HBA caps
    port->hba_capabilities = capabilities;

    // init port
    AHCI_DEBUG("mapping port in our address space\n");
    // map device in our address space
    err = vspace_map_one_frame_attr(&port->mapped_vaddr, offset + PORT_SIZE,
            controller_mem, VREGION_FLAGS_READ_WRITE_NOCACHE, NULL, NULL);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "vspace_map_one_frame failed: '%s'", err_getstring(err));
        goto error_map_controller;
    }
    port->port_base = (char *)port->mapped_vaddr + offset;

    // store controller mem cap in port
    port->hba_cap = controller_mem;

    // setup mackerel struct for port
    ahci_port_initialize(&port->port, port->port_base);
    err = ahci_port_alloc_dma_structs(&port->port,
            &port->command_list, &port->receive_fis);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "ahci_port_alloc_dma_structs failed");
        goto error_dma_allocate;
    }
    AHCI_DEBUG("enabling rFIS area + start running commands\n");

    // fetch identify data from management daemon
    err = ahci_mgmt_identify_call__tx(b, NOP_CONT, ahci_binding->port_id);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "failed to send identify_call to ahci_mgmt");
        goto error_identify_send;
    }

    goto end;

error_identify_send:
    // free port dma region
    ahci_port_free_dma_structs(port);

error_dma_allocate:
    // cleanup mapping of controller mem
    cleanup_err = vspace_unmap(port->mapped_vaddr);
    if (err_is_fail(cleanup_err)) {
        USER_PANIC_ERR(cleanup_err, "vspace_unmap failed");
    }

error_map_controller:
    // destroy controller mem cap
    cleanup_err = cap_destroy(controller_mem);
    if (err_is_fail(cleanup_err)) {
        USER_PANIC_ERR(cleanup_err, "cap_destroy failed");
    }

error_status:
    // cleanup semi-initialized libahci binding
    waitset_chanstate_destroy(&ahci_binding->register_chanstate);
    waitset_chanstate_destroy(&ahci_binding->tx_cont_chanstate);

    free(ahci_binding);
    bst->ahci_binding[bst->port] = NULL;

    if (bst->cont) {
        bst->cont(bst->st, err, NULL);
    }

    // free user state
    free(bst);

end:
    AHCI_DEBUG("ahci_mgmt_open_cb: exiting\n");
}

static void ahci_mgmt_command_completed_cb(struct ahci_mgmt_binding *b,
        uint8_t port_id, uint32_t interrupt_status)
{
    AHCI_DEBUG("got command completed message\n");
    struct bind_st *bst = b->st;
    struct ahci_port_info *port_info = &bst->ahci_binding[port_id]->port_info;

    if (interrupt_status == 0) {
        // ghost irq, ignore. (although this should not even get to here)
        return;
    }

    AHCI_DEBUG("interrupt status = 0x%"PRIx32"\n", interrupt_status);

    uint8_t tfes, ifs, hbfs, hbds;
    if ((tfes = ahci_port_is_tfes_extract(interrupt_status)) ||
        (ifs = ahci_port_is_ifs_extract(interrupt_status)) ||
        (hbfs = ahci_port_is_hbfs_extract(interrupt_status)) ||
        (hbds = ahci_port_is_hbds_extract(interrupt_status)))
    {
        char buf[4096];
        ahci_port_serr_t errstate = ahci_port_serr_rd(&port_info->port);
        ahci_port_tfd_t tfd = ahci_port_tfd_rd(&port_info->port);
        ahci_dump_rfis(port_info);
        ahci_port_serr_prtval(buf, 4096, errstate);
        puts(buf);
        printf("task file data:\n");
        ahci_port_tfd_prtval(buf, 4096, tfd);
        puts(buf);
        assert(!"error");
    }
    else if (ahci_port_is_infs_extract(interrupt_status)) {
        char buf[4096];
        ahci_port_serr_t errstate = ahci_port_serr_rd(&port_info->port);
        ahci_port_serr_prtval(buf, 4096, errstate);
        puts(buf);
        assert(!"non-fatal error");
    }

    uint32_t ci = ahci_port_ci_rd(&port_info->port);

    // which commands have been completed?
    for (int i = 0; i < 32; i++) {
        if (!port_info->command_slots[i].in_use || (ci & (1<<i))) {
            continue; // skip slots we didn't use or that have not been processed yet
        }

        // free command table
        AHCI_DEBUG("freeing command table for slot %d (in_use=%d): %p\n",
                i, port_info->command_slots[i].in_use,
                port_info->command_slots[i].command_table);
        ahci_dma_region_free(port_info->command_slots[i].command_table);
        port_info->command_slots[i].command_table = 0;

        void *tag = port_info->command_slots[i].tag;
        port_info->command_slots[i].tag = 0;
        port_info->command_slots[i].in_use = false;

        AHCI_DEBUG("dispatching to user level\n");
        // FIXME: does this make sense? will we even return to here in the near future?
        bst->ahci_binding[port_id]->rx_vtbl.command_completed(bst->ahci_binding[port_id], tag);
        AHCI_DEBUG("return from user level\n");
    }

    AHCI_DEBUG("cc_cb exiting\n");
}

static void ahci_mgmt_close_cb(struct ahci_mgmt_binding *_binding, errval_t status)
{
    AHCI_DEBUG("mgmt close cb\n");
    if (err_is_fail(status)) {
        DEBUG_ERR(status, "close callback");
    }
    struct bind_st *bst = _binding->st;
    struct mgmt_close_call_st *st = bst->st;
    if (st->continuation.handler != NULL) {
        waitset_chan_trigger_closure(st->binding->waitset,
                &st->binding->tx_cont_chanstate, st->continuation);
    }
    free(st->binding->identify_data);
    free(st->binding);
    free(st);
}

static void ahci_mgmt_bind_cb(void *st, errval_t err, struct ahci_mgmt_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_mgmt bind failed in callback");
    }

    struct bind_st *bst = st;

    // set up our binding to the mgmt daemon
    mgmt_binding = b;
    b->rx_vtbl.open_response = ahci_mgmt_open_cb;
    b->rx_vtbl.command_completed = ahci_mgmt_command_completed_cb;
    b->rx_vtbl.close_response = ahci_mgmt_close_cb;
    b->rx_vtbl.identify_response = ahci_mgmt_identify_response_cb;

    b->st = bst;

    // try to open the port
    ahci_mgmt_open_call__tx(b, NOP_CONT, bst->port);
}

static bool ahci_can_send(struct ahci_binding *_binding)
{
    return ahci_find_free_command_slot(&_binding->port_info) != -1;
}

static errval_t ahci_register_send(struct ahci_binding *_binding,
        struct waitset *ws, struct event_closure _continuation)
{
    if (ahci_can_send(_binding)) {
        return waitset_chan_trigger_closure(ws,
                &(_binding->register_chanstate), _continuation);
    }
    else {
        return waitset_chan_register(ws,
                &(_binding->register_chanstate), _continuation);
    }
}

static errval_t ahci_change_waitset(struct ahci_binding *_binding, struct waitset *ws)
{
    AHCI_DEBUG("change waitset, ws = %p\n", ws);
    // TODO: re-register somewhere?
    _binding->waitset = ws;
    mgmt_binding->change_waitset(mgmt_binding, ws);

    return SYS_ERR_OK;
}

static errval_t ahci_control(struct ahci_binding *_binding, idc_control_t control)
{
    // TODO: implement
    return SYS_ERR_OK;
}

static void ahci_error_handler(struct ahci_binding *_binding, errval_t err)
{
    DEBUG_ERR(err, "Asynchronous in ahci messaging. Aborting.");
    abort();
}

errval_t ahci_init(uint8_t port, ahci_bind_continuation_fn *_continuation,
        void *st, struct waitset *waitset)
{
    AHCI_DEBUG("ahci_init: waitset = %p\n", waitset);
    errval_t err;
    iref_t iref;

    // setup binding struct
    struct ahci_binding *binding = calloc(1, sizeof(struct ahci_binding));
    binding->st = NULL;
    binding->waitset = waitset;
    event_mutex_init(&binding->mutex, waitset);
    /* set up management function pointers */
    binding->can_send = ahci_can_send;
    binding->register_send = ahci_register_send;
    binding->change_waitset = ahci_change_waitset;
    binding->control = ahci_control;
    binding->error_handler = ahci_error_handler;
    memset(&binding->rx_vtbl, 0, sizeof(binding->rx_vtbl));
    waitset_chanstate_init(&binding->register_chanstate, CHANTYPE_AHCI);
    waitset_chanstate_init(&binding->tx_cont_chanstate, CHANTYPE_AHCI);

    //binding->st = st;
    binding->port_id = port;

    // setup binding to ahcid and init dma pool
    if (mgmt_binding == NULL) {
        err = nameservice_blocking_lookup("ahcid", &iref);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "nameservice_blocking_lookup for ahcid");
            return err; // FIXME
        }

        // couple continuation and binding
        struct bind_st *bst = malloc(sizeof(struct bind_st));
        assert(bst != NULL);
        bst->cont = _continuation;
        // temp; used as flag in open_cb
        bst->num_ports = 0;
        // temp; realloc as soon as we know num_ports
        bst->ahci_binding = malloc(sizeof(struct ahci_binding*));
        bst->ahci_binding[0] = binding;
        bst->port = port;
        bst->st = st;

        // init dma pool to 1M
        ahci_dma_pool_init(1024 * 1024);

        err = ahci_mgmt_bind(iref, ahci_mgmt_bind_cb, bst, waitset,
                IDC_BIND_FLAG_RPC_CAP_TRANSFER);

        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ahci_mgmt bind failed");
            return err; // FIXME
        }
    }
    else {
        // change mgmt binding waitset to supplied waitset
        struct bind_st *bst = mgmt_binding->st;
        bst->cont = _continuation;
        bst->ahci_binding[port] = binding;
        bst->port = port;
        bst->st = st;

        // try to open the port
        mgmt_binding->change_waitset(mgmt_binding, waitset);
        ahci_mgmt_open_call__tx(mgmt_binding, NOP_CONT, binding->port_id);
    }

    AHCI_DEBUG("ahci_init: exiting\n");
    return SYS_ERR_OK;
}

errval_t ahci_issue_command(struct ahci_binding *_binding,
        struct event_closure _continuation, void *tag, uint8_t *fis,
        size_t fis_length, bool is_write, struct ahci_dma_region *buf, size_t buflen)
{
    errval_t err = 0;

    AHCI_DEBUG("entering ahci_issue_command: tag = %p; buf = %p; buflen = %zd\n",
            tag, buf, buflen);

    struct ahci_port_info *port = &_binding->port_info;
    int command;
    AHCI_DEBUG("ahci_issue_command: fis_length = %zd\n", fis_length);
    size_t num_prds = 0;
    if (buf) {
        assert((buflen & 1) == 0); // force even byte count
#ifdef AHCI_FIXED_PR_SIZE
        num_prds = CEIL_DIV(buflen, PR_SIZE);
#else
        num_prds = CEIL_DIV(buflen, MAX_PR_SIZE);
#endif
    }
    err = ahci_setup_command(&command, port, fis, fis_length, num_prds, is_write);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "ahci_setup_command failed");
        return err;
    }

    // save tag
    port->command_slots[command].tag = tag;

    if (buf) {
        err = ahci_add_physical_regions(port, command, buf, buflen);
        if (err_is_fail(err)) {
            DEBUG_ERR(err, "ahci_add_physical_regions failed");
            return err;
        }
    }

    // issue command
    ahci_port_ci_wr(&port->port, (1<<command));

    AHCI_DEBUG("ahci_load_fis: calling user continuation\n");
    if (_continuation.handler != NULL) {
        waitset_chan_trigger_closure(_binding->waitset,
                &_binding->tx_cont_chanstate, _continuation);
    }
    AHCI_DEBUG("ahci_load_fis: exiting\n");
    return SYS_ERR_OK;
}

errval_t ahci_close(struct ahci_binding *_binding, struct event_closure _continuation)
{
    AHCI_DEBUG("ahci_close: callback = %p\n", _continuation.handler);
    struct mgmt_close_call_st *st = calloc(1, sizeof(struct mgmt_close_call_st));
    if (!st) {
        return LIB_ERR_MALLOC_FAIL;
    }
    st->binding = _binding;
    st->continuation = _continuation;
    struct bind_st *bst = mgmt_binding->st;
    bst->st = st;
    // unmap controller memory
    errval_t err = cap_destroy(_binding->port_info.hba_cap);
    if (err_is_fail(err)) {
        printf("cap_destroy: %s (%ld)\n", err_getstring(err), err);
    }
    ahci_mgmt_close_call__tx(mgmt_binding, NOP_CONT, _binding->port_id);

    return SYS_ERR_OK;
}
