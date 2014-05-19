/*
 * Copyright (c) 2009, 2011, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#define _USE_XOPEN // for strdup()
#include <stdio.h>
#include <string.h>
#include <barrelfish/barrelfish.h>
#include <barrelfish/nameservice_client.h>
#include <if/ahci_mgmt_defs.h>
#include <if/ahci_mgmt_rpcclient_defs.h>
#include <if/ahci_mgmt_defs.h>
#include <if/ata_rw28_defs.h>
#include <if/ata_rw28_ahci_defs.h>
#include <if/ata_rw28_rpcclient_defs.h>
#include <dev/ata_identify_dev.h>
#include <dev/ahci_port_dev.h>
#include <ahci/ahci.h>
#include <ahci/sata_fis.h>
#include <ahci/ahci_dma_pool.h>

#include "vfs_blockdevfs.h"

static struct ahci_mgmt_rpc_client ahci_mgmt_rpc;
static struct ahci_mgmt_binding *ahci_mgmt_binding;
static bool ahci_mgmt_bound = false;

#define RFIS_OFFSET_DMA_SETUP_FIS 0x00
#define RFIS_OFFSET_PIO_SETUP_FIS 0x20
#define RFIS_OFFSET_D2H_REGISTER_FIS 0x40
#define RFIS_OFFSET_SET_DEVICE_BITS_FIS 0x58
#define RFIS_OFFSET_UNKNOWN_FIS 0x60

struct ata_handle {
    struct ahci_binding *ahci_binding;
    struct ata_rw28_binding *ata_rw28_binding;
    struct ata_rw28_rpc_client ata_rw28_rpc;
    uint8_t port_num;
    bool waiting;
    errval_t wait_status;
    size_t bytes_transferred;
};

static void ahci_init_cb(void *st, errval_t err, struct ahci_binding *binding)
{
    struct ata_handle *h = st;

    if (err_is_fail(err)) {
        printf("ahci_init_cb returned '%s'\n", err_getstring(err));
        h->wait_status = err;
        h->waiting = false;
        return;
    }

    h->ahci_binding = binding;
    binding->st = h;
    h->waiting = false;
}

errval_t blockdevfs_ata_open(void *handle)
{
    VFS_BLK_DEBUG("blockdevfs_ata_open: entering\n");
    errval_t err;
    struct ata_handle *h = handle;

    h->wait_status = SYS_ERR_OK;
    h->waiting = true;

    err = ahci_init(h->port_num, ahci_init_cb, h, get_default_waitset());
    if (err_is_fail(err)) {
        printf("ahci_init failed: '%s'\n", err_getstring(err));
        h->waiting = false;
        return err;
    }

    // XXX: block for command completion (broken API!)
    while (h->waiting) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for blockdevfs_ata_open");
        }
    }

    struct ahci_ata_rw28_binding *ahci_ata_rw28_binding;
    ahci_ata_rw28_binding = calloc(1, sizeof(struct ahci_ata_rw28_binding));
    ahci_ata_rw28_init(ahci_ata_rw28_binding, get_default_waitset(), h->ahci_binding);
    h->ata_rw28_binding = (struct ata_rw28_binding*)ahci_ata_rw28_binding;
    err = ata_rw28_rpc_client_init(&h->ata_rw28_rpc, h->ata_rw28_binding);
    if (err_is_fail(err)) {
        // TODO: bindings leak
        VFS_BLK_DEBUG("blockdevfs_ata_open: failed to init ata_rw28 rpc client\n");
        return err;
    }
    VFS_BLK_DEBUG("blockdevfs_ata_open: exiting\n");
    return h->wait_status;
}

static void
ahci_close_cb(void *arg)
{
    struct ata_handle *h = arg;
    h->waiting = false;
    h->wait_status = SYS_ERR_OK;
}

errval_t blockdevfs_ata_flush(void *handle)
{
    struct ata_handle *h = handle;
    errval_t err, status;
    err = h->ata_rw28_rpc.vtbl.flush_cache(&h->ata_rw28_rpc, &status);
    if (err_is_fail(err)) {
        printf("failed calling flush_cache\n");
        return err;
    }
    VFS_BLK_DEBUG("status: %s\n", err_getstring(status));
    return err;
}

errval_t blockdevfs_ata_close(void *handle)
{
    struct ata_handle *h = handle;
    errval_t err, status;

    err = h->ata_rw28_rpc.vtbl.flush_cache(&h->ata_rw28_rpc, &status);
    if (err_is_fail(err)) {
        printf("failed calling flush_cache\n");
        return err;
    }
    VFS_BLK_DEBUG("status: %s\n", err_getstring(status));

    free(h->ata_rw28_binding);
    h->waiting = true;
    err = ahci_close(h->ahci_binding, MKCLOSURE(ahci_close_cb, h));
    if (err_is_fail(err)) {
        printf("ahci_init failed: '%s'\n", err_getstring(err));
        h->waiting = false;
        return err;
    }

    // XXX: block for command completion (broken API!)
    while (h->waiting) {
        err = event_dispatch(h->ahci_binding->waitset);
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for blockdevfs_ata_close");
        }
    }

    return h->wait_status;
}

errval_t blockdevfs_ata_read(void *handle, size_t pos, void *buffer,
        size_t bytes, size_t *bytes_read)
{
    errval_t err;
    struct ata_handle *h = handle;
    size_t aligned_bytes = bytes / PR_SIZE * PR_SIZE;
    size_t blockpos = pos / PR_SIZE;

    //VFS_BLK_DEBUG("bdfs_ahci: read begin: %zu -> %zu\n", bytes, aligned_bytes);

    uint8_t *data;
    err = h->ata_rw28_rpc.vtbl.read_dma(&h->ata_rw28_rpc,
            aligned_bytes, blockpos, &data, bytes_read);
    memcpy(buffer, data, *bytes_read);
    free(data);

    return err;
}

errval_t blockdevfs_ata_write(void *handle, size_t pos, const void *buffer,
        size_t bytes, size_t *bytes_written)
{
    errval_t err;
    struct ata_handle *h = handle;

    size_t aligned_bytes = bytes / PR_SIZE * PR_SIZE;
    size_t blockpos = pos / PR_SIZE;

    errval_t status;
    err = h->ata_rw28_rpc.vtbl.write_dma(&h->ata_rw28_rpc,
            buffer, aligned_bytes, blockpos, &status);
    *bytes_written = aligned_bytes;
    return err;
}

static void ahci_mgmt_bind_cb(void *st, errval_t err, struct ahci_mgmt_binding *b)
{
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_mgmt bind failed in callback");
    }

    ahci_mgmt_binding = b;
    err = ahci_mgmt_rpc_client_init(&ahci_mgmt_rpc, b);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_mgmt RPC init failed");
    }

    ahci_mgmt_bound = true;

    // populate list
    uint8_t *port_ids;
    size_t len;

    err = ahci_mgmt_rpc.vtbl.list(&ahci_mgmt_rpc, &port_ids, &len);
    assert(err_is_ok(err));

    for (size_t i = 0; i < len; i++) {
        if (i > 9) {
            break;
        }
        uint8_t *data;
        size_t identifylen = 0;
        err = ahci_mgmt_rpc.vtbl.identify(&ahci_mgmt_rpc,
                port_ids[i], &data, &identifylen);
        assert(err_is_ok(err));
        assert(identifylen == 512);

        ata_identify_t identify;
        ata_identify_initialize(&identify, (void *)data);

        //char buf[8192];
        //ata_identify_pr(buf, 8192, &identify);
        //printf("New Disk. Identify:\n");
        //puts(buf);

        // determine sector size
        size_t sector_size = 512;
        if (ata_identify_plss_lls_rdf(&identify)) {
            sector_size = 2*ata_identify_wpls_rd(&identify);
        }

        // FIXME: support LBA28
        size_t sectors = ata_identify_tnuas48_rd(&identify);

        VFS_BLK_DEBUG("Disk %" PRIu8 " has %zd sectors of %zd bytes\n",
                port_ids[i], sectors, sector_size);

        struct ata_handle *handle = calloc(1, sizeof(struct ata_handle));
        handle->port_num = port_ids[i];

        struct blockdev_entry *newentry = calloc(1, sizeof(struct blockdev_entry));
        newentry->open = false;
        newentry->size = sectors * sector_size;
        newentry->type = blockdev_backend_type_ata;
        newentry->path = strdup("ataX");
        newentry->path[3] = '0' + i; // FIXME: make it work for more than 9
        newentry->backend_handle = handle;

        // append to list
        blockdev_append_entry(newentry);
    }
}

errval_t blockdevfs_ata_init(void)
{
    errval_t err;
    iref_t iref;

    err = nameservice_blocking_lookup("ahcid", &iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "nameservice_blocking_lookup for ahcid");
        return err; // FIXME
    }

    err = ahci_mgmt_bind(iref, ahci_mgmt_bind_cb, NULL, get_default_waitset(),
                  IDC_BIND_FLAG_RPC_CAP_TRANSFER);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "ahci_mgmt bind failed");
        return err; // FIXME
    }

    // init DMA pool
    ahci_dma_pool_init(1024*1024);

    // XXX: block for bind completion (broken API!)
    while (!ahci_mgmt_bound) {
        err = event_dispatch(get_default_waitset());
        if (err_is_fail(err)) {
            USER_PANIC_ERR(err, "error in event_dispatch for blockdevfs_ata_init");
        }
    }

    return SYS_ERR_OK;
}
