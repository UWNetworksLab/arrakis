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

struct ahci_handle {
    struct ahci_binding *binding;
    uint8_t port_num;
    bool waiting;
    errval_t wait_status;
    size_t bytes_transferred;
};

static void ahci_init_cb(void *st, errval_t err, struct ahci_binding *binding)
{
    struct ahci_handle *h = st;

    if (err_is_fail(err)) {
        printf("ahci_init_cb returned '%s'\n", err_getstring(err));
        h->wait_status = err;
        h->waiting = false;
        return;
    }

    h->binding = binding;
    binding->st = h;
    h->waiting = false;
}

errval_t blockdevfs_ahci_open(void *handle)
{
    VFS_BLK_DEBUG("blockdevfs_ahci_open: entering\n");
    errval_t err;
    struct ahci_handle *h = handle;

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
        messages_wait_and_handle_next();
    }

    VFS_BLK_DEBUG("blockdevfs_ahci_open: exiting\n");
    return h->wait_status;
}

static void ahci_close_cb(void *arg)
{
    struct ahci_handle *h = arg;
    h->waiting = false;
}

errval_t blockdevfs_ahci_close(void *handle)
{
    struct ahci_handle *h = handle;
    h->waiting = true;
    errval_t err = ahci_close(h->binding, MKCLOSURE(ahci_close_cb, h));
    if (err_is_fail(err)) {
        printf("ahci_init failed: '%s'\n", err_getstring(err));
        h->waiting = false;
        return err;
    }
    while (h->waiting) {
        messages_wait_and_handle_next();
    }
    return SYS_ERR_OK;
}

static void rx_flush_command_completed_cb(struct ahci_binding *binding, void *tag)
{
    struct ahci_handle *h = binding->st;
    VFS_BLK_DEBUG("rx_flush_command_completed_cb(%p, tag: %p): entering\n",
            binding, tag);
    h->waiting = false;
}

errval_t blockdevfs_ahci_flush(void *handle)
{
    errval_t err = SYS_ERR_OK;
    struct ahci_handle *h = handle;
    h->waiting = true;
    // setup FIS
    struct sata_fis_reg_h2d fis;
    memset(&fis, 0, sizeof(struct sata_fis_reg_h2d));
    fis.type = SATA_FIS_TYPE_H2D;
    fis.device = 1 << 6; /* LBA mode, not CHS; ??? */
    sata_set_command(&fis, 0xE7); /* flush cache; ATA Command Set, 7.24 */
    sata_set_count(&fis, 0); /* nr. of sectors/blocks */
    sata_set_lba28(&fis, 0);

    // set handle to waiting
    h->waiting = true;
    h->wait_status = SYS_ERR_OK;
    h->bytes_transferred = 0;
    h->binding->rx_vtbl.command_completed = rx_flush_command_completed_cb;

    // load fis and fire commands
    err = ahci_issue_command(h->binding, NOP_CONT, 0,
            (uint8_t*)&fis, sizeof(fis), false, NULL, 0);
    if (err_is_fail(err)) {
        printf("bdfs_ahci: read load_fis failed: 0x%" PRIxPTR "\n", err);
        h->waiting = false;
        goto cleanup;
    }

    while (h->waiting) {
        messages_wait_and_handle_next();
    }

cleanup:

    h->binding->rx_vtbl.command_completed = NULL;

    return err;
}

static void rx_read_command_completed_cb(struct ahci_binding *binding, void *tag)
{
    struct ahci_handle *h = binding->st;
    VFS_BLK_DEBUG("rx_read_command_completed_cb(%p, tag: %p): entering\n",
            binding, tag);

    h->waiting = false;
}

errval_t blockdevfs_ahci_read(void *handle, size_t pos, void *buffer, size_t
        bytes, size_t *bytes_read)
{
    errval_t err;
    struct ahci_handle *h = handle;
    size_t aligned_bytes = bytes / PR_SIZE * PR_SIZE;

    VFS_BLK_DEBUG("bdfs_ahci: read begin: %zu -> %zu\n", bytes, aligned_bytes);

    // setup DMA regions for receiving data
    struct ahci_dma_region *bufregion = NULL;
    err = ahci_dma_region_alloc(aligned_bytes, &bufregion);
    if (err_is_fail(err)) {
        printf("bdfs_ahci: read alloc_region failed: 0x%" PRIxPTR  "\n", err);
        return err;
    }
    VFS_BLK_DEBUG("bdfs_ahci_read: bufregion = %p\n", bufregion);
    VFS_BLK_DEBUG("bdfs_ahci_read: bufregion->vaddr = %p\n", bufregion->vaddr);

    // setup FIS
    struct sata_fis_reg_h2d read_fis;
    memset(&read_fis, 0, sizeof(struct sata_fis_reg_h2d));
    read_fis.type = SATA_FIS_TYPE_H2D;
    read_fis.device = 1 << 6; // LBA mode, not CHS; ???
    sata_set_command(&read_fis, 0xC8); // read dma; ATA Command Set, 7.24
    sata_set_count(&read_fis, aligned_bytes / PR_SIZE); // nr. of sectors/blocks
    sata_set_lba28(&read_fis, pos / PR_SIZE);

    // set handle to waiting
    h->waiting = true;
    h->wait_status = SYS_ERR_OK;
    h->bytes_transferred = 0;
    h->binding->rx_vtbl.command_completed = rx_read_command_completed_cb;

    // load fis and fire commands
    err = ahci_issue_command(h->binding, NOP_CONT, 0,
            (uint8_t*)&read_fis, sizeof(read_fis), false, bufregion, aligned_bytes);
    if (err_is_fail(err)) {
        printf("bdfs_ahci: read load_fis failed: 0x%" PRIxPTR  "\n", err);
        h->waiting = false;
        goto cleanup;
    }

    // XXX: block for command completion (broken API!)
    while (h->waiting) {
        messages_wait_and_handle_next();
    }

    VFS_BLK_DEBUG("bdfs_ahci: read wait status: %lu\n", h->wait_status);

    err = h->wait_status;

    // cleanup and output
    if (err_is_ok(err)) {
        ahci_dma_region_copy_out(bufregion, buffer, 0, aligned_bytes);
        *bytes_read = aligned_bytes;
    }

cleanup:

    h->binding->rx_vtbl.command_completed = NULL;

    VFS_BLK_DEBUG("read: freeing bufregion (%p)\n", bufregion);
    ahci_dma_region_free(bufregion);

    return err;

}

static void rx_write_command_completed_cb(struct ahci_binding *binding, void *tag)
{
    struct ahci_handle *h = binding->st;
    VFS_BLK_DEBUG("rx_write_command_completed_cb(%p, tag: %p): entering\n",
            binding, tag);
    h->waiting = false;
}

errval_t blockdevfs_ahci_write(void *handle, size_t pos, const void *buffer,
        size_t bytes, size_t *bytes_written)
{
    errval_t err;
    struct ahci_handle *h = handle;

    size_t aligned_bytes = bytes / PR_SIZE * PR_SIZE;

    // setup DMA regions and copy data over
    struct ahci_dma_region *bufregion;
    err = ahci_dma_region_alloc(aligned_bytes, &bufregion);
    if (err_is_fail(err)) {
        return err;
    }
    ahci_dma_region_copy_in(bufregion, buffer, 0, aligned_bytes);

    // setup FIS
    struct sata_fis_reg_h2d write_fis;
    memset(&write_fis, 0, sizeof(struct sata_fis_reg_h2d));
    write_fis.type = SATA_FIS_TYPE_H2D;
    write_fis.device = 1 << 6; // LBA mode, not CHS; ???
    sata_set_command(&write_fis, 0xCA); // write dma; ATA Command Set, 7.60
    sata_set_count(&write_fis, aligned_bytes / PR_SIZE); // nr. of sectors/blocks
    sata_set_lba28(&write_fis, pos / PR_SIZE);

    // set handle to waiting
    h->waiting = true;
    h->wait_status = SYS_ERR_OK;
    h->binding->rx_vtbl.command_completed = rx_write_command_completed_cb;

    // load fis and fire commands
    err = ahci_issue_command(h->binding, NOP_CONT, 0,
            (uint8_t*)&write_fis, sizeof(write_fis), true, bufregion, aligned_bytes);
    if (err_is_fail(err)) {
        h->waiting = false;
        ahci_dma_region_free(bufregion);
        return err;
    }

    // XXX: block for command completion (broken API!)
    while (h->waiting) {
        messages_wait_and_handle_next();
    }

    // cleanup and output
    h->binding->rx_vtbl.command_completed = NULL;
    ahci_dma_region_free(bufregion);
    if (err_is_ok(h->wait_status)) {
        *bytes_written = aligned_bytes;
    }
    return h->wait_status;
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

        struct ahci_handle *handle = calloc(1, sizeof(struct ahci_handle));
        handle->port_num = port_ids[i];

        struct blockdev_entry *newentry = calloc(1, sizeof(struct blockdev_entry));
        newentry->open = false;
        newentry->size = sectors * sector_size;
        newentry->type = blockdev_backend_type_ahci;
        newentry->path = strdup("ahciX");
        newentry->path[4] = '0' + i; // FIXME: make it work for more than 9
        newentry->backend_handle = handle;

        // append to list
        blockdev_append_entry(newentry);
    }
}

errval_t blockdevfs_ahci_init(void)
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

    // init DMA pool
    ahci_dma_pool_init(1024*1024);

    if (err_is_fail(err)) {
        DEBUG_ERR(err, "ahci_mgmt bind failed");
        return err; // FIXME
    }

    // XXX: block for bind completion (broken API!)
    while (!ahci_mgmt_bound) {
        messages_wait_and_handle_next();
    }

    return SYS_ERR_OK;
}
