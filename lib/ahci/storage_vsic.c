/*
 * Copyright (c) 2014, University of Washington.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/waitset.h>
#include <if/ata_rw28_defs.h>
#include <if/ata_rw28_ahci_defs.h>
#include <if/ata_rw28_rpcclient_defs.h>
#include <storage/vsic.h>

struct ahci_vsic {
    struct ahci_ata_rw28_binding ahci_ata_rw28_binding;
    struct ata_rw28_rpc_client ata_rw28_rpc;
    struct ata_rw28_binding *ata_rw28_binding;
    struct ahci_binding *ahci_binding;
    errval_t bind_err;
};

static errval_t vsic_write(struct storage_vsic *vsic, struct storage_vsa *vsa,
                           off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    struct ahci_vsic *mydata = vsic->data;
    errval_t status;

    errval_t err = mydata->ata_rw28_rpc.vtbl.
      write_dma(&mydata->ata_rw28_rpc, buffer, STORAGE_VSIC_ROUND(vsic, size),
		offset, &status);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "write_dma rpc");
    }
    if (err_is_fail(status)) {
        USER_PANIC_ERR(status, "write_dma status");
    }

    return SYS_ERR_OK;
}

static errval_t vsic_read(struct storage_vsic *vsic, struct storage_vsa *vsa,
                          off_t offset, size_t size, void *buffer)
{
    assert(vsic != NULL);
    assert(vsa != NULL);
    assert(buffer != NULL);
    struct ahci_vsic *mydata = vsic->data;
    uint8_t *buf = NULL;
    size_t bytes_read, toread = STORAGE_VSIC_ROUND(vsic, size);

    errval_t err = mydata->ata_rw28_rpc.vtbl.
      read_dma(&mydata->ata_rw28_rpc, toread, offset, &buf, &bytes_read);
    if (err_is_fail(err))
        USER_PANIC_ERR(err, "read_dma rpc");
    if (!buf)
        USER_PANIC("read_dma -> !buf");
    if (bytes_read != toread)
        USER_PANIC("read_dma -> read_size != size");

    // XXX: Copy from DMA buffer to user buffer
    memcpy(buffer, buf, size);
    free(buf);

    return SYS_ERR_OK;
}

static errval_t vsic_flush(struct storage_vsic *vsic, struct storage_vsa *vsa)
{
  assert(vsic != NULL);
  assert(vsa != NULL);
  struct ahci_vsic *mydata = vsic->data;
  errval_t outerr;

  errval_t err = mydata->ata_rw28_rpc.vtbl.
    flush_cache(&mydata->ata_rw28_rpc, &outerr);
  assert(err_is_ok(err));

  return outerr;
}

static errval_t vsic_wait(struct storage_vsic *vsic)
{
  // XXX: Interface currently synchronous
  return SYS_ERR_OK;
}

static struct storage_vsic_ops ahci_vsic_ops = {
    .write = vsic_write,
    .read = vsic_read,
    .flush = vsic_flush,
    .wait = vsic_wait,
};

static void ahci_bind_cb(void *st, errval_t err, struct ahci_binding *_binding)
{
    assert(err_is_ok(err));
    struct ahci_vsic *mydata = st;
    mydata->ahci_binding = _binding;
}

// XXX: This could be made public and controlled by the programmer instead of
// the commandline
static errval_t ahci_vsic_alloc(struct storage_vsic *vsic, uint8_t port)
{
    assert(vsic != NULL);
    errval_t err;
    struct ahci_vsic *mydata = malloc(sizeof(struct ahci_vsic));
    assert(mydata != NULL);
    memset(mydata, 0, sizeof(struct ahci_vsic));

    // init ahci management binding
    err = ahci_init(port, ahci_bind_cb, mydata, get_default_waitset());
    assert(err_is_ok(err));

    while(!mydata->ahci_binding) {
        event_dispatch(get_default_waitset());
    }

    // init ata flounder binding
    err = ahci_ata_rw28_init(&mydata->ahci_ata_rw28_binding,
                             get_default_waitset(), mydata->ahci_binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_ata_rw28_init");
    }
    mydata->ata_rw28_binding =
        (struct ata_rw28_binding*)&mydata->ahci_ata_rw28_binding;

    // init RPC client
    err = ata_rw28_rpc_client_init(&mydata->ata_rw28_rpc,
                                   mydata->ata_rw28_binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ata_rw28_rpc_client_init");
    }

    // Init VSIC data structure
    vsic->ops = ahci_vsic_ops;
    vsic->data = mydata;
    vsic->blocksize = 512;	// XXX: Determine from drive?

    return SYS_ERR_OK;
}

errval_t storage_vsic_driver_init(int argc, const char **argv,
				  struct storage_vsic *vsic)
{
    // init dma pool
    ahci_dma_pool_init(1024*1024);

    // Allocate port 0
    return ahci_vsic_alloc(vsic, 0);
}
