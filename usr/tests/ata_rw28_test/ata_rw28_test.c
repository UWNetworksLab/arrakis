/**
 * \file
 * \brief Tests for ata_rw28 interface and AHCI backend
 */

/*
 * Copyright (c) 2011, ETH Zurich.
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

struct ahci_ata_rw28_binding ahci_ata_rw28_binding;
struct ata_rw28_rpc_client ata_rw28_rpc;
struct ata_rw28_binding *ata_rw28_binding = NULL;
struct ahci_binding *ahci_binding = NULL;
volatile errval_t bind_err = SYS_ERR_OK;

static void write_and_check_32(uint32_t pat, size_t start_lba, size_t block_size, size_t block_count)
{
    printf("write_and_check_32(pat=0x%"PRIx32", start_lba=%zu, block_size=%zu, block_count=%zu)\n",
            pat, start_lba, block_size, block_count);
    errval_t err;
    size_t bytes = block_size*block_count;
    uint8_t *buf = malloc(bytes);
    assert(buf);
    size_t step = sizeof(pat);
    size_t count = bytes / step;
    assert(bytes % sizeof(pat) == 0);
    for (size_t i = 0; i < count; ++i)
        *(uint32_t*)(buf+i*step) = pat;

    printf("writing data\n");
    errval_t status;
    err = ata_rw28_rpc.vtbl.write_dma(&ata_rw28_rpc, buf, bytes, start_lba, &status);
    if (err_is_fail(err))
        USER_PANIC_ERR(err, "write_dma rpc");
    if (err_is_fail(status))
        USER_PANIC_ERR(status, "write_dma status");
    free(buf);

    printf("reading data\n");
    size_t bytes_read;
    err = ata_rw28_rpc.vtbl.read_dma(&ata_rw28_rpc, bytes, start_lba, &buf, &bytes_read);
    if (err_is_fail(err))
        USER_PANIC_ERR(err, "read_dma rpc");
    if (!buf)
        USER_PANIC("read_dma -> !buf");
    if (bytes_read != bytes)
        USER_PANIC("read_dma -> read_size != size");

    printf("checking data\n");
    for (size_t i = 0; i < count; ++i)
    {
        uint32_t val = *(uint32_t*)(buf+i*step);
        if (val != pat)
        {
            printf("pattern at byte offset 0x%zx differs: "
                    "got 0x%"PRIx32", exp 0x%"PRIx32"\n",
                    i*step, val, pat);
        }
    }
    free(buf);

    printf("write_and_check_32 completed\n");
}

static void run_tests(void)
{
    write_and_check_32(0xdeadbeef, 0, 512, 1);
    write_and_check_32(0xdeadbeef, 4, 512, 4);
    write_and_check_32(0xdeadbeef, 16, 512, 32);
}

static void ahci_bind_cb(void *st, errval_t err, struct ahci_binding *_binding)
{
    bind_err = err;
    if (err_is_ok(err)) {
        ahci_binding = _binding;
    }
}

static errval_t wait_bind(void **bind_p)
{
    while (!*bind_p && err_is_ok(bind_err)) {
        messages_wait_and_handle_next();
    }
    return bind_err;
}

int main(int argc, char **argv)
{
    errval_t err;
    printf("ata_rw28_test: starting\n");

    // init dma pool
    ahci_dma_pool_init(1024*1024);
    // init ahci management binding on port 0 (= first disk)
    err = ahci_init(0 /* port */, ahci_bind_cb, NULL, get_default_waitset());
    if (err_is_fail(err) || err_is_fail(err=wait_bind((void**)&ahci_binding))) {
        USER_PANIC_ERR(err, "ahci_init");
    }

    // init ata flounder binding
    err = ahci_ata_rw28_init(&ahci_ata_rw28_binding, get_default_waitset(), ahci_binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_ata_rw28_init");
    }

    ata_rw28_binding = (struct ata_rw28_binding*)&ahci_ata_rw28_binding;

    // init RPC client
    err = ata_rw28_rpc_client_init(&ata_rw28_rpc, ata_rw28_binding);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ata_rw28_rpc_client_init");
    }

    // run tests
    run_tests();

    // clean up
    ahci_close(ahci_binding, NOP_CONT);
    printf("ata_rw28_test: completed\n");
    return 0;
}
