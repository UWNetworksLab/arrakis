/**
 * \file
 * \brief Implementation of ata_rw28.if interface (to enable working vfs_fat)
 */
/*
 * Copyright (c) 2013, ETH Zurich.
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

#include <barrelfish/nameservice_client.h>

#include <if/ata_rw28_defs.h>
#include <if/ata_rw28_thc.h>
#include <thc/thc.h>

#include "mmchs.h"
#include "mmchs_debug.h"

#define SECTION_SIZE 512
#define SECTION_ROUND_UP(x) ( ((x) + (SECTION_SIZE-1))  & (~(SECTION_SIZE-1)) )

static void read_dma(struct ata_rw28_thc_service_binding_t *sv,
                     uint32_t read_size, uint32_t start_lba)
{
    size_t buffer_size = SECTION_ROUND_UP(read_size);
    MMCHS_DEBUG("%s:%d read_size=%d buffer_size=%d\n", __FUNCTION__, __LINE__, read_size, buffer_size);
    void *buffer = malloc(buffer_size);
    assert(buffer != NULL);

    uint8_t *bufptr = (uint8_t *)buffer;
    for (size_t i = 0; i < (buffer_size / SECTION_SIZE); i++) {
        MMCHS_DEBUG("%s:%d: i=%d start_lba=%d\n", __FUNCTION__, __LINE__, i, start_lba);
        errval_t err = mmchs_read_block(start_lba+i, bufptr);
        assert(err_is_ok(err));
        bufptr += SECTION_SIZE;
    }
    sv->send.read_dma(sv, buffer, buffer_size);
    free(buffer);
}

static void read_dma_block(struct ata_rw28_thc_service_binding_t *sv, uint32_t lba)
{
    MMCHS_DEBUG("%s:%d lba=%d\n", __FUNCTION__, __LINE__, lba);

    void *buffer = malloc(SECTION_SIZE);
    assert(buffer != NULL);

    errval_t err = mmchs_read_block(lba, buffer);
    assert(err_is_ok(err));

    sv->send.read_dma_block(sv, buffer, SECTION_SIZE);
    free(buffer);
}

static void write_dma(struct ata_rw28_thc_service_binding_t *sv,
                      uint8_t *buffer, size_t buffer_len, uint32_t lba)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    sv->send.write_dma(sv, LIB_ERR_NOT_IMPLEMENTED);
}

static void identify_device(struct ata_rw28_thc_service_binding_t *sv)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    sv->send.identify_device(sv, NULL, 0);
}


static void flush_cache(struct ata_rw28_thc_service_binding_t *sv)
{
    MMCHS_DEBUG("%s:%d\n", __FUNCTION__, __LINE__);
    sv->send.flush_cache(sv, SYS_ERR_OK);
}

static void service_client(struct ata_rw28_thc_service_binding_t *sv)
{
    DO_FINISH({
        bool stop = false;
        while (!stop) {
            ata_rw28_service_msg_t m;
            sv->recv_any(sv, &m, (struct ata_rw28_service_selector) {
                .read_dma = 1,
                .read_dma_block = 1,
                .write_dma = 1,
                .identify_device = 1,
                .flush_cache = 1
            });

            switch (m.msg) {

            case ata_rw28_read_dma:
                read_dma(sv, m.args.read_dma.in.read_size, m.args.read_dma.in.start_lba);
                break;

            case ata_rw28_read_dma_block:
                read_dma_block(sv, m.args.read_dma_block.in.lba);
                break;

            case ata_rw28_write_dma:
                write_dma(sv, m.args.write_dma.in.buffer, m.args.write_dma.in.buffer_size, m.args.write_dma.in.lba);
                break;

            case ata_rw28_identify_device:
                identify_device(sv);
                break;

            case ata_rw28_flush_cache:
                flush_cache(sv);
                break;

            default:
                assert(!"Unexpected message");
                break;
            }
        }
    });
}

void init_service(void)
{
    errval_t err;
    iref_t iref;
    struct ata_rw28_thc_service_binding_t *sv;
    struct ata_rw28_binding *b;
    struct ata_rw28_thc_export_info info;

    MMCHS_DEBUG("%s:%d: Starting server\n", __FUNCTION__, __LINE__);
    err = ata_rw28_thc_export(&info,
                              "mmchs",
                              get_default_waitset(),
                              IDC_EXPORT_FLAGS_DEFAULT,
                              &iref);

    MMCHS_DEBUG("%s:%d: Done export iref=%"PRIuIREF"\n", __FUNCTION__, __LINE__, iref);
    if (err_is_fail(err)) {
        DEBUG_ERR(err, "export failed");
        abort();
    }

    DO_FINISH({
        while (1) {
            MMCHS_DEBUG("%s:%d: Server waiting for connection\n", __FUNCTION__, __LINE__);
            err = ata_rw28_thc_accept(&info, &b);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "accept failed");
                abort();
            }

            sv = malloc(sizeof(struct ata_rw28_thc_service_binding_t));
            if (sv == NULL) {
                DEBUG_ERR(err, "malloc failed");
                abort();
            }

            err = ata_rw28_thc_init_service(sv, b, b);
            if (err_is_fail(err)) {
                DEBUG_ERR(err, "init failed");
                abort();
            }

            MMCHS_DEBUG("%s:%d: Got service %p\n", __FUNCTION__, __LINE__, sv);
            ASYNC({service_client(sv);});
        }
    });
}
