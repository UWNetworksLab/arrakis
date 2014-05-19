/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdlib.h>
#include <errors/errno.h>
#include <ahci/sata_fis.h>

errval_t sata_alloc_h2d_register_fis(void **fis_p, size_t *fis_size_p)
{
    struct sata_fis_reg_h2d *fis;

    fis = calloc(1, sizeof(*fis));
    if (!fis) {
        return LIB_ERR_MALLOC_FAIL;
    }
    fis->type = SATA_FIS_TYPE_H2D;

    /* Device Shadow Register layout (see: [1])
     * [  7  |  6  |  5  |  4  |  3  |  2  |  1  |  0  ]
     * [  -  |  L  |  -  | DEV | HS3 | HS2 | HS1 | HS0 ]
     *
     * L is the address mode, cleared implies CHS addressing, set, LBA addressing
     * DEV device select, cleared and set imply Device 0 and 1 resp.
     *   for SATA this should always be cleared (see: [2])
     * HS3-HS0 are bits 28-25 of the LBA 28 (not used for LBA 48, see [3])
     *
     * [1] Serial ATA NSSD Rev. 1.0 (Sept 2008), section 6.3.1
     * [2] Serial ATA Rev. 2.6 (15-February-2007), section 13.1, paragraph 2
     * [3] ATA8-ACS Rev. 3f (December 11, 2006), section 7.1.5.2
     */
    fis->device |= (1 << 6);

    *fis_p = fis;
    *fis_size_p = sizeof(*fis);

    return SYS_ERR_OK;
}

errval_t sata_set_command(void *fis, uint8_t command)
{
    uint8_t fis_type = *(uint8_t*)fis;

    if (fis_type == SATA_FIS_TYPE_H2D) {
        struct sata_fis_reg_h2d *fis_reg_h2d = fis;
        fis_reg_h2d->command = command;

        /* set bit to indicate update of command register (see [1])
         *
         * [1]: SATA Rev. 2.6 (15-February-2007), section 10.3.4
         */
        fis_reg_h2d->specialstuff |= (1 << 7);

        return SYS_ERR_OK;
    }
    else {
        return SATA_ERR_INVALID_TYPE;
    }
}

errval_t sata_set_feature(void *fis, uint8_t feature)
{
    uint8_t fis_type = *(uint8_t*)fis;

    if (fis_type == SATA_FIS_TYPE_H2D) {
        struct sata_fis_reg_h2d *fis_reg_h2d = fis;
        fis_reg_h2d->feature = feature;

        return SYS_ERR_OK;
    }
    else {
        return SATA_ERR_INVALID_TYPE;
    }
}

errval_t sata_set_lba28(void *fis, uint32_t lba)
{
    uint8_t fis_type = *(uint8_t*)fis;

    if (fis_type == SATA_FIS_TYPE_H2D) {
        struct sata_fis_reg_h2d *fis_reg_h2d = fis;
        fis_reg_h2d->lba0 = lba & 0xFF;
        fis_reg_h2d->lba1 = (lba >> 8) & 0xFF;
        fis_reg_h2d->lba2 = (lba >> 16) & 0xFF;
        fis_reg_h2d->device = (fis_reg_h2d->device & ~0x0F) | ((lba >> 24) & 0x0F);

        return SYS_ERR_OK;
    }
    else {
        return SATA_ERR_INVALID_TYPE;
    }
}

errval_t sata_set_lba48(void *fis, uint64_t lba)
{
    uint8_t fis_type = *(uint8_t*)fis;

    if (fis_type == SATA_FIS_TYPE_H2D) {
        struct sata_fis_reg_h2d *fis_reg_h2d = fis;
        fis_reg_h2d->lba0 = lba & 0xFF;
        fis_reg_h2d->lba1 = (lba >> 8) & 0xFF;
        fis_reg_h2d->lba2 = (lba >> 16) & 0xFF;
        fis_reg_h2d->device &= 0xF0; // clear bits otherwise used by lba28

        fis_reg_h2d->lba3 = (lba >> 24) & 0xFF;
        fis_reg_h2d->lba4 = (lba >> 32) & 0xFF;
        fis_reg_h2d->lba5 = (lba >> 40) & 0xFF;

        return SYS_ERR_OK;
    }
    else {
        return SATA_ERR_INVALID_TYPE;
    }
}

errval_t sata_set_count(void *fis, uint16_t count)
{
    uint8_t fis_type = *(uint8_t*)fis;

    if (fis_type == SATA_FIS_TYPE_H2D) {
        struct sata_fis_reg_h2d *fis_reg_h2d = fis;
        fis_reg_h2d->countl = count & 0xFF;
        fis_reg_h2d->counth = (count >> 8) & 0xFF;

        return SYS_ERR_OK;
    }
    else {
        return SATA_ERR_INVALID_TYPE;
    }
}
