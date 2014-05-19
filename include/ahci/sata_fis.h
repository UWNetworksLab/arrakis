/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#ifndef _AHCI_SATA_FIS_H
#define _AHCI_SATA_FIS_H

#define SATA_FIS_TYPE_H2D	0x27 // Register FIS - Host to Device
#define SATA_FIS_TYPE_D2H	0x34 // Register FIS - Device to Host
#define SATA_FIS_TYPE_DMAA	0x39 // DMA Activate FIS - Device to Host
#define SATA_FIS_TYPE_DMAS	0x41 // DMA Setup FIS - Bi-directional
#define SATA_FIS_TYPE_DATA	0x46 // Data FIS - Bi-directional
#define SATA_FIS_TYPE_BIST	0x58 // BIST Activate FIS - Bi-directional
#define SATA_FIS_TYPE_PIO	0x5F // PIO Setup FIS - Device to Host
#define SATA_FIS_TYPE_SDB	0xA1 // Set Device Bits FIS - Device to Host

struct sata_fis_reg_h2d {
	unsigned char type;
	unsigned char specialstuff;
	unsigned char command;
	unsigned char feature;

	unsigned char lba0;
	unsigned char lba1;
	unsigned char lba2;
	unsigned char device;

	unsigned char lba3;
	unsigned char lba4;
	unsigned char lba5;
	unsigned char featureh;

	unsigned char countl;
	unsigned char counth;
	unsigned char icc;
	unsigned char control;

	unsigned char reserved[4];
};

struct sata_fis_reg_d2h {
	unsigned char type;
	unsigned char specialstuff;
	unsigned char status;
	unsigned char error;

	unsigned char lba0;
	unsigned char lba1;
	unsigned char lba2;
	unsigned char device;

	unsigned char lba3;
	unsigned char lba4;
	unsigned char lba5;
	unsigned char reserved;

	unsigned char countl;
	unsigned char counth;
	unsigned char reserved2[2];

	unsigned char reserved3[4];
};

errval_t sata_alloc_h2d_register_fis(void **fis, size_t *fis_size);

errval_t sata_set_command(void *fis, uint8_t command);
errval_t sata_set_feature(void *fis, uint8_t feature);
errval_t sata_set_lba28(void *fis, uint32_t lba);
errval_t sata_set_lba48(void *fis, uint64_t lba);
errval_t sata_set_count(void *fis, uint16_t count);

#endif // _AHCI_SATA_FIS_H
