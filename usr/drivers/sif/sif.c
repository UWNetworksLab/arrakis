/**
 * \file
 * \brief SIF driver.
 *
 * \bug Currently, this will only init the device, not drive it yet.
 */

/*
 * Copyright (c) 2007, 2008, 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <stdio.h>
#include <assert.h>
#include <inttypes.h>
#include "sif.h"
#include "crb_sif_dev.h"

static struct crb_sif_t sif;

void sif_interrupt_handler(void *arg)
{
    printf("SIF interrupt!\n");
}

void sif_init(struct device_mem *bar_info, int nr_mapped_regions)
{
    struct device_mem *bar = &bar_info[0];

    assert(nr_mapped_regions == 1);

    map_device(bar);

    printf("SIF BAR at: %" PRIxGENPADDR ", size: %lu, mapped at: %p"
           "\n", bar->paddr, bar->bytes, bar->vaddr);

    crb_sif_initialize(&sif, (void *)bar->vaddr);

    printf("sif_init: BITSID: 0x%" PRIx32 "\n", crb_sif_id0_rd(&sif));

    uint64_t grbtest = crb_sif_id1_rd(&sif) |
        ((uint64_t)crb_sif_id2_rd(&sif) << 32);
    printf("sif_init: grbtest: 0x%" PRIx64 "\n", grbtest);

    crb_sif_id2_wr(&sif, 0x35377353);
    crb_sif_id1_wr(&sif, (grbtest & 0xffffffff) + 1);

    grbtest = crb_sif_id1_rd(&sif) | ((uint64_t)crb_sif_id2_rd(&sif) << 32);
    printf("sif_init: grbtest: 0x%" PRIx64 "\n", grbtest);

    char str[256];
    crb_sif_dcsr1_pr(str, 256, &sif);
    printf("%s\n", str);

    crb_sif_dlwstat_pr(str, 256, &sif);
    printf("%s\n", str);

    crb_sif_dltrsstat_pr(str, 256, &sif);
    printf("%s\n", str);

    crb_sif_dmisccont_pr(str, 256, &sif);
    printf("%s\n", str);
}
