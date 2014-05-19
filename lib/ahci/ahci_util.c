/*
 * Copyright (c) 2011 ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <barrelfish/barrelfish.h>
#include <barrelfish/memobj.h>
#include <barrelfish/vregion.h>
#include <ahci/ahci.h>
#include <ahci/ahci_dma_pool.h>
#include <dev/ahci_hba_dev.h>
#include <string.h>
#include "ahci_debug.h"
#include "ahci_internal.h"

#define AHCI_MAX_PRD_COUNT 65535

// CL is 1K, see AHCI Spec 1.3 Section 3.3.1
#define AHCI_CL_SIZE 1024

// rFIS is 256 bytes, see AHCI Spec 1.3 Section 3.3.3
#define AHCI_RFIS_SIZE 256


int ahci_find_free_command_slot(struct ahci_port_info *port)
{
    for (int i = 0; i < ahci_hba_cap_ncs_extract(port->hba_capabilities) + 1; i++) {
        if (!port->command_slots[i].in_use) {
            return i;
        }
    }

    return -1;
}

errval_t ahci_port_alloc_dma_structs(ahci_port_t *port,
        struct ahci_dma_region **command_list,
        struct ahci_dma_region **receive_fis)
{
    errval_t r;

    // allocate frame for command header list
    r = ahci_dma_region_alloc_aligned(AHCI_CL_SIZE, AHCI_CL_SIZE,
            command_list);
    assert(err_is_ok(r));
    // CLB must be aligned to 1024 bytes
    assert(((*command_list)->paddr & 0x3FF) == 0);
    ahci_port_clb_wr(port, (*command_list)->paddr);

    // allocate frame for received FIS
    r = ahci_dma_region_alloc_aligned(AHCI_RFIS_SIZE, AHCI_RFIS_SIZE,
           receive_fis);
    assert(err_is_ok(r));
    // FIS must be aligned to 256 bytes
    assert(((*receive_fis)->paddr & 0xFF) == 0);
    ahci_port_fb_wr(port, (*receive_fis)->paddr);

    return 0;
}

void ahci_port_free_dma_structs(struct ahci_port_info *port)
{
    errval_t err;
    err = ahci_dma_region_free(port->command_list);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_dma_region_free failed for port command list");
    }
    err = ahci_dma_region_free(port->receive_fis);
    if (err_is_fail(err)) {
        USER_PANIC_ERR(err, "ahci_dma_region_free failed for port receive fis");
    }
}

errval_t ahci_setup_command(int *command, struct ahci_port_info *port,
        uint8_t *fis, size_t fis_length, size_t num_prds, bool is_write)
{
    errval_t r;
    AHCI_DEBUG("ahci_setup_command(%p, %p, %p, %zu, %zu, %d): entering\n",
            command, port, fis, fis_length, num_prds, is_write);

    int command_slot = ahci_find_free_command_slot(port);
    assert(command_slot != -1); //TODO: what to do if we run out of slots? wait?
    port->command_slots[command_slot].in_use = true;

    // setup command table w/ enough entries for PRDs
    size_t prdt_size = num_prds*ahci_port_prd_size;
    size_t cmd_table_size = PRDT_OFFSET + prdt_size;
    struct ahci_dma_region *ct;
    r = ahci_dma_region_alloc(cmd_table_size, &ct);
    if (err_is_fail(r)) {
        DEBUG_ERR(r, "failed to allocate memory for command table");
        return r;
    }
    port->command_slots[command_slot].command_table = ct;


    AHCI_DEBUG("Using command slot: %d\n", command_slot);
    // insert command table into command header at pos `next_cmd'
    uint8_t *command_header_entry =
        ((uint8_t*)port->command_list->vaddr) + command_slot*ahci_port_chdr_size;
    memset(command_header_entry, 0, ahci_port_chdr_size);
    ahci_port_chdr_t chdr = (ahci_port_chdr_t) command_header_entry;
    ahci_port_chdr_cfl_insert(chdr, fis_length / 4);
    ahci_port_chdr_w_insert(chdr, is_write);
    ahci_port_chdr_ctba_insert(chdr, (uint32_t)ct->paddr);
    ahci_port_chdr_ctbau_insert(chdr, (uint32_t)(ct->paddr >> 32));

    // copy fis into command table
    memset(ct->vaddr, 0, cmd_table_size);
    ahci_dma_region_copy_in(ct, fis, 0, fis_length);

    *command = command_slot;

    AHCI_DEBUG("ahci_setup_command: exiting\n");
    return SYS_ERR_OK;
}

static void ahci_set_physical_region(ahci_port_prd_t prd,
        genpaddr_t physical_base, size_t length)
{
    //AHCI_DEBUG("ahci_set_physical_region: entering\n");
#ifdef AHCI_FIXED_PR_SIZE
    assert(length == PR_SIZE);
#else
    assert(length <= MAX_PR_SIZE);
    assert((length & 1) == 0);
#endif

    ahci_port_prd_dba_insert(prd, (uint32_t)physical_base);
    ahci_port_prd_dbau_insert(prd, (uint32_t)(physical_base >> 32));
    ahci_port_prd_dbc_insert(prd, length-1);
    //AHCI_DEBUG("ahci_set_physical_region: exiting\n");
}

static inline genpaddr_t ahci_get_sector_paddr(struct ahci_dma_region *region,
        size_t offset)
{
    return region->paddr + offset;
}

errval_t ahci_add_physical_regions(struct ahci_port_info *port,
        int command, struct ahci_dma_region *buf, size_t buflen)
{
    AHCI_DEBUG("ahci_add_physical_regions: entering\n");
    uint8_t *command_header_base =
        ((uint8_t*)port->command_list->vaddr) + command*ahci_port_chdr_size;
    ahci_port_chdr_t chdr = (ahci_port_chdr_t) command_header_base;

    // find next free prd entry in command header
    size_t prd_count = ahci_port_chdr_prdtl_extract(chdr);
    AHCI_DEBUG("prd_count = %zd\n", prd_count);
    // position for next prd
    size_t prdt_index = prd_count;
    // get base address of prd table
    uint8_t *prdt_base =
        (uint8_t*)port->command_slots[command].command_table->vaddr +
        PRDT_OFFSET;

    // max 512 bytes per PR
#ifdef AHCI_FIXED_PR_SIZE
    size_t num_prds_needed = CEIL_DIV(buflen, PR_SIZE);
#else
    size_t num_prds_needed = CEIL_DIV(buflen, MAX_PR_SIZE);
#endif
    AHCI_DEBUG("#prds = %zd\n", num_prds_needed);

    // check that remaining space is enough for requested length
    if (prdt_index + num_prds_needed >= AHCI_MAX_PRD_COUNT) {
        // TODO: error code
	AHCI_DEBUG("not enough prd space left.\n");
        return -1;
    }

    AHCI_DEBUG("PRDT base: %p\n", prdt_base);
    // initialize a prd for each region
    ahci_port_prd_t prd;
    size_t offset = 0;
    do {
        prd = (ahci_port_prd_t) prdt_base + (prdt_index * ahci_port_prd_size);
        memset(prd, 0, ahci_port_prd_size);
        genpaddr_t sector_addr = ahci_get_sector_paddr(buf, offset);
        //AHCI_DEBUG("sector_addr = 0x%lx\n", sector_addr);
#ifdef AHCI_FIXED_PR_SIZE
        ahci_set_physical_region(prd, sector_addr, PR_SIZE);
        buflen -= PR_SIZE;
        offset += PR_SIZE;
#else
        size_t chunksize = buflen < MAX_PR_SIZE ? buflen : MAX_PR_SIZE;
        ahci_set_physical_region(prd, sector_addr, chunksize);
        buflen -= chunksize;
        offset += chunksize;
#endif
        prdt_index += 1;
    } while (buflen > 0);

    // update number of PRDs for command
    ahci_port_chdr_prdtl_insert(chdr, prdt_index);

    AHCI_DEBUG("ahci_add_physical_regions: exiting\n");
    return 0;
}

#if defined(AHCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
void ahci_dump_command(int command, struct ahci_port_info *port)
{
    char buf_[4096];
    ahci_port_chdr_t chdr =
        (ahci_port_chdr_t)((uint8_t*) port->command_list->vaddr) +
        command*ahci_port_chdr_size;
    ahci_port_chdr_prtval(buf_, 4096, chdr);
    AHCI_DEBUG("\n%s\n", buf_);
    int prd_count = ahci_port_chdr_prdtl_extract(chdr);
    ahci_port_prd_t prd;
    for (int k = 0; k < prd_count; k++) {
        uint8_t *cmd_table_base = (uint8_t*) port->command_slots[command].command_table->vaddr;
        prd = (ahci_port_prd_t) (cmd_table_base + PRDT_OFFSET + k*ahci_port_prd_size);
        ahci_port_prd_prtval(buf_, 4096, prd);
        AHCI_DEBUG("\nPRD %d:\n%s\n", k, buf_);
    }
}
#else
void ahci_dump_command(int command, struct ahci_port_info *port) {}
#endif

#if defined(AHCI_LIB_DEBUG) || defined(GLOBAL_DEBUG)
void ahci_dump_rfis(struct ahci_port_info *port)
{
    printf("rfis:\n");
    for (int i = 0; i < 256; i += 4) {
        printf("%3x: 0x%02x 0x%02x 0x%02x 0x%02x\n", i,
            *(((uint8_t*)port->receive_fis->vaddr) + i),
            *(((uint8_t*)port->receive_fis->vaddr) + i + 1),
            *(((uint8_t*)port->receive_fis->vaddr) + i + 2),
            *(((uint8_t*)port->receive_fis->vaddr) + i + 3)
        );
    }
}
#else
void ahci_dump_rfis(struct ahci_port_info *port) {}
#endif
